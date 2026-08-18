pool_letters <- function(n) LETTERS[seq_len(n)]

us_championship_pool_sizes <- function(n_teams) {
  plans <- list(
    `10` = c(5, 5),
    `11` = c(6, 5),
    `12` = c(6, 6)
  )
  plan <- plans[[as.character(n_teams)]]
  if (is.null(plan)) {
    stop("The US Championship preset currently supports 10, 11, or 12 teams.", call. = FALSE)
  }
  plan
}

parse_team_roster <- function(team_lines) {
  cleaned <- stringr::str_squish(team_lines)
  cleaned <- cleaned[cleaned != ""]
  players <- stringr::str_split(cleaned, "\\s*(?:/|\\+|&|\\band\\b)\\s*", n = 2)

  tibble::tibble(
    team_id = seq_along(cleaned),
    team = cleaned,
    player1 = purrr::map_chr(players, ~ .x[[1]] %||% ""),
    player2 = purrr::map_chr(players, ~ .x[[2]] %||% "")
  )
}

seed_teams <- function(teams) {
  teams %>%
    dplyr::arrange(.data$seed, .data$team_id) %>%
    dplyr::mutate(team_id = dplyr::row_number())
}

seed_teams_from_rankings <- function(teams, rankings) {
  player_key <- function(x) stringr::str_to_lower(stringr::str_squish(x))
  rankings_keyed <- rankings %>%
    dplyr::mutate(player_key = player_key(.data$player)) %>%
    dplyr::select("player_key", "player_rank", "ranking_points")

  teams %>%
    dplyr::mutate(
      player1_key = player_key(.data$player1),
      player2_key = player_key(.data$player2)
    ) %>%
    dplyr::left_join(rankings_keyed, by = c("player1_key" = "player_key")) %>%
    dplyr::rename(player1_rank = "player_rank", player1_points = "ranking_points") %>%
    dplyr::left_join(rankings_keyed, by = c("player2_key" = "player_key")) %>%
    dplyr::rename(player2_rank = "player_rank", player2_points = "ranking_points") %>%
    dplyr::mutate(
      player1_points = tidyr::replace_na(.data$player1_points, 0),
      player2_points = tidyr::replace_na(.data$player2_points, 0),
      team_points = .data$player1_points + .data$player2_points,
      missing_ranking = is.na(.data$player1_rank) | is.na(.data$player2_rank)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$team_points), .data$missing_ranking, .data$team) %>%
    dplyr::mutate(seed = dplyr::row_number(), team_id = dplyr::row_number()) %>%
    dplyr::select(
      "team_id", "seed", "team", "player1", "player2",
      "player1_rank", "player2_rank", "team_points", "missing_ranking"
    )
}

snake_seed_us_pools <- function(teams) {
  teams <- seed_teams(teams)
  sizes <- us_championship_pool_sizes(nrow(teams))
  pool_count <- length(sizes)
  pools <- vector("list", pool_count)
  names(pools) <- paste("Pool", pool_letters(pool_count))
  for (i in seq_along(pools)) pools[[i]] <- teams[0, ]

  for (block_start in seq(1, nrow(teams), by = pool_count)) {
    block <- block_start:min(block_start + pool_count - 1, nrow(teams))
    block_index <- ((block_start - 1) / pool_count) + 1
    destinations <- if (block_index %% 2 == 1) seq_len(pool_count) else rev(seq_len(pool_count))

    for (j in seq_along(block)) {
      target <- destinations[[j]]
      if (nrow(pools[[target]]) >= sizes[[target]]) {
        available <- destinations[sizes[destinations] > purrr::map_int(pools[destinations], nrow)]
        target <- available[[1]]
      }
      pools[[target]] <- dplyr::bind_rows(pools[[target]], teams[block[[j]], ])
    }
  }

  purrr::imap(pools, function(pool, nm) {
    pool$pool <- nm
    pool$pool_position <- seq_len(nrow(pool))
    pool
  })
}

round_robin_pairs <- function(n) {
  players <- seq_len(n)
  if (n %% 2 == 1) players <- c(players, NA_integer_)
  rounds <- length(players) - 1
  half <- length(players) / 2
  out <- vector("list", rounds)

  for (round in seq_len(rounds)) {
    left <- players[seq_len(half)]
    right <- rev(players[(half + 1):length(players)])
    games <- tibble::tibble(team1 = left, team2 = right) %>%
      dplyr::filter(!is.na(.data$team1), !is.na(.data$team2))
    if (round %% 2 == 0) {
      games <- tibble::tibble(team1 = games$team2, team2 = games$team1)
    }
    out[[round]] <- games %>% dplyr::mutate(round = round, .before = 1)
    players <- c(players[[1]], players[[length(players)]], players[2:(length(players) - 1)])
  }

  dplyr::bind_rows(out)
}

make_pool_schedule <- function(pool, stage, court_offset = 0, game_offset = 0, day = 1) {
  rr <- round_robin_pairs(nrow(pool))
  rr %>%
    dplyr::mutate(
      game_id = dplyr::row_number() + game_offset,
      game_key = paste0("P", .data$game_id),
      day = day,
      stage = stage,
      group = pool$pool[[1]],
      label = "Pool play",
      court = dplyr::row_number() - ave(dplyr::row_number(), .data$round, FUN = min) + 1 + court_offset,
      teamA_id = pool$team_id[.data$team1],
      teamB_id = pool$team_id[.data$team2],
      teamA = pool$team[.data$team1],
      teamB = pool$team[.data$team2],
      sourceA = NA_character_,
      sourceB = NA_character_
    ) %>%
    dplyr::select(
      "game_id", "game_key", "day", "stage", "group",
      "round", "court", "label", "teamA_id", "teamB_id",
      "teamA", "teamB", "sourceA", "sourceB"
    )
}

make_us_pool_stage <- function(teams) {
  pools <- snake_seed_us_pools(teams)
  game_offset <- 0
  court_offset <- 0
  schedules <- list()

  for (i in seq_along(pools)) {
    schedules[[i]] <- make_pool_schedule(pools[[i]], "Pool Play", court_offset, game_offset, day = 1)
    game_offset <- max(schedules[[i]]$game_id)
    court_offset <- court_offset + max(schedules[[i]]$court - court_offset)
  }

  list(games = dplyr::bind_rows(schedules), pools = pools)
}

game_results <- function(games, scores) {
  games %>%
    dplyr::left_join(scores, by = "game_id") %>%
    dplyr::mutate(
      diff = dplyr::if_else(is.na(.data$scoreA) | is.na(.data$scoreB), NA_real_, .data$scoreA - .data$scoreB),
      winner_id = dplyr::case_when(.data$diff > 0 ~ .data$teamA_id, .data$diff < 0 ~ .data$teamB_id, TRUE ~ NA_integer_),
      loser_id = dplyr::case_when(.data$diff > 0 ~ .data$teamB_id, .data$diff < 0 ~ .data$teamA_id, TRUE ~ NA_integer_)
    )
}

games_complete <- function(scores) {
  nrow(scores) > 0 && all(!is.na(scores$scoreA), !is.na(scores$scoreB))
}

team_standings <- function(teams, games, scores, group_name = NULL) {
  scoped_games <- games
  if (!is.null(group_name)) scoped_games <- scoped_games %>% dplyr::filter(.data$group == group_name)
  results <- game_results(scoped_games, scores)

  records <- dplyr::bind_rows(
    results %>% dplyr::transmute(team_id = .data$teamA_id, pf = .data$scoreA, pa = .data$scoreB, result = dplyr::case_when(.data$diff > 0 ~ "W", .data$diff < 0 ~ "L", TRUE ~ NA_character_)),
    results %>% dplyr::transmute(team_id = .data$teamB_id, pf = .data$scoreB, pa = .data$scoreA, result = dplyr::case_when(.data$diff < 0 ~ "W", .data$diff > 0 ~ "L", TRUE ~ NA_character_))
  ) %>%
    dplyr::group_by(.data$team_id) %>%
    dplyr::summarise(
      played = sum(!is.na(.data$result)),
      wins = sum(.data$result == "W", na.rm = TRUE),
      losses = sum(.data$result == "L", na.rm = TRUE),
      points_for = sum(.data$pf, na.rm = TRUE),
      points_against = sum(.data$pa, na.rm = TRUE),
      point_diff = .data$points_for - .data$points_against,
      .groups = "drop"
    )

  h2h <- function(tied_ids) {
    if (length(tied_ids) <= 1) return(rep(0, length(tied_ids)))
    tied_results <- results %>%
      dplyr::filter(.data$teamA_id %in% tied_ids, .data$teamB_id %in% tied_ids, !is.na(.data$winner_id))
    purrr::map_int(tied_ids, ~ sum(tied_results$winner_id == .x))
  }

  base <- teams %>%
    dplyr::select("team_id", "seed", "team") %>%
    dplyr::filter(.data$team_id %in% unique(c(scoped_games$teamA_id, scoped_games$teamB_id))) %>%
    dplyr::left_join(records, by = "team_id") %>%
    dplyr::mutate(dplyr::across(c("played", "wins", "losses", "points_for", "points_against", "point_diff"), ~ tidyr::replace_na(.x, 0)))

  base %>%
    dplyr::group_by(.data$wins) %>%
    dplyr::mutate(head_to_head_wins = h2h(.data$team_id)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(.data$wins), dplyr::desc(.data$head_to_head_wins), dplyr::desc(.data$point_diff), .data$seed) %>%
    dplyr::mutate(place = dplyr::row_number(), win_pct = dplyr::if_else(.data$played > 0, .data$wins / .data$played, NA_real_)) %>%
    dplyr::select("place", "team", "seed", "played", "wins", "losses", "win_pct", "point_diff", "points_for", "points_against", "head_to_head_wins", "team_id")
}

seed_top_eight <- function(teams, pool_stage, pool_scores) {
  pool_rankings <- purrr::imap_dfr(pool_stage$pools, function(pool, pool_name) {
    team_standings(teams, pool_stage$games, pool_scores, pool_name) %>%
      dplyr::mutate(pool = pool_name, pool_place = .data$place)
  })

  pool_rankings %>%
    dplyr::arrange(.data$pool_place, dplyr::desc(.data$wins), dplyr::desc(.data$point_diff), .data$seed) %>%
    dplyr::mutate(championship_seed = dplyr::row_number()) %>%
    dplyr::arrange(.data$championship_seed)
}

slot_label <- function(type, ref) {
  ifelse(type == "seed", paste("Seed", ref), paste(stringr::str_to_title(type), ref))
}

make_dependency_games <- function(template, teams_by_seed, game_offset, stage, group, day, court_start = 1) {
  slotA_seed <- suppressWarnings(as.integer(template$slotA_ref))
  slotB_seed <- suppressWarnings(as.integer(template$slotB_ref))
  template %>%
    dplyr::mutate(
      game_id = game_offset + dplyr::row_number(),
      day = day,
      stage = stage,
      group = group,
      court = court_start + ((dplyr::row_number() - 1) %% max(1, min(4, nrow(template)))),
      teamA_id = dplyr::if_else(.data$slotA_type == "seed", teams_by_seed$team_id[match(slotA_seed, teams_by_seed$championship_seed)], NA_integer_),
      teamB_id = dplyr::if_else(.data$slotB_type == "seed", teams_by_seed$team_id[match(slotB_seed, teams_by_seed$championship_seed)], NA_integer_),
      teamA = dplyr::if_else(.data$slotA_type == "seed", teams_by_seed$team[match(slotA_seed, teams_by_seed$championship_seed)], NA_character_),
      teamB = dplyr::if_else(.data$slotB_type == "seed", teams_by_seed$team[match(slotB_seed, teams_by_seed$championship_seed)], NA_character_),
      sourceA = dplyr::if_else(.data$slotA_type == "seed", NA_character_, slot_label(.data$slotA_type, .data$slotA_ref)),
      sourceB = dplyr::if_else(.data$slotB_type == "seed", NA_character_, slot_label(.data$slotB_type, .data$slotB_ref))
    ) %>%
    dplyr::select(
      "game_id", "game_key", "day", "stage", "group",
      "round", "court", "label", "teamA_id", "teamB_id",
      "teamA", "teamB", "sourceA", "sourceB",
      "slotA_type", "slotA_ref", "slotB_type", "slotB_ref"
    )
}

make_top8_double_elim <- function(teams, seeded_order, game_offset = 0) {
  top8 <- seeded_order %>% dplyr::slice_head(n = 8)
  template <- tibble::tribble(
    ~game_key, ~round, ~label, ~slotA_type, ~slotA_ref, ~slotB_type, ~slotB_ref,
    "W1", 1, "Winners round 1", "seed", "1", "seed", "8",
    "W2", 1, "Winners round 1", "seed", "4", "seed", "5",
    "W3", 1, "Winners round 1", "seed", "3", "seed", "6",
    "W4", 1, "Winners round 1", "seed", "2", "seed", "7",
    "L1", 2, "Elimination round 1", "loser", "W1", "loser", "W2",
    "L2", 2, "Elimination round 1", "loser", "W3", "loser", "W4",
    "W5", 2, "Winners semifinal", "winner", "W1", "winner", "W2",
    "W6", 2, "Winners semifinal", "winner", "W3", "winner", "W4",
    "L3", 3, "Elimination round 2", "winner", "L1", "loser", "W6",
    "L4", 3, "Elimination round 2", "winner", "L2", "loser", "W5",
    "W7", 3, "Winners final", "winner", "W5", "winner", "W6",
    "L5", 4, "Elimination semifinal", "winner", "L3", "winner", "L4",
    "L6", 5, "Elimination final", "winner", "L5", "loser", "W7",
    "GF", 6, "Grand final", "winner", "W7", "winner", "L6",
    "GFR", 7, "Bracket reset if needed", "loser", "GF", "winner", "GF"
  )

  make_dependency_games(template, top8, game_offset, "Championship Bracket", "Top 8 Double Elimination", 2)
}

make_bottom_placement <- function(teams, seeded_order, game_offset = 0) {
  bottom <- seeded_order %>% dplyr::filter(.data$championship_seed > 8)
  n <- nrow(bottom)
  if (n == 0) return(NULL)

  bottom <- bottom %>% dplyr::mutate(championship_seed = .data$championship_seed - 8)

  if (n == 2) {
    template <- tibble::tribble(
      ~game_key, ~round, ~label, ~slotA_type, ~slotA_ref, ~slotB_type, ~slotB_ref,
      "P9", 1, "9th place match", "seed", "1", "seed", "2"
    )
  } else if (n == 3) {
    return(make_pool_schedule(
      bottom %>% dplyr::mutate(pool = "Places 9-11", pool_position = dplyr::row_number()),
      "Placement", game_offset = game_offset, day = 2
    ) %>% dplyr::mutate(group = "Places 9-11", label = "Placement round robin"))
  } else {
    template <- tibble::tribble(
      ~game_key, ~round, ~label, ~slotA_type, ~slotA_ref, ~slotB_type, ~slotB_ref,
      "P1", 1, "Placement semifinal", "seed", "1", "seed", "4",
      "P2", 1, "Placement semifinal", "seed", "2", "seed", "3",
      "P9", 2, "9th place match", "winner", "P1", "winner", "P2",
      "P11", 2, "11th place match", "loser", "P1", "loser", "P2"
    )
  }

  make_dependency_games(template, bottom, game_offset, "Placement", paste0("Places 9-", 8 + n), 2)
}

resolve_dependency_games <- function(games, scores) {
  if (!all(c("slotA_type", "slotA_ref", "slotB_type", "slotB_ref") %in% names(games))) return(games)

  out <- games
  results <- game_results(out, scores)
  team_lookup <- out %>%
    dplyr::select("teamA_id", "teamA") %>%
    dplyr::rename(team_id = "teamA_id", team = "teamA") %>%
    dplyr::bind_rows(out %>% dplyr::select("teamB_id", "teamB") %>% dplyr::rename(team_id = "teamB_id", team = "teamB")) %>%
    dplyr::filter(!is.na(.data$team_id)) %>%
    dplyr::distinct(.data$team_id, .keep_all = TRUE)

  resolve_slot <- function(type, ref) {
    if (type == "seed") return(NA_integer_)
    source <- results %>% dplyr::filter(.data$game_key == ref)
    if (nrow(source) == 0) return(NA_integer_)
    if (type == "winner") source$winner_id[[1]] else source$loser_id[[1]]
  }

  for (pass in seq_len(nrow(out))) {
    changed <- FALSE
    results <- game_results(out, scores)
    for (i in seq_len(nrow(out))) {
      if (is.na(out$teamA_id[[i]]) && out$slotA_type[[i]] != "seed") {
        resolved_id <- resolve_slot(out$slotA_type[[i]], out$slotA_ref[[i]])
        if (!is.na(resolved_id)) {
          out$teamA_id[[i]] <- resolved_id
          changed <- TRUE
        }
      }
      if (is.na(out$teamB_id[[i]]) && out$slotB_type[[i]] != "seed") {
        resolved_id <- resolve_slot(out$slotB_type[[i]], out$slotB_ref[[i]])
        if (!is.na(resolved_id)) {
          out$teamB_id[[i]] <- resolved_id
          changed <- TRUE
        }
      }
    }
    if (!changed) break
  }

  results <- game_results(out, scores)
  reset_row <- which(out$game_key == "GFR")
  if (length(reset_row) == 1) {
    gf <- results %>% dplyr::filter(.data$game_key == "GF")
    winners_final <- results %>% dplyr::filter(.data$game_key == "W7")
    reset_needed <- nrow(gf) == 1 &&
      nrow(winners_final) == 1 &&
      !is.na(gf$winner_id[[1]]) &&
      !is.na(winners_final$winner_id[[1]]) &&
      gf$winner_id[[1]] != winners_final$winner_id[[1]]
    if (!reset_needed) {
      out$teamA_id[[reset_row]] <- NA_integer_
      out$teamB_id[[reset_row]] <- NA_integer_
    }
  }

  out %>%
    dplyr::select(-"teamA", -"teamB") %>%
    dplyr::left_join(team_lookup %>% dplyr::rename(teamA_id = "team_id", teamA = "team"), by = "teamA_id") %>%
    dplyr::left_join(team_lookup %>% dplyr::rename(teamB_id = "team_id", teamB = "team"), by = "teamB_id")
}

make_us_championship_preset <- function(teams) {
  if (!"seed" %in% names(teams)) {
    teams <- teams %>% dplyr::mutate(seed = dplyr::row_number(), .after = .data$team_id)
  }
  pool_stage <- make_us_pool_stage(teams)
  list(teams = seed_teams(teams), pool_stage = pool_stage)
}
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

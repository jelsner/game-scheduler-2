ddc_rankings_api_url <- "https://doubledisccourt.com/data/ddc.php"

read_ddc_api_json <- function(params) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Install jsonlite to fetch DDC rankings.", call. = FALSE)
  }

  if (requireNamespace("httr2", quietly = TRUE)) {
    response <- httr2::request(ddc_rankings_api_url) %>%
      httr2::req_url_query(!!!params) %>%
      httr2::req_headers(
        accept = "*/*",
        referer = "https://doubledisccourt.com/results/rankings.html",
        `user-agent` = "Mozilla/5.0",
        `x-requested-with` = "XMLHttpRequest"
      ) %>%
      httr2::req_perform()
    return(jsonlite::fromJSON(httr2::resp_body_string(response)))
  }

  query <- paste(paste0(names(params), "=", utils::URLencode(as.character(params), reserved = TRUE)), collapse = "&")
  jsonlite::fromJSON(paste0(ddc_rankings_api_url, "?", query))
}

fetch_ddc_player_rankings <- function(year = as.integer(format(Sys.Date(), "%Y")), division = "O") {
  rankings <- read_ddc_api_json(list(op = "get-rankings", year = year))
  players <- read_ddc_api_json(list(op = "load-player"))
  target_division <- division


  if (is.null(rankings) || is.null(players)) {
    stop("The DDC rankings API returned no data.", call. = FALSE)
  }

  player_names <- stats::setNames(players$name, players$id)

  rankings %>%
    dplyr::filter(.data$division == target_division) %>%
    dplyr::transmute(
      player_id = as.character(.data$player_id),
      player = unname(player_names[as.character(.data$player_id)]),
      player_rank = as.integer(.data$rank),
      ranking_points = as.numeric(.data$points),
      division = .data$division,
      ranking_year = year
    ) %>%
    dplyr::filter(!is.na(.data$player), .data$player != "") %>%
    dplyr::arrange(.data$player_rank)
}

ranking_choice_labels <- function(rankings) {
  stats::setNames(
    rankings$player_id,
    paste0(
      rankings$player,
      " (#", rankings$player_rank,
      ", ", format(round(rankings$ranking_points, 2), nsmall = 2), " pts)"
    )
  )
}

seed_teams_from_player_ids <- function(player1_ids, player2_ids, rankings) {
  lookup <- rankings %>%
    dplyr::mutate(player_id = as.character(.data$player_id)) %>%
    dplyr::select("player_id", "player", "player_rank", "ranking_points")

  team_pairs <- tibble::tibble(
    player1_id = as.character(player1_ids),
    player2_id = as.character(player2_ids)
  ) %>%
    dplyr::mutate(team_id = dplyr::row_number(), .before = 1) %>%
    dplyr::left_join(lookup, by = c("player1_id" = "player_id")) %>%
    dplyr::rename(player1 = "player", player1_rank = "player_rank", player1_points = "ranking_points") %>%
    dplyr::left_join(lookup, by = c("player2_id" = "player_id")) %>%
    dplyr::rename(player2 = "player", player2_rank = "player_rank", player2_points = "ranking_points")

  team_pairs %>%
    dplyr::mutate(
      player1_points = tidyr::replace_na(.data$player1_points, 0),
      player2_points = tidyr::replace_na(.data$player2_points, 0),
      player1 = tidyr::replace_na(.data$player1, ""),
      player2 = tidyr::replace_na(.data$player2, ""),
      team = stringr::str_squish(paste(.data$player1, "/", .data$player2)),
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

fetch_latest_ddc_player_rankings <- function(
  start_year = as.integer(format(Sys.Date(), "%Y")),
  earliest_year = 2018,
  division = "O"
) {
  errors <- character()
  for (year in seq(start_year, earliest_year)) {
    result <- tryCatch(
      fetch_ddc_player_rankings(year = year, division = division),
      error = function(error) {
        errors[[as.character(year)]] <<- conditionMessage(error)
        NULL
      }
    )
    if (!is.null(result) && nrow(result) > 0) return(result)
  }
  stop(
    "Could not fetch DDC rankings for any year from ", start_year, " back to ", earliest_year,
    ". Last error: ", tail(errors, 1),
    call. = FALSE
  )
}

rank_team_lines_from_ddc_rankings <- function(team_lines, year = as.integer(format(Sys.Date(), "%Y")), division = "O") {
  rankings <- fetch_latest_ddc_player_rankings(start_year = year, division = division)
  teams <- parse_team_roster(team_lines)
  seed_teams_from_rankings(teams, rankings)
}

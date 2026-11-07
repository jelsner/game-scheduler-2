# 2026 tournament rules. All scoring, PDFs, and standings use this model.
championship_teams <- function() {
  teams <- read.csv("data/teams-2026.csv", stringsAsFactors = FALSE)
  destinations <- c("A", "B", "C", "C", "B", "A", "A", "B", "C", "C", "B", "A", "A")
  teams$pool <- destinations
  teams$pool_seed <- ave(teams$seed, teams$pool, FUN = seq_along)
  teams
}

roster_key_13 <- function(teams = championship_teams()) {
  paste(paste(teams$seed, teams$team, sep = "="), collapse = "|")
}

empty_tournament <- function() list(version = 1L, results = list(), ties = list())

pool_matches_13 <- function(teams = championship_teams()) {
  do.call(rbind, lapply(c("A", "B", "C"), function(p) {
    pool <- teams[teams$pool == p, ]
    rr <- round_robin_pairs(nrow(pool))
    top <- (rr$team1 == 1 & rr$team2 == 2) | (rr$team1 == 2 & rr$team2 == 1)
    rr <- rr[order(rr$round, top), ]
    data.frame(key = paste0("P", p, seq_len(nrow(rr))), stage = "Pool play", pool = p,
      wave = rr$round, court = rep(seq_len(2), nrow(rr) / 2) + match(p, c("A", "B", "C")) * 2 - 2,
      sourceA = paste("Pool seed", rr$team1), sourceB = paste("Pool seed", rr$team2),
      teamA_id = pool$team_id[rr$team1], teamB_id = pool$team_id[rr$team2],
      target = 21L, best_of = 1L, stringsAsFactors = FALSE)
  }))
}

bracket_matches_13 <- function() {
  pairs <- list(c("S8", "S9"), c("S4", "S13"), c("S5", "S12"), c("S6", "S11"), c("S7", "S10"),
    c("S1", "W1"), c("W2", "W3"), c("S3", "W4"), c("W5", "S2"),
    c("W6", "W7"), c("W8", "W9"), c("W10", "W11"), c("L2", "L3"),
    c("L7", "L1"), c("L6", "W13"), c("L9", "L4"), c("L8", "L5"),
    c("W14", "W15"), c("W16", "W17"), c("W18", "L11"), c("W19", "L10"),
    c("W20", "W21"), c("W22", "L12"), c("W12", "W23"), c("W24", "L24"),
    c("M1", "M4"), c("M2", "M3"), c("WP1", "WP2"), c("LP1", "LP2"), c("L18", "L19"), c("L20", "L21"))
  waves <- list(1:5, c(6:9, 13), c(10, 11, 14:17), c(12, 18, 19), c(20, 21), 22, 23, 24, 25)
  assignments <- do.call(rbind, lapply(seq_along(waves), function(w) data.frame(g = waves[[w]], wave = w, court = seq_along(waves[[w]]))))
  assignments <- assignments[match(1:25, assignments$g), ]
  data.frame(key = c(paste0("G", 1:25), paste0("P", 1:6)),
    stage = c(rep("Double elimination", 25), rep("Placement", 6)), pool = "",
    wave = c(assignments$wave, 4, 4, 5, 5, 5, 6), court = c(assignments$court, 4, 5, 3, 4, 5, 2),
    sourceA = vapply(pairs, `[`, "", 1), sourceB = vapply(pairs, `[`, "", 2),
    teamA_id = NA_integer_, teamB_id = NA_integer_,
    target = rep(21L, 31), best_of = c(rep(1L, 23), 3L, rep(1L, 7)),
    stringsAsFactors = FALSE)
}

# A rally awards one or two points. The final rally must be able to reach
# this win-by-two result from a score at which the game was still undecided.
valid_game_13 <- function(a, b, target = 21L) {
  length(a) == 1L && length(b) == 1L && is.numeric(a) && is.numeric(b) &&
    !is.na(a) && !is.na(b) && is.finite(a) && is.finite(b) && a >= 0 && b >= 0 &&
    a == floor(a) && b == floor(b) && max(a, b) >= target && abs(a - b) >= 2 &&
    (max(a, b) - 2 < target || abs(a - b) - 2 < 2)
}

score_outcome_13 <- function(scores, target = 21L, best_of = 1L) {
  if (!is.matrix(scores)) scores <- as.matrix(scores)
  if (!is.numeric(scores) || ncol(scores) != 2L || !nrow(scores) || anyNA(scores)) stop("Enter both scores for every played game.", call. = FALSE)
  if (nrow(scores) > best_of) stop("Too many games for this match.", call. = FALSE)
  wins <- c(0L, 0L)
  for (i in seq_len(nrow(scores))) {
    if (max(wins) >= (best_of + 1) / 2) stop("The series was already decided; remove the extra game.", call. = FALSE)
    if (!valid_game_13(scores[i, 1], scores[i, 2], target)) stop(paste0("Game ", i, ": use a final score to ", target, ", win by two (no cap). A final double may add two points, but play must stop once the game is won."), call. = FALSE)
    winner <- if (scores[i, 1] > scores[i, 2]) 1L else 2L
    wins[winner] <- wins[winner] + 1L
  }
  if (max(wins) < (best_of + 1) / 2) stop("Enter the complete match; the championship needs two game wins.", call. = FALSE)
  list(side = which.max(wins), wins = wins, score = if (best_of == 1L) paste(scores[1, ], collapse = " - ") else
    paste0(paste(wins, collapse = " - "), " series (", paste(apply(scores, 1, paste, collapse = "-"), collapse = ", "), ")"))
}

attach_result_13 <- function(game, results) {
  game$status <- if (is.na(game$teamA_id) || is.na(game$teamB_id)) "Waiting" else "Ready"
  game$winner_id <- game$loser_id <- NA_integer_
  game$score <- ""
  record <- results[[game$key]]
  if (game$status == "Ready" && !is.null(record) &&
      identical(as.integer(c(record$teamA_id, record$teamB_id)), as.integer(c(game$teamA_id, game$teamB_id)))) {
    out <- score_outcome_13(record$scores, game$target, game$best_of)
    ids <- c(game$teamA_id, game$teamB_id)
    game$winner_id <- ids[out$side]; game$loser_id <- ids[3 - out$side]
    game$status <- "Complete"; game$score <- out$score
  }
  game
}

# WFDF 206.03 comparison sequence, without forfeits (not entered as scores).
# A tie remaining after all comparisons blocks reseeding until the director resolves it.
pool_table_13 <- function(teams, games, results, tie_order = integer()) {
  t <- teams
  t$played <- t$wins <- t$losses <- t$points_for <- t$points_against <- 0L
  done <- games[games$status == "Complete", ]
  for (i in seq_len(nrow(done))) {
    g <- done[i, ]; sc <- results[[g$key]]$scores
    for (side in 1:2) {
      id <- c(g$teamA_id, g$teamB_id)[side]; j <- match(id, t$team_id)
      t$played[j] <- t$played[j] + 1L
      t$wins[j] <- t$wins[j] + as.integer(g$winner_id == id)
      t$losses[j] <- t$losses[j] + as.integer(g$loser_id == id)
      t$points_for[j] <- t$points_for[j] + sc[1, side]
      t$points_against[j] <- t$points_against[j] + sc[1, 3 - side]
    }
  }
  t$point_diff <- t$points_for - t$points_against
  t$tied_wins <- t$tied_diff <- t$above_wins <- t$above_diff <- 0L
  for (w in unique(t$wins)) {
    tied <- t$team_id[t$wins == w]; above <- t$team_id[t$wins > w]
    for (id in tied) {
      j <- match(id, t$team_id)
      for (opponents in list(tied, above)) {
        scoped <- done[(done$teamA_id == id & done$teamB_id %in% opponents) |
                         (done$teamB_id == id & done$teamA_id %in% opponents), ]
        net <- 0L
        for (i in seq_len(nrow(scoped))) {
          g <- scoped[i, ]; sc <- results[[g$key]]$scores[1, ]
          net <- net + if (g$teamA_id == id) sc[1] - sc[2] else sc[2] - sc[1]
        }
        if (identical(opponents, tied)) {
          t$tied_wins[j] <- sum(scoped$winner_id == id); t$tied_diff[j] <- net
        } else {
          t$above_wins[j] <- sum(scoped$winner_id == id); t$above_diff[j] <- net
        }
      }
    }
  }
  columns <- c("wins", "tied_wins", "tied_diff", "above_wins", "above_diff", "point_diff")
  metric <- do.call(paste, c(t[columns], sep = ":"))
  t$unresolved <- duplicated(metric) | duplicated(metric, fromLast = TRUE)
  t$manual <- match(t$seed, tie_order)
  t$resolved <- !t$unresolved | !is.na(t$manual)
  # Stable original-seed ordering is display only, never an automatic tiebreak.
  o <- do.call(order, c(lapply(t[columns], function(x) -x), list(t$manual, t$seed, na.last = TRUE)))
  t <- t[o, ]; t$pool_finish <- seq_len(nrow(t))
  complete <- nrow(done) == nrow(games)
  t$earned_seed <- if (complete && all(t$resolved)) sort(t$seed) else NA_integer_
  if (!complete || !all(t$resolved)) t$pool_finish <- NA_integer_
  t
}

tournament_state_13 <- function(snapshot = empty_tournament(), teams = championship_teams()) {
  records <- snapshot$results
  pool <- pool_matches_13(teams)
  pool <- do.call(rbind, lapply(seq_len(nrow(pool)), function(i) attach_result_13(pool[i, ], records)))
  tables <- lapply(c("A", "B", "C"), function(p) pool_table_13(teams[teams$pool == p, ], pool[pool$pool == p, ], records, snapshot$ties[[p]] %||% integer()))
  tally <- do.call(rbind, tables); rownames(tally) <- NULL
  seeded <- all(!is.na(tally$earned_seed))
  bracket <- bracket_matches_13()
  resolved <- list()
  mini <- integer()
  get_slot <- function(s) {
    type <- substr(s, 1, 1); ref <- substring(s, 2)
    if (type == "S") return(if (seeded) tally$team_id[match(as.integer(ref), tally$earned_seed)] else NA_integer_)
    if (type == "M") return(if (length(mini)) mini[as.integer(ref)] else NA_integer_)
    key <- if (startsWith(ref, "P")) ref else paste0("G", ref)
    g <- resolved[[key]]
    if (is.null(g) || g$status != "Complete") return(NA_integer_)
    if (type == "W") g$winner_id else g$loser_id
  }
  # Dependency order: main games first, then placement sources.
  for (i in seq_len(nrow(bracket))) {
    g <- bracket[i, ]
    if (g$key == "P1" && all(vapply(paste0("G", 14:17), function(k) resolved[[k]]$status == "Complete", TRUE))) {
      ids <- vapply(paste0("G", 14:17), function(k) resolved[[k]]$loser_id, 1L)
      mini <- ids[order(tally$earned_seed[match(ids, tally$team_id)])]
    }
    g$teamA_id <- get_slot(g$sourceA); g$teamB_id <- get_slot(g$sourceB)
    g <- attach_result_13(g, records)
    if (g$key == "G25") {
      gf <- resolved$G24
      if (gf$status != "Complete" || gf$winner_id == resolved$G12$winner_id) {
        g$teamA_id <- g$teamB_id <- g$winner_id <- g$loser_id <- NA_integer_
        g$score <- ""; g$status <- if (gf$status == "Complete") "Not needed" else "Conditional"
      }
    }
    resolved[[g$key]] <- g
  }
  bracket <- do.call(rbind, resolved)
  all_games <- rbind(pool, bracket)
  all_games$teamA <- teams$team[match(all_games$teamA_id, teams$team_id)]
  all_games$teamB <- teams$team[match(all_games$teamB_id, teams$team_id)]
  final_ids <- rep(NA_integer_, 13)
  assign_finish <- function(key, win_place = NULL, lose_place = NULL) {
    g <- resolved[[key]]
    if (g$status == "Complete") {
      if (!is.null(win_place)) final_ids[win_place] <<- g$winner_id
      if (!is.null(lose_place)) final_ids[lose_place] <<- g$loser_id
    }
  }
  assign_finish("G13", lose_place = 13); assign_finish("P4", 11, 12); assign_finish("P3", 9, 10)
  assign_finish("P5", 7, 8); assign_finish("P6", 5, 6); assign_finish("G22", lose_place = 4); assign_finish("G23", lose_place = 3)
  if (resolved$G25$status == "Not needed") assign_finish("G24", 1, 2) else assign_finish("G25", 1, 2)
  final <- data.frame(place = 1:13, team_id = final_ids, team = teams$team[match(final_ids, teams$team_id)], stringsAsFactors = FALSE)
  played <- vapply(teams$team_id, function(id) sum(vapply(which(all_games$status == "Complete"), function(i) {
    g <- all_games[i, ]; if (id %in% c(g$teamA_id, g$teamB_id)) nrow(records[[g$key]]$scores) else 0L
  }, 1L)), 1L)
  final$games_played <- played[match(final$team_id, teams$team_id)]
  invalid <- setdiff(names(records), all_games$key[all_games$status == "Complete"])
  list(teams = teams, games = all_games, tally = tally, final = final, seeded = seeded, invalidated = invalid, mini = mini)
}

# Bind a saved result to its actual participants. Correcting an upstream result
# removes obsolete downstream results permanently, including a no-longer-needed reset.
save_result_13 <- function(snapshot, key, scores = NULL) {
  state <- tournament_state_13(snapshot)
  g <- state$games[state$games$key == key, ]
  if (nrow(g) != 1L) stop("Unknown match.", call. = FALSE)
  if (is.null(scores)) {
    snapshot$results[[key]] <- NULL
  } else {
    if (!g$status %in% c("Ready", "Complete")) stop("Complete this match's prerequisites first.", call. = FALSE)
    score_outcome_13(scores, g$target, g$best_of)
    snapshot$results[[key]] <- list(teamA_id = g$teamA_id, teamB_id = g$teamB_id, scores = unname(as.matrix(scores)))
  }
  if (g$stage == "Pool play") snapshot$ties[[g$pool]] <- NULL
  after <- tournament_state_13(snapshot)
  removed <- after$invalidated
  snapshot$results[removed] <- NULL
  attr(snapshot, "cleared") <- removed
  snapshot
}

save_tie_order_13 <- function(snapshot, pool, seeds) {
  t <- tournament_state_13(snapshot)$tally
  t <- t[t$pool == pool, ]
  if (!pool %in% c("A", "B", "C") || length(seeds) != nrow(t) || anyNA(seeds) || !setequal(seeds, t$seed) || anyDuplicated(seeds)) stop("List every original seed in this pool once, in the desired tie order.", call. = FALSE)
  snapshot$ties[[pool]] <- as.integer(seeds)
  state <- tournament_state_13(snapshot)
  snapshot$results[state$invalidated] <- NULL
  snapshot
}

write_snapshot_13 <- function(snapshot, file) {
  payload <- list(version = 1L, roster = roster_key_13(), results = lapply(snapshot$results, function(r) {
    list(teamA_id = r$teamA_id, teamB_id = r$teamB_id,
         scores = lapply(seq_len(nrow(r$scores)), function(i) as.numeric(r$scores[i, ])))
  }), ties = snapshot$ties)
  jsonlite::write_json(payload, file, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

read_snapshot_13 <- function(file) {
  x <- jsonlite::read_json(file, simplifyVector = FALSE)
  if (!identical(x$version, 1L) || !is.list(x$results) || !is.list(x$ties)) stop("Not a version 1 tournament backup.", call. = FALSE)
  # Seed-indexed team IDs changed with the replacement team. Never reuse an
  # earlier roster's scores merely because its numeric IDs still match.
  if ((length(x$results) || length(x$ties)) && !identical(x$roster, roster_key_13())) {
    stop("This backup belongs to an earlier or different roster. Its results cannot be applied to the current teams.", call. = FALSE)
  }
  snap <- empty_tournament()
  for (key in names(x$results)) {
    r <- x$results[[key]]
    if (!is.list(r$scores) || !length(r$scores) || any(lengths(r$scores) != 2L)) stop("Invalid saved score rows.", call. = FALSE)
    sc <- do.call(rbind, lapply(r$scores, unlist))
    if (is.numeric(sc)) storage.mode(sc) <- "double"
    if (length(r$teamA_id) != 1 || length(r$teamB_id) != 1 || !is.numeric(r$teamA_id) || !is.numeric(r$teamB_id) || !r$teamA_id %in% 1:13 || !r$teamB_id %in% 1:13 || r$teamA_id == r$teamB_id) stop("Invalid participants in backup.", call. = FALSE)
    snap$results[[key]] <- list(teamA_id = r$teamA_id, teamB_id = r$teamB_id, scores = sc)
  }
  for (p in names(x$ties)) {
    seeds <- unlist(x$ties[[p]])
    roster <- championship_teams(); expected <- roster$seed[roster$pool == p]
    if (!p %in% c("A", "B", "C") || !is.numeric(seeds) || anyNA(seeds) || length(seeds) != length(expected) || anyDuplicated(seeds) || !setequal(seeds, expected)) stop("Invalid pool tie decision in backup.", call. = FALSE)
    snap$ties[[p]] <- as.integer(seeds)
  }
  state <- tournament_state_13(snap)
  if (length(state$invalidated)) stop("Backup contains unknown matches or results for incorrect participants.", call. = FALSE)
  snap
}

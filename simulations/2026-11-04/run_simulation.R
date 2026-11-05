# Run from the repository root: Rscript simulations/2026-11-04/run_simulation.R
# Demonstration only. Never reads into or writes to the app's live score store.
suppressPackageStartupMessages(library(dplyr))
source("R/us_championship_engine.R")
source("R/championship_13.R")
source("R/championship_pdfs.R")

# Preview the agreed 21-point format in this isolated simulation. The app's
# live scoring model and any previously saved practice scores are untouched.
original_pool_matches <- pool_matches_13
pool_matches_13 <- function(teams = championship_teams()) {
  games <- original_pool_matches(teams)
  games$target <- 21L
  games
}
original_bracket_matches <- bracket_matches_13
bracket_matches_13 <- function() {
  games <- original_bracket_matches()
  games$target <- 21L
  games
}

out <- "output/pdf/simulation-2026-11-04"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
live_file <- "data/tournament-state.json"
live_hash <- function() if (file.exists(live_file)) unname(tools::md5sum(live_file)) else NA_character_
before <- live_hash()
set.seed(20261104)
teams <- championship_teams()
snapshot <- empty_tournament()

# Seed-weighted random outcomes allow upsets. This is an example, not a forecast.
random_side <- function(g) {
  a <- teams$seed[match(g$teamA_id, teams$team_id)]
  b <- teams$seed[match(g$teamB_id, teams$team_id)]
  if (runif(1) < plogis((b - a) / 4)) 1L else 2L
}
game_score <- function(side, target = 21L, original_target = 15L) {
  # Keep the earlier example's random draws, winners and point differences;
  # shift both scores to the new target. The reset was already played to 21.
  loser <- sample(5:(original_target + 2L), 1)
  score <- c(max(original_target, loser + 2L), loser) + target - original_target
  if (side == 2L) score <- rev(score)
  score
}
score_match <- function(g) {
  if (g$key == "G24") {
    # Demonstrate all three championship games and the conditional reset.
    scores <- rbind(game_score(2L), game_score(1L), game_score(2L))
  } else scores <- matrix(game_score(random_side(g), g$target,
                                    if (g$key == "G25") 21L else 15L), nrow = 1)
  snapshot <<- save_result_13(snapshot, g$key, scores)
}

initial <- tournament_state_13(snapshot)
for (i in which(initial$games$stage == "Pool play")) score_match(initial$games[i, ])
tie_notes <- character()
for (pool in c("A", "B", "C")) {
  tally <- tournament_state_13(snapshot)$tally
  t <- tally[tally$pool == pool, ]
  if (any(!t$resolved)) {
    flip_order <- sample(t$seed)
    snapshot <- save_tie_order_13(snapshot, pool, flip_order)
    tie_notes <- c(tie_notes, paste("Pool", pool, "simulated disc-flip tie order (original seeds):", paste(flip_order, collapse = ", ")))
  }
}
stopifnot(tournament_state_13(snapshot)$seeded)
for (wave in 1:9) {
  state <- tournament_state_13(snapshot)
  ready <- state$games[state$games$stage != "Pool play" & state$games$wave == wave & state$games$status == "Ready", ]
  stopifnot(!anyDuplicated(c(ready$teamA_id, ready$teamB_id)), !anyDuplicated(ready$court))
  for (i in seq_len(nrow(ready))) score_match(ready[i, ])
}
state <- tournament_state_13(snapshot)
stopifnot(all(state$games$status == "Complete"), nrow(state$games) == 53L,
          !anyNA(state$final$team_id), !anyDuplicated(state$final$team_id),
          length(state$invalidated) == 0L, setequal(state$tally$earned_seed, 1:13))
for (i in seq_len(nrow(state$games))) {
  g <- state$games[i, ]
  score_outcome_13(snapshot$results[[g$key]]$scores, g$target, g$best_of)
}

# Keep demonstration labeling inside each PDF without changing the app templates.
pdf_stamp_13 <- function() "SIMULATION ONLY - NOT ACTUAL RESULTS | Reproducible example, November 4, 2026"
write_pool_pdfs_13(file.path(out, "simulated-pool-score-sheets.pdf"), state, snapshot)
write_tally_pdf_13(file.path(out, "simulated-reseeding-tally.pdf"), state)
write_bracket_pdf_13(file.path(out, "simulated-bracket-and-placement.pdf"), state, snapshot)
write.csv(state$final, file.path(out, "simulated-final-standings.csv"), row.names = FALSE)
write.csv(state$games, file.path(out, "simulated-all-matches.csv"), row.names = FALSE)
write_snapshot_13(snapshot, file.path(out, "simulated-tournament.json"))
writeLines(c("SIMULATION ONLY - NOT ACTUAL RESULTS", "Random seed: 20261104.",
  "Seed-weighted random match results, allowing upsets. All games to 21, win by two, no cap.",
  "G24 deliberately goes three games and is won by the lower-bracket finalist to illustrate G25.",
  if (length(tie_notes)) tie_notes else "All pool ties resolved statistically; no director decisions needed.",
  "Generated separately from the live tournament; do not restore this example into your live scoring session."),
  file.path(out, "simulation-notes.txt"))
stopifnot(identical(before, live_hash()))
cat("Simulation complete: 22 pool matches, 25 bracket matches, 6 placement matches. Live score file unchanged.\n")
print(state$final[, c("place", "team", "games_played")], row.names = FALSE)

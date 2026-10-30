# Run from the repository root: Rscript tests/pool_score_sheets.R
library(dplyr)
source("R/us_championship_engine.R")
source("R/pool_score_sheets.R")

for (n in 3:6) {
  games <- round_robin_pairs(n)
  pairs <- apply(games[c("team1", "team2")], 1, function(x) paste(sort(x), collapse = "-"))
  expected <- apply(combn(n, 2), 2, paste, collapse = "-")
  stopifnot(nrow(games) == choose(n, 2), setequal(pairs, expected), !anyDuplicated(pairs))
  final <- filter(games, round == max(round))
  stopifnot(any((final$team1 == 1 & final$team2 == 2) | (final$team1 == 2 & final$team2 == 1)))
  for (r in unique(games$round)) {
    participants <- unlist(games[games$round == r, c("team1", "team2")])
    stopifnot(!anyDuplicated(participants))
  }
  if (n %% 2 == 1) {
    byes <- unlist(lapply(unique(games$round), function(r) {
      setdiff(seq_len(n), unlist(games[games$round == r, c("team1", "team2")]))
    }))
    stopifnot(setequal(byes, seq_len(n)), !anyDuplicated(byes))
  }
}

for (n in 10:12) {
  teams <- tibble(team_id = seq_len(n), seed = seq_len(n), team = paste("Team", seq_len(n)))
  stage <- make_us_pool_stage(teams)
  stopifnot(!anyDuplicated(stage$games$game_id))
  for (pool in stage$pools) {
    games <- filter(stage$games, group == pool$pool[1])
    stopifnot(nrow(games) == choose(nrow(pool), 2))
    final <- filter(games, round == max(round))
    stopifnot(any((final$teamA_id == pool$team_id[1] & final$teamB_id == pool$team_id[2]) |
                  (final$teamA_id == pool$team_id[2] & final$teamB_id == pool$team_id[1])))
  }
}

stopifnot(identical(pool_sheet_teams(4), rep("", 4)),
          identical(pool_sheet_teams(4, "First\n\nThird"), c("First", "", "Third", "")),
          inherits(try(pool_sheet_teams(4, "A\nB\nC\nD\nE"), silent = TRUE), "try-error"),
          inherits(try(pool_sheet_teams(6), silent = TRUE), "try-error"))

for (n in c(4, 5)) {
  file <- tempfile(fileext = ".pdf")
  write_pool_score_sheet(file, n)
  stopifnot(file.info(file)$size > 0)
  unlink(file)
}
cat("Pool scheduling and PDF checks passed.\n")

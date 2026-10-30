library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)
source("R/us_championship_engine.R")
source("R/ddc_rankings.R")
`%||%` <- function(x, y) if (is.null(x)) y else x

# Snapshot of the 10 distinct partnered teams in the registration sheet.
# Francis Flores is listed as Fran Flores in the DDC player dropdown.
roster <- c("Jasun Burdick / Drew Magee", "John Elsner / Tobias Cole",
  "Vergil Weatherford / Conrad Damon", "Fran Flores / Hank Bass",
  "Diana Elsner / Lindsi Allman", "Jim Elsner / Drew Preiss",
  "Mike Prost / Scott Schmucker", "Jordan Huston / Jacob Dummeldinger",
  "Travis Ward / Oscar Patterson", "Jay Koonts / Jack Cooksey")
snapshot <- "simulations/2026-10-03/player_rankings.csv"
rankings <- read.csv(if (file.exists(snapshot)) snapshot else "/private/tmp/ddc-simulation-rankings.csv")
teams <- seed_teams_from_rankings(parse_team_roster(roster), rankings)
stopifnot(nrow(teams) == 10, !any(teams$missing_ranking))
dir.create("simulations/2026-10-03", recursive = TRUE, showWarnings = FALSE)
write.csv(rankings %>% filter(player %in% c(teams$player1, teams$player2)),
  "simulations/2026-10-03/player_rankings.csv", row.names = FALSE)
set.seed(20261003)

# Illustrative independent point model, not a calibrated prediction.
# Compression prevents historical participation points from dominating results.
sim_game <- function(a, b, target = 15) {
  strength <- teams$team_points[match(c(a, b), teams$team_id)] ^ 0.45
  p <- strength[1] / sum(strength)
  s <- c(0, 0)
  while (max(s) < target || abs(diff(s)) < 2) {
    i <- if (runif(1) < p) 1 else 2
    s[i] <- s[i] + 1
  }
  s
}
preset <- make_us_championship_preset(teams)
pool_stage <- preset$pool_stage
pool_scores <- map_dfr(seq_len(nrow(pool_stage$games)), function(i) {
  g <- pool_stage$games[i, ]
  s <- sim_game(g$teamA_id, g$teamB_id)
  tibble(game_id = g$game_id, scoreA = s[1], scoreB = s[2])
})
seeds <- seed_top_eight(teams, pool_stage, pool_scores)
bracket <- make_top8_double_elim(teams, seeds, max(pool_stage$games$game_id))
scores <- tibble(game_id = integer(), scoreA = double(), scoreB = double())
final_games <- list()
for (i in seq_len(nrow(bracket))) {
  bracket <- resolve_dependency_games(bracket, scores)
  g <- bracket[i, ]
  if (is.na(g$teamA_id) || is.na(g$teamB_id)) next
  if (g$game_key %in% c("GF", "GFR")) {
    wins <- c(0, 0)
    series <- list()
    while (max(wins) < 2) {
      s <- sim_game(g$teamA_id, g$teamB_id, 21)
      w <- if (s[1] > s[2]) 1 else 2
      wins[w] <- wins[w] + 1
      game_number <- length(series) + 1
      series[[game_number]] <- tibble(series = g$game_key,
        game = game_number, teamA_id = g$teamA_id,
        teamB_id = g$teamB_id, teamA = g$teamA, teamB = g$teamB,
        scoreA = s[1], scoreB = s[2], winner_id = c(g$teamA_id, g$teamB_id)[w])
    }
    final_games[[g$game_key]] <- bind_rows(series)
    # The engine accepts a match result; 2-0 or 2-1 represents series wins.
    s <- wins
  } else {
    s <- sim_game(g$teamA_id, g$teamB_id)
  }
  scores <- bind_rows(scores, tibble(game_id = g$game_id, scoreA = s[1], scoreB = s[2]))
}
bracket <- resolve_dependency_games(bracket, scores)
bracket_results <- game_results(bracket, scores) %>% filter(!is.na(winner_id))
placement <- make_bottom_placement(teams, seeds, max(bracket$game_id))
s <- sim_game(placement$teamA_id, placement$teamB_id)
placement_results <- game_results(placement, tibble(game_id = placement$game_id, scoreA = s[1], scoreB = s[2]))
pool_results <- game_results(pool_stage$games, pool_scores)
standings <- imap_dfr(pool_stage$pools, function(p, nm) {
  team_standings(teams, pool_stage$games, pool_scores, nm) %>% mutate(pool = nm)
})
name_of <- function(id) teams$team[match(id, teams$team_id)]
last_final <- bracket_results %>% filter(game_key %in% c("GF", "GFR")) %>% slice_tail(n = 1)
finishes <- tibble(place = c("1", "2", "3", "4", "5=", "5=", "7=", "7=", "9", "10"),
  team_id = c(last_final$winner_id, last_final$loser_id,
    bracket_results$loser_id[match(c("L6", "L5", "L3", "L4", "L1", "L2"), bracket_results$game_key)],
    placement_results$winner_id, placement_results$loser_id)) %>% mutate(team = name_of(team_id))
finals <- bind_rows(final_games)

# Validate score termination, round-robin coverage, source paths and elimination.
ordinary <- bind_rows(pool_results, bracket_results %>% filter(!game_key %in% c("GF", "GFR")), placement_results)
valid_scores <- function(a, b, target) {
  hi <- pmax(a, b); lo <- pmin(a, b)
  all(hi >= target & hi - lo >= 2 & ((hi == target & lo <= target - 2) | (hi > target & hi - lo == 2)))
}
stopifnot(valid_scores(ordinary$scoreA, ordinary$scoreB, 15), valid_scores(finals$scoreA, finals$scoreB, 21),
  nrow(pool_results) == 20, all(standings$played == 4),
  !anyDuplicated(paste(pmin(pool_results$teamA_id, pool_results$teamB_id), pmax(pool_results$teamA_id, pool_results$teamB_id))),
  nrow(finishes) == 10, !anyDuplicated(finishes$team_id))
losses <- table(bracket_results$loser_id)
stopifnot(all(losses[as.character(finishes$team_id[2:8])] == 2),
  sum(bracket_results$loser_id == finishes$team_id[1]) <= 1)
for (i in seq_len(nrow(bracket_results))) {
  g <- bracket_results[i, ]
  for (side in c("A", "B")) {
    type <- g[[paste0("slot", side, "_type")]]
    ref <- g[[paste0("slot", side, "_ref")]]
    expected <- if (type == "seed") seeds$team_id[seeds$championship_seed == as.integer(ref)] else {
      src <- bracket_results[bracket_results$game_key == ref, ]
      src[[if (type == "winner") "winner_id" else "loser_id"]]
    }
    stopifnot(g[[paste0("team", side, "_id")]] == expected)
  }
}
result <- list(random_seed = 20261003, ranking_year = unique(rankings$ranking_year),
  teams = teams, pools = bind_rows(pool_stage$pools), pool_games = pool_results,
  standings = standings, championship_seeds = seeds, bracket = bracket_results,
  finals = finals, placement = placement_results, finishes = finishes,
  reset_played = "GFR" %in% bracket_results$game_key,
  checks = list(pool_games = 20, games_per_team = 4, valid_scores = TRUE,
    bracket_paths = TRUE, double_elimination = TRUE))
write_json(result, "simulations/2026-10-03/results.json", dataframe = "rows", pretty = TRUE, auto_unbox = TRUE, na = "null")
write.csv(teams, "simulations/2026-10-03/team_seeds.csv", row.names = FALSE)
write.csv(bind_rows(ordinary %>% mutate(target = 15), finals %>% mutate(game_key = paste0(series, "-", game), target = 21)),
  "simulations/2026-10-03/game_results.csv", row.names = FALSE)
print(teams[, c("seed", "team", "team_points")])
print(finals)
print(finishes)
cat("Validated all game scores, pool coverage, bracket paths and double elimination.\n")

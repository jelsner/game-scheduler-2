# Run from repository root: Rscript tests/championship_13.R
library(dplyr)
source("R/us_championship_engine.R")
source("R/championship_13.R")
source("R/championship_pdfs.R")
assert_error <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))
record_for <- function(g, winner = g$teamA_id, third = FALSE) {
  sc <- if (g$best_of == 3L) {
    if (third) rbind(c(15, 10), c(10, 15), c(15, 13)) else rbind(c(15, 10), c(15, 11))
  } else matrix(c(g$target, 10), nrow = 1)
  if (winner == g$teamB_id) sc <- sc[, c(2,1), drop = FALSE]
  list(teamA_id = g$teamA_id, teamB_id = g$teamB_id, scores = sc)
}

teams <- championship_teams(); start <- tournament_state_13()
stopifnot(teams$team[8] == "Hank B. & Richard P.", teams$points[8] == 2116.40,
          teams$team[9] == "Travis W. & Oscar P.", teams$team[13] == "Diana E. & Lindsi A.",
          !any(grepl("Fran F.", teams$team, fixed = TRUE)))
stopifnot(identical(as.integer(table(teams$pool)), c(5L,4L,4L)),
          identical(teams$seed[teams$pool == "A"], c(1L,6L,7L,12L,13L)),
          sum(start$games$stage == "Pool play") == 22L)
for (p in c("A", "B", "C")) {
  g <- subset(start$games, pool == p)
  pairs <- apply(g[c("teamA_id", "teamB_id")], 1, function(x) paste(sort(x), collapse = ":"))
  stopifnot(!anyDuplicated(pairs), length(pairs) == choose(sum(teams$pool == p),2))
  top <- teams$team_id[teams$pool == p][1:2]
  last <- tail(g, 1)
  stopifnot(setequal(c(last$teamA_id, last$teamB_id), top), last$wave == max(g$wave))
}
for (pair in list(c(15,0),c(15,13),c(16,0),c(16,10),c(16,13),c(16,14),c(17,14),c(21,19),c(30,28),c(30,27))) {
  stopifnot(valid_game_13(pair[1],pair[2]),valid_game_13(pair[2],pair[1]))
}
for (pair in list(c(15,14),c(17,10),c(17,13),c(18,14),c(30,26),c(10,0),c(-1,15),c(15.5,13),c(NA,15),c(Inf,0))) stopifnot(!valid_game_13(pair[1],pair[2]))
stopifnot(valid_game_13(21,19,21), valid_game_13(22,10,21), valid_game_13(10,22,21),
          valid_game_13(23,20,21), !valid_game_13(23,19,21), !valid_game_13(15,0,21))
# Independently enumerate rallies awarding 1 or 2, stopping at a win. Every
# reachable terminal score must agree with validation, including extended play.
for (target in c(15L,21L)) {
  limit <- 34L
  reachable <- matrix(FALSE,limit+1L,limit+1L); reachable[1,1] <- TRUE
  for (a in 0:limit) for (b in 0:limit) {
    won <- max(a,b) >= target && abs(a-b) >= 2
    stopifnot(identical(valid_game_13(a,b,target),reachable[a+1,b+1] && won))
    if (!reachable[a+1,b+1] || won) next
    for (points in 1:2) {
      if (a+points <= limit) reachable[a+points+1,b+1] <- TRUE
      if (b+points <= limit) reachable[a+1,b+points+1] <- TRUE
    }
  }
}
assert_error(score_outcome_13(rbind(c(15,10)), best_of=3))
assert_error(score_outcome_13(rbind(c(15,10),c(15,10),c(10,15)), best_of=3))
stopifnot(score_outcome_13(rbind(c(15,10),c(10,15),c(16,14)), best_of=3)$side == 1)

# Seed 6 wins Pool A; it earns seed 1, and the original seed remains 6.
base <- empty_tournament()
for (i in which(start$games$stage == "Pool play")) {
  g <- start$games[i, ]; priority <- function(id) if (id == 6) -1 else id
  winner <- if (priority(g$teamA_id) < priority(g$teamB_id)) g$teamA_id else g$teamB_id
  base$results[[g$key]] <- record_for(g, winner)
}
s <- tournament_state_13(base)
stopifnot(s$seeded, s$tally$earned_seed[s$tally$team_id == 6] == 1,
          s$tally$seed[s$tally$team_id == 6] == 6,
          setequal(s$tally$earned_seed,1:13))
for (p in c("A","B","C")) stopifnot(setequal(s$tally$earned_seed[s$tally$pool == p], teams$seed[teams$pool == p]))

# An exact three-way cyclic tie cannot silently fall back to original seeds.
tied <- base
for (i in which(start$games$pool == "C")) {
  g <- start$games[i, ]; ids <- c(g$teamA_id,g$teamB_id)
  winner <- if (10 %in% ids) setdiff(ids,10) else if (setequal(ids,c(3,4))) 3L else if (setequal(ids,c(4,9))) 4L else 9L
  sc <- if (10 %in% ids) c(15,0) else c(15,10)
  if (winner == ids[2]) sc <- rev(sc)
  tied$results[[g$key]] <- list(teamA_id=ids[1],teamB_id=ids[2],scores=matrix(sc,nrow=1))
}
tie_state <- tournament_state_13(tied)
stopifnot(!tie_state$seeded, sum(tie_state$tally$unresolved[tie_state$tally$pool=="C"]) == 3)
assert_error(save_tie_order_13(tied,"C",c(3,4,4,10)))
tied <- save_tie_order_13(tied,"C",c(9,3,4,10))
tie_state <- tournament_state_13(tied)
stopifnot(tie_state$seeded, tie_state$tally$earned_seed[tie_state$tally$team_id==9]==3)

# Complete whole tournaments, with and without reset and with 2/3-game finals.
set.seed(13)
final_examples <- list()
for (trial in 1:24) {
  snap <- base
  for (w in 1:9) {
    state <- tournament_state_13(snap)
    games <- subset(state$games, stage != "Pool play" & wave == w & status == "Ready")
    stopifnot(!anyDuplicated(c(games$teamA_id,games$teamB_id)), !anyDuplicated(games$court), all(games$court <= 6))
    for (i in seq_len(nrow(games))) {
      g <- games[i, ]
      winner <- if (g$key == "G24") { if (trial %% 2) g$teamA_id else g$teamB_id } else sample(c(g$teamA_id,g$teamB_id),1)
      snap$results[[g$key]] <- record_for(g,winner,third = trial %% 3 == 0)
    }
  }
  state <- tournament_state_13(snap)
  stopifnot(all(!is.na(state$final$team_id)), !anyDuplicated(state$final$team_id), length(state$invalidated)==0)
  stopifnot(state$games$status[state$games$key=="G25"] == if (trial %% 2) "Not needed" else "Complete")
  losses <- integer(13)
  for (i in which(state$games$stage=="Double elimination" & state$games$status=="Complete")) losses[state$games$loser_id[i]] <- losses[state$games$loser_id[i]] + 1L
  champ <- state$final$team_id[1]
  stopifnot(losses[champ] <= 1, all(losses[-champ] == 2L))
  stopifnot(state$final$games_played[13] %in% c(5,6), all(state$final$games_played[1:12] >= 7))
  stopifnot(identical(state$mini, state$mini[order(state$tally$earned_seed[match(state$mini,state$tally$team_id)])]))
  if (trial <= 2) final_examples[[trial]] <- snap
}

# Edits invalidate scores whose participants or prerequisites changed, including reset.
finished <- final_examples[[2]]
state <- tournament_state_13(finished)
g <- state$games[state$games$key=="G24", ]
new_score <- record_for(g,g$teamA_id)$scores
changed <- save_result_13(finished,"G24",new_score)
stopifnot(is.null(changed$results$G25), tournament_state_13(changed)$games$status[tournament_state_13(changed)$games$key=="G25"]=="Not needed")
cleared <- save_result_13(finished,"G1")
stopifnot(is.null(cleared$results$G6), is.null(cleared$results$G14), is.null(cleared$results$G24), is.null(cleared$results$P1))
# A correction with the same winner preserves legitimately completed downstream matches.
g <- state$games[state$games$key=="G1", ]; old_winner <- g$winner_id
sc <- matrix(if (old_winner==g$teamA_id) c(15,8) else c(8,15),nrow=1)
unchanged <- save_result_13(finished,"G1",sc)
stopifnot(length(unchanged$results) == length(finished$results))
# A pool correction clears all earned seeds/descendants if it makes the pool incomplete.
cleared_pool <- save_result_13(finished,"PA1")
stopifnot(!tournament_state_13(cleared_pool)$seeded, !any(grepl("^(G|P[1-6]$)", names(cleared_pool$results))))

# Backup round trips include series scores and director tie decisions.
for (snap in c(final_examples,list(tied,empty_tournament()))) {
  file <- tempfile(fileext=".json"); write_snapshot_13(snap,file); restored <- read_snapshot_13(file)
  a <- tournament_state_13(snap); b <- tournament_state_13(restored)
  stopifnot(identical(a$games,b$games),identical(a$tally,b$tally),identical(a$final,b$final));unlink(file)
}
bad <- finished; bad$results$G999 <- bad$results$G1
file <- tempfile(fileext=".json");write_snapshot_13(bad,file);assert_error(read_snapshot_13(file));unlink(file)

# Results from a different or unidentified roster cannot be reassigned to new teams.
file <- tempfile(fileext=".json"); write_snapshot_13(finished,file)
x <- jsonlite::read_json(file); x$roster <- sub("Hank B. & Richard P.", "Fran F. & Hank B.", x$roster, fixed=TRUE)
jsonlite::write_json(x,file,auto_unbox=TRUE);assert_error(read_snapshot_13(file))
x$roster <- NULL;jsonlite::write_json(x,file,auto_unbox=TRUE);assert_error(read_snapshot_13(file))
jsonlite::write_json(list(version=1L,results=list(),ties=list()),file,auto_unbox=TRUE)
stopifnot(length(read_snapshot_13(file)$results)==0);unlink(file)

# Filled PDFs exercise names, individual championship scores, and final rankings.
dir.create("tmp/pdfs",recursive=TRUE,showWarnings=FALSE)
write_pool_pdfs_13("tmp/pdfs/filled-pools.pdf",state,finished)
write_tally_pdf_13("tmp/pdfs/filled-tally.pdf",state)
write_bracket_pdf_13("tmp/pdfs/filled-bracket.pdf",state,finished)
write_snapshot_13(finished,"tmp/pdfs/finished-tournament.json")
cat("13-team engine checks passed: seeding, ties, scoring, all 13 places, corrections, backups and PDFs.\n")

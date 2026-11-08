# Standalone: Rscript tests/bracket_seed_order_13.R
Sys.setenv(DDC_STATE_FILE = tempfile(fileext = ".json"))
source("app.R")
assert_error <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))
snapshot <- empty_tournament()
initial <- tournament_state_13(snapshot)
for (i in which(initial$games$stage == "Pool play")) {
  g <- initial$games[i, ]
  score <- if (g$teamA_id < g$teamB_id) c(21,10) else c(10,21)
  snapshot <- save_result_13(snapshot, g$key, matrix(score,nrow=1))
}
for (key in paste0("G",1:7)) snapshot <- save_result_13(snapshot,key,matrix(c(21,10),nrow=1))
original <- snapshot
before <- tournament_state_13(snapshot)
seed_order <- before$tally$team_id[order(before$tally$earned_seed)]
seed_order[c(2,3)] <- seed_order[c(3,2)]
snapshot$bracket_seed_order <- seed_order
after <- tournament_state_13(snapshot)
stopifnot(identical(original$results,snapshot$results),length(after$invalidated)==0,
  identical(before$tally$pool_finish,after$tally$pool_finish),
  identical(before$tally$earned_seed,after$tally$pool_earned_seed),
  after$games$teamA_id[after$games$key=="G8"]==seed_order[3],
  after$games$teamB_id[after$games$key=="G9"]==seed_order[2])
for (bad in list(rep(1,13),1:12,c(1:12,99),c(1:12,13.5),as.character(1:13))) {
  assert_error(bracket_seed_order_13(bad))
}
file <- tempfile(fileext=".json")
write_snapshot_13(snapshot,file)
stopifnot(jsonlite::read_json(file)$version==2L)
restored <- read_snapshot_13(file)
stopifnot(identical(restored$bracket_seed_order,as.integer(seed_order)),
          identical(after$games,tournament_state_13(restored)$games))
# Restore through the actual scoring desk, then save G8 and re-export.
shiny::testServer(server, {
  session$setInputs(restore_file=list(datapath=file),review_restore=1,confirm_restore=0)
  stopifnot(length(tournament_store_13()$results)==0)
  session$setInputs(confirm_restore=1)
  stopifnot(length(tournament_store_13()$results)==29,
            grepl("S2 = Mike",as.character(output$bracket_seed_notice)[1]))
  session$setInputs(score_stage="Double elimination",match_key="G8",save_score=0)
  session$setInputs(scoreA_1=22,scoreB_1=10,save_score=1)
  saved <- read_snapshot_13(state_file_13)
  stopifnot(identical(saved$bracket_seed_order,as.integer(seed_order)),
            saved$results$G8$teamA_id==seed_order[3])
})
# The adjusted draw continues all the way through final placements.
repeat {
  state <- tournament_state_13(restored)
  ready <- state$games[state$games$status=="Ready", ]
  if (!nrow(ready)) break
  g <- ready[1, ]
  scores <- if (g$best_of==3L) rbind(c(21,10),c(21,11)) else matrix(c(21,10),nrow=1)
  restored <- save_result_13(restored,g$key,scores)
}
state <- tournament_state_13(restored)
stopifnot(!anyNA(state$final$team_id),!anyDuplicated(state$final$team_id),length(state$invalidated)==0,
          identical(original$results,restored$results[names(original$results)]),
          identical(restored$bracket_seed_order,as.integer(seed_order)))
unlink(c(file,state_file_13))
cat("Bracket seed adjustment passed: pool and G1-G7 preservation, validation, app restore/save, backup round trip and complete tournament.\n")

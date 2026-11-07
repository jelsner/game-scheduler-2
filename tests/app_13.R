# Run after tests/championship_13.R, from repository root.
# Test storage is isolated from real tournament progress.
Sys.setenv(DDC_STATE_FILE = tempfile(fileext = ".json"))
source("app.R")
finish_backup <- "tmp/pdfs/finished-tournament.json"
stopifnot(file.exists(finish_backup))
shiny::testServer(server, {
  session$setInputs(score_stage = "Pool play", match_key = "PA1", save_score = 0, confirm_clear = 0, confirm_restore = 0)
  stopifnot(length(tournament_store_13()$results)==0)
  stopifnot(grepl("To 21", as.character(output$score_editor)[1]))
  session$setInputs(scoreA_1 = 21, scoreB_1 = 20, save_score = 1)
  stopifnot(length(tournament_store_13()$results)==0)
  session$setInputs(scoreA_1 = 15, scoreB_1 = 10, save_score = 2)
  stopifnot(length(tournament_store_13()$results)==0)
  session$setInputs(scoreA_1 = 22, scoreB_1 = 10, save_score = 3)
  stopifnot(length(tournament_store_13()$results)==1, file.exists(state_file_13))
  restored <- read_snapshot_13(state_file_13)
  stopifnot(tournament_state_13(restored)$games$status[1]=="Complete")
  stopifnot(identical(as.numeric(restored$results$PA1$scores),c(22,10)))
  # Showing the clear modal cannot delete a result before confirmation.
  session$setInputs(clear_score = 1, confirm_clear = 0)
  stopifnot(length(tournament_store_13()$results)==1)
  session$setInputs(confirm_clear = 1)
  stopifnot(length(tournament_store_13()$results)==0)
  # All PDF and backup download handlers execute in the live server.
  for (name in c("pool_pdf","tally_pdf","bracket_pdf","backup","schedule_csv","standings_csv")) {
    file <- output[[name]]
    stopifnot(file.exists(file),file.info(file)$size > 0)
  }
  session$setInputs(restore_file = list(datapath = finish_backup), review_restore = 1, confirm_restore = 0)
  stopifnot(length(tournament_store_13()$results)==0)
  session$setInputs(confirm_restore = 1)
  stopifnot(all(!is.na(state()$final$team_id)))
  # The game to 21 also accepts a final double past the target and persists it.
  session$setInputs(score_stage = "Double elimination", match_key = "G25")
  session$setInputs(scoreA_1=22,scoreB_1=10,save_score=4)
  stopifnot(identical(as.numeric(read_snapshot_13(state_file_13)$results$G25$scores),c(22,10)))
  session$setInputs(score_stage = "Double elimination", match_key = "G24")
  html <- as.character(output$score_editor)[1]
  stopifnot(grepl("scoreA_3",html),grepl("Best of three to 21",html))
  # Changing a final from a lower-bracket win to an upper-bracket win clears reset.
  session$setInputs(scoreA_1=22,scoreB_1=10,scoreA_2=23,scoreB_2=20,scoreA_3=NA_real_,scoreB_3=NA_real_,save_score=5)
  stopifnot(is.null(tournament_store_13()$results$G25),state()$games$status[state()$games$key=="G25"]=="Not needed")
  session$setInputs(match_key="G1",clear_score=2,confirm_clear=0)
  stopifnot(!is.null(tournament_store_13()$results$G1))
  session$setInputs(confirm_clear=2)
  stopifnot(is.null(tournament_store_13()$results$G1),is.null(tournament_store_13()$results$G24))
})
unlink(state_file_13)
cat("Shiny server checks passed: validated scoring, confirmation, series, corrections, downloads, persistence and restore.\n")

# Stage navigation must warn without ending the scoring session or losing results.
# Standalone: Rscript tests/stage_navigation_13.R
Sys.setenv(DDC_STATE_FILE = tempfile(fileext = ".json"))
source("app.R")
add_ready_results <- function(snapshot, stages, wave = NULL) {
  state <- tournament_state_13(snapshot)
  games <- subset(state$games, stage %in% stages & status == "Ready")
  if (!is.null(wave)) games <- games[games$wave == wave, ]
  for (i in seq_len(nrow(games))) {
    g <- games[i, ]
    scores <- matrix(if (g$teamA_id < g$teamB_id) c(15,10) else c(10,15), nrow=1)
    snapshot$results[[g$key]] <- list(teamA_id=g$teamA_id,teamB_id=g$teamB_id,scores=scores)
  }
  snapshot
}
shiny::testServer(server, {
  session$setInputs(score_stage="Pool play",match_key="PA1",save_score=0)
  stopifnot(active_stage()=="Pool play", is.null(output$stage_notice))
  stopifnot(!is.null(stage_problem(character(),state())), !is.null(stage_problem(NA_character_,state())))
  session$setInputs(match_key=character())
  stopifnot(is.null(selected_game()),grepl("Choose a ready",as.character(output$score_editor)[1]))
  session$setInputs(match_key="PA1")
  session$setInputs(score_stage="Double elimination")
  stopifnot(active_stage()=="Pool play",grepl("22 pool matches",as.character(output$stage_notice)[1]),
            grepl("Jasun",as.character(output$score_editor)[1]),length(tournament_store_13()$results)==0)
  session$setInputs(score_stage="Placement",match_key="")
  stopifnot(active_stage()=="Pool play",grepl("Placement games are not ready",as.character(output$stage_notice)[1]),
            grepl("Choose a ready",as.character(output$score_editor)[1]))
  # The rejected navigation cannot terminate the session: pool scoring still works.
  session$setInputs(score_stage="Pool play",match_key="PA1",scoreA_1=15,scoreB_1=12,save_score=1)
  stopifnot(length(tournament_store_13()$results)==1)
  session$setInputs(score_stage="Double elimination")
  stopifnot(grepl("21 pool matches",as.character(output$stage_notice)[1]),!is.null(tournament_store_13()$results$PA1))
  pool_done <- add_ready_results(empty_tournament(),"Pool play")
  tied <- pool_done
  pool_games <- subset(tournament_state_13(tied)$games,pool=="C")
  for (i in seq_len(nrow(pool_games))) {
    g <- pool_games[i, ]; ids <- c(g$teamA_id,g$teamB_id)
    winner <- if (10 %in% ids) setdiff(ids,10) else if (setequal(ids,c(3,4))) 3L else if (setequal(ids,c(4,9))) 4L else 9L
    sc <- if (10 %in% ids) c(15,0) else c(15,10)
    if (winner==ids[2]) sc <- rev(sc)
    tied$results[[g$key]] <- list(teamA_id=ids[1],teamB_id=ids[2],scores=matrix(sc,nrow=1))
  }
  tournament_store_13(tied)
  session$setInputs(score_stage="Double elimination")
  stopifnot(active_stage()=="Pool play",grepl("unresolved ties",as.character(output$stage_notice)[1]))
  tournament_store_13(pool_done)
  session$setInputs(score_stage="Double elimination",match_key="PA1")
  stopifnot(state()$seeded,active_stage()=="Double elimination",is.null(selected_game()),
            grepl("Choose a ready",as.character(output$score_editor)[1]))
  before <- tournament_store_13()
  session$setInputs(save_score=2,clear_score=1)
  stopifnot(identical(before,tournament_store_13())) # stale pool key cannot be saved/cleared in bracket stage
  session$setInputs(match_key="G1",scoreA_1=15,scoreB_1=10,save_score=3)
  stopifnot(!is.null(tournament_store_13()$results$G1))
  session$setInputs(score_stage="Placement")
  stopifnot(active_stage()=="Double elimination",grepl("not ready",as.character(output$stage_notice)[1]),
            grepl("Court 1",as.character(output$score_editor)[1]))
  # Placement is intentionally available before the ENTIRE bracket is complete.
  through_wave3 <- pool_done
  for (w in 1:3) through_wave3 <- add_ready_results(through_wave3,"Double elimination",w)
  tournament_store_13(through_wave3)
  session$setInputs(score_stage="Double elimination")
  session$setInputs(score_stage="Placement",match_key="P1")
  stopifnot(active_stage()=="Placement",is.null(output$stage_notice),!is.null(selected_game()),
            any(state()$games$stage=="Double elimination" & state()$games$status!="Complete"))
  # A restore/correction that removes prerequisites also recovers gracefully.
  tournament_store_13(empty_tournament())
  session$flushReact()
  stopifnot(active_stage()=="Pool play",grepl("not ready",as.character(output$stage_notice)[1]))
  session$setInputs(score_stage="Pool play",match_key="PA1",scoreA_1=15,scoreB_1=9,save_score=4)
  stopifnot(length(tournament_store_13()$results)==1)
})
unlink(state_file_13)
cat("Stage navigation passed: warnings, live session recovery, preserved scores, stale selections and concurrent placement.\n")

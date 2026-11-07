# 2026 US DDC Championship: offline-ready, fixed 13-team tournament desk.
library(shiny)
library(dplyr)
library(DT)
source("R/us_championship_engine.R")
source("R/championship_13.R")
source("R/championship_pdfs.R")

state_file_13 <- Sys.getenv("DDC_STATE_FILE", "data/tournament-state.json")
initial_13 <- if (file.exists(state_file_13)) read_snapshot_13(state_file_13) else empty_tournament()
# Shared by sessions in this R process, not private to each browser tab.
tournament_store_13 <- reactiveVal(initial_13)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f3f6f8; color:#192c3c; }
    .container-fluid { max-width:1440px; margin:auto; }
    .hero { padding:22px 0 12px; } .hero h1 { margin:0 0 5px; font-size:29px; font-weight:700; }
    .hero p,.muted { color:#526776; } .well,.panel-card { background:white; border:1px solid #dbe3e8; border-radius:9px; padding:18px; }
    .panel-card { margin:14px 0; } .panel-card h3 { margin-top:0; font-size:20px; }
    .format-note { background:#e9f4ef; border-left:4px solid #24785a; padding:12px 16px; margin-bottom:16px; }
    .progress-note { font-size:17px; font-weight:600; margin:12px 0; }
    .score-pair { display:grid; grid-template-columns:1fr 1fr; gap:12px; }
    .score-game { border-top:1px solid #dbe3e8; padding-top:8px; }
    .btn { margin-bottom:6px; } .downloads .btn { margin-right:8px; }
    .saved-note { font-size:12px; color:#526776; margin-top:10px; }
    .tab-content { padding-top:8px; }
    table.dataTable td, table.dataTable th { white-space:nowrap; } .help-block { color:#526776; }
    .shiny-output-error-validation { color:#a13e23; }
    @media(max-width:760px){ .hero h1 {font-size:24px;} .well {margin-bottom:12px;} }
  ")),
    tags$link(rel = "stylesheet", href = "readability.css?v=2"),
    tags$script(src = "readability.js")
  ),
  div(class = "hero", h1("US DDC Championship 2026"),
      p("November 7-8 | Tallahassee | 13 teams | Six courts")),
  div(class = "display-controls", role = "region", `aria-label` = "Display preferences",
    checkboxInput("sunlight_mode", "Sunlight mode", value = TRUE),
    selectInput("desk_text_size", "Text size", choices = c("Standard" = "16", "Large" = "19", "Extra large" = "22"),
      selected = "19", selectize = FALSE, width = "190px"),
    span(class = "display-hint muted", "Display preferences are remembered on this browser.")),
  div(class = "format-note", strong("Tournament format: "),
      "Three pools (5 + 4 + 4), then all 13 teams enter double elimination. Each pool retains its original seed slots; teams earn those slots by pool finish. Placement games decide 5th-12th."),
  sidebarLayout(
    sidebarPanel(width = 4,
      h3("Score a match"),
      selectInput("score_stage", "Stage", c("Pool play", "Double elimination", "Placement")),
      uiOutput("stage_notice"),
      selectInput("match_key", "Ready or completed match", choices = character()),
      uiOutput("score_editor"),
      actionButton("save_score", "Save result", class = "btn-primary"),
      actionButton("clear_score", "Clear result"),
      textOutput("save_status"),
      div(class = "saved-note", "Results are saved on this server after each change. Download a backup regularly, especially when using shinyapps.io."),
      hr(), h4("Backup and restore"),
      downloadButton("backup", "Download tournament backup"),
      fileInput("restore_file", "Restore a tournament backup", accept = ".json"),
      actionButton("review_restore", "Review restore"),
      p(class = "muted", "This app is a tournament scoring desk. Use one scoring operator and one server process.")),
    mainPanel(width = 8,
      textOutput("progress", container = function(...) div(class = "progress-note", ...)),
      tabsetPanel(id = "main_tab",
        tabPanel("Pools", value = "pools", uiOutput("pools_view")),
        tabPanel("Reseeding tally", div(class = "panel-card",
          h3("Earned bracket seeds"),
          p("Pool finish determines which of that pool's original overall seed slots a team earns. Other pools' records are never compared."),
          DTOutput("tally"), uiOutput("ties_view"))),
        tabPanel("Bracket", div(class = "panel-card", h3("Double elimination"),
          p("Seeds 1-3 receive opening byes. G24 is best of three to 21; G25 is one game to 21, only if the lower-bracket finalist wins G24. All games win by two."),
          p("A wave starts games together on separate courts. Start the next wave after the previous one finishes, allowing the agreed rest period."),
          DTOutput("bracket"))),
        tabPanel("Placement", div(class = "panel-card", h3("Final position games"),
          p("Losers of G14-G17 enter the 9th-12th mini bracket, ordered by earned bracket seed: M1 vs M4 and M2 vs M3. Winners play P3 for 9th/10th; losers play P4 for 11th/12th. P5 decides 7th/8th and P6 decides 5th/6th. The loser of G13 finishes 13th."),
          DTOutput("placements"))),
        tabPanel("Final standings", div(class = "panel-card", h3("Final positions"),
          DTOutput("final_table"), downloadButton("standings_csv", "Download final standings"))),
        tabPanel("Schedule", div(class = "panel-card", h3("Courts and waves"),
          DTOutput("schedule"), downloadButton("schedule_csv", "Download schedule"))),
        tabPanel("Print PDFs", div(class = "panel-card downloads", h3("Print the current tournament"),
          p("PDFs include the registered teams and every saved result. Download fresh copies after scoring. Unplayed scores stay blank."),
          downloadButton("pool_pdf", "Pool score sheets (3 pages)"),
          downloadButton("tally_pdf", "Reseeding tally (1 page)"),
          downloadButton("bracket_pdf", "Bracket and placement (2 pages)"),
          p("Pool sheets: US Letter portrait. Tally and bracket: US Letter landscape, actual size."))),
        tabPanel("Teams & rules", div(class = "panel-card", h3("Registered teams and initial seeds"),
          DTOutput("roster"),
          p("Roster and initial seed order: ", tags$a("usddc2026.com", href = "https://usddc2026.com/", target = "_blank"), "; ranking snapshot published October 28, 2026. No network connection is needed to score."),
          p("Standard and placement matches: one game to 21, win by two, no cap. Championship: best of three to 21, each win by two. Reset: one game to 21, win by two, no cap. Byes are not games."),
          p("Within-pool tiebreaks use tied-team record, tied-team point difference, record against teams above the tied group, point difference against those teams, then overall pool point difference (WFDF 206.03, assuming no forfeits). Unresolved ties require an organizer decision before bracket seeds are released. Forfeits require a director ruling; do not enter invented scores.")))
      )
    )
  )
)

server <- function(input, output, session) {
  save_status <- reactiveVal("")
  pending_restore <- reactiveVal(NULL)
  state <- reactive(tournament_state_13(tournament_store_13()))
  active_stage <- reactiveVal("Pool play")
  blocked_stage <- reactiveVal(NULL)
  stage_problem <- function(stage, tournament) {
    if (length(stage) != 1L || is.na(stage) || !stage %in% c("Pool play", "Double elimination", "Placement")) return("Choose one of the listed stages.")
    if (stage == "Pool play") return(NULL)
    if (stage == "Double elimination") {
      if (tournament$seeded) return(NULL)
      remaining <- sum(tournament$games$stage == "Pool play" & tournament$games$status != "Complete")
      return(if (remaining) paste0("Pool play is not complete: ", remaining,
        " pool matches still need results. Finish pool play and resolve any ties before scoring double elimination.") else
        "Pool play has unresolved ties. Record the tie decisions in Reseeding tally before scoring double elimination.")
    }
    if (stage == "Placement") {
      ready <- tournament$games$stage == "Placement" & tournament$games$status %in% c("Ready", "Complete")
      if (any(ready)) return(NULL)
      return("Placement games are not ready yet. Continue double-elimination scoring until their prerequisite matches are complete.")
    }
    "Choose one of the listed stages."
  }
  observe({
    requested <- input$score_stage %||% "Pool play"
    tournament <- state()
    problem <- stage_problem(requested, tournament)
    previous <- isolate(active_stage())
    if (!is.null(problem)) {
      # Use the accepted stage for match choices; never clear them while the
      # browser is briefly displaying an unavailable requested stage.
      fallback <- if (is.null(stage_problem(previous, tournament))) previous else "Pool play"
      active_stage(fallback)
      blocked_stage(requested)
      updateSelectInput(session, "score_stage", selected = fallback)
      showNotification(problem, type = "warning", duration = 10, id = "stage-warning")
    } else {
      active_stage(requested)
      if (requested != previous) blocked_stage(NULL)
    }
  })
  output$stage_notice <- renderUI({
    stage <- blocked_stage()
    if (is.null(stage)) return(NULL)
    problem <- stage_problem(stage, state())
    if (!is.null(problem)) div(class = "alert alert-warning", role = "status", problem)
  })
  commit <- function(snapshot) {
    dir.create(dirname(state_file_13), recursive = TRUE, showWarnings = FALSE)
    temp <- tempfile("tournament-", tmpdir = dirname(state_file_13))
    on.exit(unlink(temp), add = TRUE)
    write_snapshot_13(snapshot, temp)
    if (!file.rename(temp, state_file_13)) stop("Could not save tournament progress. Download a backup and check the save directory.")
    tournament_store_13(snapshot)
    save_status(paste("Saved", format(Sys.time(), "%I:%M:%S %p", tz = "America/New_York"), "Eastern"))
  }
  handle_error <- function(expr) tryCatch(expr, error = function(e) {
    if (!inherits(e, "shiny.silent.error")) showNotification(conditionMessage(e), type = "error", duration = 9)
  })
  selected_game <- reactive({
    key <- input$match_key
    if (length(key) != 1L || is.na(key) || !nzchar(key)) return(NULL)
    g <- state()$games[state()$games$key == key, ]
    if (nrow(g) != 1L || g$stage != active_stage() || !g$status %in% c("Ready", "Complete")) return(NULL)
    g
  })
  observe({
    games <- state()$games
    games <- games[games$stage == active_stage() & games$status %in% c("Ready", "Complete"), ]
    games <- if (active_stage() == "Pool play") {
      games[order(games$pool, games$wave, games$court), ]
    } else games[order(games$wave, games$court), ]
    choices <- setNames(games$key, paste0(games$key, " | ", games$teamA, " vs ", games$teamB, " [", games$status, "]"))
    if (active_stage() == "Pool play") {
      pool_groups <- split(seq_len(nrow(games)), games$pool)
      choices <- setNames(lapply(pool_groups, function(rows) choices[rows]),
                          paste("Pool", names(pool_groups)))
    }
    current <- isolate(input$match_key)
    selected <- if (length(current) == 1L && !is.na(current) && current %in% games$key) current else {
      next_ready <- games$key[games$status == "Ready"]
      if (length(next_ready)) next_ready[1] else if (nrow(games)) games$key[1] else character()
    }
    updateSelectInput(session, "match_key", choices = choices, selected = selected)
  })
  output$score_editor <- renderUI({
    games <- state()$games
    available <- games$stage == active_stage() & games$status %in% c("Ready", "Complete")
    if (!any(available)) return(div(class = "alert alert-warning", role = "status", "Complete the prerequisite matches to unlock this stage."))
    g <- selected_game()
    if (is.null(g)) return(helpText("Choose a ready or completed match from the list."))
    record <- tournament_store_13()$results[[g$key]]
    scores <- matrix(NA_real_, nrow = g$best_of, ncol = 2)
    if (!is.null(record)) scores[seq_len(nrow(record$scores)), ] <- record$scores
    tagList(
      h4(paste(g$teamA, "vs", g$teamB)),
      p(paste0(if (g$stage == "Pool play") "Round " else "Wave ", g$wave, " | Court ", g$court,
               if (g$best_of == 3) " | Best of three to 21, win by two" else paste0(" | To ", g$target, ", win by two"))),
      if (g$best_of == 3) helpText("Enter each game's points. Leave game 3 blank after a 2-0 series."),
      lapply(seq_len(g$best_of), function(i) div(class = "score-game",
        if (g$best_of == 3) strong(paste("Game", i)),
        div(class = "score-pair",
          numericInput(paste0("scoreA_", i), g$teamA, value = scores[i, 1], min = 0, step = 1, width = "100%"),
          numericInput(paste0("scoreB_", i), g$teamB, value = scores[i, 2], min = 0, step = 1, width = "100%"))))
    )
  })
  observeEvent(input$save_score, handle_error({
    req(input$save_score > 0)
    g <- selected_game()
    if (is.null(g)) showNotification("Choose a ready or completed match before saving a result.", type = "warning")
    req(!is.null(g))
    scores <- do.call(rbind, lapply(seq_len(g$best_of), function(i) c(input[[paste0("scoreA_", i)]] %||% NA_real_, input[[paste0("scoreB_", i)]] %||% NA_real_)))
    used <- rowSums(!is.na(scores)) > 0
    if (any(used) && !identical(which(used), seq_len(sum(used)))) stop("Enter games in order, without gaps.")
    updated <- save_result_13(tournament_store_13(), g$key, scores[used, , drop = FALSE])
    cleared <- attr(updated, "cleared")
    commit(updated)
    showNotification(if (length(cleared)) paste("Result saved. Cleared obsolete results:", paste(cleared, collapse = ", ")) else "Result saved.", type = "message", duration = 8)
  }))
  observeEvent(input$clear_score, {
    req(input$clear_score > 0, input$match_key)
    if (is.null(selected_game())) {
      showNotification("Choose a ready or completed match before clearing a result.", type = "warning")
      return()
    }
    showModal(modalDialog(title = paste("Clear", input$match_key, "result?"),
      "Dependent results will also be cleared if their participants can no longer be determined.",
      footer = tagList(modalButton("Cancel"), actionButton("confirm_clear", "Clear result", class = "btn-danger"))))
  })
  observeEvent(input$confirm_clear, handle_error({ req(input$confirm_clear > 0); commit(save_result_13(tournament_store_13(), input$match_key)); removeModal() }))
  observeEvent(input$review_restore, handle_error({
    req(input$review_restore > 0, input$restore_file)
    snap <- read_snapshot_13(input$restore_file$datapath)
    pending_restore(snap)
    showModal(modalDialog(title = "Restore this tournament backup?",
      paste(length(snap$results), "saved match results. This replaces the current tournament progress."),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_restore", "Restore backup", class = "btn-primary"))))
  }))
  observeEvent(input$confirm_restore, handle_error({ req(input$confirm_restore > 0, pending_restore()); commit(pending_restore()); pending_restore(NULL); removeModal() }))
  output$save_status <- renderText(save_status())
  output$progress <- renderText({
    s <- state(); g <- s$games
    paste0(sum(g$stage == "Pool play" & g$status == "Complete"), "/22 pool matches complete | ",
           sum(g$stage != "Pool play" & g$status == "Complete"), " bracket / placement matches complete | ",
           sum(!is.na(s$final$team_id)), "/13 final places decided")
  })
  display_games <- function(games) {
    data.frame(Match = games$key, Stage = games$stage,
      Group = ifelse(games$pool == "", "Bracket", paste("Pool", games$pool)),
      `Round / wave` = games$wave, Court = games$court,
      `Team A` = ifelse(is.na(games$teamA), games$sourceA, games$teamA),
      `Team B` = ifelse(is.na(games$teamB), games$sourceB, games$teamB),
      Result = games$score, Status = games$status, check.names = FALSE)
  }
  dt <- function(x, pageLength = 40L) {
    labels <- c(pool_seed = "Pool seed", seed = "Original seed", team = "Team", pool = "Pool",
      played = "Played", wins = "Wins", losses = "Losses", point_diff = "Point diff", pool_finish = "Pool finish",
      earned_seed = "Earned seed", points_for = "Points for", points_against = "Points against",
      place = "Place", games_played = "Games played", points = "Ranking points")
    found <- names(x) %in% names(labels)
    names(x)[found] <- labels[names(x)[found]]
    datatable(x, rownames = FALSE, selection = "none", options = list(pageLength = pageLength, scrollX = TRUE, order = list(), dom = "tip"))
  }
  for (p in c("A", "B", "C")) local({
    pool <- p
    output[[paste0("pool_", pool)]] <- renderDT({
      t <- state()$tally; t <- t[t$pool == pool, ]
      dt(t[, c("pool_seed", "seed", "team", "played", "wins", "losses", "point_diff", "pool_finish", "earned_seed")])
    })
    output[[paste0("games_", pool)]] <- renderDT({ g <- state()$games; dt(display_games(g[g$pool == pool, ])) })
    observeEvent(input[[paste0("save_tie_", pool)]], handle_error({
      req(input[[paste0("save_tie_", pool)]] > 0)
      order <- suppressWarnings(as.integer(trimws(strsplit(input[[paste0("tie_", pool)]], ",", fixed = TRUE)[[1]])))
      commit(save_tie_order_13(tournament_store_13(), pool, order))
      showNotification("Tie order saved. Only otherwise unresolved ties use this order.", type = "message")
    }))
  })
  output$pools_view <- renderUI(tagList(lapply(c("A", "B", "C"), function(p) div(class = "panel-card",
    h3(paste("Pool", p)), p(paste("Courts", paste(seq_len(2) + match(p, c("A", "B", "C")) * 2 - 2, collapse = " and "))),
    DTOutput(paste0("pool_", p)), h4("Matches"), DTOutput(paste0("games_", p))))))
  output$tally <- renderDT(dt(state()$tally[, c("pool", "pool_seed", "seed", "team", "wins", "losses", "points_for", "points_against", "pool_finish", "earned_seed")]))
  output$ties_view <- renderUI({
    s <- state(); tagList(lapply(c("A", "B", "C"), function(p) {
      t <- s$tally[s$tally$pool == p, ]; g <- s$games[s$games$pool == p, ]
      if (!all(g$status == "Complete") || !any(t$unresolved)) return(NULL)
      div(class = "panel-card", h4(paste("Pool", p, if (any(!t$resolved)) "needs a tie decision" else "tie decision saved")),
        p("The statistical comparisons still leave a tie. Resolve it with a playoff or disc flip, then list every original seed in this pool in the desired tie order. Only otherwise tied teams use this order."),
        textInput(paste0("tie_", p), "Original seeds, separated by commas", value = paste(t$seed, collapse = ", ")),
        actionButton(paste0("save_tie_", p), "Save tie decision"))
    }))
  })
  output$bracket <- renderDT({ g <- state()$games; dt(display_games(g[g$stage == "Double elimination", ])) })
  output$placements <- renderDT({ g <- state()$games; dt(display_games(g[g$stage == "Placement", ])) })
  output$schedule <- renderDT({ g <- state()$games; g <- g[order(g$stage != "Pool play", g$wave, g$court), ]; dt(display_games(g), 60) })
  output$final_table <- renderDT(dt(state()$final[, c("place", "team", "games_played")]))
  output$roster <- renderDT(dt(state()$teams[, c("seed", "team", "pool", "pool_seed", "points")]))
  output$backup <- downloadHandler(filename = function() paste0("ddc-tournament-", Sys.Date(), ".json"), contentType = "application/json",
    content = function(file) write_snapshot_13(tournament_store_13(), file))
  output$pool_pdf <- downloadHandler(filename = function() "ddc-2026-pool-score-sheets.pdf", contentType = "application/pdf",
    content = function(file) write_pool_pdfs_13(file, state(), tournament_store_13()))
  output$tally_pdf <- downloadHandler(filename = function() "ddc-2026-reseeding-tally.pdf", contentType = "application/pdf",
    content = function(file) write_tally_pdf_13(file, state()))
  output$bracket_pdf <- downloadHandler(filename = function() "ddc-2026-bracket-and-placement.pdf", contentType = "application/pdf",
    content = function(file) write_bracket_pdf_13(file, state(), tournament_store_13()))
  output$standings_csv <- downloadHandler(filename = function() "ddc-2026-final-standings.csv", content = function(file) write.csv(state()$final, file, row.names = FALSE))
  output$schedule_csv <- downloadHandler(filename = function() "ddc-2026-schedule.csv", content = function(file) write.csv(display_games(state()$games), file, row.names = FALSE))
}
shinyApp(ui, server)

# DDC fixed-team tournament scheduler
# Shiny app for the 10-12 team US Championship preset.

library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DT)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

source("R/us_championship_engine.R")
source("R/ddc_rankings.R")

short_team_name <- function(name) {
  name <- str_squish(name %||% "")
  if_else(str_length(name) > 28, paste0(str_sub(name, 1, 25), "..."), name)
}

score_input_id <- function(prefix, gid, version) paste(prefix, version, gid, sep = "_")

scores_for_games <- function(games, input, version) {
  if (is.null(games) || nrow(games) == 0) {
    return(tibble(game_id = integer(), scoreA = numeric(), scoreB = numeric()))
  }
  pull_score <- function(prefix, gid) {
    value <- input[[score_input_id(prefix, gid, version)]]
    if (is.null(value)) NA_real_ else as.numeric(value)
  }
  tibble(
    game_id = games$game_id,
    scoreA = map_dbl(games$game_id, ~ pull_score("A", .x)),
    scoreB = map_dbl(games$game_id, ~ pull_score("B", .x))
  )
}

display_match <- function(teamA, teamB, sourceA, sourceB) {
  left <- if_else(is.na(teamA) | teamA == "", sourceA, teamA)
  right <- if_else(is.na(teamB) | teamB == "", sourceB, teamB)
  paste(short_team_name(left), "vs", short_team_name(right))
}

display_schedule <- function(games, scores = NULL) {
  out <- games
  if (!is.null(scores)) out <- out %>% left_join(scores, by = "game_id")
  out %>%
    transmute(
      Game = game_id,
      Key = game_key,
      Day = day,
      Stage = stage,
      Group = group,
      Round = round,
      Court = court,
      Match = display_match(teamA, teamB, sourceA, sourceB),
      Label = label,
      Score = if ("scoreA" %in% names(.)) if_else(is.na(scoreA) | is.na(scoreB), "", paste(scoreA, "-", scoreB)) else ""
    )
}

score_card <- function(games, version) {
  ready_games <- games %>% filter(!is.na(teamA_id), !is.na(teamB_id))
  if (nrow(ready_games) == 0) return(NULL)
  tagList(
    lapply(split(ready_games, ready_games$stage), function(stage_games) {
      tags$section(
        class = "score-section",
        tags$h3(unique(stage_games$stage)),
        lapply(split(stage_games, stage_games$round), function(round_games) {
          tags$div(
            class = "round-block",
            tags$h4(paste("Round", unique(round_games$round))),
            lapply(seq_len(nrow(round_games)), function(i) {
              game <- round_games[i, ]
              tags$div(
                class = "score-row",
                tags$div(
                  class = "match-label",
                  tags$strong(game$label),
                  tags$span(paste0("Court ", game$court)),
                  tags$p(paste(game$teamA, "vs", game$teamB))
                ),
                numericInput(score_input_id("A", game$game_id, version), game$teamA, value = NA, min = 0, step = 1, width = "120px"),
                numericInput(score_input_id("B", game$game_id, version), game$teamB, value = NA, min = 0, step = 1, width = "120px")
              )
            })
          )
        })
      )
    })
  )
}

format_summary_text <- function(n) {
  sizes <- us_championship_pool_sizes(n)
  placement <- switch(
    as.character(n),
    `10` = "Seeds 9-10 play one placement match.",
    `11` = "Seeds 9-11 play a placement round robin.",
    `12` = "Seeds 9-12 play placement semifinals, then 9th and 11th place matches."
  )
  paste0(
    "Day 1: ", paste(sizes, collapse = " + "), " pools, full round robin. ",
    "Day 2: top 8 advance to a double-elimination championship bracket. ",
    placement
  )
}

selected_player_ids <- function(input, n, prefix) {
  map_chr(seq_len(n), function(i) {
    value <- input[[paste0(prefix, "_", i)]]
    if (is.null(value) || identical(value, "")) NA_character_ else as.character(value)
  })
}

default_teams <- function(n) {
  tibble(
    team_id = seq_len(n),
    seed = seq_len(n),
    team = sprintf("Team %02d", seq_len(n)),
    player1 = "",
    player2 = "",
    player1_rank = NA_integer_,
    player2_rank = NA_integer_,
    team_points = NA_real_,
    missing_ranking = TRUE
  )
}

team_builder_ui <- function(n) {
  tagList(
    lapply(seq_len(n), function(i) {
      tags$div(
        class = "team-entry-row",
        tags$div(class = "team-entry-label", paste("Team", i)),
        selectizeInput(
          paste0("player1_", i),
          "Player 1",
          choices = NULL,
          selected = NULL,
          width = "100%",
          options = list(placeholder = "Search player")
        ),
        selectizeInput(
          paste0("player2_", i),
          "Player 2",
          choices = NULL,
          selected = NULL,
          width = "100%",
          options = list(placeholder = "Search player")
        )
      )
    })
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f6f7f9; color: #17202a; }
      .app-shell { max-width: 1280px; margin: 0 auto; }
      .topbar { padding: 22px 0 10px; border-bottom: 1px solid #d7dde4; margin-bottom: 18px; }
      .topbar h1 { font-size: 28px; margin: 0 0 4px; font-weight: 700; }
      .topbar p { color: #56616f; margin: 0; }
      .panel { background: #fff; border: 1px solid #dce2e8; border-radius: 8px; padding: 16px; margin-bottom: 16px; }
      .panel h2, .panel h3 { margin-top: 0; }
      .format-note { background: #edf7f3; border-left: 4px solid #16805b; padding: 10px 12px; margin-bottom: 12px; }
      .warn-note { background: #fff7e6; border-left: 4px solid #c77900; padding: 10px 12px; margin-bottom: 12px; }
      .score-section h3 { font-size: 20px; margin-top: 8px; }
      .round-block { border-top: 1px solid #e6ebf0; padding-top: 12px; margin-top: 12px; }
      .round-block h4 { font-size: 16px; color: #394655; }
      .score-row { display: grid; grid-template-columns: minmax(240px, 1fr) auto auto; gap: 12px; align-items: end; padding: 10px 0; border-bottom: 1px solid #eef2f5; }
      .match-label p { margin: 4px 0 0; }
      .match-label span { color: #697586; margin-left: 8px; font-size: 13px; }
      .team-entry-row { border-top: 1px solid #e4e9ef; padding-top: 12px; margin-top: 12px; }
      .team-entry-label { font-weight: 700; color: #263342; margin-bottom: 2px; }
      .team-entry-row .form-group { margin-bottom: 8px; }
      .ranking-status { color: #56616f; font-size: 13px; margin: 8px 0 12px; }
      .credit { color: #697586; font-size: 13px; }
      @media (max-width: 760px) {
        .score-row { grid-template-columns: 1fr; align-items: stretch; }
      }
    "))
  ),
  div(
    class = "app-shell",
    div(
      class = "topbar",
      h1("DDC (Escape!! Frisbee) Fixed-Team Tournament Scheduler"),
      p("US Championship preset: Day 1 pool play, Day 2 top-8 double elimination and placement matches.")
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        numericInput("team_count", "Teams", value = 12, min = 10, max = 12, step = 1),
        numericInput("ranking_year", "Ranking year", value = as.integer(format(Sys.Date(), "%Y")), min = 2018, max = as.integer(format(Sys.Date(), "%Y")), step = 1),
        selectInput("ranking_division", "Ranking division", choices = c("Open" = "O", "Women" = "W", "Mixed" = "X"), selected = "O"),
        actionButton("load_rankings", "Load rankings", class = "btn-default"),
        div(class = "ranking-status", textOutput("ranking_status", inline = TRUE)),
        uiOutput("team_builder"),
        actionButton("apply_teams", "Build schedule", class = "btn-primary"),
        br(),
        br(),
        downloadButton("download_schedule", "Download schedule"),
        downloadButton("download_standings", "Download standings")
      ),
      mainPanel(
        width = 8,
        uiOutput("format_summary"),
        tabsetPanel(
          tabPanel("Teams", div(class = "panel", DTOutput("teams_table"))),
          tabPanel("Schedule", div(class = "panel", DTOutput("schedule_table"))),
          tabPanel("Scores", div(class = "panel", uiOutput("score_inputs"))),
          tabPanel("Standings", div(class = "panel", uiOutput("standings_ui"))),
          tabPanel("Credit", div(class = "panel", uiOutput("credit_ui")))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  ranking_data <- reactiveVal(NULL)
  ranking_error <- reactiveVal(NULL)
  teams_value <- reactiveVal(default_teams(12))

  load_rankings <- function() {
    ranking_error(NULL)
    tryCatch({
      rankings <- fetch_latest_ddc_player_rankings(
        start_year = input$ranking_year,
        division = input$ranking_division
      )
      ranking_data(rankings)
      showNotification("DDC rankings loaded.", type = "message")
    }, error = function(error) {
      ranking_data(NULL)
      ranking_error(conditionMessage(error))
      showNotification(conditionMessage(error), type = "error", duration = 8)
    })
  }

  observeEvent(input$load_rankings, load_rankings(), ignoreInit = FALSE)

  output$ranking_status <- renderText({
    rankings <- ranking_data()
    if (!is.null(rankings)) {
      return(paste0(
        nrow(rankings), " players loaded from ",
        unique(rankings$ranking_year), " ",
        unique(rankings$division), " rankings."
      ))
    }
    if (!is.null(ranking_error())) return(paste("Rankings not loaded:", ranking_error()))
    "Rankings not loaded yet."
  })

  output$team_builder <- renderUI({
    team_builder_ui(input$team_count)
  })

  observe({
    rankings <- ranking_data()
    choices <- if (is.null(rankings)) character(0) else c("Choose player" = "", ranking_choice_labels(rankings))
    for (i in seq_len(input$team_count)) {
      updateSelectizeInput(session, paste0("player1_", i), choices = choices, selected = "", server = TRUE)
      updateSelectizeInput(session, paste0("player2_", i), choices = choices, selected = "", server = TRUE)
    }
  })

  observeEvent(input$team_count, {
    if (nrow(teams_value()) != input$team_count) {
      teams_value(default_teams(input$team_count))
    }
  })

  observeEvent(input$apply_teams, {
    rankings <- ranking_data()
    validate(need(!is.null(rankings), "Load DDC rankings before building the schedule."))

    player1_ids <- selected_player_ids(input, input$team_count, "player1")
    player2_ids <- selected_player_ids(input, input$team_count, "player2")
    selected_ids <- c(player1_ids, player2_ids)

    validate(need(!any(is.na(selected_ids)), "Choose two ranked players for every team."))
    validate(need(length(unique(selected_ids)) == length(selected_ids), "Each player can only appear once."))

    teams_value(seed_teams_from_player_ids(player1_ids, player2_ids, rankings))
    showNotification("Schedule rebuilt from selected teams and ranking points.", type = "message")
  })

  teams <- reactive(teams_value())

  preset <- reactive(make_us_championship_preset(teams()))
  pool_stage <- reactive(preset()$pool_stage)
  pool_scores <- reactive(scores_for_games(pool_stage()$games, input, "pool"))

  championship_seed_order <- reactive({
    if (!games_complete(pool_scores())) return(NULL)
    seed_top_eight(teams(), pool_stage(), pool_scores())
  })

  bracket_template <- reactive({
    order <- championship_seed_order()
    if (is.null(order)) return(NULL)
    make_top8_double_elim(teams(), order, game_offset = max(pool_stage()$games$game_id))
  })

  bracket_games <- reactive({
    games <- bracket_template()
    if (is.null(games)) return(NULL)
    resolve_dependency_games(games, bracket_scores())
  })

  bracket_scores <- reactive({
    games <- bracket_template()
    if (is.null(games)) return(tibble(game_id = integer(), scoreA = numeric(), scoreB = numeric()))
    scores_for_games(games, input, "bracket")
  })

  placement_template <- reactive({
    order <- championship_seed_order()
    if (is.null(order)) return(NULL)
    offset <- max(pool_stage()$games$game_id) + nrow(bracket_template())
    make_bottom_placement(teams(), order, game_offset = offset)
  })
9
  placement_games <- reactive({
    games <- placement_template()
    if (is.null(games)) return(NULL)
    resolve_dependency_games(games, placement_scores())
  })

  placement_scores <- reactive({
    games <- placement_template()
    if (is.null(games)) return(tibble(game_id = integer(), scoreA = numeric(), scoreB = numeric()))
    scores_for_games(games, input, "placement")
  })

  all_games <- reactive(bind_rows(
    pool_stage()$games,
    if (!is.null(bracket_games())) bracket_games(),
    if (!is.null(placement_games())) placement_games()
  ))

  all_scores <- reactive(bind_rows(
    pool_scores(),
    if (!is.null(bracket_games())) bracket_scores(),
    if (!is.null(placement_games())) placement_scores()
  ))

  output$format_summary <- renderUI({
    div(class = "format-note", strong("Format: "), format_summary_text(nrow(teams())))
  })

  output$teams_table <- renderDT({
    pool_tbl <- bind_rows(pool_stage()$pools)
    datatable(
      pool_tbl %>%
        select(
          Seed = seed,
          Team = team,
          `Team points` = team_points,
          Player1 = player1,
          `P1 rank` = player1_rank,
          Player2 = player2,
          `P2 rank` = player2_rank,
          Pool = pool,
          `Pool seed` = pool_position
        ),
      rownames = FALSE,
      options = list(pageLength = 12)
    )
  })

  output$schedule_table <- renderDT({
    datatable(display_schedule(all_games(), all_scores()), rownames = FALSE, options = list(pageLength = 30))
  })

  output$score_inputs <- renderUI({
    tagList(
      score_card(pool_stage()$games, "pool"),
      if (!games_complete(pool_scores())) div(class = "warn-note", "Complete all pool scores to seed the top-8 championship bracket and placement games."),
      if (!is.null(bracket_games())) score_card(bracket_games(), "bracket"),
      if (!is.null(placement_games())) score_card(placement_games(), "placement")
    )
  })

  output$standings_ui <- renderUI({
    pool_names <- names(pool_stage()$pools)
    tagList(
      lapply(pool_names, function(group_name) {
        table_id <- paste0("standings_", str_replace_all(str_to_lower(group_name), "[^a-z0-9]+", "_"))
        output[[table_id]] <- renderDT({
          team_standings(teams(), pool_stage()$games, pool_scores(), group_name) %>%
            select(Place = place, Team = team, Seed = seed, Played = played, Wins = wins, Losses = losses,
                   `Win %` = win_pct, `Point diff` = point_diff, `For` = points_for, `Against` = points_against) %>%
            datatable(rownames = FALSE, options = list(dom = "t"))
        })
        div(class = "panel", h3(group_name), DTOutput(table_id))
      }),
      if (!is.null(championship_seed_order())) {
        output$championship_seeds <- renderDT({
          championship_seed_order() %>%
            select(`Champ seed` = championship_seed, Team = team, `Pool place` = pool_place, Pool = pool, Wins = wins, `Point diff` = point_diff, `Original seed` = seed) %>%
            datatable(rownames = FALSE, options = list(dom = "t"))
        })
        div(class = "panel", h3("Championship Seeds"), DTOutput("championship_seeds"))
      }
    )
  })

  output$credit_ui <- renderUI({
    tags$div(
      tags$p("This Shiny app was built for U.S. fixed-team DDC tournaments."),
      tags$p(
        class = "credit",
        "Initial seeding can use the current player ranking API behind ",
        tags$a("doubledisccourt.com/results/rankings.html", href = "https://doubledisccourt.com/results/rankings.html", target = "_blank"),
        "."
      )
    )
  })

  output$download_schedule <- downloadHandler(
    filename = function() paste0("ddc-us-championship-schedule-", Sys.Date(), ".csv"),
    content = function(file) write.csv(display_schedule(all_games(), all_scores()), file, row.names = FALSE)
  )

  output$download_standings <- downloadHandler(
    filename = function() paste0("ddc-us-championship-standings-", Sys.Date(), ".csv"),
    content = function(file) {
      standings <- map_dfr(names(pool_stage()$pools), function(group_name) {
        team_standings(teams(), pool_stage()$games, pool_scores(), group_name) %>% mutate(group = group_name, .before = 1)
      })
      write.csv(standings, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

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
        width = 3,
        numericInput("team_count", "Teams", value = 12, min = 10, max = 12, step = 1),
        textAreaInput(
          "team_names",
          "Teams in seed order",
          value = paste(sprintf("Player %02da / Player %02db", 1:12, 1:12), collapse = "\n"),
          rows = 14,
          resize = "vertical"
        ),
        actionButton("apply_teams", "Update teams", class = "btn-primary"),
        actionButton("rank_from_ddc", "Seed from DDC rankings"),
        br(), br(),
        numericInput("ranking_year", "Ranking year", value = as.integer(format(Sys.Date(), "%Y")), min = 2018, max = as.integer(format(Sys.Date(), "%Y")), step = 1),
        selectInput("ranking_division", "Ranking division", choices = c("Open" = "O", "Women" = "W", "Mixed" = "X"), selected = "O"),
        br(),
        downloadButton("download_schedule", "Download schedule"),
        downloadButton("download_standings", "Download standings")
      ),
      mainPanel(
        width = 9,
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
  observeEvent(input$team_count, {
    current <- str_split(input$team_names %||% "", "\n")[[1]] %>% str_squish() %>% discard(~ .x == "")
    n <- input$team_count
    if (length(current) < n) {
      current <- c(current, sprintf("Player %02da / Player %02db", (length(current) + 1):n, (length(current) + 1):n))
    }
    current <- current[seq_len(n)]
    updateTextAreaInput(session, "team_names", value = paste(current, collapse = "\n"))
  })

  ranked_team_text <- reactiveVal(NULL)

  observeEvent(input$rank_from_ddc, {
    lines <- str_split(input$team_names %||% "", "\n")[[1]] %>% str_squish() %>% discard(~ .x == "")
    tryCatch({
      ranked <- rank_team_lines_from_ddc_rankings(lines, year = input$ranking_year, division = input$ranking_division)
      ranked_team_text(paste(ranked$team, collapse = "\n"))
      updateTextAreaInput(session, "team_names", value = ranked_team_text())
      showNotification("Teams reseeded from current DDC ranking points.", type = "message")
    }, error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = 8)
    })
  })

  teams <- eventReactive(input$apply_teams, {
    names <- str_split(input$team_names %||% "", "\n")[[1]] %>% str_squish() %>% discard(~ .x == "")
    validate(need(length(names) == input$team_count, "Enter one team per line, matching the team count."))
    validate(need(length(unique(names)) == length(names), "Team names must be unique."))
    parse_team_roster(names) %>%
      mutate(seed = row_number(), .after = team_id)
  }, ignoreNULL = FALSE)

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
      pool_tbl %>% select(Seed = seed, Team = team, Player1 = player1, Player2 = player2, Pool = pool, `Pool seed` = pool_position),
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

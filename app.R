# DDC fixed-team tournament scheduler
# Shiny app for pool play, A/B groups, and placement matches.

library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DT)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

short_team_name <- function(name) {
  name <- str_squish(name)
  if_else(str_length(name) > 28, paste0(str_sub(name, 1, 25), "..."), name)
}

pool_letters <- function(n) LETTERS[seq_len(n)]

pool_size_plan <- function(n) {
  if (n < 4 || n > 20) {
    stop("This scheduler supports 4 to 20 fixed teams.")
  }
  if (n <= 7) return(n)
  plans <- list(
    `8` = c(4, 4),
    `9` = c(5, 4),
    `10` = c(5, 5),
    `11` = c(4, 4, 3),
    `12` = c(4, 4, 4),
    `13` = c(5, 4, 4),
    `14` = c(5, 5, 4),
    `15` = c(5, 5, 5),
    `16` = c(4, 4, 4, 4),
    `17` = c(5, 4, 4, 4),
    `18` = c(5, 5, 4, 4),
    `19` = c(5, 5, 5, 4),
    `20` = c(4, 4, 4, 4, 4)
  )
  plans[[as.character(n)]]
}

snake_seed_pools <- function(teams) {
  n <- nrow(teams)
  sizes <- pool_size_plan(n)
  if (length(sizes) == 1) {
    teams$pool <- "Round Robin"
    teams$pool_position <- seq_len(n)
    return(split(teams, teams$pool))
  }

  pool_count <- length(sizes)
  pools <- vector("list", pool_count)
  names(pools) <- paste("Pool", pool_letters(pool_count))
  for (i in seq_along(pools)) pools[[i]] <- teams[0, ]

  seed_order <- seq_len(n)
  block_size <- pool_count
  for (block_start in seq(1, n, by = block_size)) {
    block <- seed_order[block_start:min(block_start + block_size - 1, n)]
    block_index <- ((block_start - 1) / block_size) + 1
    destinations <- if (block_index %% 2 == 1) seq_len(pool_count) else rev(seq_len(pool_count))

    for (j in seq_along(block)) {
      target <- destinations[j]
      if (nrow(pools[[target]]) >= sizes[target]) {
        available <- destinations[sizes[destinations] > map_int(pools[destinations], nrow)]
        target <- available[1]
      }
      pools[[target]] <- bind_rows(pools[[target]], teams[block[j], ])
    }
  }

  imap(pools, function(pool, nm) {
    pool$pool <- nm
    pool$pool_position <- seq_len(nrow(pool))
    pool
  })
}

round_robin_pairs <- function(n) {
  players <- seq_len(n)
  if (n %% 2 == 1) players <- c(players, NA_integer_)
  rounds <- length(players) - 1
  half <- length(players) / 2
  out <- vector("list", rounds)

  for (round in seq_len(rounds)) {
    left <- players[seq_len(half)]
    right <- rev(players[(half + 1):length(players)])
    games <- tibble(team1 = left, team2 = right) %>%
      filter(!is.na(team1), !is.na(team2))
    if (round %% 2 == 0) {
      games <- tibble(team1 = games$team2, team2 = games$team1)
    }
    out[[round]] <- games %>% mutate(round = round, .before = 1)
    players <- c(players[1], players[length(players)], players[2:(length(players) - 1)])
  }

  bind_rows(out)
}

make_pool_schedule <- function(pool, stage, court_offset = 0, game_offset = 0) {
  rr <- round_robin_pairs(nrow(pool))
  rr %>%
    mutate(
      game_id = row_number() + game_offset,
      stage = stage,
      group = pool$pool[1],
      label = "Round robin",
      court = row_number() - ave(row_number(), round, FUN = min) + 1 + court_offset,
      teamA_id = pool$team_id[team1],
      teamB_id = pool$team_id[team2],
      teamA = pool$team[team1],
      teamB = pool$team[team2]
    ) %>%
    select(game_id, stage, group, round, court, label, teamA_id, teamB_id, teamA, teamB)
}

make_stage1 <- function(teams) {
  pools <- snake_seed_pools(teams)
  game_offset <- 0
  court_offset <- 0
  schedules <- list()

  for (i in seq_along(pools)) {
    schedules[[i]] <- make_pool_schedule(pools[[i]], "Preliminary", court_offset, game_offset)
    game_offset <- max(schedules[[i]]$game_id)
    court_offset <- court_offset + max(schedules[[i]]$court - court_offset)
  }

  list(games = bind_rows(schedules), pools = pools)
}

score_input_id <- function(prefix, gid, version) paste(prefix, version, gid, sep = "_")

scores_for_games <- function(games, input, version) {
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

games_complete <- function(scores) {
  nrow(scores) > 0 && all(!is.na(scores$scoreA), !is.na(scores$scoreB))
}

game_results <- function(games, scores) {
  games %>%
    left_join(scores, by = "game_id") %>%
    mutate(
      diff = if_else(is.na(scoreA) | is.na(scoreB), NA_real_, scoreA - scoreB),
      winner_id = case_when(diff > 0 ~ teamA_id, diff < 0 ~ teamB_id, TRUE ~ NA_integer_),
      loser_id = case_when(diff > 0 ~ teamB_id, diff < 0 ~ teamA_id, TRUE ~ NA_integer_)
    )
}

team_standings <- function(teams, games, scores, group_name = NULL) {
  scoped_games <- games
  if (!is.null(group_name)) scoped_games <- scoped_games %>% filter(group == group_name)
  results <- game_results(scoped_games, scores)

  records <- bind_rows(
    results %>% transmute(team_id = teamA_id, pf = scoreA, pa = scoreB, result = case_when(diff > 0 ~ "W", diff < 0 ~ "L", TRUE ~ NA_character_)),
    results %>% transmute(team_id = teamB_id, pf = scoreB, pa = scoreA, result = case_when(diff < 0 ~ "W", diff > 0 ~ "L", TRUE ~ NA_character_))
  ) %>%
    group_by(team_id) %>%
    summarise(
      played = sum(!is.na(result)),
      wins = sum(result == "W", na.rm = TRUE),
      losses = sum(result == "L", na.rm = TRUE),
      points_for = sum(pf, na.rm = TRUE),
      points_against = sum(pa, na.rm = TRUE),
      point_diff = points_for - points_against,
      .groups = "drop"
    )

  h2h <- function(tied_ids) {
    if (length(tied_ids) <= 1) return(rep(0, length(tied_ids)))
    tied_results <- results %>%
      filter(teamA_id %in% tied_ids, teamB_id %in% tied_ids, !is.na(winner_id))
    map_int(tied_ids, ~ sum(tied_results$winner_id == .x))
  }

  base <- teams %>%
    select(team_id, seed, team) %>%
    filter(team_id %in% unique(c(scoped_games$teamA_id, scoped_games$teamB_id))) %>%
    left_join(records, by = "team_id") %>%
    mutate(across(c(played, wins, losses, points_for, points_against, point_diff), ~ replace_na(.x, 0)))

  base %>%
    group_by(wins) %>%
    mutate(head_to_head_wins = h2h(team_id)) %>%
    ungroup() %>%
    arrange(desc(wins), desc(head_to_head_wins), desc(point_diff), seed) %>%
    mutate(place = row_number(), win_pct = if_else(played > 0, wins / played, NA_real_)) %>%
    select(place, team, seed, played, wins, losses, win_pct, point_diff, points_for, points_against, head_to_head_wins, team_id)
}

make_ab_groups <- function(teams, stage1, stage1_scores) {
  pool_names <- names(stage1$pools)
  if (identical(pool_names, "Round Robin")) return(NULL)

  ranked <- map(pool_names, ~ team_standings(teams, stage1$games, stage1_scores, .x))
  names(ranked) <- pool_names

  a_ids <- flatten_int(map(ranked, ~ head(.x$team_id, min(2, nrow(.x)))))
  b_ids <- flatten_int(map(ranked, ~ setdiff(.x$team_id, head(.x$team_id, min(2, nrow(.x))))))

  groups <- list(
    "Group A" = teams %>% filter(team_id %in% a_ids) %>% mutate(pool = "Group A") %>% arrange(match(team_id, a_ids)),
    "Group B" = teams %>% filter(team_id %in% b_ids) %>% mutate(pool = "Group B") %>% arrange(match(team_id, b_ids))
  )
  groups <- keep(groups, ~ nrow(.x) >= 2)
  imap(groups, ~ mutate(.x, pool_position = row_number()))
}

make_stage2 <- function(teams, stage1, stage1_scores) {
  groups <- make_ab_groups(teams, stage1, stage1_scores)
  if (is.null(groups)) return(NULL)

  game_offset <- max(stage1$games$game_id)
  court_offset <- 0
  schedules <- list()
  for (i in seq_along(groups)) {
    schedules[[i]] <- make_pool_schedule(groups[[i]], "A/B Group", court_offset, game_offset)
    game_offset <- max(schedules[[i]]$game_id)
    court_offset <- court_offset + max(schedules[[i]]$court - court_offset)
  }
  list(games = bind_rows(schedules), groups = groups)
}

make_finals_groups <- function(order_tbl) {
  ids <- order_tbl$team_id
  groups <- list()
  start <- 1
  while (start <= length(ids)) {
    remaining <- length(ids) - start + 1
    size <- if (remaining == 5) 5 else min(4, remaining)
    if (remaining == 1 && length(groups) > 0) {
      groups[[length(groups)]] <- c(groups[[length(groups)]], ids[start])
      break
    }
    groups[[length(groups) + 1]] <- ids[start:(start + size - 1)]
    start <- start + size
  }
  groups
}

make_finals_semis <- function(teams, stage2, stage2_scores) {
  if (is.null(stage2) || !games_complete(stage2_scores)) return(NULL)
  a <- team_standings(teams, stage2$games, stage2_scores, "Group A")
  b <- team_standings(teams, stage2$games, stage2_scores, "Group B")
  order_tbl <- bind_rows(a, b)
  groups <- make_finals_groups(order_tbl)

  game_offset <- max(stage2$games$game_id)
  games <- imap_dfr(groups, function(ids, idx) {
    previous_groups <- if (idx == 1) list() else groups[seq_len(idx - 1)]
    base <- sum(map_int(previous_groups, length))
    label <- paste0("Places ", base + 1, "-", base + length(ids))
    group_teams <- teams %>% filter(team_id %in% ids) %>% arrange(match(team_id, ids))
    if (length(ids) == 2) {
      tibble(
        game_id = game_offset + idx,
        stage = "Placement",
        group = label,
        round = 1,
        court = idx,
        label = label,
        teamA_id = ids[1],
        teamB_id = ids[2]
      )
    } else if (length(ids) %in% c(3, 5)) {
      rr <- make_pool_schedule(group_teams %>% mutate(pool = label), "Placement", idx - 1, game_offset + idx * 10)
      rr %>% mutate(group = label, label = "Placement round robin")
    } else {
      tibble(
        game_id = game_offset + idx * 10 + c(1, 2),
        stage = "Placement",
        group = label,
        round = 1,
        court = idx * 2 + c(-1, 0),
        label = if_else(base == 0, "Semifinal", "Placement semifinal"),
        teamA_id = c(ids[1], ids[2]),
        teamB_id = c(ids[4], ids[3])
      )
    }
  }) %>%
    left_join(teams %>% select(teamA_id = team_id, teamA = team), by = "teamA_id") %>%
    left_join(teams %>% select(teamB_id = team_id, teamB = team), by = "teamB_id") %>%
    arrange(group, round, court)

  list(games = games, order = order_tbl, groups = groups)
}

make_placement_matches <- function(teams, finals_semis, finals_scores) {
  if (is.null(finals_semis)) return(NULL)
  semis <- game_results(finals_semis$games, finals_scores)
  extra <- list()

  for (group_name in unique(semis$group)) {
    group_games <- semis %>% filter(group == group_name)
    if (nrow(group_games) != 2 || any(is.na(group_games$winner_id)) || any(group_games$label == "Placement round robin")) next

    base <- as.integer(str_match(group_name, "Places ([0-9]+)-")[, 2])
    extra[[group_name]] <- tibble(
      game_id = max(finals_semis$games$game_id) + length(extra) * 2 + c(1, 2),
      stage = "Placement",
      group = group_name,
      round = 2,
      court = group_games$court,
      label = c(
        if (base == 1) "Final" else paste0("Places ", base, "-", base + 1),
        if (base == 1) "Bronze match" else paste0("Places ", base + 2, "-", base + 3)
      ),
      teamA_id = c(group_games$winner_id[1], group_games$loser_id[1]),
      teamB_id = c(group_games$winner_id[2], group_games$loser_id[2])
    )
  }

  if (length(extra) == 0) return(NULL)
  bind_rows(extra) %>%
    left_join(teams %>% select(teamA_id = team_id, teamA = team), by = "teamA_id") %>%
    left_join(teams %>% select(teamB_id = team_id, teamB = team), by = "teamB_id")
}

display_schedule <- function(games, scores = NULL) {
  out <- games
  if (!is.null(scores)) out <- out %>% left_join(scores, by = "game_id")
  out %>%
    transmute(
      Game = game_id,
      Stage = stage,
      Group = group,
      Round = round,
      Court = court,
      Match = paste(short_team_name(teamA), "vs", short_team_name(teamB)),
      Label = label,
      Score = if ("scoreA" %in% names(.)) if_else(is.na(scoreA) | is.na(scoreB), "", paste(scoreA, "-", scoreB)) else ""
    )
}

score_card <- function(games, version) {
  if (is.null(games) || nrow(games) == 0) return(NULL)
  tagList(
    lapply(split(games, games$stage), function(stage_games) {
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
                tags$div(class = "match-label", tags$strong(game$label), tags$span(paste0("Court ", game$court)), tags$p(paste(game$teamA, "vs", game$teamB))),
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
      h1("DDC Fixed-Team Tournament Scheduler"),
      p("Pool play, A/B groups, and placement matches for 4-20 fixed teams.")
    ),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        numericInput("team_count", "Teams", value = 12, min = 4, max = 20, step = 1),
        textAreaInput(
          "team_names",
          "Seeded team list",
          value = paste(sprintf("Team %02d", 1:12), collapse = "\n"),
          rows = 14,
          resize = "vertical"
        ),
        actionButton("apply_teams", "Update teams", class = "btn-primary"),
        br(), br(),
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
    if (length(current) < n) current <- c(current, sprintf("Team %02d", (length(current) + 1):n))
    current <- current[seq_len(n)]
    updateTextAreaInput(session, "team_names", value = paste(current, collapse = "\n"))
  })

  teams <- eventReactive(input$apply_teams, {
    names <- str_split(input$team_names %||% "", "\n")[[1]] %>% str_squish() %>% discard(~ .x == "")
    validate(need(length(names) == input$team_count, "Enter one team per line, matching the team count."))
    validate(need(length(unique(names)) == length(names), "Team names must be unique."))
    tibble(team_id = seq_along(names), seed = seq_along(names), team = names)
  }, ignoreNULL = FALSE)

  stage1 <- reactive(make_stage1(teams()))
  stage1_scores <- reactive(scores_for_games(stage1()$games, input, "s1"))
  stage2 <- reactive({
    if (!games_complete(stage1_scores())) return(NULL)
    make_stage2(teams(), stage1(), stage1_scores())
  })
  stage2_scores <- reactive({
    if (is.null(stage2())) return(tibble(game_id = integer(), scoreA = numeric(), scoreB = numeric()))
    scores_for_games(stage2()$games, input, "s2")
  })
  finals_semis <- reactive({
    if (is.null(stage2()) || !games_complete(stage2_scores())) return(NULL)
    make_finals_semis(teams(), stage2(), stage2_scores())
  })
  finals_semi_scores <- reactive({
    if (is.null(finals_semis())) return(tibble(game_id = integer(), scoreA = numeric(), scoreB = numeric()))
    scores_for_games(finals_semis()$games, input, "f1")
  })
  placement_matches <- reactive(make_placement_matches(teams(), finals_semis(), finals_semi_scores()))
  placement_scores <- reactive({
    if (is.null(placement_matches())) return(tibble(game_id = integer(), scoreA = numeric(), scoreB = numeric()))
    scores_for_games(placement_matches(), input, "f2")
  })

  all_games <- reactive(bind_rows(
    stage1()$games,
    if (!is.null(stage2())) stage2()$games,
    if (!is.null(finals_semis())) finals_semis()$games,
    if (!is.null(placement_matches())) placement_matches()
  ))

  all_scores <- reactive(bind_rows(
    stage1_scores(),
    if (!is.null(stage2())) stage2_scores(),
    if (!is.null(finals_semis())) finals_semi_scores(),
    if (!is.null(placement_matches())) placement_scores()
  ))

  output$format_summary <- renderUI({
    n <- nrow(teams())
    sizes <- pool_size_plan(n)
    if (length(sizes) == 1) {
      div(class = "format-note", strong("Format: "), paste(n, "teams play a single round robin."))
    } else {
      note_class <- if (n == 11) "warn-note" else "format-note"
      div(
        class = note_class,
        strong("Format: "),
        paste0("Preliminary pools of ", paste(sizes, collapse = " + "), ". "),
        "Top two from each preliminary pool advance to Group A; the rest advance to Group B. ",
        "After A/B group play, teams are ordered A then B and play placement groups.",
        if (n == 11) " Eleven teams require one 3-team preliminary pool." else ""
      )
    }
  })

  output$teams_table <- renderDT({
    pool_tbl <- bind_rows(stage1()$pools)
    datatable(pool_tbl %>% select(Seed = seed, Team = team, Pool = pool, `Pool seed` = pool_position), rownames = FALSE)
  })

  output$schedule_table <- renderDT({
    datatable(display_schedule(all_games(), all_scores()), rownames = FALSE, options = list(pageLength = 25))
  })

  output$score_inputs <- renderUI({
    tagList(
      score_card(stage1()$games, "s1"),
      if (!games_complete(stage1_scores())) div(class = "warn-note", "Complete preliminary scores to create A/B group games."),
      if (!is.null(stage2())) score_card(stage2()$games, "s2"),
      if (!is.null(stage2()) && !games_complete(stage2_scores())) div(class = "warn-note", "Complete A/B group scores to create placement games."),
      if (!is.null(finals_semis())) score_card(finals_semis()$games, "f1"),
      if (!is.null(finals_semis()) && is.null(placement_matches())) div(class = "warn-note", "Complete placement semifinals to create final placement matches."),
      if (!is.null(placement_matches())) score_card(placement_matches(), "f2")
    )
  })

  output$standings_ui <- renderUI({
    groups <- unique(all_games()$group)
    tagList(
      lapply(groups, function(group_name) {
        games <- all_games() %>% filter(group == group_name)
        scores <- all_scores() %>% semi_join(games, by = "game_id")
        table_id <- paste0("standings_", str_replace_all(str_to_lower(group_name), "[^a-z0-9]+", "_"))
        output[[table_id]] <- renderDT({
          team_standings(teams(), games, scores) %>%
            select(Place = place, Team = team, Seed = seed, Played = played, Wins = wins, Losses = losses,
                   `Win %` = win_pct, `Point diff` = point_diff, `For` = points_for, `Against` = points_against) %>%
            datatable(rownames = FALSE, options = list(dom = "t"))
        })
        div(class = "panel", h3(group_name), DTOutput(table_id))
      })
    )
  })

  output$credit_ui <- renderUI({
    tags$div(
      tags$p("This Shiny app was built for U.S. fixed-team DDC tournaments."),
      tags$p(
        class = "credit",
        "Tournament structure is inspired by Heikki Wilenius's GPL DDC tournament manager: ",
        tags$a("github.com/wilenius/ddc", href = "https://github.com/wilenius/ddc", target = "_blank"),
        ". This R implementation translates the scheduling ideas into a new Shiny app and keeps the project under GPL-3."
      )
    )
  })

  output$download_schedule <- downloadHandler(
    filename = function() paste0("ddc-schedule-", Sys.Date(), ".csv"),
    content = function(file) write.csv(display_schedule(all_games(), all_scores()), file, row.names = FALSE)
  )

  output$download_standings <- downloadHandler(
    filename = function() paste0("ddc-standings-", Sys.Date(), ".csv"),
    content = function(file) {
      standings <- map_dfr(unique(all_games()$group), function(group_name) {
        games <- all_games() %>% filter(group == group_name)
        scores <- all_scores() %>% semi_join(games, by = "game_id")
        team_standings(teams(), games, scores) %>% mutate(group = group_name, .before = 1)
      })
      write.csv(standings, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

# Printable single-pool forms, independent of the championship preset.
# Requires round_robin_pairs() from us_championship_engine.R.
pool_sheet_teams <- function(n, team_text = "") {
  if (length(n) != 1L || is.na(n) || !n %in% c(4L, 5L)) {
    stop("Score sheets support 4 or 5 teams.", call. = FALSE)
  }
  lines <- trimws(strsplit(paste0(team_text, "\n"), "\n", fixed = TRUE)[[1]])
  # Empty lines retain their seed; extra names must never be silently dropped.
  while (length(lines) && tail(lines, 1) == "") lines <- head(lines, -1)
  if (length(lines) > n) stop("Enter no more than one team per seed.", call. = FALSE)
  teams <- rep("", n)
  teams[seq_along(lines)] <- lines
  teams
}

write_pool_score_sheet <- function(file, n, team_text = "", pool_name = "") {
  teams <- pool_sheet_teams(n, team_text)
  games <- round_robin_pairs(n) %>%
    dplyr::mutate(top_seed_match = (.data$team1 == 1 & .data$team2 == 2) |
                    (.data$team1 == 2 & .data$team2 == 1)) %>%
    dplyr::arrange(.data$round, .data$top_seed_match) %>%
    dplyr::select(-"top_seed_match")
  grDevices::pdf(file, width = 8.5, height = 11, family = "Helvetica",
                title = paste(n, "team pool play score sheet"), paper = "special")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = rep(0, 4))
  graphics::plot.new()
  graphics::plot.window(c(0, 612), c(0, 792), xaxs = "i", yaxs = "i")
  label <- function(x, y, text, size = 10, bold = FALSE, adj = 0) {
    graphics::text(x, y, text, adj = c(adj, 0.5), cex = size / 12,
                   font = if (bold) 2 else 1)
  }
  # Wrap and fit names within the available cell, without truncating them.
  name_label <- function(x, y, name, width, height, size = 11) {
    wrap <- function(value) {
      chars <- strsplit(value, "", fixed = TRUE)[[1]]
      lines <- character()
      line <- ""
      for (ch in chars) {
        candidate <- paste0(line, ch)
        if (nzchar(line) && graphics::strwidth(candidate, units = "user", cex = size / 12) > width) {
          lines <- c(lines, line)
          line <- ch
        } else line <- candidate
      }
      paste(c(lines, line), collapse = "\n")
    }
    repeat {
      wrapped <- wrap(name)
      if (graphics::strheight(wrapped, units = "user", cex = size / 12) <= height || size <= 5) break
      size <- size - 0.5
    }
    label(x, y, wrapped, size)
  }
  table_row <- function(y, widths, values, height = 30, header = FALSE) {
    edges <- 36 + c(0, cumsum(widths))
    for (j in seq_along(widths)) {
      graphics::rect(edges[j], y - height, edges[j + 1], y,
                     col = if (header) "#eeeeee" else "white", border = "#555555", lwd = 0.6)
      if (header) label(edges[j] + 7, y - height / 2, values[j], 8, TRUE)
      else name_label(edges[j] + 7, y - height / 2, values[j], widths[j] - 14, height - 8)
    }
  }

  label(36, 751, "POOL PLAY SCORE SHEET", 20, TRUE)
  label(36, 726, sprintf("%d teams  |  %d games  |  Round robin", n, nrow(games)), 11)
  label(36, 696, "Event: ____________________________________", 11)
  label(365, 696, "Date: __________________", 11)
  label(36, 670, "Pool:", 11)
  name_label(66, 670, if (nzchar(trimws(pool_name))) pool_name else "________________________", 275, 22)
  label(365, 670, "Courts: A _____ B _____", 11)

  label(36, 637, "TEAMS IN POOL SEED ORDER", 11, TRUE)
  label(36, 619, "Original seed = overall seed before pool play. Pool seeds identify teams within this pool.", 9)
  table_row(604, c(64, 80, 396), c("Pool seed", "Original seed", "Team / players"), 24, TRUE)
  y <- 580
  for (i in seq_len(n)) {
    table_row(y, c(64, 80, 396), c(as.character(i), "", teams[i]), 25)
    y <- y - 25
  }

  label(36, y - 18, "TWO-COURT ROUND SCHEDULE", 11, TRUE)
  y <- y - 31
  widths <- c(40, 140, 70, 140, 70, 80)
  table_row(y, widths, c("Round", "Court A: pool seeds", "Score", "Court B: pool seeds", "Score", "Bye seed"), 24, TRUE)
  y <- y - 24
  rounds <- seq_len(max(games$round))
  for (r in rounds) {
    matches <- games[games$round == r, ]
    bye <- setdiff(seq_len(n), c(matches$team1, matches$team2))
    table_row(y, widths, c(as.character(r),
                           paste(matches$team1[1], "vs", matches$team2[1]), "",
                           paste(matches$team1[2], "vs", matches$team2[2]), "",
                           if (length(bye)) as.character(bye) else "-"), 30)
    y <- y - 30
  }

  label(36, y - 20, "RUNNING WIN / LOSS RECORD", 11, TRUE)
  label(36, y - 36, "Enter cumulative W-L after each round; carry the record forward on a bye.", 9)
  y <- y - 50
  widths <- c(56, rep(256 / length(rounds), length(rounds)), 40, 40, 64, 84)
  table_row(y, widths, c("Pool seed", paste0("R", rounds, " W-L"), "Final W", "Final L", "Pool finish", "Earned seed"), 24, TRUE)
  y <- y - 24
  for (i in seq_len(n)) {
    table_row(y, widths, c(as.character(i), rep("__-__", length(rounds)), "", "", "", ""), 25)
    y <- y - 25
  }
  label(36, 34, "Award the pool's original overall seed slots in order of final pool finish.", 9, TRUE)
  invisible(file)
}

# A paper worksheet for combining 12-15 teams across three independent pools.
write_pool_reseeding_tally <- function(file) {
  grDevices::pdf(file, width = 11, height = 8.5, family = "Helvetica",
                title = "Three-pool bracket reseeding tally", paper = "special")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = rep(0, 4))
  graphics::plot.new()
  graphics::plot.window(c(0, 792), c(0, 612), xaxs = "i", yaxs = "i")
  label <- function(x, y, value, size = 9, bold = FALSE) {
    graphics::text(x, y, value, adj = c(0, 0.5), cex = size / 12, font = if (bold) 2 else 1)
  }
  label(28, 578, "THREE-POOL BRACKET RESEEDING TALLY", 19, TRUE)
  label(28, 555, "Event: ___________________________________________________    Date: _____________________", 10)
  label(28, 534, "Each pool retains its original overall seed slots. Award those slots by final pool finish.", 10, TRUE)
  label(28, 514, "1. Rank teams within each pool by W-L record. Resolve tied records within that pool (WFDF 206.03).", 10)
  label(28, 497, "2. Sort that pool's original overall seeds, lowest number first. Winner earns the best slot, runner-up the next, etc.", 10)
  label(28, 480, "3. Transfer earned seeds to the double-elimination bracket. Keep original seeds unchanged. No cross-pool record comparisons.", 9)

  widths <- c(42, 220, 86, 36, 36, 86, 112, 118)
  edges <- 28 + c(0, cumsum(widths))
  row <- function(y, values, height = 23, header = FALSE, shade = FALSE) {
    for (j in seq_along(widths)) {
      graphics::rect(edges[j], y - height, edges[j + 1], y,
                     col = if (header) "#dddddd" else if (shade) "#f3f3f3" else "white",
                     border = "#555555", lwd = 0.6)
      label(edges[j] + 5, y - height / 2, values[j], if (header) 8 else 10, header)
    }
  }
  row(460, c("Pool ID", "Team / players", "Original seed", "W", "L", "Pool finish", "Earned bracket seed", "Notes / tiebreak"), 28, TRUE)
  y <- 432
  for (pool in c("A", "B", "C")) {
    for (seed in 1:5) {
      row(y, c(paste0(pool, seed), rep("", 7)), shade = pool == "B")
      y <- y - 23
    }
  }
  label(28, 68, "Cross out A5, B5 or C5 for four-team pools. Check that earned seeds reuse every original seed exactly once.", 9)
  label(28, 50, "Example: a pool with original seeds 1, 6, 7, 12 awards those slots to its 1st, 2nd, 3rd and 4th finishers, respectively.", 9)
  label(28, 32, "Within-pool tiebreak reference: doubledisccourt.com/rules/2016/wfdf_rulebook_article_ii.pdf (section 206.03).", 9)
  invisible(file)
}

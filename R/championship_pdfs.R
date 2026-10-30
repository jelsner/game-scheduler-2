# Native R PDF drawing keeps downloads available on shinyapps.io without Python.
pdf_page_13 <- function(width, height) {
  par(mar = rep(0, 4)); plot.new(); plot.window(c(0, width), c(0, height), xaxs = "i", yaxs = "i")
}
pdf_label_13 <- function(x, y, value, size = 9, bold = FALSE) {
  if (length(value) == 0 || is.na(value)) value <- ""
  text(x, y, as.character(value), adj = c(0, 0.5), cex = size / 12, font = if (bold) 2 else 1)
}
pdf_fit_13 <- function(x, y, value, width, size = 9, min_size = 6) {
  if (is.na(value)) value <- ""
  repeat {
    lines <- strsplit(as.character(value), "\n", fixed = TRUE)[[1]]
    if (!length(lines) || max(strwidth(lines, cex = size / 12)) <= width || size <= min_size) break
    size <- size - .25
  }
  pdf_label_13(x, y, value, size)
}
pdf_row_13 <- function(x, top, widths, values, height = 26, header = FALSE, shade = FALSE) {
  edges <- x + c(0, cumsum(widths))
  for (j in seq_along(widths)) {
    rect(edges[j], top - height, edges[j + 1], top, col = if (header) "#e5e9ec" else if (shade) "#f4f6f8" else "white", border = "#666666", lwd = .6)
    if (header) pdf_label_13(edges[j] + 5, top - height / 2, values[j], 8, TRUE)
    else pdf_fit_13(edges[j] + 5, top - height / 2, values[j], widths[j] - 10, 10)
  }
}
pdf_stamp_13 <- function() paste("US DDC Championship 2026 | Snapshot", format(Sys.time(), "%b %d, %Y %I:%M %p", tz = "America/New_York"), "Eastern")

write_pool_pdfs_13 <- function(file, state, snapshot) {
  pdf(file, width = 8.5, height = 11, family = "Helvetica", title = "2026 pool play score sheets", paper = "special")
  on.exit(dev.off(), add = TRUE)
  for (p in c("A", "B", "C")) {
    pdf_page_13(612, 792)
    pool <- state$teams[state$teams$pool == p, ]; n <- nrow(pool)
    games <- state$games[state$games$pool == p, ]; tally <- state$tally[match(pool$team_id, state$tally$team_id), ]
    courts <- sort(unique(games$court)); rounds <- sort(unique(games$wave))
    pdf_label_13(36, 751, paste("POOL", p, "- SCORE SHEET"), 21, TRUE)
    pdf_label_13(36, 726, pdf_stamp_13(), 8)
    pdf_label_13(36, 705, paste0(n, " teams | ", nrow(games), " games | Courts ", paste(courts, collapse = " and "), " | To 15, win by 2"), 11)
    pdf_label_13(36, 681, "TEAMS IN POOL SEED ORDER", 11, TRUE)
    pdf_row_13(36, 667, c(64, 80, 396), c("Pool seed", "Original seed", "Team / players"), 24, TRUE)
    y <- 643
    for (i in seq_len(n)) { pdf_row_13(36, y, c(64, 80, 396), c(pool$pool_seed[i], pool$seed[i], pool$team[i]), 26); y <- y - 26 }
    pdf_label_13(36, y - 21, "TWO-COURT ROUND SCHEDULE", 11, TRUE)
    y <- y - 35
    widths <- c(40, 140, 70, 140, 70, 80)
    pdf_row_13(36, y, widths, c("Round", paste("Court", courts[1], "pool seeds"), "Score", paste("Court", courts[2], "pool seeds"), "Score", "Bye seed"), 24, TRUE)
    y <- y - 24
    for (r in rounds) {
      gg <- games[games$wave == r, ]; gg <- gg[order(gg$court), ]
      a <- pool$pool_seed[match(gg$teamA_id, pool$team_id)]; b <- pool$pool_seed[match(gg$teamB_id, pool$team_id)]
      bye <- setdiff(pool$pool_seed, c(a, b))
      pdf_row_13(36, y, widths, c(r, paste0(gg$key[1], ": ", a[1], " vs ", b[1]), gg$score[1], paste0(gg$key[2], ": ", a[2], " vs ", b[2]), gg$score[2], if (length(bye)) bye else "-"), 30)
      y <- y - 30
    }
    pdf_label_13(36, y - 22, "RUNNING WIN / LOSS RECORD", 11, TRUE)
    pdf_label_13(36, y - 38, "Cumulative W-L after each round; carry the record forward on a bye.", 9)
    y <- y - 51
    widths <- c(56, rep(256 / length(rounds), length(rounds)), 40, 40, 64, 84)
    pdf_row_13(36, y, widths, c("Pool seed", paste0("R", rounds, " W-L"), "Final W", "Final L", "Pool finish", "Earned seed"), 24, TRUE)
    y <- y - 24
    done <- games[games$status == "Complete", ]; max_round <- if (nrow(done)) max(done$wave) else 0
    for (i in seq_len(n)) {
      running <- vapply(rounds, function(r) {
        if (r > max_round) return("__-__")
        g <- done[done$wave <= r, ]
        paste(sum(g$winner_id == pool$team_id[i]), sum(g$loser_id == pool$team_id[i]), sep = "-")
      }, "")
      pdf_row_13(36, y, widths, c(pool$pool_seed[i], running,
        if (tally$played[i]) tally$wins[i] else "", if (tally$played[i]) tally$losses[i] else "", tally$pool_finish[i], tally$earned_seed[i]), 25)
      y <- y - 25
    }
    pdf_label_13(36, 52, "Original seed = overall seed before pool play. Score using the pool seeds above.", 9)
    pdf_label_13(36, 35, "Award this pool's original overall seed slots in order of final pool finish.", 9, TRUE)
  }
}

write_tally_pdf_13 <- function(file, state) {
  pdf(file, width = 11, height = 8.5, family = "Helvetica", title = "2026 reseeding tally", paper = "special")
  on.exit(dev.off(), add = TRUE); pdf_page_13(792, 612)
  pdf_label_13(28, 577, "THREE-POOL BRACKET RESEEDING TALLY", 20, TRUE)
  pdf_label_13(28, 553, pdf_stamp_13(), 8)
  pdf_label_13(28, 530, "Each pool retains its original overall seed slots. Award those slots by final pool finish.", 11, TRUE)
  pdf_label_13(28, 508, "Rank within each pool by W-L, then the within-pool tiebreaks. An unresolved tie needs a director decision.", 10)
  pdf_label_13(28, 490, "Assign the lowest original seed number to the winner, the next to the runner-up, and so on. No cross-pool comparisons.", 10)
  pdf_label_13(28, 469, if (state$seeded) "All 13 earned bracket seeds are confirmed." else "Earned seeds remain blank until each pool is complete and any ties are resolved.", 9)
  widths <- c(32, 48, 50, 258, 32, 32, 50, 74, 160)
  pdf_row_13(28, 450, widths, c("Pool", "Pool seed", "Orig. seed", "Team / players", "W", "L", "Finish", "Earned seed", "Notes / tiebreak"), 24, TRUE)
  y <- 426
  t <- state$tally[order(state$tally$pool, state$tally$pool_seed), ]
  for (i in seq_len(nrow(t))) {
    r <- t[i, ]
    pdf_row_13(28, y, widths, c(r$pool, r$pool_seed, r$seed, r$team,
      if (r$played) r$wins else "", if (r$played) r$losses else "", r$pool_finish, r$earned_seed,
      if (r$unresolved && !r$resolved && r$played == sum(t$pool == r$pool) - 1) "Tie needs decision" else ""), 26, shade = r$pool == "B")
    y <- y - 26
  }
  pdf_label_13(28, 65, "Check: earned seeds reuse every original overall seed exactly once. Original seeds stay unchanged on the roster.", 9)
  pdf_label_13(28, 47, "Pool A slots: 1, 6, 7, 12, 13 | Pool B slots: 2, 5, 8, 11 | Pool C slots: 3, 4, 9, 10", 9)
  pdf_label_13(28, 29, "Initial seed order: usddc2026.com (ranking snapshot October 28, 2026).", 8)
}

write_bracket_pdf_13 <- function(file, state, snapshot) {
  pdf(file, width = 11, height = 8.5, family = "Helvetica", title = "2026 bracket and placement - two Letter pages", paper = "special")
  on.exit(dev.off(), add = TRUE)
  g <- state$games; lookup <- function(key) g[g$key == key, ]
  line <- function(x1, y1, x2, y2) segments(x1, y1, x2, y2, col = "#666666", lwd = .6)
  arrow <- function(x, y) polygon(c(x, x-4, x-4), c(y, y+2, y-2), col = "#666666", border = NA)
  header <- function(title, page) {
    pdf_page_13(792, 612)
    pdf_label_13(28, 575, title, 18, TRUE)
    pdf_label_13(28, 554, pdf_stamp_13(), 8)
    pdf_label_13(28, 536, "13 teams | W# / L# = winner / loser of game # | C = court | Wv = wave", 9)
    pdf_label_13(28, 20, "US Letter landscape, actual size. Print both pages single-sided. Standard games: 15, win by 2.", 8)
    pdf_label_13(710, 20, paste("Page", page, "of 2"), 8)
  }
  box <- function(key, x, y, width = 170, caption = NULL) {
    game <- lookup(key); top <- y + 24
    rect(x, y-24, x+width, y+24, col = "white", border = "#333333", lwd = .7)
    rect(x+.4, top-13, x+width-.4, top-.4, col = "#e5e9ec", border = NA)
    pdf_label_13(x+4, top-7, key, 8, TRUE)
    pdf_label_13(x+29, top-7, paste0("Wv ", game$wave, " / C", game$court), 7.5)
    record <- if (game$status == "Complete") snapshot$results[[key]] else NULL
    for (i in 1:2) {
      yy <- top - 24 - (i-1)*15
      source <- c(game$sourceA, game$sourceB)[i]
      if (startsWith(source, "S")) source <- paste0("S", substring(source, 2))
      pdf_label_13(x+4, yy, source, if (width < 140) 7 else 8)
      start <- x + if (width >= 140) 45 else 30
      end <- x + width - if (key == "G24") 58 else 27
      team <- c(game$teamA, game$teamB)[i]
      if (!is.na(team)) {
        if (width < 140) team <- gsub(" & ", "\n", team, fixed = TRUE)
        pdf_fit_13(start, yy, team, end - start, if (width < 140) 6.75 else 8, 6)
      } else line(start, yy - 2, end, yy - 2)
      if (key == "G24") {
        for (j in 1:3) {
          xx <- x + width - c(52, 35, 18)[j]
          rect(xx, yy-6, xx+14, yy+6, border = "#888888", lwd = .6)
          if (!is.null(record) && j <= nrow(record$scores)) pdf_label_13(xx+2, yy, record$scores[j, i], 8)
        }
      } else {
        rect(x+width-22, yy-6, x+width-4, yy+6, border = "#888888", lwd = .6)
        if (!is.null(record)) pdf_label_13(x+width-20, yy, record$scores[1, i], 8)
      }
    }
    if (!is.null(caption)) pdf_label_13(x+1, y-31, caption, 7.5)
  }
  connect <- function(pos, width) {
    for (key in names(pos)) {
      game <- lookup(key); target <- pos[[key]]
      for (i in 1:2) {
        source <- c(game$sourceA, game$sourceB)[i]
        if (!startsWith(source, "W")) next
        ref <- substring(source, 2); ref <- if (startsWith(ref, "P")) ref else paste0("G", ref)
        if (!ref %in% names(pos)) next
        prior <- pos[[ref]]
        if (target[1] <= prior[1]) next
        sx <- prior[1]+width; mx <- (sx+target[1])/2; ty <- target[2]+if (i == 1) 1 else -16
        line(sx, prior[2], mx, prior[2]); line(mx, prior[2], mx, ty); line(mx, ty, target[1], ty); arrow(target[1], ty)
      }
    }
  }
  header("13-TEAM DOUBLE ELIMINATION - UPPER BRACKET", 1)
  pos <- list(G1=c(28,488), G2=c(28,428), G3=c(28,368), G4=c(28,308), G5=c(28,248),
    G6=c(213,488), G7=c(213,398), G8=c(213,308), G9=c(213,248), G10=c(398,443), G11=c(398,278), G12=c(583,360))
  connect(pos, 170)
  headings <- c("OPENING ROUND", "QUARTERFINALS", "SEMIFINALS", "WINNERS FINAL")
  for (i in 1:4) pdf_label_13(c(28,213,398,583)[i], 522, headings[i], 10, TRUE)
  destinations <- c(14,13,13,16,17,15,14,17,16,21,20,23)
  for (key in names(pos)) box(key, pos[[key]][1], pos[[key]][2], caption = paste0("Loser -> G", destinations[as.integer(substring(key, 2))], " on page 2"))
  for (i in seq_along(c("Seeds 1, 2 and 3 receive", "opening-round byes.", "", "Standard: 15, win by 2."))) pdf_label_13(583, 493-(i-1)*14, c("Seeds 1, 2 and 3 receive", "opening-round byes.", "", "Standard: 15, win by 2.")[i], 9)
  pdf_label_13(583, 430, "Use earned bracket seeds.", 9)
  pdf_label_13(583, 416, "Keep them fixed in this stage.", 9)
  pdf_label_13(583, 307, "W12 -> G24 below.", 9, TRUE)
  pdf_label_13(398, 191, "CHAMPIONSHIP SERIES", 10, TRUE); pdf_label_13(583, 191, "RESET (IF NEEDED)", 10, TRUE)
  box("G24", 398, 142); box("G25", 583, 142)
  line(568,142,583,142); arrow(583,142)
  pdf_label_13(398, 103, "Best of 3 to 15, each win by 2.", 8.5); pdf_label_13(583, 103, "One game to 21, win by 2.", 8.5)
  pdf_label_13(398, 86, "W23 comes from page 2. Play G25 only if W23 wins the G24 series.", 8.3)
  pdf_label_13(398, 73, "G24 boxes: game 1 / 2 / 3. W24 and L24 mean the series result.", 8)
  for (place in 1:2) {
    xx <- c(398, 583)[place]; name <- state$final$team[place]
    pdf_label_13(xx, 52, c("Champion:", "Runner-up:")[place], 9, TRUE)
    if (is.na(name)) line(xx+55,49,xx+170,49) else pdf_fit_13(xx+55,52,name,115,8)
  }
  pdf_label_13(28, 195, "SIX-COURT PLAN (G = MAIN; P = PLACEMENT)", 9, TRUE)
  widths <- c(35, rep(49,6)); all_bracket <- g[g$stage != "Pool play", ]
  pdf_row_13(28,181,widths,c("Wave",paste("Court",1:6)),18,TRUE)
  y <- 163
  for (w in 1:9) {
    values <- rep("-",6); gg <- all_bracket[all_bracket$wave == w, ]
    values[gg$court] <- gg$key
    if (w == 9) values[1] <- "G25*"
    pdf_row_13(28,y,widths,c(w,values),14); y <- y-14
  }
  pdf_label_13(398, 34, "Start waves after prior games finish; allow agreed rest. *G25 if needed.", 8)
  header("13-TEAM DOUBLE ELIMINATION - LOWER & PLACEMENT", 2)
  pos <- list(G13=c(28,400),G14=c(153,490),G15=c(153,422),G16=c(153,354),G17=c(153,286),
    G18=c(278,456),G19=c(278,320),G20=c(403,456),G21=c(403,320),G22=c(528,388),G23=c(653,388))
  connect(pos,111)
  for (i in 1:6) pdf_label_13(c(28,153,278,403,528,653)[i],522,c(paste("ROUND",1:5),"LOWER FINAL")[i],9,TRUE)
  for (key in names(pos)) {
    num <- as.integer(substring(key,2))
    cap <- if (num == 13) "Loser: 13th (finished)" else if (num %in% 14:17) "Loser -> 9-12 mini" else if (num %in% 18:19) "Loser -> P5" else if (num %in% 20:21) "Loser -> P6" else if (num == 22) "Loser: 4th" else "Loser: 3rd"
    box(key,pos[[key]][1],pos[[key]][2],111,cap)
  }
  pdf_label_13(653,338,"W23 -> G24 on page 1",8,TRUE)
  for (i in 1:3) pdf_label_13(28,c(490,354,286)[i],c("L1 -> G14","L4 -> G16","L5 -> G17")[i],9)
  pdf_label_13(28,228,"PLACEMENT GAMES - ONE GAME TO 15, WIN BY 2",11,TRUE)
  pdf_label_13(28,209,"9TH-12TH MINI BRACKET",9,TRUE); pdf_label_13(398,209,"7TH / 8TH",9,TRUE); pdf_label_13(583,209,"5TH / 6TH",9,TRUE)
  pp <- list(P1=c(28,177),P2=c(28,99),P3=c(213,177),P4=c(213,99))
  connect(pp,150)
  captions <- c(P1="Winner -> P3; loser -> P4",P2="Winner -> P3; loser -> P4",P3="Winner: 9th / Loser: 10th",P4="Winner: 11th / Loser: 12th")
  for (key in names(pp)) box(key,pp[[key]][1],pp[[key]][2],150,captions[key])
  box("P5",398,177,170,"Winner: 7th / Loser: 8th"); box("P6",583,177,170,"Winner: 5th / Loser: 6th")
  pdf_label_13(398,115,"MINI-BRACKET SEEDING",9,TRUE)
  lines <- c("M1-M4 are losers of G14-G17, ordered by earned bracket seed,", "best to worst (lowest number first). P1: M1 vs M4; P2: M2 vs M3.",
    "Winners play P3 for 9th/10th; losers play P4 for 11th/12th.", "W / L before P# means that placement game's winner / loser.", "The 13th-place team is finished after its second bracket loss.")
  for (i in seq_along(lines)) pdf_label_13(398,c(100,87,74,54,41)[i],lines[i],8.3)
}

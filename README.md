# DDC Fixed-Team Tournament Scheduler

An R/Shiny app for scheduling fixed-team Double Disc Court tournaments, focused first on a 10-12 team U.S. Championship format.

## US Championship Preset

The active preset supports 10, 11, or 12 fixed teams:

- 10 teams: two 5-team pools, then a top-8 double-elimination bracket and a 9th-place match.
- 11 teams: one 6-team pool and one 5-team pool, then a top-8 double-elimination bracket and a 9th-11th placement round robin.
- 12 teams: two 6-team pools, then a top-8 double-elimination bracket and a 9th-12th placement bracket.

Teams are built with searchable player dropdowns backed by the current international DDC player ranking API. Each team's seed comes from the sum of both players' ranking points.

## Printing Pool Score Sheets

Standalone US Letter templates for four- and five-team pools are in
`output/pdf/`. Each sheet uses two courts and includes original overall seeds,
pool seeds, running W/L records, pool finishing places, and earned bracket seeds.
The combined three-pool tally has space for up to 15 teams. Within each pool,
rank teams by W/L and resolve ties using the agreed DDC tiebreaks. Assign that
pool's original overall seed slots from best to worst to its finishers in order.
Keep the original seeds on the roster and record the earned bracket seeds
separately. This method makes no cross-pool record comparisons.

To regenerate the blank templates from the repository root:

```r
library(dplyr)
source("R/us_championship_engine.R")
source("R/pool_score_sheets.R")
for (n in c(4, 5)) {
  write_pool_score_sheet(sprintf("output/pdf/ddc-%d-team-pool-score-sheet.pdf", n), n)
}
write_pool_reseeding_tally("output/pdf/ddc-three-pool-reseeding-tally.pdf")
```

## Printable 13-Team Bracket

`output/pdf/ddc-13-team-double-elimination-bracket.pdf` is a standalone,
single-page 11 x 17 landscape bracket. Enter the earned seeds from the pool
tally. Seeds 1-3 receive opening byes. The sheet includes winner/loser paths,
score boxes, a six-court wave schedule, and a conditional championship reset.
There are 24 matches, or 25 with a reset. Standard matches are one game to
15, win by 2; the championship is best of three games to 15, win by 2, and
the reset is one game to 21, win by 2. Placement matches are not yet included. Start each wave after the previous
wave finishes and allow the agreed rest period between games.

Regenerate it with Python and `reportlab`:

```sh
python3 scripts/create_13_team_bracket.py
```

The generator verifies 10,002 simulated tournaments, including both final
outcomes, second-loss eliminations, and simultaneous court assignments.

## Two-Page Letter Bracket

`output/pdf/ddc-13-team-double-elimination-bracket-letter.pdf` is the two-page
US Letter landscape version. Page 1 has the upper bracket, championship/reset,
and six-court schedule. Page 2 has the lower bracket and all placement games:
a four-team mini bracket for 9th-12th, one game for 7th/8th, and one for 5th/6th.
Mini-bracket seeds use the earned bracket seeds of the losers of G14-G17,
ordered from lowest number to highest. The 13th-place team is finished.

The placement games use courts left free by the main bracket: P1/P2 in wave 4,
P3/P4/P5 in wave 5, and P6 in wave 6. Print both pages at actual size on separate
8.5 x 11 sheets. Generate this version with:

```sh
python3 scripts/create_13_team_letter_bracket.py
```

The generator also verifies placement destinations and the combined court
schedule across 5,000 simulated tournaments.

## Running Locally

Open `app.R` in RStudio and click **Run App**, or run:

```r
shiny::runApp()
```

Required R packages:

```r
install.packages(c("shiny", "dplyr", "tidyr", "purrr", "stringr", "DT", "jsonlite", "httr2"))
```

## Ranking Integration

Ranking fetches use the doubledisccourt.com endpoint behind:

<https://doubledisccourt.com/results/rankings.html>

The ranked player list is kept in the Shiny session while the app is running. It is not written to local disk, so the app remains suitable for shinyapps.io deployment.

## Credit And License

This project is inspired by Heikki Wilenius's GPL DDC tournament manager:

<https://github.com/wilenius/ddc>

This project is released under GPL-3. See `LICENSE`.

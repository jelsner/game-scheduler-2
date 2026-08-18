# DDC Fixed-Team Tournament Scheduler

An R/Shiny app for scheduling fixed-team Double Disc Court tournaments, focused first on a 10-12 team U.S. Championship format.

## US Championship Preset

The active preset supports 10, 11, or 12 fixed teams:

- 10 teams: two 5-team pools, then a top-8 double-elimination bracket and a 9th-place match.
- 11 teams: one 6-team pool and one 5-team pool, then a top-8 double-elimination bracket and a 9th-11th placement round robin.
- 12 teams: two 6-team pools, then a top-8 double-elimination bracket and a 9th-12th placement bracket.

Teams are entered one per line, preferably as `Player One / Player Two`. Manual seed order is supported. The app can also seed teams from the current international DDC player ranking API by summing the two players' ranking points.

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

## Credit And License

This project is inspired by Heikki Wilenius's GPL DDC tournament manager:

<https://github.com/wilenius/ddc>

This project is released under GPL-3. See `LICENSE`.

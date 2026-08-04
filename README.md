# DDC Fixed-Team Tournament Scheduler

An R/Shiny app for scheduling fixed-team Double Disc Court tournaments in the United States.

The app supports 4-20 teams:

- 4-7 teams play a single round robin.
- 8-10 teams play preliminary pools, then Group A and Group B, then placement matches.
- 11 teams use a practical 4 + 4 + 3 preliminary pool split.
- 12-20 teams use preliminary pools of 4 or 5 teams, then Group A and Group B, then placement matches. The 20-team format uses five pools of four, matching the larger fixed-team flow in Heikki's app.

Teams are entered in seed order. Preliminary pools are assigned by snake seeding. In multi-pool events, the top two teams from each preliminary pool advance to Group A and the rest advance to Group B. Placement groups are created from the Group A standings followed by the Group B standings.

## Running Locally

Open `app.R` in RStudio and click **Run App**, or run:

```r
shiny::runApp()
```

Required R packages:

```r
install.packages(c("shiny", "dplyr", "tidyr", "purrr", "stringr", "DT"))
```

## Credit And License

This project is inspired by Heikki Wilenius's GPL DDC tournament manager:

<https://github.com/wilenius/ddc>

The tournament flow implemented here follows the same broad idea used in that project for larger fixed-team tournaments: preliminary pool play, A/B group play, and final placement matches. This is a new R/Shiny implementation rather than a direct Python port.

This project is released under GPL-3. See `LICENSE`.

# Simulated 10-team tournament

Run from the project root: `Rscript simulations/2026-10-03/run_simulation.R`.

The simulation calls the app's seeding, pool scheduling, standings, top-eight seeding and double-elimination dependency functions directly. It does not change the Shiny app or enter simulated results into a live tournament.

The roster comes from [the registration sheet](https://docs.google.com/spreadsheets/d/1nkYIMeVzAX0DezESp1atoudO4a1HHLt2eqpQ3ns8TO0), read October 3, 2026. Reciprocal partner registrations are deduplicated. Sam Kaye and Ian Sterrett have no partner listed and are excluded from the ten partnered teams. The one women's team, Diana Elsner / Lindsi Allman, enters Open. Francis Flores is matched to the app's ranking entry Fran Flores.

Initial seeds use the sum of the two players' 2026 Open ranking points, fetched with the app's ranking function. The 20 relevant player records are saved in player_rankings.csv so the run remains reproducible.

The app assigns initial seeds 1, 4, 5, 8, 9 to Pool A and 2, 3, 6, 7, 10 to Pool B. Each team plays four pool games. Pool places use wins, tied-team head-to-head wins, point differential, then original seed. Championship seeds compare pool place, wins, point differential, then original seed. Four teams per pool advance. Fifth-place teams play for ninth.

Pool and non-final bracket/placement games are to 15, win by two, without a cap. Both championship and any required reset are best-of-three series of games to 21, assumed win by two without a cap. Losing a championship series counts as one bracket loss. A reset series is played only if the losers-bracket finalist wins the first series. Fifth and seventh places are tied because the app has no matches to break those ties.

Scores are illustrative and are not a calibrated forecast. Each point goes to team A with probability A_points^0.45 / (A_points^0.45 + B_points^0.45). The exponent compresses ranking-point differences. This simplified point model does not model DDC double scores, serve, wind, or rally tactics. Random seed: 20261003. No results were selected or rerun to choose a preferred winner.

Validation checks all score termination rules, 20 unique pool pairings, four pool games per team, every bracket dependency, two bracket losses for each eliminated top-eight team, and ten unique finishing teams. Championship scores in game_results.csv are individual game points. In results.json's bracket records, GF and GFR scores represent series wins.

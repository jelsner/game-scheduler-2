# Second tournament simulation

Run from the project root: `Rscript simulations/2026-10-03-run-2/run_simulation.R`.

Uses the same ten partnered teams and the same 2026 Open ranking snapshot as the first simulation. Diana Elsner / Lindsi Allman play in Open. Francis Flores is matched to the ranking entry Fran Flores.

Initial seeds sort combined individual ranking points in descending order: the greatest sum receives seed 1. Simulated point strength is directly proportional to that sum. For a matchup, team A's probability of scoring each point is A_points / (A_points + B_points). For example, a team with twice the ranking points receives twice the point strength and a 2/3 probability of scoring each point. Game-winning probability follows from playing to the target score, rather than being set directly to that percentage. This replaces the first run's exponent of 0.45. It is an illustrative model, not a calibrated forecast.

The app's two five-team pools and pool-result-based top-eight bracket seeding remain in use. Pool standings break ties by tied-team head-to-head wins, point differential, then original seed. Bracket seeds compare pool place, wins, point differential, then original seed. Pool results therefore determine bracket seeds, while combined ranking points determine initial seeds and point strength.

Pool, ordinary bracket and ninth-place games are to 15, win by two. The grand final is best of three games to 21, win by two. If the losers-bracket finalist wins that series, both finalists have one bracket loss and play exactly one deciding game to 21, win by two. There is no second best-of-three series. All games are uncapped.

Random seed: 20261004. This is a new draw, not a selection among outcomes. The first simulation is preserved in its original directory. Validation checks descending initial seed points, score termination, all pool pairings, four pool games per team, all bracket dependencies, championship series termination, the conditional single-game reset, and bracket losses. Fifth and seventh places remain tied.

In results.json, GF scores are series wins and GFR scores are actual deciding-game points. game_results.csv contains individual game-point scores throughout. The roster and ranking source details are documented in the first simulation's README.

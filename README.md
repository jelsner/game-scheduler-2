# US DDC Championship 2026 scoring desk

An R/Shiny app for the 13 registered teams at the November 7-8, 2026 championship
in Tallahassee. The app runs offline after its R packages are installed. The
roster uses first names and last initials from the tournament website's October
28 initial-seed list: [usddc2026.com](https://usddc2026.com/).

## Tournament format

The original seeds are snake-seeded into three pools, with two courts per pool:

| Pool | Original overall seeds | Courts |
| --- | --- | --- |
| A (5 teams) | 1, 6, 7, 12, 13 | 1-2 |
| B (4 teams) | 2, 5, 8, 11 | 3-4 |
| C (4 teams) | 3, 4, 9, 10 | 5-6 |

All 22 pool matches are round robin. Pool seeds 1 and 2 meet in each pool's
final round. Each pool keeps its original overall seed slots: pool finish
assigns those slots, lowest number first. Records from different pools are
never compared. Original seeds remain on the roster alongside earned seeds.

Pool standings compare wins, tied-team wins, tied-team point difference,
wins against teams above the initial tied group, point difference against those
teams, and overall pool point difference, following WFDF 206.03 for completed
matches without forfeits. A remaining tie blocks earned seeds until the director
records the result of a playoff/disc flip in the **Reseeding tally** tab. The
entered tie order only affects teams still tied on all statistical comparisons.
The app does not invent forfeit scores; a forfeit requires a director ruling.

All 13 teams then enter the seeded double-elimination bracket. Seeds 1-3 get
opening byes. The bracket and six-court wave plan match the agreed paper draw.
Standard matches are one game to 15, win by two, no cap. G24 is best of three
games to 15, each win by two. If the lower-bracket finalist wins that series,
G25 is one game to 21, win by two; otherwise G25 is not played.

Placement matches determine every final position:

- Loser of G13 finishes 13th and is done.
- Losers of G14-G17 form the 9th-12th mini bracket. Order their **earned**
  bracket seeds best to worst: P1 is M1 vs M4; P2 is M2 vs M3. Winners play P3
  for 9th/10th, and losers play P4 for 11th/12th.
- Losers of G18/G19 play P5 for 7th/8th.
- Losers of G20/G21 play P6 for 5th/6th.
- Losers of G22/G23 finish 4th/3rd; the championship decides 1st/2nd.

Placement games use free courts: P1/P2 in wave 4, P3/P4/P5 in wave 5, P6 in
wave 6. A wave starts games together. Start the next wave once the prior wave
finishes, allowing the agreed rest period; there are no assumed clock times.

## Run and score locally

Open `app.R` in RStudio and click **Run App**, or run from the repository root:

```r
install.packages(c("shiny", "dplyr", "DT", "jsonlite"))
shiny::runApp()
```

1. Review the **Pools** and **Teams & rules** tabs.
2. In **Score a match**, choose the stage and a ready match. Enter points and
   click **Save result**. Completed matches remain available for corrections.
   Selecting a stage before its prerequisites finish shows a warning and keeps
   the current scoring stage open. Placement unlocks as soon as its source
   matches finish; it does not wait for the whole double-elimination bracket.
3. Enter individual game scores for G24; leave game 3 blank after a 2-0 series.
   Invalid scores, incomplete series, and games entered after a clinch are rejected.
4. After pool play, resolve any remaining pool ties in **Reseeding tally**.
   The bracket unlocks when all earned seeds are confirmed.
5. Continue scoring bracket and placement matches as prerequisites finish.
   **Final standings** shows decided places and actual games played, including
   pool/placement games and individual championship games. Byes are excluded.

When a correction changes a later match's participants, the app permanently
clears that match's old result and affected descendants. Scores are never
silently assigned to different teams. A correction that leaves the participants
unchanged preserves valid later results. Clearing a result requires confirmation.

## Saving and backups

Every saved change is atomically written to `data/tournament-state.json`.
Reloads and local app restarts recover it. Sessions in the same R process share
the tournament. Set `DDC_STATE_FILE` to use a different save path, for example
when testing. The save file is excluded from Git.

Download a JSON backup regularly. **Review restore** validates a selected backup
and asks before replacing the current tournament. Backups include individual
series-game scores and director tie decisions. Malformed/incompatible backups
are rejected. Backups also identify the roster: scored backups from an earlier
roster cannot be applied to replacement teams. Empty older backups are accepted.
If an existing local save is corrupt, startup reports the error
rather than overwriting it.

Use one scoring operator and one server process. This implementation is not a
multi-worker database or a public read-only spectator view. On shinyapps.io,
local disk is ephemeral across deployments/instance replacement; download
backups and restore them as needed. The refactor does not deploy itself.

## PDFs

The **Print PDFs** tab generates current snapshots directly in R, with no Python
or system PDF dependency on the server:

- `output/pdf/ddc-2026-pool-score-sheets.pdf`: three portrait Letter pages with
  named rosters, original and pool seeds, match IDs/scores, cumulative W/L and earned seeds.
- `output/pdf/ddc-2026-reseeding-tally.pdf`: one landscape Letter page with all
  13 teams, original seeds, pool records, finishing places and earned seeds.
- `output/pdf/ddc-2026-bracket-and-placement.pdf`: two landscape Letter pages,
  upper/championship on page 1 and lower/placement on page 2, plus the six-court plan.

Unplayed scores and unknown bracket participants remain blank. Bracket names
fill in after pool results establish earned seeds. All downloads reflect saved
scores, so download fresh PDFs after scoring. Print at actual size.

Regenerate the initial named sheets from the repository root:

```r
library(dplyr)
source("R/us_championship_engine.R")
source("R/championship_13.R")
source("R/championship_pdfs.R")
snapshot <- empty_tournament()
state <- tournament_state_13(snapshot)
write_pool_pdfs_13("output/pdf/ddc-2026-pool-score-sheets.pdf", state, snapshot)
write_tally_pdf_13("output/pdf/ddc-2026-reseeding-tally.pdf", state)
write_bracket_pdf_13("output/pdf/ddc-2026-bracket-and-placement.pdf", state, snapshot)
```

Earlier blank templates and the 11 x 17 PDF/Python generators remain in the
repository as reference artifacts. The active app uses the R generators and
13-team model, not the legacy 10-12-team/top-eight helpers.

## Verification

```sh
Rscript tests/championship_13.R
Rscript tests/app_13.R
Rscript tests/stage_navigation_13.R
Rscript tests/pool_score_sheets.R
```

The engine suite checks pool pair coverage and courts, earned seeds, exact
cyclic ties/director decisions, score/series validation, all 13 final places,
reset/no-reset tournaments, downstream correction, backup round trips and
filled PDFs. It runs 24 complete simulated tournaments. The Shiny suite checks
actual save/restore/download handlers using isolated test storage. Run it after
the engine suite, which creates the completed-tournament test fixture.

## Credit and license

Inspired by Heikki Wilenius's GPL DDC tournament manager:
<https://github.com/wilenius/ddc>. Released under GPL-3; see `LICENSE`.

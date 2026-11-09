"""Score the 14 saved predictions against the completed tournament standings.
Run from the repository root: python3 output/predictions/score_predictions.py
"""
from pathlib import Path
import json

root = Path(__file__).resolve().parent
final = json.loads((root / 'final-standings-2026-11-08.json').read_text())
submitted = json.loads((root / 'submitted-predictions-2026-11-08.json').read_text())
actual = {row['team_id']: row['place'] for row in final}
team_names = {row['team_id']: row['team'] for row in final}
assert len(actual) == 13 and set(actual.values()) == set(range(1, 14))
assert submitted['total'] == len(submitted['entries']) == 14
rows = []
for entry in submitted['entries']:
    ranking = entry['ranking']
    assert len(ranking) == 13 and set(ranking) == set(actual)
    predicted = {team_id: index + 1 for index, team_id in enumerate(ranking)}
    details = [dict(team_id=row['team_id'], team=row['team'], actual=row['place'],
                    predicted=predicted[row['team_id']],
                    places_off=abs(predicted[row['team_id']] - row['place'])) for row in final]
    error = sum(row['places_off'] for row in details)
    # Independent expression: iterate predicted positions instead of final positions.
    assert error == sum(abs(index + 1 - actual[team]) for index, team in enumerate(ranking))
    assert error % 2 == 0 and 0 <= error <= 84
    rows.append(dict(name=entry['name'], total_places_off=error,
                     exact_places=sum(row['places_off'] == 0 for row in details),
                     predicted_champion=team_names[ranking[0]],
                     champion_correct=ranking[0] == final[0]['team_id'],
                     details=details))
rows.sort(key=lambda row: (row['total_places_off'], row['name'].casefold()))
for row in rows:
    row['rank'] = 1 + sum(other['total_places_off'] < row['total_places_off'] for other in rows)
report = ['# Tournament prediction results - November 8, 2026', '',
          'Final standings source: ddc-tournament-2026-11-08 Final.json (completed tournament).',
          'All 14 submissions were retrieved from the prediction website; all are included.', '',
          '## Scoring', '',
          'Total places off = sum of the absolute differences between each team\'s predicted and actual finishing place. Lower is better; zero is perfect. A team predicted 3rd that finishes 6th adds 3. All 13 teams have equal weight. Equal totals share a rank; exact-place picks are shown for context and are not a tiebreaker.', '',
          '## Predictions, best to worst', '',
          '| Rank | Entrant | Total places off | Exact places | Predicted champion |',
          '|---:|---|---:|---:|---|']
for row in rows:
    name = row['name'].replace('|', r'\|')
    report.append(f"| {row['rank']} | {name} | {row['total_places_off']} | {row['exact_places']} | {row['predicted_champion']} |")
report += ['', '## Actual final standings', '', '| Place | Team |', '|---:|---|']
for row in final:
    report.append(f"| {row['place']} | {row['team']} |")
report += ['', '## Team-by-team comparisons', '']
for row in rows:
    report += [f"### {row['name']} - {row['total_places_off']} total places off", '',
               '| Team | Actual place | Predicted place | Places off |', '|---|---:|---:|---:|']
    for detail in row['details']:
        report.append(f"| {detail['team']} | {detail['actual']} | {detail['predicted']} | {detail['places_off']} |")
    report.append('')
(root / 'prediction-results-2026-11-08.md').write_text('\n'.join(report) + '\n')
(root / 'scored-predictions-2026-11-08.json').write_text(json.dumps(rows, indent=2) + '\n')
for row in rows:
    print(f"{row['rank']:>2}  {row['name']:<22} {row['total_places_off']:>2} places off  {row['exact_places']:>2} exact")
print('Correct champion:', sum(row['champion_correct'] for row in rows), 'of 14')

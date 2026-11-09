"""Create printable prediction rankings and actual final standings from saved results."""
from pathlib import Path
import json
from reportlab.pdfgen import canvas
from reportlab.lib.colors import HexColor, white
from pypdf import PdfReader

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'output/pdf'
DATA = ROOT / 'output/predictions'
OUT.mkdir(parents=True, exist_ok=True)
rankings = json.loads((DATA / 'scored-predictions-2026-11-08.json').read_text())
standings = json.loads((DATA / 'final-standings-2026-11-08.json').read_text())
assert len(rankings) == 14 and len(standings) == 13
assert [r['total_places_off'] for r in rankings] == sorted(r['total_places_off'] for r in rankings)
assert [r['place'] for r in standings] == list(range(1, 14))
INK = HexColor('#19392e')
MUTED = HexColor('#59645c')
LINE = HexColor('#cbd2c9')
PALE = HexColor('#edf2e9')


def label(c, x, y, text, size=11, bold=False, color=INK):
    c.setFillColor(color)
    c.setFont('Helvetica-Bold' if bold else 'Helvetica', size)
    c.drawString(x, y, str(text))


def header(c, title, subtitle):
    label(c, 36, 754, 'US DDC CHAMPIONSHIP 2026', 10, True)
    label(c, 36, 718, title, 25, True)
    label(c, 36, 695, subtitle, 10, color=MUTED)
    c.setStrokeColor(INK)
    c.setLineWidth(1.2)
    c.line(36, 678, 576, 678)


def footer(c, detail):
    c.setStrokeColor(LINE)
    c.setLineWidth(.5)
    c.line(36, 57, 576, 57)
    label(c, 36, 40, detail, 8, color=MUTED)
    c.setFillColor(MUTED)
    c.setFont('Helvetica', 8)
    c.drawRightString(576, 40, '1 of 1')


def table(c, top, widths, headings, rows, highlights=()):
    x = [36]
    for width in widths:
        x.append(x[-1] + width)
    height = 31
    c.setFillColor(INK)
    c.rect(36, top-height, 540, height, fill=1, stroke=0)
    for i, heading in enumerate(headings):
        label(c, x[i]+12, top-20, heading, 10, True, white)
    y = top-height
    for index, row in enumerate(rows):
        if index in highlights:
            c.setFillColor(PALE)
            c.rect(36, y-height, 540, height, fill=1, stroke=0)
        c.setStrokeColor(LINE)
        c.setLineWidth(.5)
        c.line(36, y-height, 576, y-height)
        for i, value in enumerate(row):
            label(c, x[i]+12, y-20, value, 11, index in highlights)
        y -= height
    return y


prediction_file = OUT / 'ddc-2026-prediction-rankings.pdf'
c = canvas.Canvas(str(prediction_file), pagesize=(612, 792))
c.setTitle('2026 US DDC Championship - Prediction Rankings')
c.setAuthor('US DDC Championship 2026')
header(c, 'Prediction rankings', 'Best to worst | All 14 entries | Final results: November 8, 2026')
rows = [(r['rank'], r['name'], r['total_places_off'], r['exact_places']) for r in rankings]
table(c, 657, [52, 248, 130, 110], ['Rank', 'Entrant', 'Total places off', 'Exact picks'], rows, (0, 1))
label(c, 36, 160, 'HOW THE PREDICTIONS WERE SCORED', 10, True)
for y, line in zip([140, 123, 106, 89], [
    "Add each team's absolute difference between predicted and actual finishing place.",
    'Lower is better; zero is perfect. Example: predicted 3rd, finished 6th = 3 places off.',
    'Equal totals share a rank. Exact picks are shown for context, not as a tiebreaker.',
    'CD and Trav tied for best prediction with 10 total places off.'
]):
    label(c, 36, y, line, 10, color=MUTED)
footer(c, 'Tallahassee, Florida | November 7-8, 2026 | Fan prediction results')
c.showPage()
c.save()

standings_file = OUT / 'ddc-2026-actual-final-standings.pdf'
c = canvas.Canvas(str(standings_file), pagesize=(612, 792))
c.setTitle('2026 US DDC Championship - Actual Final Standings')
c.setAuthor('US DDC Championship 2026')
header(c, 'Actual final standings', 'All 13 teams | Tournament completed November 8, 2026')
table(c, 657, [66, 474], ['Place', 'Team / players'], [(r['place'], r['team']) for r in standings], (0,))
label(c, 36, 163, 'CHAMPIONS', 10, True)
label(c, 36, 141, standings[0]['team'], 15, True)
label(c, 36, 111, 'Includes championship and placement-game results.', 10, color=MUTED)
label(c, 36, 93, 'Source: completed tournament save, November 8, 2026.', 10, color=MUTED)
footer(c, 'Tallahassee, Florida | November 7-8, 2026 | Final tournament results')
c.showPage()
c.save()

for path, expected_names in [
    (prediction_file, [r['name'] for r in rankings]),
    (standings_file, [r['team'] for r in standings]),
]:
    reader = PdfReader(path)
    assert len(reader.pages) == 1
    assert tuple(reader.pages[0].mediabox) == (0, 0, 612, 792)
    text = reader.pages[0].extract_text()
    for name in expected_names:
        assert name in text, name
    print(f'{path.name}: one Letter page; all {len(expected_names)} names verified')

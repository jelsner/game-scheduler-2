"""Create a standalone 11 x 17 bracket. Run with the bundled Python runtime."""
from pathlib import Path
import random
from reportlab.pdfgen import canvas
from reportlab.lib.colors import HexColor, black, white

ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / 'output/pdf/ddc-13-team-double-elimination-bracket.pdf'
# Sources: S = earned bracket seed, W = winner, L = loser of a numbered game.
GAMES = {
    1: ('S8', 'S9'), 2: ('S4', 'S13'), 3: ('S5', 'S12'),
    4: ('S6', 'S11'), 5: ('S7', 'S10'),
    6: ('S1', 'W1'), 7: ('W2', 'W3'), 8: ('S3', 'W4'), 9: ('W5', 'S2'),
    10: ('W6', 'W7'), 11: ('W8', 'W9'), 12: ('W10', 'W11'),
    13: ('L2', 'L3'),
    14: ('L7', 'L1'), 15: ('L6', 'W13'), 16: ('L9', 'L4'), 17: ('L8', 'L5'),
    18: ('W14', 'W15'), 19: ('W16', 'W17'),
    20: ('W18', 'L11'), 21: ('W19', 'L10'),
    22: ('W20', 'W21'), 23: ('W22', 'L12'),
    24: ('W12', 'W23'), 25: ('W24', 'L24'),
}
WAVES = [[1, 2, 3, 4, 5], [6, 7, 8, 9, 13], [10, 11, 14, 15, 16, 17],
         [12, 18, 19], [20, 21], [22], [23], [24], [25]]
ASSIGNMENT = {g: (w, court) for w, games in enumerate(WAVES, 1)
              for court, g in enumerate(games, 1)}
# Box left edge and vertical center, in PDF points.
POS = {
    1: (30, 674), 2: (30, 620), 3: (30, 566), 4: (30, 512), 5: (30, 458),
    6: (220, 674), 7: (220, 593), 8: (220, 512), 9: (220, 458),
    10: (410, 633.5), 11: (410, 485), 12: (600, 559),
    13: (30, 245), 14: (190, 355), 15: (190, 275), 16: (190, 185), 17: (190, 105),
    18: (350, 315), 19: (350, 145), 20: (510, 315), 21: (510, 145),
    22: (670, 230), 23: (830, 230), 24: (790, 680), 25: (790, 590),
}
WIDTH, HEIGHT = 140, 44


def verify():
    """Validate bracket flow, double elimination, reset, and simultaneous games."""
    assert set(GAMES) == set(range(1, 26)) == set(ASSIGNMENT)
    assert len(ASSIGNMENT) == sum(map(len, WAVES))
    seeds = [int(s[1:]) for pair in GAMES.values() for s in pair if s.startswith('S')]
    assert sorted(seeds) == list(range(1, 14))
    drops = [int(s[1:]) for g, pair in GAMES.items() if g < 25
             for s in pair if s.startswith('L')]
    assert sorted(drops) == list(range(1, 13))
    for g, pair in GAMES.items():
        assert len(pair) == 2
        for s in pair:
            if s[0] != 'S':
                assert ASSIGNMENT[int(s[1:])][0] < ASSIGNMENT[g][0]
    rng = random.Random(13)
    reset_cases = 0
    for trial in range(10002):
        results = {}
        losses = {s: 0 for s in range(1, 14)}
        def resolve(source):
            kind, ref = source[0], int(source[1:])
            return ref if kind == 'S' else results[ref][0 if kind == 'W' else 1]
        played = 0
        for wave in WAVES:
            playing = set()
            for g in wave:
                if g == 25 and results[24][0] == results[12][0]:
                    continue
                a, b = map(resolve, GAMES[g])
                assert a != b and not {a, b} & playing
                playing.update((a, b))
                assert losses[a] < 2 and losses[b] < 2
                if g <= 12:
                    assert losses[a] == losses[b] == 0
                elif g <= 23 or g == 25:
                    assert losses[a] == losses[b] == 1
                else:
                    assert losses[a] == 0 and losses[b] == 1
                if trial == 0:
                    winner = min(a, b)
                elif trial == 1:
                    winner = max(a, b)
                else:
                    winner = rng.choice((a, b))
                loser = b if winner == a else a
                results[g] = winner, loser
                losses[loser] += 1
                played += 1
            assert len(playing) <= 12
        champion = results.get(25, results[24])[0]
        assert losses[champion] <= 1
        assert all(losses[s] == 2 for s in losses if s != champion)
        assert played in (24, 25)
        reset_cases += played == 25
    assert 0 < reset_cases < 10002
    print('Verified 10,002 tournaments: two losses eliminate, correct resets, no court conflicts.')


def create():
    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    c = canvas.Canvas(str(OUTPUT), pagesize=(1224, 792))
    c.setTitle('13-team seeded double-elimination bracket - six courts')
    def text(x, y, value, size=9, bold=False, color=black):
        c.setFillColor(color)
        c.setFont('Helvetica-Bold' if bold else 'Helvetica', size)
        c.drawString(x, y, value)
    def line(x1, y1, x2, y2):
        c.setStrokeColor(HexColor('#666666')); c.setLineWidth(0.6)
        c.line(x1, y1, x2, y2)
    def arrow(x, y, direction=1):
        c.setFillColor(HexColor('#666666'))
        p = c.beginPath(); p.moveTo(x, y); p.lineTo(x - 4 * direction, y + 2)
        p.lineTo(x - 4 * direction, y - 2); p.close()
        c.drawPath(p, fill=1, stroke=0)
    # Draw winner progression first so boxes sit over connector ends.
    for g, sources in GAMES.items():
        if g == 25:
            continue
        tx, ty = POS[g]
        for idx, source in enumerate(sources):
            if not source.startswith('W'):
                continue
            prior = int(source[1:]); sx, sy = POS[prior]
            if prior == 23:  # The lower winner is identified at both endpoints.
                continue
            end_y = ty + (2 if idx == 0 else -14)
            start_x = sx + WIDTH
            mid_x = start_x + (tx - start_x) / 2
            line(start_x, sy, mid_x, sy)
            line(mid_x, sy, mid_x, end_y)
            line(mid_x, end_y, tx, end_y); arrow(tx, end_y)
    # Conditional reset connection.
    line(930, 680, 960, 680)
    line(960, 680, 960, 590)
    line(960, 590, 930, 590); arrow(930, 590, direction=-1)
    text(870, 633, 'If needed', 8)
    text(30, 754, '13-TEAM SEEDED DOUBLE-ELIMINATION BRACKET', 22, True)
    text(30, 735, 'Event: __________________________________________    Date: __________________   Use earned bracket seeds from pool play.', 10)
    for x, label in [(30, 'OPENING ROUND'), (220, 'QUARTERFINALS'), (410, 'SEMIFINALS'), (600, 'WINNERS FINAL'), (790, 'CHAMPIONSHIP')]:
        text(x, 711, label, 10, True)
    text(30, 407, 'ELIMINATION BRACKET - SECOND LOSS ELIMINATES', 13, True)
    for x, label in [(30, 'ROUND 1'), (190, 'ROUND 2'), (350, 'ROUND 3'), (510, 'ROUND 4'), (670, 'ROUND 5'), (830, 'LOWER FINAL')]:
        text(x, 389, label, 9, True)
    dest = {int(s[1:]): g for g, sources in GAMES.items() if g < 25
            for s in sources if s.startswith('L')}
    for g, sources in GAMES.items():
        x, y = POS[g]; top = y + HEIGHT / 2
        c.setFillColor(white); c.setStrokeColor(HexColor('#333333')); c.setLineWidth(0.7)
        c.rect(x, y - HEIGHT / 2, WIDTH, HEIGHT, stroke=1, fill=1)
        c.setFillColor(HexColor('#eaeaea'))
        c.rect(x + 0.4, top - 13, WIDTH - 0.8, 12.6, stroke=0, fill=1)
        wave, court = ASSIGNMENT[g]
        text(x + 5, top - 9, f'G{g}', 8, True)
        text(x + 34, top - 9, f'Wave {wave} / Court {court}', 7.5)
        for i, s in enumerate(sources):
            row_y = top - 24 - i * 15
            value = 'Seed ' + s[1:] if s.startswith('S') else s
            text(x + 5, row_y, value, 8)
            line(x + 50, row_y - 2, x + WIDTH - (57 if g == 24 else 29), row_y - 2)
            c.setStrokeColor(HexColor('#777777'))
            if g == 24:
                # Separate scores for each game in the championship series.
                for offset in (51, 35, 19):
                    c.rect(x + WIDTH - offset, row_y - 3, 13, 11, stroke=1, fill=0)
            else:
                c.rect(x + WIDTH - 23, row_y - 3, 18, 11, stroke=1, fill=0)
        if g <= 12:
            text(x + 3, y - HEIGHT / 2 - 9, f'Loser -> G{dest[g]}', 7.5)
        elif g == 23:
            text(x + 3, y - HEIGHT / 2 - 11, 'Winner -> G24 (W23)', 8, True)
    text(600, 614, 'Seeds 1, 2 and 3 receive', 9)
    text(600, 601, 'opening-round byes.', 9)
    text(790, 643, 'Best of 3 to 15, win by 2. Scores: 1 / 2 / 3.', 8)
    text(790, 549, 'Reset: one game to 21, win by 2.', 9, True)
    text(790, 535, 'Play only if W23 wins the G24 series.', 8.5)
    # Lower byes are collapsed to source labels rather than fictitious games.
    for y, label in [(355, 'L1 goes directly to G14'), (185, 'L4 goes directly to G16'), (105, 'L5 goes directly to G17')]:
        text(30, y, label, 9)
    # Independent operations panel.
    line(1000, 50, 1000, 714)
    text(1015, 709, 'SIX-COURT PLAN', 13, True)
    text(1015, 693, 'Games in one wave run together.', 9)
    table_x, top, widths = 1015, 678, [28] + [26] * 6
    labels = ['Wave', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6']
    for row_index in range(10):
        y = top - row_index * 24
        values = labels if row_index == 0 else [str(row_index)] + [f'G{g}' for g in WAVES[row_index-1]] + ['-'] * (6-len(WAVES[row_index-1]))
        x = table_x
        for width, value in zip(widths, values):
            c.setFillColor(HexColor('#eaeaea') if row_index == 0 else white)
            c.setStrokeColor(HexColor('#888888')); c.setLineWidth(0.5)
            c.rect(x, y - 24, width, 24, stroke=1, fill=1)
            text(x + 3, y - 15, value, 7.5, row_index == 0)
            x += width
    for y, label in [(421, 'Wave 9 / G25: only if needed.'),
                     (396, 'Start each wave after the previous'), (383, 'wave finishes. Allow agreed rest'), (370, 'between games; no fixed times set.'),
                     (347, 'SCORING'),
                     (333, 'Standard: one game to 15, win by 2.'),
                     (319, 'G24: best of 3 games to 15,'),
                     (306, 'each game win by 2.'),
                     (292, 'G25: one game to 21, win by 2.'),
                     (270, 'W# / L# = winner / loser of game #.'),
                     (256, 'For G24, W24 / L24 mean the'),
                     (243, 'series winner / loser.'),
                     (221, 'If W12 wins G24, tournament ends.'),
                     (208, 'If W23 wins, play the G25 reset.'),
                     (186, 'Write seeds / teams on the lines;'),
                     (173, 'boxes record individual game scores.')]:
        text(1015, y, label, 8.5)
    text(1015, 150, 'CHAMPION', 10, True); line(1015, 128, 1196, 128)
    text(1015, 102, 'RUNNER-UP', 10, True); line(1015, 80, 1196, 80)
    text(30, 52, 'Print on 11 x 17 paper, landscape, at 100%. Court numbers are 1-6; unused courts are available for warm-up.', 9)
    text(30, 36, 'Keep earned bracket seeds fixed throughout this stage. Follow the winner and loser destinations shown; do not reseed between rounds.', 9)
    c.showPage(); c.save()
    print(OUTPUT)


if __name__ == '__main__':
    verify()
    create()

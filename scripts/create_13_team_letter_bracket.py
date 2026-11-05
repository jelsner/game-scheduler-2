"""Two US Letter landscape pages, including the agreed placement matches."""
from pathlib import Path
import random
from reportlab.pdfgen import canvas
from reportlab.lib.colors import HexColor, black, white
from create_13_team_bracket import GAMES, ASSIGNMENT, WAVES, verify

ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / 'output/pdf/ddc-13-team-double-elimination-bracket-letter.pdf'
PLACEMENTS = {
    'P1': ('M1', 'M4'), 'P2': ('M2', 'M3'),
    'P3': ('WP1', 'WP2'), 'P4': ('LP1', 'LP2'),
    'P5': ('L18', 'L19'), 'P6': ('L20', 'L21'),
}
EXTRA_COURTS = {'P1': (4, 4), 'P2': (4, 5), 'P3': (5, 3),
                'P4': (5, 4), 'P5': (5, 5), 'P6': (6, 2)}


def verify_placements():
    rng = random.Random(1313)
    for _ in range(5000):
        results = {}
        played = {seed: 0 for seed in range(1, 14)}
        mini = []
        def resolve(source):
            kind, ref = source[0], source[1:]
            if kind == 'S': return int(ref)
            if kind == 'M': return mini[int(ref)-1]
            return results[int(ref) if ref.isdigit() else ref][0 if kind == 'W' else 1]
        for wave_index, wave in enumerate(WAVES, 1):
            if wave_index == 4:
                mini = sorted(results[g][1] for g in (14, 15, 16, 17))
            active = set()
            courts = set()
            jobs = [(g, GAMES[g], ASSIGNMENT[g][1]) for g in wave]
            jobs += [(g, PLACEMENTS[g], court) for g, (w, court) in EXTRA_COURTS.items() if w == wave_index]
            for g, sources, court in jobs:
                if g == 25 and results[24][0] == results[12][0]: continue
                a, b = map(resolve, sources)
                assert a != b and not {a,b} & active and court not in courts
                active.update((a,b)); courts.add(court)
                winner, loser = (a,b) if rng.randrange(2) else (b,a)
                results[g] = winner, loser
                played[a] += 1; played[b] += 1
            assert max(courts, default=0) <= 6
        ranks = {13: results[13][1], 12: results['P4'][1], 11: results['P4'][0],
                 10: results['P3'][1], 9: results['P3'][0],
                 8: results['P5'][1], 7: results['P5'][0],
                 6: results['P6'][1], 5: results['P6'][0],
                 4: results[22][1], 3: results[23][1]}
        final = results.get(25, results[24])
        ranks[1], ranks[2] = final
        assert len(set(ranks.values())) == 13
        assert played[ranks[13]] == 2
        assert all(played[ranks[place]] >= 4 for place in range(3,13))
    print('Verified 5,000 placement schedules: all 13 places resolved; no team or court conflicts.')


class Sheet:
    def __init__(self):
        self.c = canvas.Canvas(str(OUTPUT), pagesize=(792,612))
        self.c.setTitle('13-team double-elimination and placement brackets - two Letter pages')
    def text(self,x,y,value,size=9,bold=False):
        self.c.setFillColor(black)
        self.c.setFont('Helvetica-Bold' if bold else 'Helvetica',size)
        self.c.drawString(x,y,value)
    def line(self,x1,y1,x2,y2):
        self.c.setStrokeColor(HexColor('#666666')); self.c.setLineWidth(.6)
        self.c.line(x1,y1,x2,y2)
    def arrow(self,x,y,direction=1):
        self.c.setFillColor(HexColor('#666666'))
        p=self.c.beginPath(); p.moveTo(x,y); p.lineTo(x-4*direction,y+2)
        p.lineTo(x-4*direction,y-2); p.close(); self.c.drawPath(p,fill=1,stroke=0)
    def header(self,title,page):
        self.text(28,575,title,18,True)
        self.text(28,555,'Event: __________________________________________    Date: __________________',10)
        self.text(28,537,'13 teams | Earned seeds from pool play | Six courts | W# / L# = winner / loser of game #',9)
        self.text(28,20,'US Letter (8.5 x 11), landscape, 100% / actual size. Print both pages single-sided.',8)
        self.text(710,20,f'Page {page} of 2',8)
    def connectors(self,positions,width,games):
        for g,(tx,ty) in positions.items():
            for idx,source in enumerate(games[g]):
                if not source.startswith('W'): continue
                ref=source[1:]
                prior=int(ref) if ref.isdigit() else ref
                if prior not in positions or g==25: continue
                sx,sy=positions[prior]
                if tx <= sx: continue
                start=sx+width; mid=(start+tx)/2
                target=ty+(1 if idx==0 else -16)
                self.line(start,sy,mid,sy); self.line(mid,sy,mid,target)
                self.line(mid,target,tx,target); self.arrow(tx,target)
    def box(self,g,x,y,width=170,sources=None,caption=None):
        sources=sources or GAMES[g]
        wave,court=EXTRA_COURTS[g] if isinstance(g,str) else ASSIGNMENT[g]
        top=y+24
        self.c.setStrokeColor(HexColor('#333333')); self.c.setLineWidth(.7)
        self.c.setFillColor(white); self.c.rect(x,y-24,width,48,stroke=1,fill=1)
        self.c.setFillColor(HexColor('#eaeaea'))
        self.c.rect(x+.4,top-13,width-.8,12.6,stroke=0,fill=1)
        title=g if isinstance(g,str) else f'G{g}'
        self.text(x+4,top-9,title,8,True)
        self.text(x+29,top-9,f'Wv {wave} / C{court}',7.5)
        for i,source in enumerate(sources):
            yy=top-25-i*17
            label='Seed '+source[1:] if source.startswith('S') else source
            self.text(x+4,yy,label,8.5)
            start=x+(45 if width>=140 else 30)
            end=x+width-(58 if g==24 else 27)
            self.line(start,yy-2,end,yy-2)
            self.c.setStrokeColor(HexColor('#777777'))
            if g==24:
                for offset in (52,35,18): self.c.rect(x+width-offset,yy-3,14,12,stroke=1,fill=0)
            else: self.c.rect(x+width-22,yy-3,18,12,stroke=1,fill=0)
        if caption: self.text(x+1,y-35,caption,7.5)
    def court_table(self):
        self.text(28,195,'SIX-COURT WAVE PLAN (G = MAIN; P = PLACEMENT)',9,True)
        widths=[35]+[49]*6
        for row in range(10):
            top=181-(0 if row==0 else 18+(row-1)*14)
            height=18 if row==0 else 14
            if row==0: values=['Wave','Court 1','Court 2','Court 3','Court 4','Court 5','Court 6']
            else:
                values=[str(row)]+['-']*6
                for g in WAVES[row-1]: values[ASSIGNMENT[g][1]]=f'G{g}'+('*' if g==25 else '')
                for g,(wave,court) in EXTRA_COURTS.items():
                    if wave==row: values[court]=g
            x=28
            for w,value in zip(widths,values):
                self.c.setFillColor(HexColor('#eaeaea') if row==0 else white)
                self.c.setStrokeColor(HexColor('#999999')); self.c.setLineWidth(.5)
                self.c.rect(x,top-height,w,height,stroke=1,fill=1)
                self.text(x+3,top-height+5,value,7.5,row==0)
                x+=w
    def upper(self):
        self.header('13-TEAM DOUBLE ELIMINATION - UPPER BRACKET',1)
        positions={1:(28,488),2:(28,428),3:(28,368),4:(28,308),5:(28,248),
                   6:(213,488),7:(213,398),8:(213,308),9:(213,248),
                   10:(398,443),11:(398,278),12:(583,360)}
        self.connectors(positions,170,GAMES)
        dest={int(s[1:]):g for g,pair in GAMES.items() if g<25 for s in pair if s.startswith('L')}
        for x,title in [(28,'OPENING ROUND'),(213,'QUARTERFINALS'),(398,'SEMIFINALS'),(583,'WINNERS FINAL')]:
            self.text(x,523,title,10,True)
        for g,(x,y) in positions.items(): self.box(g,x,y,caption=f'Loser -> G{dest[g]} on page 2')
        self.text(583,493,'Seeds 1, 2 and 3 receive',9)
        self.text(583,480,'opening-round byes.',9)
        self.text(583,452,'Standard games: 21, win by 2.',9,True)
        self.text(583,430,'Follow earned bracket seeds;',9)
        self.text(583,417,'keep them fixed during this stage.',9)
        self.text(583,308,'W12 -> G24 below.',9,True)
        self.text(398,192,'CHAMPIONSHIP SERIES',10,True)
        self.text(583,192,'RESET (IF NEEDED)',10,True)
        self.box(24,398,142)
        self.box(25,583,142)
        self.line(568,142,583,142); self.arrow(583,142)
        self.text(398,103,'Best of 3 to 21, each win by 2.',8.5)
        self.text(583,103,'One game to 21, win by 2.',8.5)
        self.text(398,87,'W23 comes from page 2. Play G25 only if W23 wins the G24 series.',8.5)
        self.text(398,73,'G24 score boxes: game 1 / 2 / 3. W24 / L24 refer to the series result.',8)
        self.text(398,51,'Champion:',9,True); self.line(449,49,568,49)
        self.text(583,51,'Runner-up:',9,True); self.line(638,49,753,49)
        self.court_table()
        self.text(398,34,'Start each wave after the previous wave finishes; allow agreed rest. *G25 if needed.',7.8)
        self.c.showPage()
    def lower(self):
        self.header('13-TEAM DOUBLE ELIMINATION - LOWER & PLACEMENT',2)
        positions={13:(28,400),14:(153,490),15:(153,422),16:(153,354),17:(153,286),
                   18:(278,456),19:(278,320),20:(403,456),21:(403,320),22:(528,388),23:(653,388)}
        for x,title in [(28,'ROUND 1'),(153,'ROUND 2'),(278,'ROUND 3'),(403,'ROUND 4'),(528,'ROUND 5'),(653,'LOWER FINAL')]:
            self.text(x,523,title,9,True)
        self.connectors(positions,111,GAMES)
        for g,(x,y) in positions.items():
            cap=('Loser: 13th (finished)' if g==13 else 'Loser -> 9-12 mini' if g in (14,15,16,17)
                 else 'Loser -> P5' if g in (18,19) else 'Loser -> P6' if g in (20,21)
                 else 'Loser: 4th' if g==22 else 'Loser: 3rd')
            self.box(g,x,y,width=111,caption=cap)
        self.text(653,338,'W23 -> G24 on page 1',8,True)
        for y,label in [(490,'L1 -> G14'),(354,'L4 -> G16'),(286,'L5 -> G17')]: self.text(28,y,label,9)
        self.text(28,228,'PLACEMENT GAMES - ONE GAME TO 21, WIN BY 2',11,True)
        self.text(28,209,'9TH-12TH MINI BRACKET',9,True)
        self.text(398,209,'7TH / 8TH',9,True); self.text(583,209,'5TH / 6TH',9,True)
        positions_p={'P1':(28,177),'P2':(28,99),'P3':(213,177),'P4':(213,99)}
        self.connectors(positions_p,150,PLACEMENTS)
        captions={'P1':'Winner -> P3; loser -> P4','P2':'Winner -> P3; loser -> P4',
                  'P3':'Winner: 9th / Loser: 10th','P4':'Winner: 11th / Loser: 12th',
                  'P5':'Winner: 7th / Loser: 8th','P6':'Winner: 5th / Loser: 6th'}
        for g,(x,y) in positions_p.items(): self.box(g,x,y,150,PLACEMENTS[g],captions[g])
        for g,x in [('P5',398),('P6',583)]: self.box(g,x,177,170,PLACEMENTS[g],captions[g])
        self.text(398,115,'MINI-BRACKET SEEDING',9,True)
        for y,line in [(100,'M1-M4 are the losers of G14-G17, ordered by their earned'),
                       (87,'bracket seeds, best to worst (lowest number first).'),
                       (74,'P1: M1 vs M4. P2: M2 vs M3. Winners play P3; losers play P4.'),
                       (54,'W / L before P# means the winner / loser of that placement game.'),
                       (41,'The 13th-place team is finished. Placement games set final ranks.')]:
            self.text(398,y,line,8.5)
        self.c.showPage()
    def save(self): self.c.save()


if __name__=='__main__':
    verify()
    verify_placements()
    OUTPUT.parent.mkdir(parents=True,exist_ok=True)
    sheet=Sheet(); sheet.upper(); sheet.lower(); sheet.save()
    print(OUTPUT)

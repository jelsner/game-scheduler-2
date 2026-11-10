"""Summarize the final tournament's individual-game ledger and create a report."""
from pathlib import Path
import hashlib
import json
import numpy as np
from reportlab.pdfgen import canvas
from reportlab.lib.colors import HexColor, white
from pypdf import PdfReader

ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'output/statistics'
data=json.loads((OUT/'tournament-game-ledger-2026.json').read_text())
assert hashlib.md5((ROOT/data['source']).read_bytes()).hexdigest()==data['source_md5']
teams={t['team_id']:t for t in data['teams']}
games=data['games']; matches=data['matches']; rows=[]
assert len(games)==53 and len(matches)==52
for finish in data['standings']:
    tid=finish['team_id']; played=[]
    for g in games:
        if tid not in (g['teamA_id'],g['teamB_id']): continue
        a=tid==g['teamA_id']; pf=g['pointsA'] if a else g['pointsB']; pa=g['pointsB'] if a else g['pointsA']
        played.append(dict(match_id=g['match_id'],game_number=g['game_number'],stage=g['stage'],
                           opponent_id=g['teamB_id'] if a else g['teamA_id'],pf=pf,pa=pa,won=pf>pa,
                           close=abs(pf-pa)<=3,extended=min(pf,pa)>=g['target']-1))
    gp=len(played);w=sum(g['won'] for g in played);pf=sum(g['pf'] for g in played);pa=sum(g['pa'] for g in played)
    def record(subset):
        wins=sum(g['won'] for g in subset);return {'wins':wins,'losses':len(subset)-wins}
    stage={s:record([g for g in played if g['stage']==s]) for s in ['Pool play','Double elimination','Placement']}
    mw=sum(m['winner_id']==tid for m in matches);ml=sum(m['loser_id']==tid for m in matches)
    row=dict(place=finish['place'],team_id=tid,team=finish['team'],original_seed=teams[tid]['seed'],
             seed_improvement=teams[tid]['seed']-finish['place'],games=gp,wins=w,losses=gp-w,
             win_pct=w/gp,points_for=pf,points_against=pa,point_diff=pf-pa,
             avg_points_for=pf/gp,avg_points_against=pa/gp,avg_margin=(pf-pa)/gp,point_share=pf/(pf+pa),
             match_wins=mw,match_losses=ml,stages=stage,
             close_record=record([g for g in played if g['close']]),
             extended_record=record([g for g in played if g['extended']]),
             largest_win=max((g['pf']-g['pa'] for g in played if g['won']),default=None),
             largest_loss=max((g['pa']-g['pf'] for g in played if not g['won']),default=None),
             games_detail=played)
    assert gp==finish['games_played']
    assert sum(v['wins'] for v in stage.values())==w
    assert sum(v['losses'] for v in stage.values())==gp-w
    rows.append(row)
assert sum(r['wins'] for r in rows)==sum(r['losses'] for r in rows)==len(games)
assert sum(r['games'] for r in rows)==2*len(games)
assert sum(r['points_for'] for r in rows)==sum(r['points_against'] for r in rows)
assert sum(r['point_diff'] for r in rows)==0
assert sum(r['match_wins'] for r in rows)==sum(r['match_losses'] for r in rows)==len(matches)
assert len({r['team_id'] for r in rows})==13
# Fit every individual game's margin to rating(A) - rating(B).
# The connected schedule identifies all rating differences; center teams at zero.
team_ids=sorted(teams)
index={tid:i for i,tid in enumerate(team_ids)}
design=np.zeros((len(games),len(team_ids)))
margins=np.array([g['pointsA']-g['pointsB'] for g in games],dtype=float)
for i,g in enumerate(games):
    design[i,index[g['teamA_id']]]=1
    design[i,index[g['teamB_id']]]=-1
assert np.linalg.matrix_rank(design)==len(team_ids)-1, 'Schedule must connect all teams'
ratings=np.linalg.lstsq(design,margins,rcond=None)[0]
ratings-=ratings.mean()
assert abs(ratings.sum())<1e-9
assert np.allclose(design.T@(design@ratings-margins),0,atol=1e-9)
for r in rows:
    r['srs']=float(ratings[index[r['team_id']]])
    r['schedule_strength']=float(np.mean([ratings[index[g['opponent_id']]] for g in r['games_detail']]))
    assert abs(r['srs']-r['avg_margin']-r['schedule_strength'])<1e-9
rating_order=sorted(rows,key=lambda r:(-r['srs'],r['team_id']))
for r in rows:
    r['srs_rank']=1+sum(other['srs']>r['srs']+1e-10 for other in rows)
(OUT/'team-statistics-2026.json').write_text(json.dumps(rows,indent=2)+'\n')
def wl(r):return f"{r['wins']}-{r['losses']}"
report=['# US DDC Championship 2026 - Team statistics','','Source: completed tournament save.','',
        '## Overall individual-game statistics','',
        '| Place | Team | Games | W-L | Win % | Points for | Points against | Differential | PF/game | PA/game | Net/game | Point share |',
        '|---:|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|']
for r in rows:
    report.append(f"| {r['place']} | {r['team']} | {r['games']} | {wl(r)} | {r['win_pct']:.1%} | {r['points_for']} | {r['points_against']} | {r['point_diff']:+} | {r['avg_points_for']:.1f} | {r['avg_points_against']:.1f} | {r['avg_margin']:+.1f} | {r['point_share']:.1%} |")
report+=['','## Stage and match records','','| Place | Team | Pool W-L | Double-elimination game W-L | Placement W-L | All match W-L | Close W-L | Extended W-L | Original seed | Finish improvement |',
         '|---:|---|---|---|---|---|---|---|---:|---:|']
for r in rows:
    report.append(f"| {r['place']} | {r['team']} | {wl(r['stages']['Pool play'])} | {wl(r['stages']['Double elimination'])} | {wl(r['stages']['Placement'])} | {r['match_wins']}-{r['match_losses']} | {wl(r['close_record'])} | {wl(r['extended_record'])} | {r['original_seed']} | {r['seed_improvement']:+} |")
report+=['','## Schedule-adjusted ratings','','| SRS rank | Team | Final place | Games | W-L | Net/game | Schedule strength | SRS |',
         '|---:|---|---:|---:|---|---:|---:|---:|']
for r in rating_order:
    report.append(f"| {r['srs_rank']} | {r['team']} | {r['place']} | {r['games']} | {wl(r)} | {r['avg_margin']:+.2f} | {r['schedule_strength']:+.2f} | {r['srs']:+.2f} |")
report+=['',
'SRS = average point margin + schedule strength. Schedule strength is the average SRS of opponents, counting each game separately.',
'Ratings are fitted simultaneously by least squares to all 53 individual-game margins, with the 13 team ratings averaging zero. The schedule connects all teams.',
'All games have equal weight, including rematches and both championship games. Actual margins are uncapped; seeds and final places are not inputs.',
'Positive SRS means points per game above the average team on this model; positive schedule strength means stronger-than-average opponents.',
'These descriptive ratings do not change official standings. Small samples and large margins can affect the ordering.',
'Method reference: [Sports Reference SRS explainer](https://faq.sports-reference.com/portal/en/kb/articles/srs-explainer).']
report+=['','## Definitions and checks','',
'- Game W-L counts every individual game, including both championship games. Byes and the unplayed reset are excluded.',
'- Match W-L counts the best-of-three championship as one match; other matches contain one game.',
'- PF/PA: all scored points for/against; differential = PF - PA. Scoring averages divide by games played.',
'- Point share = PF / (PF + PA). This is a descriptive share of recorded points, not a probability of winning.',
'- Close games: final margin of 2 or 3 points (the latter can occur with a final double).',
'- Extended games: both teams reached at least 20 points. A 22-10 final double does not qualify.',
'- Stage records count individual games. A 0-0 placement record means no placement games were played.',
'- Finish improvement = original overall seed minus final place; positive means finishing above the initial seed.',
'- The totals reconcile: 53 games, 52 matches; wins = losses, total PF = total PA, and team differentials sum to zero.',
'- Raw totals and averages are unadjusted; the separate SRS table adjusts average margin for opponent strength.','']
(OUT/'team-statistics-2026.md').write_text('\n'.join(report))

INK=HexColor('#19392e');MUTED=HexColor('#59645c');LINE=HexColor('#cbd2c9');PALE=HexColor('#edf2e9')
pdf=ROOT/'output/pdf/ddc-2026-team-statistics.pdf'
c=canvas.Canvas(str(pdf),pagesize=(792,612));c.setTitle('2026 US DDC Championship - Team Statistics');c.setAuthor('US DDC Championship 2026')
def label(x,y,text,size=10,bold=False,color=INK):
    c.setFillColor(color);c.setFont('Helvetica-Bold' if bold else 'Helvetica',size);c.drawString(x,y,str(text))
def header(title,subtitle,page):
    label(28,578,'US DDC CHAMPIONSHIP 2026',9,True);label(28,550,title,23,True);label(28,530,subtitle,10,color=MUTED)
    c.setStrokeColor(INK);c.setLineWidth(1);c.line(28,516,764,516)
    c.setStrokeColor(LINE);c.setLineWidth(.5);c.line(28,36,764,36)
    label(28,21,'Tallahassee, Florida | November 7-8, 2026 | Final tournament statistics',8,color=MUTED)
    label(715,21,f'{page} of 3',8,color=MUTED)
def table(widths,headings,values):
    assert sum(widths)==736
    edges=[28]
    for width in widths:edges.append(edges[-1]+width)
    top=497;height=27
    c.setFillColor(INK);c.rect(28,top-27,736,27,fill=1,stroke=0)
    for i,value in enumerate(headings):label(edges[i]+5,top-17,value,8,True,white)
    y=top-27
    for j,row in enumerate(values):
        if j==0:c.setFillColor(PALE);c.rect(28,y-height,736,height,fill=1,stroke=0)
        c.setStrokeColor(LINE);c.setLineWidth(.4);c.line(28,y-height,764,y-height)
        for i,value in enumerate(row):
            # Keep the longest player names inside the team column at print size.
            label(edges[i]+5,y-17,value,9,j==0)
        y-=height
header('Team statistics','All stages combined | 53 individual games across 52 matches | Sorted by final place',1)
widths=[28,224,30,28,28,50,44,44,50,50,50,54,56]
values=[(r['place'],r['team'],r['games'],r['wins'],r['losses'],f"{r['win_pct']:.1%}",r['points_for'],r['points_against'],f"{r['point_diff']:+}",f"{r['avg_points_for']:.1f}",f"{r['avg_points_against']:.1f}",f"{r['avg_margin']:+.1f}",f"{r['point_share']:.1%}") for r in rows]
table(widths,['Place','Team / players','GP','W','L','Win %','PF','PA','Diff.','PF/GP','PA/GP','Net/GP','Pts %'],values)
for y,line in zip([98,82,66,50],[
'GP = individual games played; W/L = game wins/losses. Both championship games count; byes do not.',
'PF/PA = points for/against; Diff. = PF - PA; Net/GP = average point differential per game.',
'Pts % = PF / (PF + PA). All points include pool, bracket and placement play.',
'Teams played different numbers of games and different opponents; averages do not adjust for opponent strength.'
]):label(28,y,line,8.5,color=MUTED)
c.showPage()
header('Records by stage','Game records, match records, and performance in competitive finishes',2)
values=[(r['place'],r['team'],wl(r['stages']['Pool play']),wl(r['stages']['Double elimination']),wl(r['stages']['Placement']),f"{r['match_wins']}-{r['match_losses']}",wl(r['close_record']),wl(r['extended_record'])) for r in rows]
table([28,202,70,80,80,90,80,106],['Place','Team / players','Pool W-L','Bracket W-L','Place W-L','Match W-L','Close W-L','Extended W-L'],values)
for y,line in zip([98,82,66,50],[
'Pool, Bracket and Place W-L count individual games. Bracket includes the championship; Place means placement games.',
'Match W-L counts the championship series as one match. A 0-0 stage record means no games in that stage.',
'Close = final margin of 2 or 3 points. Extended = both teams reached at least 20; a final double alone does not qualify.',
'Checks passed: game wins = losses = 53; match wins = losses = 52; total points for = total points against.'
]):label(28,y,line,8.5,color=MUTED)
c.showPage()
header('Schedule-adjusted ratings','All 53 individual games | Sorted by SRS | Official final places shown separately',3)
values=[(r['srs_rank'],r['team'],r['place'],r['games'],wl(r),f"{r['avg_margin']:+.2f}",f"{r['schedule_strength']:+.2f}",f"{r['srs']:+.2f}") for r in rating_order]
table([50,240,65,45,65,85,100,86],['SRS rank','Team / players','Final place','GP','W-L','Net/GP','Schedule','SRS'],values)
for y,line in zip([98,82,66,50],[
'SRS = Net/GP + Schedule. Schedule is opponents\' average SRS, counting each game; higher means tougher opposition.',
'Least-squares margin ratings; team average = 0. All games weighted equally; no margin cap or seed inputs.',
'SRS estimates points/game above an average team. Small samples and large margins affect ratings; official places are unchanged.',
'Method: Sports Reference SRS explainer | faq.sports-reference.com/portal/en/kb/articles/srs-explainer'
]):label(28,y,line,8.5,color=MUTED)
c.linkURL('https://faq.sports-reference.com/portal/en/kb/articles/srs-explainer',(28,46,764,61),relative=0)
c.showPage();c.save()
r=PdfReader(pdf);assert len(r.pages)==3
for p in r.pages:
    text=p.extract_text()
    for team in teams.values():assert team['team'] in text
print('All 13 teams verified on all three PDF pages; game, match, point and SRS checks passed.')
for r in rows:print(f"{r['place']:2} {r['team']:<23} {wl(r):>4}  PF {r['points_for']:3} PA {r['points_against']:3} Diff {r['point_diff']:+4} Net/game {r['avg_margin']:+5.1f} Match {r['match_wins']}-{r['match_losses']}")
print('Total points:',sum(r['points_for'] for r in rows))
for r in rating_order:print(f"SRS rank {r['srs_rank']:2} {r['team']:<23} Schedule {r['schedule_strength']:+6.2f} SRS {r['srs']:+6.2f}")

from pyquery import PyQuery as pq
#from collections import namedtuple
import sqlite3
import os

#Paper = namedtuple("Paper", "board module name qp_link ms_link")

d = pq(url="http://www.physicsandmathstutor.com/a-level-maths-papers/")
a = d(".one_fourth > ul > li > a, .one_fourth_last > ul > li > a")

def gmb(s):
	url = s.attrib['href']
	module, board, *r = url.split("/")[-2].split("-")
	if board not in ('by', 'solutionbank', 'level') and module != 'cie' and len(r) == 0:
		return (board, module)

print("> Getting Papers")
ps = []
for (board, module) in set(x for x in map(gmb, a) if x is not None):
	d = pq(url="http://www.physicsandmathstutor.com/a-level-maths-papers/%s-%s/" % (module, board))
	bps = []
	for s in d(".files > li > a"):
		ft = s.text_content().split(" - ")[0]
		if ft[-2:] not in ('QP', 'MS'): continue
		bps.append((board, module, ft[:-3], ft[-2:], s.attrib['href']))
	names = list(set([n for (_, _, n, _, _) in bps]))
	for name in names:
		try:
			a, b = [x for x in bps if x[2] == name]
			if a[3] == 'QP':
				ps.append((a[0], a[1], a[2], a[4], b[4]))
			else:
				ps.append((a[0], a[1], a[2], b[4], a[4]))
		except:
			continue
	print("done", board, module)
print("> Done")

print("> Updating Database")
os.remove("papers.db")
conn = sqlite3.connect("papers.db")
c = conn.cursor()
c.execute("create table papers (board text, module text, name text, qp_link text, ms_link text)")
c.executemany('insert into papers values (?,?,?,?,?)', ps)
conn.commit()
conn.close()
print("> Done")
import subprocess
import zmq
import json
import random
import blessed
import numpy as np
import numpy.linalg as l
from numpy.linalg import LinAlgError
from scipy import stats
import math

term = blessed.Terminal(force_styling=True)

class AMERepl:
	def __init__(self, ctx):
		self.sock = ctx.socket(zmq.REQ)
		self.proc = None

	def start(self):
		self.proc = subprocess.Popen(["ame-repl", "tcp://*:8080"], stdout=subprocess.DEVNULL)
		self.sock.connect("tcp://127.0.0.1:8080")

	def stop(self):
		self.sock.close()
		self.proc.kill()

	def submit(self, s):
		self.sock.send(s.encode())
		return json.loads(self.sock.recv().decode())

def random_matrix(n=None):
	if n is None:
		n = random.randrange(2, 5)
	ls = [[random.randrange(1, 10) for _ in range(n)] for _ in range(n)]
	p = '[' + ", ".join([str(l) for l in ls]) + ']'
	return ls, p

def random_vector(n=None):
	if n is None:
		n = random.randrange(2, 5)
	ls = [random.randrange(1, 10) for _ in range(n)]
	p = '[' + ", ".join(map(str, ls)) + ']'
	return ls, p

def random_poly(n=5):
	rl = [random.randrange(1, 100) for _ in range(random.randrange(1, n))]
	p = " * ".join("(x - %s)" % i for i in rl)
	return rl, p

def random_table1():
	rl = [random.randrange(10) for _ in range(random.randrange(10, 20))]
	p = str(rl)
	return rl, p

def random_table2():
	rl1 = [random.randrange(1, 10) for _ in range(random.randrange(10, 20))]
	p1 = str(rl1)
	rl2 = [random.randrange(1, 10) for _ in range(len(rl1))]
	p2 = str(rl2)
	return rl1, rl2, p1, p2

class MatrixDeterminant:
	name = "Matrix Determinants"

	def generate(self, i):
		ls, p = random_matrix()
		l1 = "determinant of " + p
		return (ls, [l1])

	def check(self, ls, v):
		assert(v['type'] == 'value')
		assert(abs(l.det(ls) - float(v['value'])) < 0.00001)

class MatrixMultiply:
	name = "Matrix Multiplication"

	def generate(self, i):
		ls, p = random_matrix()
		ls2, p2 = random_matrix(n=len(ls))
		l1 = p + " * " + p2
		return ([ls, ls2], [l1])

	def check(self, lss, v):
		assert(v['type'] == 'value')
		out = np.matrix(lss[0]) * np.matrix(lss[1])
		v['value'] = [[int(float(f)) for f in s.split("&")] for s in v['value'][15:-13].split("\\\\")]
		assert(all(all(a == b for (a, b) in zip(ra, rb)) for (ra, rb) in zip(v['value'], out.tolist()))) 

class MatrixInverse:
	name = "Matrix Inverses"

	def generate(self, i):
		ls, p = random_matrix()
		l1 = "the inverse of " + p
		return (ls, [l1])

	def check(self, ls, v):
		try:
			#try and compute the inverse matrix
			out = l.inv(ls)
			#if we can, then we should have got a value
			assert(v['type'] == 'value')
			#parse the latex source code.
			v['value'] = [[float(f) for f in s.split("&")] for s in v['value'][15:-13].split("\\\\")]
			#check all the values are close to the predicted ones
			assert(all(all(abs(a - b) < 0.0001 for (a, b) in zip(ra, rb)) for (ra, rb) in zip(v['value'], out.tolist())))
		except LinAlgError:
			#if the matrix is not invertible, we should have got an error.
			assert(v['type'] == 'error')

class SimulSolve:
	name = "Linear Simultaneous Equations"

	def generate(self, i):
		#generate a coefficient matrix
		ls, _ = random_matrix()
		#and an answer vector
		rs, _ = random_vector(len(ls))
		v = "abcdefghijklmnopqrstuv"
		p = []
		for l, r in zip(ls, rs):
			#using the alphabet as variables names
			#generate one equation from a row of the coefficient matrix
			p.append(" + ".join("%s * %s" % (a, b) for (a, b) in zip(l, v)) + " = " + str(r))
		l2 = ["let %s = a variable %s" % (x, x) for x in v[:len(rs)]]
		l1 = "solve { " + "; ".join(p) + " }"
		return ([ls, rs], l2 + [l1])

	def check(self, lss, v):
		ls, rs = lss
		try:
			#try and solve the equations
			ss = np.linalg.solve(np.array(ls), np.array(rs)).tolist()
			#if we can, then we should hae a list of solutions
			assert(v['type'] == 'list')
			for r, l in zip(ss, v['values']):
				#check that each solution is close to the predicted solution
				assert(abs(float(l) - r) < 0.001)
		except LinAlgError:
			#if the system could not be solved, we expect an error.
			assert(v['type'] == 'error')

class PolySolve:
	name = "Solver"

	def generate(self, i):
		rl, p = random_poly()
		l1 = "let f(x) = " + p
		l2 = "solve f = 0"
		return (list(set(rl)), [l1, l2])

	def check(self, rl, r):
		#polynomials may have one solution or more solutions
		#so we need both lists and values.
		assert(r['type'] in ('list', 'value'))
		rl.sort()
		#if its a value
		if r['type'] == 'value':
			#wrap it in a list so we dont have to worry about it
			r['values'] = [r['value']]
		#convert all the solutions to floats
		r['values'] = [float(v) for v in r['values']]
		r['values'].sort()
		#there should be as many solutions as there are were roots initially
		assert(len(rl) == len(r['values']))
		for v in r['values']:
			#check that each solution is close to one of the roots.
			assert(any(abs(v - r) < 10**(-3) for r in rl))

class PolySub:
	name = "Evaluation & Substitution"

	def generate(self, i):
		rl, p = random_poly()
		k = random.randrange(10)
		l1 = "let f(x) = " + p
		l2 = "evaluate (substitute x = " + str(k) + " in f)"
		return ([rl, k], [l1, l2])

	def check(self, rls, v):
		rl, k = rls
		#we should get a value
		assert(v['type'] == 'value')
		#parse it
		r = float(v['value'])
		nr = 1
		#and then compute the substitution ourselves
		for l in rl:
			nr *= (k - l)
		#and then check that they are close.
		assert(abs(r - nr) < 0.01)


class PolyIntDiff:
	name = "Integration & Differentiation"

	def generate(self, i):
		rl, p = random_poly(4)
		rl.append(0)
		l1 = "let f(x) = x * " + p
		#integrating and differentiating a function with no constant part
		#should get us back the same function
		l2 = "let g = integrate (differentiate f)"
		#so solving it should yield the same solutions
		l3 = "solve g = 0"
		return (rl, [l1, l2, l3])

	def check(self, rl, r):
		#this is just the same as the PolySolver checker
		assert(r['type'] in ('list', 'value'))
		rl.sort()
		if r['type'] == 'value':
			r['values'] = [r['value']]
		r['values'] = [round(float(v)) for v in r['values']]
		r['values'].sort()
		for v in r['values']:
			assert(any(v == r for r in rl))

class StatVar:
	name = "(Sample) Variance & Standard Deviation"

	def generate(self, i):
		rl, p  = random_table1()
		l1 = "let t = a table where x = " + p
		l2 = "the variance of x in t"
		l3 = "the standard deviation of x in t"
		l4 = "the sample variance of x in t"
		l5 = "the sample standard deviation of x in t"
		return (rl, [l1, [l2, l3, l4, l5]])

	def check(self, rl, vs):
		vv, sv, svv, ssv = vs
		assert(vv['type'] == 'value')
		r = float(vv['value'])
		#compute the actual variance
		nr = np.var(rl)
		#check it against the measured value
		assert(abs(r - nr) < 0.01)
		#repeat this for the other operations.
		assert(sv['type'] == 'value')
		r = float(sv['value'])
		nr = np.std(rl)
		assert(abs(r - nr) < 0.01)
		assert(svv['type'] == 'value')
		r = float(svv['value'])
		nr = np.var(rl, ddof=1)
		assert(abs(r - nr) < 0.01)
		assert(ssv['type'] == 'value')
		r = float(ssv['value'])
		nr = np.std(rl, ddof=1)
		assert(abs(r - nr) < 0.01)

#This is very similar to StatVar
class StatMean:
	name = "Mean, Median & Mode"

	def generate(self, i):
		rl, p = random_table1()
		l1 = "let t = a table where x = " + p
		l2 = "the mean of x in t"
		l3 = "the median of x in t"
		l4 = "the mode of x in t"
		return (rl, [l1, [l2, l3, l4]])

	def check(self, rl, vs):
		mean, median, mode = vs
		assert(mean['type'] == 'value')
		r = float(mean['value'])
		nr = np.mean(rl)
		assert(abs(r - nr) < 0.01)
		assert(median['type'] == 'value')
		r = float(median['value'])
		nr = np.median(rl)
		assert(abs(r - nr) < 0.01)
		assert(mode['type'] == 'value')
		r = float(mode['value'])
		rl.sort(reverse=True)
		nr = sorted(rl, key=rl.count)[-1]
		assert(abs(r - nr) < 0.01)

#And this is similar to StatMean
class StatCorrelation:
	name = "Chi Squared, Pearson/Spearman Correlation"

	def generate(self, i):
		rla, rlb, p1, p2 = random_table2()
		l1 = "let t = a table where x = " + p1 + "; y = " + p2
		l2 = "the chi squared value of x and y in t"
		l3 = "the pearson correlation of x and y in t"
		l4 = "the spearman correlation of x and y in t"
		return ([rla, rlb], [l1, [l2, l3, l4]])

	def check(self, rls, vs):
		rla, rlb = rls
		chi, pearson, spearman = vs
		assert(pearson['type'] == 'value')
		r = float(pearson['value'])
		nr = stats.pearsonr(rla, rlb)[0]
		assert(abs(r - nr) < 0.01)
		assert(spearman['type'] == 'value')
		#except we have to calculate spearmans r ourself
		#since the numpy algorithm uses fancy tie-breaking logic
		#that we shouldnt use.
		r = float(spearman['value'])
		rrla = sorted(rla)
		rrlb = sorted(rlb)
		nr = stats.pearsonr([rrla.index(r) for r in rla], [rrlb.index(r) for r in rlb])[0]
		assert(abs(r - nr) < 0.01)
		assert(chi['type'] == 'value')
		r = float(chi['value'])
		nr = stats.chisquare(rla, rlb).statistic
		assert(abs(r - nr) < 0.01)

class SumMul:
	name = "Sums & Products"

	def generate(self, i):
		rl, _ = random_vector()
		l1 = " + ".join(map(str, rl))
		l2 = " * ".join(map(str, rl))
		return (rl, [[l1, l2]])

	def check(self, rl, vs):
		p, m = vs
		assert(p['type'] == 'value')
		r = float(p['value'])
		nr = sum(rl)
		assert(abs(r - nr) < 0.01)
		assert(m['type'] == 'value')
		r = float(m['value'])
		nr = 1
		for l in rl:
			nr *= l
		assert(abs(r - nr) < 0.01)

class SubDivPow:
	name = "Subtraction, Exponentiation & Division"

	def generate(self, i):
		a, b = random.sample(range(1, 20), 2)
		l1 = "%s - %s" % (a, b)
		l2 = "%s / %s" % (a, b)
		l3 = "%s ^ %s" % (a, b)
		return ([a, b], [[l1, l2, l3]])

	def check(self, ab, vs):
		s, d, p = vs
		a, b = ab
		#standard by now.. compute the value ourself
		#and assert that its close to the measure value...
		assert(s['type'] == 'value')
		r = float(s['value'])
		nr = a - b
		assert(abs(r - nr) < 0.01)
		assert(d['type'] == 'value')
		r = float(d['value'])
		nr = a / b
		assert(abs(r - nr) < 0.01)
		assert(p['type'] == 'value')
		r = float(p['value'])
		nr = a ** b
		assert(100 * abs(r - nr)/r < 1)

class Trig:
	name = "Sin, Cos & Tan"

	def generate(self, i):
		a = (math.pi/2)*(random.random()*2 - 1)
		l1 = "sin(%s)" % a
		l2 = "cos(%s)" % a
		l3 = "tan(%s)" % a
		return (a, [[l1, l2, l3]])

	def check(self, a, vs):
		s, c, t = vs
		assert(s['type'] == 'value')
		r = float(s['value'])
		nr = math.sin(a)
		assert(abs(r - nr) < 0.01)
		assert(c['type'] == 'value')
		r = float(c['value'])
		nr = math.cos(a)
		assert(abs(r - nr) < 0.01)
		assert(t['type'] == 'value')
		r = float(t['value'])
		nr = math.tan(a)
		assert(abs(r - nr) < 0.01)

class InvTrig:
	name = "Inverse Sin, Cos & Tan"

	def generate(self, i):
		a = random.random() * 2 - 1
		l1 = "asin(%s)" % a
		l2 = "acos(%s)" % a
		l3 = "atan(%s)" % a
		return (a, [[l1, l2, l3]])

	def check(self, a, vs):
		s, c, t = vs
		assert(s['type'] == 'value')
		r = float(s['value'])
		nr = math.asin(a)
		assert(abs(r - nr) < 0.01)
		assert(c['type'] == 'value')
		r = float(c['value'])
		nr = math.acos(a)
		assert(abs(r - nr) < 0.01)
		assert(t['type'] == 'value')
		r = float(t['value'])
		nr = math.atan(a)
		assert(abs(r - nr) < 0.01)

class LogSqrt:
	name = "Log & Sqrt"

	def generate(self, i):
		a = 10*random.random()
		l1 = "log(%s)" % a
		l2 = "sqrt(%s)" % a
		return (a, [[l1, l2]])

	def check(self, a, vs):
		l, s = vs
		assert(l['type'] == 'value')
		r = float(l['value'])
		nr = math.log(a)
		assert(abs(r - nr) < 0.01)
		assert(s['type'] == 'value')
		r = float(s['value'])
		nr = math.sqrt(a)
		assert(abs(r - nr) < 0.01)

class AbsExp:
	name = "Abs & Exp"

	def generate(self, i):
		a = random.random() * 2 - 1
		l1 = "exp(%s)" % a
		l2 = "|%s|" % a
		return (a, [[l1, l2]])

	def check(self, a, vs):
		e, b = vs
		assert(e['type'] == 'value')
		r = float(e['value'])
		nr = math.exp(a)
		assert(abs(r - nr) < 0.01)
		assert(b['type'] == 'value')
		r = float(b['value'])
		nr = abs(a)
		assert(abs(r - nr) < 0.01)

#This is the framework that holds all the tests together and produces
#a nice colourful report.
class Tester:
	def __init__(self, *t):
		self.ts = t
		#start up a server
		self.r = AMERepl(zmq.Context())

	def test(self, n=100):
		self.r.start()
		tf = 0
		ts = 0
		#for each test that we are performing:
		for t in self.ts:
			print(term.bright_white("\nTesting: " + t.name))
			f = 0
			#repeat n times (usually 100)
			for i in range(n):
				#generate a random test case
				s, ls = t.generate(i)
				#occasionally print how many we've generated
				if (i + 1) % 200 == 0:
					print(term.blue("Generated %s tests." % (i + 1)))
				out = None
				#for every command that we provide
				for l in ls:
					if isinstance(l, list):
						#if it is a list, submit every command
						#in the list
						out = [self.r.submit(ll) for ll in l]
					else:
						#otherwise just submit the whole thing
						out = self.r.submit(l)
				try:
					#try and verify the output against out input
					t.check(s, out)
				except Exception as e:
					f += 1
					#but if it doesnt work increase the failure count and report it.
					print(term.bright_red("Failed test: %s" % s))
					print(term.bright_red(str(e)))
			else:
				#when we're done with the repetitions, 
				#print how many we failed and passed and our ratios.
				print(term.bright_green("Passed %s tests." % (n - f)))
				if f > 0:
					print(term.bright_red("Failed %s tests." % f))
				print(term.bright_white("Overall success rate: %s%%" % round((n - f) / n * 100, 3)))
				#then update the total failure and success counts.
				tf += f
				ts += n - f
		#when we're done with all the tests, stop the server and print the final statistics
		self.r.stop()
		print(term.bright_green("\nPassed %s tests total." % ts))
		print(term.bright_red("Failed %s tests total." % tf))
		print(term.bright_white("Overall success rate: %s%%" % round(ts / (ts + tf) * 100, 3)))

#Our default selection of tests is 1000 cases of
#every test.
def test(n=1000):
	Tester(MatrixDeterminant(), 
		   MatrixMultiply(), 
		   MatrixInverse(), 
		   SimulSolve(), 
		   PolySolve(), 
		   PolySub(),
		   PolyIntDiff(), 
		   StatMean(), 
		   StatVar(), 
		   StatCorrelation(),
		   SumMul(),
		   SubDivPow(),
		   Trig(),
		   InvTrig(),
		   LogSqrt(),
		   AbsExp()).test(n)

#if we are executing this as python3 test.py instead of
#importing it as a library, then execute the
#default tests.
if __name__ == "__main__":
	test()

import child
import robot
import time
from random import randint


di = {
	'left': -1,
	'right': 1,
	'up': 0,
	'down': 0
}

dj = {
	'left': -1,
	'right': 1,
	'up': 0,
	'down': 0
}

def printEnviroment(env):
	for i in range(N):
		for j in range(M):
			if env[i][j] == '':
				print(". ", end=" ")
			else:
				print(env[i][j] + " ", end=" ")
		print("")


class Simulator:
	env = []
	N = -1
	M = -1
	robotCarrying = False

	def random_empty_block(self):
		x, y = (randint(0, self.N-1), randint(0, self.M-1))
		while self.env[x][y] != '':
			x, y = (randint(0, self.N-1), randint(0, self.M-1))
		return (x, y)

	def __init__(self, N, M, dirtPercent, childCount, t):
		self.N = N
		self.M = M
		self.env = [["" for _ in range(M)] for _ in range(N)]
		self.changeEnviroment(dirtPercent, childCount)


	def changeEnviroment(self, dirtPercent, childCount):
		# TODO: generar nuevo ambiente
		self.env[0][3] = self.env[0][4] = self.env[1][4] = self.env[1][3] = 'C'

		for _ in range(6):
			x, y = self.random_empty_block()
			self.env[x][y] = 'O'

		for _ in range(childCount):
			x, y = self.random_empty_block()
			self.env[x][y] = 'N'

		for _ in range(4):
			x, y = self.random_empty_block()
			self.env[x][y] = 'S'

		x, y = self.random_empty_block()
		self.env[x][y] = 'R'


	def findObjs(self, obj):
		r = []
		for i in range(self.N):
			for j in range(self.M):
				if self.env[i][j] == obj:
					r.append((i,j))
		return r

	def simulate(self, n):
		for _ in range(n):
			while True:
				s = ""
				s = list(robot.move(self.findObjs('R')[0], self))[0]['Move'][0]
				print(str(s))
				time.sleep(2)
			pass

N = 8
M = 8
a = Simulator(N, M, 0, 4, 0)
printEnviroment(a.env)
a.simulate(1000)
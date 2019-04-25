import child
import robot
import time
from random import randint


di = {
	'left': 0,
	'right':0,
	'up': -1,
	'down': 1,
	'downleft': 1,
	'downright': 1,
	'upleft': -1,
	'upright': -1
}

dj = {
	'left': -1,
	'right': 1,
	'up': 0,
	'down': 0,
	'downleft': -1,
	'downright': 1,
	'upleft': -1,
	'upright': 1
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
	oldObj = ''
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
		self.env[x][y] = '#'


	def findObjs(self, obj):
		r = []
		for i in range(self.N):
			for j in range(self.M):
				if self.env[i][j] == obj or ((obj == 'N' or obj == 'C') and self.env[i][j] == '~'):
					r.append((i,j))
		if self.oldObj != 'N' and self.oldObj == obj:
			r.append(self.findObjs('#')[0])
		return r

	def move_robot(self, moves):
		for move in moves:
			x, y = self.findObjs('#')[0]
			if move == "drop":
				self.oldObj = '~'
				self.robotCarrying = False
			elif move == 'clean':
				if self.robotCarrying:
					raise Exception("Cant clean while carrying a child !!")
				self.oldObj = ''
			else:
				self.env[x][y] = self.oldObj
				self.oldObj = self.env[x+di[move]][y+dj[move]]
				if self.env[x+di[move]][y+dj[move]] == 'N':
					self.robotCarrying = True
					self.oldObj = ''
				self.env[x+di[move]][y+dj[move]] = '#'


	def simulate(self, n):
		for _ in range(n):
			while True:
				print("=============================")
				printEnviroment(a.env)
				rt = list(robot.next_move(self.findObjs('#')[0], self))
				moves = list(map(lambda x:str(x), rt[0]['Moves']))
				self.move_robot(moves)
				print(moves)
				time.sleep(2)

N = 8
M = 8
a = Simulator(N, M, 0, 4, 0)
a.simulate(1000)
from pyswip import Prolog

def next_move(pos, simulator):
	x, y = pos
	N = simulator.N
	M = simulator.M

	pl = Prolog()
	pl.consult('robot_agent.pl')

	query = "next_move(({}, {}), {}, {}, {}, {}, {}, {}, {}, Move)".format(x, y, N, M, ("true" if simulator.robotCarrying else "false"), simulator.findObjs('S'), simulator.findObjs('N'), simulator.findObjs('O'), simulator.findObjs('C'))
	print(query)
	return pl.query(query)
from pyswip import Prolog

def move(pos, env):
	pl = Prolog()
	pl.consult('robot_agent')
	# TODO: mover el robot
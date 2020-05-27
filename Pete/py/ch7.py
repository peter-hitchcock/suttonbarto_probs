import random 
import collections

#from Modules import dyna
#from dyna import test
import numpy as np
import pandas as pd 

# class dyna:

#     def __init__(self):
#         self.name = name

#     def crit_test(self):
#         '''

#         '''

#         a = 5

#         print("a")

#         return a

# Examples 8.1:8.3
######################### INITIALIZATIONS ##############################
## World
# Create a flat maze representation for easier indexing
#flat_maze = maze.reshape(-1)
# start out with a flat representation
rows = 6
cols = 9

maze = np.arange(rows*cols)
#maze.reshape(rows, cols)
start = 50
occlusions = [i for i in range(37, 41)]
rew_loc = 8
world = [
        {"maze": maze},
        {"start": start}, 
        {"occlusions": occlusions}, 
        {"rew_loc": rew_loc},
]

## Agent params
gamma = .95
alpha = .1
actions = [8, -8, -1, 1] # in flat array indices  
policy = [.25]*4 # Randomly initialize policy 
agent_pars = [
    {"gamma": gamma},
    "alpha" :alpha}, 
    {"actions": actions}, 
    {"policy": policy},
]
###########################################################################
## Plan:
# 1. implement tabular dyna-q in above grid world
# 2. test out uncertainty altering q-value vs. at action selection 







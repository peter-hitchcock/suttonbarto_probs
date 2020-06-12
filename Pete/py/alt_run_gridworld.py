######################### TRYING TO USE CLASSES ###########################
import random
from importlib import reload as ir
import numpy as np

import Modules.utilities as utils
from Modules.rl import RL
######################### INITIALIZATIONS #################################
# Initialize world, agent, and control parameters 
world = {
    "flat_grid": utils.gen_grid_acts()["flat_grid"],
    "grid": utils.gen_grid_acts()["grid"],
    "START_STATE": 24,
    "GOAL_STATE": 49,
    "OCCLUSIONS": [7, 16, 25, 34, 43],
    "REW_LOCS": [49], 
}
agent_pars = {
    "GAMMA": .95, # discount
    "ALPHA": .1,  # learning rate
    "EPSILON": .1, # randomness in e-greedy 
    # Right, left, down, up
    "ACTIONS": utils.gen_grid_acts()["ACTIONS"],
    # How many times to simulate after one real-world transition 
    "sim_steps": 10,
}
ctl = {
    # Take this many steps of experience  
    "loop_until": 1e6,
}
state = world["START_STATE"]
######################### DEFINE AGENT ####################################
# Create instance of RL agent 
agent = RL(world, agent_pars, state)

# ###########################################################################
# Line 2 pseudocode: Start loop 
i = 1
terminal = 0
# Line 3 pseudocode: set state

# Lists to keep track of visited states and actions so can sample from these in
# model 
state_recorder = []
action_recorder = []
while i < ctl["loop_until"] and not terminal:

    RL.sel_act_egreedy()
    # .. (line 4 pseudocode) and selecting e-greedily 
    action = RL.sel_act_egreedy(Q_SA, agent["EPSILON"])









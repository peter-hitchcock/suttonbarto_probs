import random 
import collections
from importlib import reload as ir

import numpy as np
import pandas as pd 
import Modules.utilities as utils
from Modules.utilities import sel_act_egreedy, get_sprime_r
import Modules.dyna as dyna
from Modules.dyna import Dyna
######################### INITIALIZATIONS #################################
# Initialize world, agent, and control parameters 
world = {
    "flat_grid": utils.gen_grid_acts()["flat_grid"],
    "grid": utils.gen_grid_acts()["grid"],
    "start_state": 24,
    "goal_state": 49,
    "occlusions": [7, 16, 25, 34, 43],
    "rew_locs": [49], 
}
agent = {
    "GAMMA": .95, # discount
    "ALPHA": .1,  # learning rate
    "EPSILON": .1, # randomness in e-greedy 
    # Right, left, down, up
    "actions": utils.gen_grid_acts()["actions"],
    # How many times to simulate after one real-world transition 
    "sim_steps": 10,
}
control = {
    # Take this many steps of experience  
    "loop_until": 1e6,
}

# Line 1, tabular dyna-q pseudocode p. 164: Initialize Q(s,a) and Model(s,a)
# First create a base dataframe of state, action pairs  
sa_df = pd.DataFrame(
    {
    "state": np.repeat([world["flat_grid"]], len(agent["actions"])),
    "action": agent["actions"] * len(world["flat_grid"]),
    }
)
# Randomly initialize Q_values 
Q_vals = pd.DataFrame.copy(sa_df)
Q_vals["values"] = np.random.random(len(Q_vals))

# Define a model comprising the state action pairs.. 
model = pd.DataFrame.copy(sa_df)
# .. state transitions in this deterministic grid world.. 
# Start with an ignorant model that will then be built from experience 
model["s_prime"] = 0
model["reward"] = 0

# Define set of valid state transitions.. 
vst = set(world["flat_grid"]).difference(set(world["occlusions"]))
# .. and states with reward
reward_locations = set(world["rew_locs"])
###########################################################################
# Line 2 pseudocode: Start loop 
i = 1
terminal = 0
# Line 3 pseudocode: set state
state = world["start_state"]

while i < control["loop_until"] and not terminal:

    # Take action by extracting values for this state..
    Q_SA = Q_vals[Q_vals.state == state] 
    # .. (line 4 pseudocode) and selecting e-greedily 
    action = sel_act_egreedy(Q_SA, agent["EPSILON"])

    # (line 5 pseudocode) Transition state to s_prime and finds reward given 
    # above action, also find if state is terminal..
    state_trans_out = \
        get_sprime_r(state, action, vst, reward_locations, world["goal_state"])
    # .. unpacking the outputs 
    s_prime, terminal, reward = {**state_trans_out}.values()

    # Pseuduocode (d): Update q-value based on real experience  
    Q_vals = dyna.update_QSA(Q_vals, state, action, s_prime, reward, \
         agent["ALPHA"], agent["GAMMA"])

    # Pseudocode (e): Update model 
    model.loc[(model.state==state) & (model.action==action), ["reward", "s_prime"]] = \
        [state_trans_out.get(key) for key in ["s_prime", "reward"]]
    
    update_from_model(Q_SA, state, action, model_SA, control["sim_steps"])
    

    


    i += 1



###########################################################################
## Refactoring plan:  
# For now just create a dyna class. May eventually want to factor into parent 
# RL starter class and have subclasses for dyna etc. 

## Plan:
# 1. implement tabular dyna-q in above grid world
# 2. test out uncertainty altering q-value vs. at action selection 
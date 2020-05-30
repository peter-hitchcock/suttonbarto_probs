import random 
import collections

#from Modules import dyna
#from dyna import test
import numpy as np
import pandas as pd 

######################### INITIALIZATIONS ##############################
def create_grid_world_and_actions(rows=6, cols=9):
    ''' Define flat and matrix representations of grid world and actions
    in that world'''

    # Flat grid representation 
    flat_grid = np.arange(rows*cols)+1
    
    grids_actions = {
        "flat_grid": flat_grid,
        # Representation where illegal actions move off grid
        "grid": np.transpose(flat_grid.reshape(cols, rows)),
        # Right, left, up, down
        "actions": [rows, -rows, 1, -1],
    }

    return grids_actions

grids_actions = create_grid_world_and_actions()

world = {
    "flat_grid": grids_actions["flat_grid"],
    "grid": grids_actions["grid"],
    "start_state": 24,
    "goal_state": 49,
    "occlusions": [7, 16, 25, 34, 43],
    "rew_loc": 49, 
}

## Agent params
agent_pars = {
    "gamma": .95,
    "alpha": .1, 
    # Right, left, down, up
    "actions": grids_actions["actions"],
    "policy": [.25]*4, # Randomly initialize policy 
}

# Base dataframe of state, action pairs  
sa_df = pd.DataFrame(
    {
    "state": np.repeat([world["flat_grid"]], len(agent_pars["actions"])),
    "action": agent_pars["actions"] * len(world["flat_grid"]),
    }
)

# Define a model comprising the state action pairs.. 
model = sa_df
# .. state transitions in this deterministic grid world.. 
model["s_prime"] = model.sum(axis=1)
# (For any states outside the boundaries of the world, the state transitions to itself. 
# So first set to Nan..
model.loc[(model["s_prime"] < min(world["flat_grid"])), "s_prime"] = np.nan
model.loc[(model["s_prime"] > max(world["flat_grid"])), "s_prime"] = np.nan
# .. then replace with the same state)
model.loc[(model["s_prime"]).isnull(), "s_prime"] = model.loc[(model["s_prime"]).isnull(), "state"]
# .. and set initial reward, which is just 0 everywhere and 1 in the goal state 
model["reward"] = 0





###########################################################################
## Plan:
# 1. implement tabular dyna-q in above grid world
# 2. test out uncertainty altering q-value vs. at action selection 




#model.loc[model.index[world["goal_state"]], "reward"] = 1


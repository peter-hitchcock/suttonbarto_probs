import random 
import collections

import numpy as np
import pandas as pd 
######################### DEFINE HELPER FUNCTIONS ##############################
def create_grid_world_and_actions(rows=6, cols=9):
    ''' 
    Define flat and matrix representations of grid world and actions
    in that world
    '''
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

def select_action_egreedily(Q_SA, eps=.1):
    '''
    Select actions e-greedily considering SA values in Q_SA (the subset of Q_sa for this state)
    '''
    if random.random() > eps:
        action = Q_SA.loc[Q_SA.index[np.argmin(Q_SA["values"])-1], "action"]
    else:
        # Randomly pick one of the actions..
        n_actions = Q_SA.shape[1]+1
        # .. by generating a uniformly spaced sequence from 0:1 and taking the first action greater
        # than a random number from 0 to 1
        acts_greater = random.random() > np.cumsum([1/n_actions for i in range(n_actions)])
        action = Q_SA.loc[Q_SA.index[np.amax(np.where(acts_greater))], "action"]
    return action

grids_actions = create_grid_world_and_actions()

def update_QSA(*env_agent):
    '''Update state values matrix based on real or simulated experience'''
    # Find Q_SA before update
    old_Q_SA \
        = Q_vals.loc[(Q_vals.state == state) & (Q_vals.action == action), "values"]
    # and max_a Q(S', a)
    sp_values = Q_vals.loc[Q_vals.state == s_prime, "values"]

    max_Q_Sp_a = \
        sp_values.iloc[np.argmax(Q_vals.loc[Q_vals.state == s_prime, "values"])]
    
    # Update the s,a in the Q_vals df
    Q_vals.loc[(Q_vals.state == state) & (Q_vals.action == action), "values"] \
        = old_Q_SA + alpha * (reward + gamma * (max_Q_Sp_a - old_Q_SA))

    return Q_vals
###########################################################################
######################### INITIALIZATIONS #################################
# Initialize world, agent, and control parameters 
world = {
    "flat_grid": grids_actions["flat_grid"],
    "grid": grids_actions["grid"],
    "start_state": 24,
    "goal_state": 49,
    "occlusions": [7, 16, 25, 34, 43],
    "rew_locs": [49], 
}
agent = {
    "gamma": .95, # discount
    "alpha": .1,  # learning rate
    "epsilon": .1, # randomness in e-greedy 
    # Right, left, down, up
    "actions": grids_actions["actions"],
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
Q_vals= pd.DataFrame.copy(sa_df)
Q_vals["values"] = np.random.random(len(Q_vals))

# Define a model comprising the state action pairs.. 
model = pd.DataFrame.copy(sa_df)
# .. state transitions in this deterministic grid world.. 
model["s_prime"] = model.sum(axis=1)
# (For any states outside the boundaries of the world, the state transitions to itself. 
# So first set to Nan..
model.loc[(model["s_prime"] < min(world["flat_grid"])), "s_prime"] = np.nan
model.loc[(model["s_prime"] > max(world["flat_grid"])), "s_prime"] = np.nan
# .. then replace with the same state)
model.loc[(model["s_prime"]).isnull(), "s_prime"] = model.loc[(model["s_prime"]).isnull(), "state"]
# Also replace any s_primes that run into occlusions with the same state
model.loc[model.index[np.where(model["s_prime"].isin(world["occlusions"]))], "s_prime"] = \
    model.loc[model.index[np.where(model["s_prime"].isin(world["occlusions"]))], "state"]
# Set initial reward, which is just 0 everywhere and 1 in the goal state 
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

while i < control["loop_until"] and is not terminal:

    # Take action by extracting values for this state..
    Q_SA = Q_vals[Q_vals.state == state] 
    # .. and selecting e-greedily 
    action = select_action_egreedily(Q_SA, agent["epsilon"])

    # Observe reward and state transition  
    put_new_state = state + action
    # Check if this is a valid state transition and accordingly transition 
    # to new state or same state..
    s_prime = put_new_state if put_new_state in vst else state
    reward = 1 if state in reward_locations else 0
    # .. and check if the new state is terminal 
    if s_prime == world["goal_state"]: 
        terminal = 1

    # Pseuduocode (d): Update q-value based on real experience  
    Q_vals = update_QSA(Q_vals, state, action, s_prime, reward, alpha, gamma)
    
    # Pseudocode (e): Update model 

    # Pseudocode (f): Sample n steps s
    def name():
        '''
    
        '''
    
        body
    
        return
    


    i += 1



###########################################################################
## Plan:
# 1. implement tabular dyna-q in above grid world
# 2. test out uncertainty altering q-value vs. at action selection 

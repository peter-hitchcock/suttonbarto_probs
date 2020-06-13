import random 

import numpy as np
import pandas as pd 

# This is old version. now trying one where don't need explicit 
# representation of grid 
# def gen_grid_acts(HEIGHT=6, WIDTH=9):
#         ''' 
#         Returns a dict with diff representations of grid, the actions available 
#         in it, and size of both 
#         '''
#         # Flat grid representation 
#         flat_grid = np.arange(HEIGHT*WIDTH)+1
#         # Right, left, up, down
#         ACTIONS = [HEIGHT, -HEIGHT, 1, -1]
#         # For setting up q and model arrays
#         GRID_ACTION_SIZE = (len(ACTIONS), HEIGHT, WIDTH)

#         grid_setup = {
#             "flat_grid": flat_grid,
#             # Representation where illegal actions move off grid
#             "grid": np.transpose(flat_grid.reshape(HEIGHT, WIDTH)),
#             "ACTIONS": ACTIONS,
#             "GRID_ACTION_SIZE": GRID_ACTION_SIZE
#         }

#         return grid_setup

# def sel_act_egreedy(Q_SA, eps=.1):
#     '''
#     Select actions e-greedily considering values in Q_SA (the subset of Q_sa for this state)
#     '''
#     if random.random() > eps:
#         action = Q_SA.loc[Q_SA.index[np.argmax(Q_SA["values"])], "action"]
#     else:
#         action = Q_SA.loc[Q_SA.index[random.randint(1, 3)], "action"]
#     return action

# def get_sprime_r(state, action, vst, reward_locations, goal_state):
#     '''
#     Returns new state and whether it's terminal and the reward achieved in the transition
#     '''
#     put_new_state = state + action
#     # Check if this is a valid state transition and accordingly transition 
#     # to new state or same state..
#     s_prime = put_new_state if put_new_state in vst else state
#     reward = 1 if s_prime in reward_locations else 0
#     # .. and check if the new state is terminal 
#     terminal = 0 if s_prime == goal_state else 1
    
#     return {"s_prime": s_prime, "terminal": terminal, "reward": reward}

        
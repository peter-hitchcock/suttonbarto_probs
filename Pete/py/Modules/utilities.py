import random 
import collections

import numpy as np
import pandas as pd 

def gen_grid_acts(rows=6, cols=9):
        ''' 
        Define flat and matrix representations of grid world and actions
        available in that world
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

def sel_act_egreedy(Q_SA, eps=.1):
    '''
    Select actions e-greedily considering values in Q_SA (the subset of Q_sa for this state)
    '''
    if random.random() > eps:
        action = Q_SA.loc[Q_SA.index[np.argmax(Q_SA["values"])], "action"]
    else:
        Q_SA.loc[Q_SA.index[random.randint(1, 3)], "action"]
    return action

        
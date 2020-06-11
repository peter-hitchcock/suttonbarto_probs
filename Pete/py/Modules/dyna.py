import random

import numpy as np
import pandas as pd

def update_QSA(Q_vals, state, action, s_prime, 
               reward, ALPHA, GAMMA):
    '''Update state values matrix based on real or simulated experience'''
    # Find Q_SA before update

    old_Q_SA \
        = Q_vals.loc[(Q_vals.state == state) & (Q_vals.action == action), "values"]
    # and max_a Q(S', a)
    sp_values = Q_vals.loc[Q_vals.state == s_prime, "values"]

    max_Q_Sp_a \
        = sp_values.iloc[np.argmax(Q_vals.loc[Q_vals.state == s_prime, "values"])]
    
    # Update the s,a in the Q_vals df
    Q_vals.loc[(Q_vals.state == state) & (Q_vals.action == action), "values"] \
        = old_Q_SA + ALPHA * (reward + GAMMA * (max_Q_Sp_a - old_Q_SA))

    return Q_vals
    
class Dyna:
    
    def __init__(self, world, agent, control, Q_vals, model):
        self.world = dict(world)
        self.agent = dict(agent)
        self.control = dict(control)
        self.Q_vals = Q_vals
        self.model = model  

    def update_from_model(self, model, Q_SA, state, action, sim_steps):










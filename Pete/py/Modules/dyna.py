import random

import numpy as np
import pandas as pd

class Dyna:
    def __init__(self, world, agent, control, Q_vals):
        self.world = dict(world)
        self.agent = dict(agent)
        self.control = dict(control)
        self.Q_vals = Q_vals 

    def update_QSA(*env_agent):
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
            = old_Q_SA + alpha * (reward + gamma * (max_Q_Sp_a - old_Q_SA))

        return Q_vals







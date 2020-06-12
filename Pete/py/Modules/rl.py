import random
from . import utilities as utils

import pandas as pd
import numpy as np

class RL:
        def __init__(self, world, agent_pars, state):
            self.world = dict(world)
            self.agent_pars_pars = dict(agent_pars)
            self.state = state
            GRID_ACTION_SIZE = utils.gen_grid_acts()["GRID_ACTION_SIZE"]
            # Line 1, tabular dyna-q pseudocode p. 164: Initialize Q(s,a) and Model(s,a)
            self.Q_vals = np.random.random(GRID_ACTION_SIZE)
            self.model_sprime = np.zeros(GRID_ACTION_SIZE), np.zeros(GRID_ACTION_SIZE)
            # Define set of valid state transitions.. 
            self.vst = set(world["flat_grid"]) - (set(world["OCCLUSIONS"]))
            # .. and states with reward
            self.reward_locations = set(world["REW_LOCS"])
            #super().__init__(**kwargs)

        def sel_act_egreedy(self, state):
            '''
            Select actions e-greedily considering values in Q_SA (the subset of Q_sa for this state)
            '''
            eps = self.world["EPSILON"]
            Q_SA = self.Q_vals[self.Q_vals.state == state]
            
            if random.random() > eps:
                action = Q_SA.loc[Q_SA.index[np.argmax(Q_SA["values"])], "action"]
            else:
                action = Q_SA.loc[Q_SA.index[random.randint(1, 3)], "action"]
            return action

        def update_QSA(self):
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

class dyna(RL):
    def __init__(self, world, agent_pars, Q_vals, model):
        super().__init__(world=world, agent_pars=agent_pars)



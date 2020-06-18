import numpy as np
import random

class Grid:
    '''Contains everything needed to set up a grid world for your agent'''
    def __init__(self, HEIGHT=6, WIDTH=9):
        self.HEIGHT = HEIGHT
        self.WIDTH = WIDTH
        self.OCCLUSION_COLS = [i+2 for i in range(4)]
        self.OCCLUSION_ROWS = 3
        self.START_STATES = [[2, 1], [3, 1]]
        self.GOAL_STATES = [[8, 0]]

    def __str__(self):
        return '\n \n This is a {self.HEIGHT} by {self.WIDTH}' \
                ' world. \n \n'.format(self=self)    

    def display_world(self):
        '''Make a nicely formatted display of the world'''
        HEIGHT, WIDTH, OCCL_COLS, OCCL_ROWS = \
            self.HEIGHT, self.WIDTH, self.OCCLUSION_ROWS, self.OCCLUSION_COLS
        flat_repr = np.arange(Grid().HEIGHT*Grid().WIDTH, dtype=float)
        viewable_grid = flat_repr.reshape(Grid().HEIGHT, Grid().WIDTH)
        viewable_grid[OCCL_COLS, OCCL_ROWS] = np.nan
        print(viewable_grid)            

class RL_agent(Grid):
    '''Defines generic RL agent that will be inherited to create specific agents'''
    def __init__(self):
        self.ACTIONS = {
            "up": [0, 1],
            "down": [0, -1],
            "left": [-1, 0],
            "right": [1, 0],
        } 
        # Initialize empty array of q values  
        HEIGHT, WIDTH, ACTIONS = (Grid().HEIGHT, Grid().WIDTH, len(self.ACTIONS))
        self.q_values = np.zeros([ACTIONS, HEIGHT, WIDTH])
        # Set parameters for RL stuff 
        self.EPSILON = .1 # action selection greediness
        self.ALPHA = .1 # learning rate 
    
    def sel_egreedy_action(self, q_sa):
        '''Returns the maximal action with EPSILON probability, else random action'''
        # Find the indices where q_sa is max  
        #** not sure this'll work with current action repr because keys aren't indexed 
        max_inds = np.where(q_sa == np.max(q_sa))
        if random.random() self.EPSILON:
            
            if (len(max_inds) == 1):
                action = np.where(q_sa == np.max(q_sa))
            else:


        pass
        #return action
       
        
class Dyna_agent(RL_agent):
    def __init__(self): 
        HEIGHT, WIDTH, ACTIONS = (Grid().HEIGHT, Grid().WIDTH, len(RL_agent().ACTIONS))       
        self.s_prime = np.zeros([ACTIONS, HEIGHT, WIDTH])
        self.reward = np.zeros([ACTIONS, HEIGHT, WIDTH])

if __name__ == "__main__":
    print(str(Grid()))

    # Instantiate grid and RL agent
    grid = Grid()
    # View world 
    grid.display_world()
    ## Trying to run a single step..
    agent = RL_agent()
    # Start moving 
    starting_state = random.choice(grid.START_STATES)
    ss_row, ss_col = starting_state[0], starting_state[1]
    print(str("\n \n ----Starting in row {} column {}-------- \n \n". \
        format(ss_row, ss_col)))
    # Find the q-values for the actions in this state 
    q_sa = agent.q_values[:, ss_row, ss_col]
    #print(q_sa)
    RL_agent.sel_egreedy_action(agent, q_sa)
    
    




import random
import collections

import numpy as np

class Grid():
    '''Sets up everything needed for a grid world for your agent'''
    def __init__(self, HEIGHT=6, WIDTH=9, verbose=1):
        self.HEIGHT = HEIGHT
        self.WIDTH = WIDTH
        self.OCCLUSION_COLS = [i+2 for i in range(2)]
        self.OCCLUSION_ROWS = [3]
        self.START_STATES = [[2, 1], [3, 1]]
        self.TERMINAL = [[0, 8]]
        self.REW_LOCS = [[0, 8]]
        self._verbose = verbose

    def __str__(self):
        return '\n \n This is a {self.HEIGHT} by {self.WIDTH}' \
            ' world. \n \n'.format(self=self) 

    def display_world(self):
        if self._verbose:
            '''Show a nicely formatted display of the world'''
            HEIGHT, WIDTH, OCCL_COLS, OCCL_ROWS = \
                self.HEIGHT, self.WIDTH, self.OCCLUSION_ROWS, self.OCCLUSION_COLS
            flat_repr = np.arange(Grid().HEIGHT*Grid().WIDTH, dtype=float)
            viewable_grid = flat_repr.reshape(Grid().HEIGHT, Grid().WIDTH)
            viewable_grid[OCCL_COLS, OCCL_ROWS] = np.nan
            print(viewable_grid)            

class RL_agent(Grid):
    '''Defines generic RL agent that can be inherited to create specific agents'''
    def __init__(self):
        # Define a named tuple of actions with the type of action and
        # the move in row, col coords  
        _ACT_TUPE = collections.namedtuple("_ACT_TUPE", "type move")
        self.ACTIONS = [
                    _ACT_TUPE("up", np.array([0, 1])),
                    _ACT_TUPE("down", np.array([0, -1])),
                    _ACT_TUPE("left", np.array([-1, 0])),
                    _ACT_TUPE("right", np.array([1, 0])),
                ]
        # Initialize empty array of q values. Note this is indexed by height,
        # width, acitons so eg. [5, 5, :] will be the value of all actions for the state 
        # corresponding to the 5th row, 5th state
        HEIGHT, WIDTH, ACTIONS = (Grid().HEIGHT, Grid().WIDTH, len(self.ACTIONS))
        # Define q-value matrix. 
        self.q_values = np.zeros([HEIGHT, WIDTH, ACTIONS])
        # Set parameters for RL stuff 
        self.EPSILON = .1 # action selection greediness
        self.ALPHA = .1 # learning rate 
    
    def sel_egreedy_action(self, q_sa, ACTIONS):
        '''Returns the maximal action with EPSILON probability else random action'''
        if random.random() > self.EPSILON:
            # Find indices where q_sa is max and randomly select between them returning
            # just one max in case of 2+ equal q_sa's. Note [0] is used to untupify result. 
            max_ind = random.choice(np.where(q_sa == np.max(q_sa))[0])
            action = ACTIONS[max_ind]
        else:
            action = random.choice(ACTIONS)
        return action

    def eval_state_transition(self, move, state, HEIGHT, WIDTH, OCCLUSION_COLS, OCCLUSION_ROWS):
        '''Return s_prime based on whether this is a valid state transition'''
        # Find the putative new state
        pst = move + state
        # If the the new state doesn't exceed the boundaries of the grid..
        # .. and isn't too an occluded state ..
        if 0 <= pst[0] <= HEIGHT and 0 <= pst[1] <= WIDTH and \
                pst[0] not in OCCLUSION_ROWS or pst[1] not in OCCLUSION_COLS:
                # .. the putative state is in fact the new state.. 
                s_prime = pst
        else: 
            s_prime = state            
        return s_prime            
                
class Dyna_agent(RL_agent):
    def __init__(self): 
        HEIGHT, WIDTH, ACTIONS = (Grid().HEIGHT, Grid().WIDTH, len(RL_agent().ACTIONS))       
        self.s_prime = np.zeros([ACTIONS, HEIGHT, WIDTH])
        self.reward = np.zeros([ACTIONS, HEIGHT, WIDTH])

if __name__ == "__main__":
    print(str(Grid()))

    # Instantiate grid and RL agent
    grid = Grid()
    agent = RL_agent()

    # Display world and set up some variables
    grid.display_world() 
    ACTIONS = agent.ACTIONS
    REW_LOCS = grid.REW_LOCS
    terminal = 0
    
    # Start moving 
    starting_state = random.choice(grid.START_STATES)
    
    print(str("\n \n ----Starting in row {} column {}-------- \n \n". \
        format(starting_state[0], starting_state[1])))
    
    state = starting_state

    ## Trying to run a single step of iteration ..   
    while not terminal:     
        # Define current state in row, col indices        
              
        # Find the q-values for the actions in this state 
        q_sa = agent.q_values[starting_state[0], starting_state[1], :]

        # Select action  
        action = RL_agent.sel_egreedy_action(agent, q_sa, ACTIONS)
        print("Action:", *action)
        
        # Find s_prime and reward  
        s_prime = RL_agent.eval_state_transition(
            agent, action.move, state, 
            grid.HEIGHT, grid.WIDTH, 
            grid.OCCLUSION_COLS, grid.OCCLUSION_ROWS
        )
        
        reward = 1 if list(state) in REW_LOCS else 0
        print("S_prime:", s_prime, "\n Reward:", reward)
    
        # s_prime -> state and check if terminal
        state = s_prime
        if list(state) in grid.TERMINAL: 
            terminal = 1
            print("\n \n ---- Reached terminal state -------- \n \n")






        
    
    




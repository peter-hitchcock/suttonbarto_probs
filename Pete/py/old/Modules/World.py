class World:
    '''Basic setup of world'''
    def __init__(self):
        self.HEIGHT = 6
        self.WIDTH = 9
        # Up down middle right 
        self.UP = 0
        self.DOWN = 1
        self.LEFT = 2
        self.RIGHT = 3
        self.ACTIONS = [self.UP, self.DOWN, self.LEFT, self.RIGHT]
        self.GOAL_STATE = [8, 8]
        self.START_STATE = [5, 1]
        self.OCCLUSIONS = [[3, 2], [3, 3], [3, 4], [3, 5]]

        self.model_qsize = (len(self.ACTIONS), HEIGHT, WIDTH)

        
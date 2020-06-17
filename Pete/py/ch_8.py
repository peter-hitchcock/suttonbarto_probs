import numpy as np
import importlib
#from importlib import reload as ir

class Grid:
    def __init__(self, HEIGHT=6, WIDTH=9):
        self.HEIGHT = HEIGHT
        self.WIDTH = WIDTH
        self.OCCLUSION_COLS = [i+2 for i in range(4)]
        self.OCCLUSION_ROWS = 3
        self.START_STATES = [18, 27]
        self.GOAL_STATES = [8]

    def __str__(self):
        return 'This is a {self.HEIGHT} by {self.WIDTH}' \
                ' world.'.format(self=self)    

if __name__ == "__main__":
    print(str(Grid()))
    HEIGHT, WIDTH, OCCL_COLS, OCCL_ROWS = \
        Grid().HEIGHT, Grid().WIDTH, Grid().OCCLUSION_ROWS, Grid().OCCLUSION_COLS
    flat_repr = np.arange(Grid().HEIGHT*Grid().WIDTH, dtype=float)
    view_grid_tmp = flat_repr.reshape(Grid().HEIGHT, Grid().WIDTH)
    view_grid_tmp[OCCL_COLS, OCCL_ROWS] = np.nan
    print(view_grid_tmp)



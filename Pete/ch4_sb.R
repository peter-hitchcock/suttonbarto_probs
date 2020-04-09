lapply(c('tidyverse', 'dplyr'), require, character=TRUE)
sf <- function() sapply(paste0('./Functions/', list.files('./Functions/')), source)
sf()
DefPlotPars()
#### 4.9. Gambler's Problem ####
############################################
####  INITIALIZATIONS ####
theta <- 1e-5 # threshold param
gamma <- 1
states <- 1:101 # one state for 1:99 capital + 2 terminal states
p_win <- .15 # probability of winning 
# initalize state values randomly.. 
state_values <- runif(length(states), 0, 1)
#state_values <- rep(0, length(states))
# .. except for setting terminal states to 0 and 1
state_values[1] <- 0; state_values[101] <- 1
quiet <- 0 
keep_iter_ests <- 1
############################################
opt_policy_and_value_fx  <-  RunGamblersProblem(
                                            p_win, # probability of winning bet
                                            states, # = capital amounts
                                            state_values, # iteratively updating state values
                                            theta, # iteration stopping criterion
                                            gamma, # discount on V(s)'
                                            quiet, # print?
                                            keep_iter_ests # save estimates from all sweeps?
                                          ) 

# plots for final value and policy estimates
if (!keep_iter_ests) {
  ggplot(opt_policy_and_value_fx, aes(x=states, y=state_values)) + 
    geom_line(size=4, color='gray57') + 
    ga + ap +
    ylab('state values')
  
  ggplot(opt_policy_and_value_fx[[23]], aes(x=states, y=optimal_policy)) + 
    geom_point(size=4, alpha=.5) +
    ga + ap +
    ylab('optimal policy (amt. to stake)')
} 
if (keep_iter_ests) {
  
  ov_df <- opt_policy_and_value_fx %>% bind_rows()
  
  ggplot(ov_df, aes(x=states, y=state_values, color=as.factor(sweep))) + 
    geom_line(size=2, alpha=.9) + 
    ga + ap + 
    theme(legend.text = element_text(size = 8),
          legend.title = element_blank(),
          legend.key.size = unit(.8, 'lines')) +
    ylab('state values')
  
  ggplot(ov_df %>% filter(sweep %in% 94), aes(x=states, y=optimal_policy, color=as.factor(sweep))) + 
    geom_jitter(size=4, alpha=.6, width=.8, height=.8) +
    ga + ap + 
    theme(legend.text = element_text(size = 14),
          legend.title = element_blank(),
          legend.key.size = unit(2, 'lines')) +
    ylab('optimal policy (amt. to stake)') + ylim(0, 100)
}

## currently has a bug: 
# - possible reasons why: 
# not implementing lack of transitions at terminal state loses (instead all 
# info from win is just propagating back)
# not sure if v' estimates are done correctly 





















#### 4.1. (** to finish) ####
############################################
#### HELPER FUNCTIONS ####
# TransitionStates <- function(action, grid, current_state) {
#   ### Returns new state indices and reward gained #
#   
#   # ** think this is wrong and instead it takes in a specific action
#   ## Generate a random action index (1, 2, 3, or 4)..
#   #rand_action_index <- round(runif(1, 1, 4))
#   # .. and select an action, given in terms of row and col indices to move
#   #action <- as[rand_action_index]
#   
#   browser()
#   ## Evaluate if the transition is 1) in the grid, 2) terminal state (NA), 
#   # or 3) off the grid 
#   # index for the new candidate state
#   poss_new_state <- current_state + as.numeric(unlist(action))
#   
#   # no state transition if the action moves off the grid
#   if (!poss_new_state[1] %in% 1:nrow(grid) & poss_new_state[2] %in% 1:ncol(grid)) {
#     new_state <- NA
#     reward <- 0
#     # if the new state is a terminal state  
#   } else if (is.na(grid[poss_new_state[1], poss_new_state[2]])) {
#     new_state <- 'terminal'
#     reward <- -1
#   } else {
#     new_state <- poss_new_state 
#     reward <- -1
#   }
#   
#   list(new_state, reward)  
# }
# # TransitionStates <- function(action, grid, current_state) {
# #   ### Returns new state indices and reward gained #
# #   
# #   # ** think this is wrong and instead it takes in a specific action
# #   ## Generate a random action index (1, 2, 3, or 4)..
# #   #rand_action_index <- round(runif(1, 1, 4))
# #   # .. and select an action, given in terms of row and col indices to move
# #   #action <- as[rand_action_index]
# #   
# #   browser()
# #   ## Evaluate if the transition is 1) in the grid, 2) terminal state (NA), 
# #   # or 3) off the grid 
# #   # index for the new candidate state
# #   poss_new_state <- current_state + as.numeric(unlist(action))
# #   
# #   # no state transition if the action moves off the grid
# #   if (!poss_new_state[1] %in% 1:nrow(grid) & poss_new_state[2] %in% 1:ncol(grid)) {
# #     new_state <- NA
# #     reward <- 0
# #   # if the new state is a terminal state  
# #   } else if (is.na(grid[poss_new_state[1], poss_new_state[2]])) {
# #     new_state <- 'terminal'
# #     reward <- -1
# #   } else {
# #     new_state <- poss_new_state 
# #     reward <- -1
# #   }
# #   
# # list(new_state, reward)  
# # }
# ############################################
# #### 4.1 ####
# # figuring out Q_pi(s, a) for Q_pi(11, down) as asked in the book seems 
# # more difficult, so starting with figuring out V_pi(s) for state 11
# # 4 * 4 grid world
# 
# grid <- matrix(0, 4, 4)
# grid[c(1, 16)] <- NA # terminal states
# grid[1, 2:4] <- 1:3 
# grid[2, ] <- 4:7
# grid[3, ] <- 8:11
# grid[4, 1:3] <- 12:14
# 
# # no discounting in exercise but lamdba left in for completeness
# lambda <- 1
# 
# # set of actions
# # actions always lead to the corresponding state transition 
# # (eg, 6, right transitions to 7) except actions that would move 
# # agent off board lead to no change 
# as <- list('up'=c(0, 1), 'down'=c(0, -1), 'right'=c(1, 0), 'left'=c(-1, 0))
# 
# # threshold to get below before terminating loop 
# theta <- 1e-5 
# # initial value for delta so can start off while condition
# delta <- 1e3
# 
# # number of states
# S <- nrow(grid) * ncol(grid) - 1
# 
# while (delta < theta) {
#   # set an arbitrary initial value of state 11
#   V <- runif(1, 0, 16)
#   
#   # iterate through states
#   for (state_num in S) {
#     
#     
#     
#     
#   }
#   
# }
# 
# 
# # loop through full list of actions 
# # for (action in seq_along(actions)) {
# #   TransitionStates(as[action], grid, current_state)
# # }
# 
# ## What is q_pi(7, down)?
# # initialize random state 
# ############################################
# 
# 
# 
# 
# 
# 
# 
# 

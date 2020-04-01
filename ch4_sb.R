lapply(c('tidyverse', 'dplyr'), require, character=TRUE)
#### 4.9. Gambler's Problem ####
#### CREATE HELPER FUNCTIONS ####

FindBestActionAndValueThereof <- function(state,
                                          state_values, # the current values for all states
                                          gamma=1, # discount on V(s')
                                          p_win=.5) {
  
  ### Finds the best action (or actions) and the value(s) thereof #
  
  # Implements right side of pseudocode for V(s) update in value iter box p. 83 ###
  
  # get all the outcomes from this state
  outcome_list <- CalcOutcomeAllStakes(state,
                                       p_win=.4)
  
  # preallocate vector of action results 
  action_expectations <- rep(NA, length(outcome_list)) 
  
  # iterate through the outcomes to find the value of all possible actions
  for (outcome in length(outcome_list)) {
    
    this_outcome <- outcome_list[[outcome]]  
    
    # first find the reward expectation for this state:
    # (since the only state yielding non-zero reward is the winning terminal 
    # state we just need to look in column 4)
    reward_expectation <- this_outcome[4, ]$prob * this_outcome[4, ]$reward
    browser()
    # now find the expected values of the non-terminal s_primes
    s_primes <- 
      data.frame(this_outcome[1:2, ] %>% filter(probs > 0))
    
    v_of_s_prime_expectations <- rep(NA, length(s_primes))
    
    for (sp in seq_along(s_primes)) {
      v_of_s_prime_expectations[s_prime] <- gamma * 
        s_primes$prob[sp] *
        # the state values vec includes a terminal state 
        # at index 1 so the appropriate value is 1 later
        state_values[s_primes$s_prime[sp]+1]
    } # end v of s prime iter
    
    # calculate the total expectations
    action_expectations[outcome] <- reward + 
      v_of_s_prime_expectations/length(s_primes)
    
  } # end outcomes iter
  
  # recalculate stakes    
  stakes <- 1:min(state, 100-state)
  
  # pass out the best stakes (ie actions) and their values
  max_inds <- which(action_expectations == max(action_expectations))
  
  best_actions_and_values <- data.frame('best_stakes'=stakes[max_inds],
                                        'values'=max(action_expectations))
  
best_actions_and_values    
}
CalcOutcomeAllStakes <- function(state,
                                 p_win=.5) {
  
  ### Returns a list of outcomes for all stakes possible from 
  # the current state, state_in ###
  
  # find all possible stakes
  stakes <- 1:min(state, 100-state)
  
  # preallocate outcome list
  outcome_list <- list()
  
  # calc outcome for each stake
  for (stake in seq_along(stakes)) {
    outcome_list[[stake]] <- CalcSPrimeRewAndProbThereof(stake=stakes[stake],
                                                         state_in=state,
                                                         p_win=p_win)
  }
  
outcome_list    
}
CalcSPrimeRewAndProbThereof <- function(stake,
                                        state_in, 
                                        p_win=.5,
                                        terminal_lose_criterion=0,
                                        terminal_win_criterion=100) {
  ### Returns values and probabilities of 4 possible s',r pairs #
  
  # Notes: Each state can result in one of four (s',r) pairs:
  
  # 1. s: state + stake (if win and don't end in a terminal state), r: 0
  # 2. s: state - stake (if lose and don't end in a terminal state), r: 0
  # 3. s: terminal state lose, r: 0 
  # 4. s: terminal state win, r: 0
  
  # The probability of ending in each state depends solely on the current state
  # (state in), the action taken (stake), and the coin probability ###
  
  # fill in the parts of the df we can already fill in
  SPrimeRewProbs <- data.frame('s_prime'=c(
    state_in + stake, # win & non-terminal state
    state_in - stake, #w lose & non-terminal state
    1, # losing terminal state
    101 # winning terminal state
  ),
  'reward'=c(0, 0, 0, 1))
  
  # preallocate probability vec
  probs <- rep(NA, 4)
  
  # if the current state + stake >= the winning terminal criterion, then 
  # the winning terminal state is reached with p(win)..
  probs[4] <- ifelse(test=state_in + stake >= terminal_win_criterion,
                     yes=p_win, 
                     no=0)
  # .. whereas if it's not, state + stake is reached with p(win)
  probs[1] <- ifelse(test=probs[4]==0,
                     yes=p_win,
                     no=0)
  
  # if the current state - stake <= the losing terminal criterion, then
  # the losing terminal state is reached with 1-p(win)
  probs[3] <- ifelse(test=state_in - stake <= terminal_lose_criterion,
                     yes=1-p_win, 
                     no=0)
  # .. if not, state - stake is reached with 1-p(win)
  probs[2] <- ifelse(test=probs[3]==0,
                     yes=1-p_win,
                     no=0)
  
  # jump in debugger if probs don't equal 1 
  browser(expr=sum(probs) != 1)
  
  SPrimeRewProbs$probs <- probs
  
SPrimeRewProbs  
}
############################################
####  INITIALIZATIONS ####
theta <- 1e-5 # threshold param
gamma <- 1
states <- 1:101 # one state for 1:99 capital + 2 terminal states
p_win <- .45 # probability of winning 
# initalize state values randomly.. 
state_values <- runif(length(states), 0, 1)
# .. except for setting terminal states to 0 and 1
state_values[1] <- 0; state_values[101] <- 1
############################################
#### FIND OPTIMAL POLICY AND VALUE FX THROUGH VALUE ITERATION ####
############################################
# iterate through the non-terminal states 
if (any(delta_vec > theta)) {
  
} else {
  # value fx
  
  # policy output
  
}
for (state in states[2:100]) {
  # pull the state's old value before updating 
  old_v_of_s <- values[state]
  
  action_value <- FindBestActionAndValueThereof(state,
                                                state_values, 
                                                gamma,
                                                p_win)
              
}





############################################























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

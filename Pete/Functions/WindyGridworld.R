#### FUNCTIONS TO CREATE WINDY GRIDWORLD (CH 6) AND SOLVE VIA DIFF RL METHODS ####
########################### INTIALIZATIONS ###########################
# Set world options
which_algo <- "SARSA"
rows <- 7 # Set rows of gridworld 
grid_world <- CreateWindyGridworld(rows)
# Where its windy by column..
winds <- c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0)
wcols <- which(winds!=0) # columns where its windy
str_winds <- noquote(as.character(winds[wcols])) # string representation of wind
# .. and by by state indices, with column names = upddraft + random string
windy_states <- setNames(data.frame(grid_world[, wcols]), paste0(str_winds))
# Up down left and right in matrix indexing
base_actions <- c(1, -1, rows, -rows)
# Diagonal actions in matrix indexing
diag_actions <- c(-rows+1, -rows-1, rows+1, rows-1)
# Allow actions = 
# 1 basic only | 2 basic + diagonal (king's rules) | 3 king's rules + same state?
action_type <- "basic" 
start_state <- 4
goal_state <- 53
# Reward for reaching goal / non-goal state
non_goal_rewards <- -1 
goal_reward <- 0
# TO DO: implement softmax and other varieties
soft_policy <- "eps_greedy"
if (soft_policy == "eps_greedy") softness <- .1
# Set debug options
quiet <- 0
######################################################################
###################### PACKAGE INITS INTO LISTS ######################
# Package up pars to run algos
if (which_algo=="SARSA") {
  pars <- list(
    "softness"=softness, # for soft action selection
    "alpha"=.5, # learning rate
    "gamma"=1, # discount 
    "zero_init_values"=1, # how to inialize q values
    "on_or_off"="on", # TO DO: edit, others not yet implemented
    "soft_policy"=soft_policy
  )
  algo_list <- list("alg"="SARSA", "pars"=pars)
}
# Package up world items
world_list <- list(
  "grid_world"=grid_world,
  "windy_states"=windy_states,
  "base_actions"=base_actions,
  "diag_actions"=diag_actions,
  "action_type"=action_type,
  "start_state"=start_state,
  "goal_state"=goal_state,
  "ng_rews"=non_goal_rewards,
  "g_rew"=goal_reward
)
# Package up debug options
debug_opts <- list("quiet"=quiet)
######################################################################

SolveWindyGridWorld <- function(world_list,
                                # Name and specs for RL algorithm to implement,
                                # defaults to SARSA
                                algo_list,
                                debug_opts
                                ) {
  ### Outer environment to call RL solutions to windy gridworld. Returns
  # policy and other information for the algorithm / parameters initialized ###
  
  if (algo_list$alg=="SARSA") inits <- InitSARSA(world_list, 
                                                 sarsa_list$pars)
  
                DoEpisode(world_list,
                          algo_list,
                          debug_opts,
                          inits)
  
  ########################### HELPER FUNCTIONS #########################
  ################################################### END HELPER FUNCTIONS
    
    
    
}
    
    


########################### WORLD FUNCTIONS ###########################
CreateWindyGridworld <- function(rows=7, columns=10) {
  ### Returns a gridworld incl optional args if want to use 
  # diff dimensions than book ###
  world <- matrix(1:(rows*columns), nrow=rows, ncol=columns)
world  
}
######################################################################
DoEpisode <- function(world_list,
                      algo_list,
                      debug_opts,
                      inits) {
  ### Runs through a single episode ###
  
  ########################### GENERAL INITS ############################
  world <- world_list$grid_world
  Q_SA_mat <- inits$Q_SA_mat
  ng_rew <- world_list$ng_rews
  g_rew <- world_list$g_rew
  goal_state <- world_list$goal_state
  windy_states <- world_list$windy_states
  gamma <- algo_list$pars$gamma
  alpha <- algo_list$pars$alpha
  reward_list <- list()
  state_list <- list()
  ######################################################################
  ########################### START EPISODE #############################
  state <- world_list$start_state 
  t_step <- 1
  
  # Continue episode until end state is reached or hit 1e10 time steps
  while (state != goal_state | t_step < 1e10) {
    
    if (algo_list$alg == "SARSA" & algo_list$pars$on_or_off == "on") {
      action <- SelActOnPolicySARSA(Q_SA_mat, state=state)
    }
    
    # Find what state would be if it was only determined by agent move
    agent_move <- state+action
    
    ## Add wind
    # If this is a windy state..
    if (any(agent_move %in% windy_states)) {
      # .. find how much wind to add by referencing the column name in 
      # windy_states.. 
      # TO DO: add stochastic option
      wind <- as.numeric(names(windy_states)[which(agent_move==windy_states, 
                                                   arr.ind=TRUE)[, 2]])
      # and add it onto putative new state
      put_new_state <- agent_move + wind
    } else {
      # Otherwise putative new state is just state + action
      put_new_state <- agent_move
    } ## END ADD WIND
    
    ## Find s',r
    # If the putative new state is the goal state then just assign final state and reward=0
    if (put_new_state == goal_state) {
      reward <- g_rew
      # If not the goal state..
    } else {
      # .. and would take us off the grid .. 
      if (!put_new_state %in% world) {
        # .. don't change state but do assign reward .. 
        reward <- ng_rew
        s_prime <- state
      } else {
        # .. change state and assign reward
        reward <- ng_rew
        s_prime <- put_new_state 
      }
    } # END ADVANCE s', r ADVANCE
    
    if (algo_list$alg == "SARSA" & algo_list$pars$on_or_off == "on") {
      a_prime <- SelActOnPolicySARSA(Q_sA_mat, state=s_prime)
    }
    
    if (algo_list$alg == "SARSA") {
      Q_SA_mat <- SARSAUpdateQSA(Q_SA_mat,
                                 action, state, a_prime, s_prime,
                                 reward, gamma, alpha)
    }
    
    ## Bookkeeping and state advance before episode ends
    reward_list[[t_step]] <- reward
    state_list[[t_step+1]] <- s_prime  
    state <- s_prime 
    
    t_step <- t_step+1 # increment time step
    
  } # END EPISODE LOOP
}
########################### GENERAL RL FUNCTIONS #####################
SoftSelect <- function(
  algo_list, 
  Q_sA_info,
  debug_opts
) {
  ### Returns a softly selected action given state and policy #
  
  # Type of selection is given by soft policy in initializations #
  # For determinstic selection, tweak softness in initialization #
  # eg 0 for eps-greedy ###
  
  if (algo_list$pars$soft_policy == "eps_greedy") {
    # Find max index
    mi <- which(Q_sA_info$policy == max(Q_sA_info$policy))
    # Randomly break ties 
    if (length(mi > 1)) mi <- sample(mi)[1]
    # Select best action with epsilon probability..
    delta <- runif(1, 0, 1) # threshold 
    if (delta >= pars$softness) {
      action <- Q_sA_info[mi, "a"]
    } else {
      # .. otherwise pick randomly from the other actions
      action <- Q_sA_info[sample(setdiff(1:nrow(Q_sA_info), mi))[1], "a"]
    }
  } 
action  
}
######################################################################
############### ALGORITHM SPECIFIC RL FUNCTIONS ######################
## SARSA ##
InitSARSA <- function(world_list,
                      pars) {
  ### Initializations for SARSA ###
  
  # Find allowed actions
  if (world_list$action_type == "basic") action_set <- world_list$base_actions
  if (world_list$action_type == "kings") action_sets <-
      c(world_list$base_actions, world_list$diag_actions)
  
  # Initalize Q(S,A) matrix..
  Q_SA_mat <- expand.grid("s"=1:length(grid_world), 
                          "a"=world_list$base_actions)
  # .. and either set values to 0..
  if (as.numeric(pars["zero_init_values"])) {
    Q_SA_mat$value <- rep(0, nrow(Q_SA_mat)) 
  } else { 
    # .. or as randomly initialized .. #
    Q_SA_mat$value <- runif(nrow(Q_SA_mat), 0, 1) 
    # .. but set terminal state value to 0
    Q_SA_mat[Q_SA_mat$s == world_list$goal_state, ]$value <- 0
  }
  
  # Start with equiprobable policy
  Q_SA_mat$policy <- 1/length(actions)
  
  sarsa_inits <- list(
    "Q_SA_mat"=Q_SA_mat, # Q_SA matrix incl policy
    "action_set"=action_set # set of unique actions
  )
  
sarsa_inits  
}
SARSAUpdateQSA <- function(Q_SA_mat,
                           action,
                           state,
                           a_prime,
                           s_prime,
                           reward,
                           gamma,
                           alpha) {
  ### Update Q(SA_t) using SARS'A', return Q_SA_mat ###
  
  # Extract Q(S, A) and Q(S', A')
  QSA_val <- Q_SA_mat[Q_SA_mat$s == state & Q_SA_mat$a == action, ]$value
  QSprApr_val <- Q_SA_mat[Q_SA_mat$s == s_prime & Q_SA_mat$a == a_prime, ]$value
  
  # Update Q(S, A) value in dataframe
  Q_SA_mat[Q_SA_mat$s == state & Q_SA_mat$a == action, "value"] <-
    QSA_val + alpha * (reward + gamma * QSprApr_val - QSA_val)
  
  Q_SA_mat  
}
SelActOnPolicySARSA <- function(Q_sA_mat, state) {
  ### Select an on-policy action|state following p. 130 SARSA algo ###
  
  # Extract information for this state 
  Q_sA_info <- Q_SA_mat %>% filter(s == state)
  # Softly select an action following policy 
  action <- SoftSelect(algo_list, Q_sA_info, debug_opts)
  
action    
}
## END SARSA ##
######################################################################




SolveWindyGridWorld(world_list, algo_list)



############################################
# .. and by by state indices, with column names = upddraft + random string
# windy_states <- setNames(data.frame(grid_world[, wcols]), 
#                          paste0(str_winds, replicate(length(wcols), GenRandString())))
#length(unlist(map(strsplit(names(windy_states), "_"), 1)))
# grid_df <- data.frame()
#ggplot(data.frame(expand.grid(0:5, 0:5)), aes(Var1, Var2)) + geom_tile()
# ggplot(data.frame(a=1:10, b=1:5), aes(as.factor(a), as.factor(b))) + geom_tile() + ga + ap

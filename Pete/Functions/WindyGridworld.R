#### FUNCTIONS TO CREATE WINDY GRIDWORLD (CH 6) AND SOLVE VIA DIFF RL METHODS ####
######################################################################
SolveWindyGridWorld <- function(world_list,
                                # Name and specs for RL algorithm to implement,
                                # defaults to SARSA
                                algo_list,
                                control_opts
) {
  ### Outer environment to call RL solutions to windy gridworld. Returns
  # policy and other information for the algorithm / parameters initialized ###
  
  ########################### HELPER FUNCTIONS ###########################
  ################################################### END HELPER FUNCTIONS
  
  ########### INITIALIZATIONS BEFORE FIRST EPISODE ######################
  # Find allowed actions
  # ** to del
  # if (world_list$action_type == "basic") action_set <- world_list$base_actions
  # if (world_list$action_type == "kings") action_set <-
  #     c(world_list$base_actions, world_list$diag_actions)
  # if (world_list$action_type == "kings_plus") action_set <-
  #     c(world_list$base_actions, world_list$diag_actions, 0)
  
  if (algo_list$alg=="SARSA") {
    algo_list[["Q_SA_mat"]] <- InitSARSA_Q_SA(world_list, algo_list$pars)
  }
  
  ep_counter <- 1
  ######################################################################
  outs_list <- list()
  for (ep in seq_along(1:control_opts$n_episodes)) {
    
    if (!control_opts$quiet) { cat('\n##### NEW EPISODE ##########\n'); pause(2) }
    
    outs <- DoEpisode(world_list,
                      algo_list,
                      control_opts,
                      inits)
    
    # Replace the Q_SA mat for SARSA
    if (algo_list$alg=="SARSA") algo_list[["Q_SA_mat"]] <- outs[["Q_SA_mat"]]
    
    # Store the outcomes
    outs_list[[ep]] <- outs
    
    ep_counter <- ep_counter+1
  }  
outs    
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
                      control_opts,
                      inits) {
  ### Runs through a single episode ###
  
  ########################### GENERAL INITS ############################
  Q_SA_mat <- algo_list[["Q_SA_mat"]]
  world <- world_list$grid_world
  ng_rew <- world_list$ng_rews
  g_rew <- world_list$g_rew
  goal_state <- world_list$goal_state
  gamma <- algo_list$pars$gamma
  alpha <- algo_list$pars$alpha
  reward_list <- list()
  state_list <- list()
  quiet <- control_opts$quiet
  # Extract and reformat winds
  winds <- world_list$winds
  wcols <- which(winds!=0) # columns where its windy
  str_winds <- noquote(as.character(winds[wcols])) # string representation of wind
  # .. and by by state indices, with column names = upddraft + random string
  windy_states <- setNames(data.frame(grid_world[, wcols]), paste0(str_winds))
  ######################################################################
  ########################### START EPISODE #############################
  state <- world_list$start_state 
  t_step <- 1
  if (!quiet) cat('\n (S) Our state is:', action)
  # Continue episode until end state is reached or hit 10k time steps
  
  if (algo_list$alg == "SARSA" & algo_list$pars$on_or_off == "on") {
    action <- SelActOnPolicySARSA(Q_SA_mat, state=state)
    if (!quiet) cat('\n (A) We pick action:', action)
  }
  # Find what state would be if it was only determined by agent move
  agent_move <- state+action
  if (!quiet) cat('\n We try to move to:', agent_move)
  
  RideTheWind <- function(world_list,
                          agent_move,
                          windy_state) {
    ### Perturbs actions by stochastic or deterministic wind,
    # outputting wind amount ###
    
    ## Add wind
    # If this is a windy state..
    if (any(agent_move == windy_states)) {
      # .. find how much wind to add by referencing the column name in 
      # windy_states.. 
      det_wind <- as.numeric(names(windy_states)[which(agent_move==windy_states, 
                                                       arr.ind=TRUE)[, 2]])
      if (!world_list$noisy_wind) { 
        det_wind <- wind
        if (!quiet) cat("\n** OH NO, a gust of --deterministic-- WIND blows us",
                        floor(wind / nrow(world)) + wind %% nrow(world), 
                        "units upward**")
      } else {
        # Add stochastic component
        rand_draw <- runif(1, 0, 1)
        if (rand_draw < 1/3) {
          stochastic_comp <- -nrow(world) # move one more down
        } else if (rand_draw < 2/3) {
          stochastic_comp <- nrow(world) # move one more up
        } else {
          stochastic_comp <- 0 
        }
        wind <- det_wind + stochastic_comp
        if (!quiet) cat("\n** OH DREAD, a !!stochastic!! WIND blows us",
                        floor(wind / nrow(world)) + wind %% nrow(world), 
                        "units upward**")
      }
      # and add it onto putative new state
      put_new_state <- agent_move + wind
    } else {
      # Otherwise putative new state is just state + action
      put_new_state <- agent_move
    }
    
  }
  while (!state == goal_state & t_step < 1e4) {
    
   
    
    if (!quiet) cat("\n Putatively we're in state", put_new_state)
    ## Find s',r
    # If the putative new state is the goal state then assign final state and reward=0
    if (put_new_state == goal_state) {
      reward <- g_rew
      s_prime <- put_new_state
      if (!quiet) cat("\n (S') and that's great because as it turns out 
                       that's the goal state! \n (R) Our reward is",  reward)
      # If not the goal state..
    } else {
      # .. and would take us off the grid .. 
      if (!put_new_state %in% world) {
        # .. don't change state but do assign reward .. 
        reward <- ng_rew
        s_prime <- state
        if (!quiet) cat("\n (S') but that doesn't exist so we're stuck in", state, ".",
                        "\n (R) Our reward is",  reward)
      } else {
        # .. change state and assign reward
        reward <- ng_rew
        s_prime <- put_new_state 
        if (!quiet) cat("\n (S') and that's a valid transition so we're now in", state, ".",
                        "\n (R) Our reward is",  reward)
      }
    } # END ADVANCE s', r ADVANCE
    
    if (algo_list$alg == "SARSA" & algo_list$pars$on_or_off == "on") {
      a_prime <- SelActOnPolicySARSA(Q_SA_mat, state=s_prime)
    }
    if (!quiet) cat("\n (A') We pick next action:", a_prime)
    
    if (algo_list$alg == "SARSA") {
      Q_SA_mat <- SARSAUpdateQSA(Q_SA_mat,
                                 action, state, a_prime, s_prime,
                                 reward, gamma, alpha, control_opts)
    }
    
    ## Bookkeeping and state advance before episode ends
    reward_list[[t_step]] <- reward
    state_list[[t_step+1]] <- s_prime  
    state <- s_prime 
    
    t_step <- t_step+1 # increment time step
    control_opts[["ts"]] <- t_step # to facilitate debugging
    if (!control_opts$quiet) cat('\n Time step:', t_step)
    
  } # END EPISODE LOOP
  
outs <- list("Q_SA_mat"=Q_SA_mat, 
             "state_trajectory"=state_list,
             "rewards"=reward_list)    
}
########################### GENERAL RL FUNCTIONS #####################
SoftSelect <- function(
  algo_list, 
  Q_sA_info,
  control_opts
) {
  ### Returns a softly selected action given state and policy #
  
  # Type of selection is given by soft policy in initializations #
  # For determinstic selection, tweak softness in initialization ###
  
  if (algo_list$pars$soft_policy == "eps_greedy") {
    # Find index of max value
    mi <- which(Q_sA_info$value == max(Q_sA_info$value))
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
InitSARSA_Q_SA <- function(world_list,
                           pars) {
  ### Build initial Q_SA_matrix for SARSA ###
  
  # Initalize Q(S,A) matrix..
  Q_SA_mat <- expand.grid("s"=1:length(grid_world), "a"=world_list$base_actions)
  # .. and either set values to 0..
  if (as.numeric(pars["zero_init_values"])) {
    Q_SA_mat$value <- rep(0, nrow(Q_SA_mat)) 
  } else { 
    # .. or as randomly initialized .. #
    Q_SA_mat$value <- runif(nrow(Q_SA_mat), 0, 1) 
    # .. but set terminal state value to 0
    Q_SA_mat[Q_SA_mat$s == world_list$goal_state, ]$value <- 0
  }

Q_SA_mat  
}
SARSAUpdateQSA <- function(Q_SA_mat,
                           action,
                           state,
                           a_prime,
                           s_prime,
                           reward,
                           gamma,
                           alpha,
                           control_opts) {
  ### Update Q(SA_t) using SARS'A', return Q_SA_mat ###
  
  # Extract Q(S, A) and Q(S', A')
  QSA_val <- Q_SA_mat[Q_SA_mat$s == state & Q_SA_mat$a == action, ]$value
  QSprApr_val <- Q_SA_mat[Q_SA_mat$s == s_prime & Q_SA_mat$a == a_prime, ]$value
  
  # Update Q(S, A) value in dataframe
  Q_SA_mat[Q_SA_mat$s == state & Q_SA_mat$a == action, "value"] <-
    QSA_val + alpha * (reward + gamma * QSprApr_val - QSA_val)
  
  if (!control_opts$quiet) {
    cat("\n\n ## SARSA UPDATE ##\n")
    cat("\n Right side: current Q_sa:", QSA_val, 
        "\n + alpha =", alpha, "* (R", reward, "+ gamma", gamma, 
        "* Q(S',A')", QSprApr_val, "- Q(S,A)", QSA_val, ")"
        )
    cat("\n Q(SA)")
    print(unlist(
      Q_SA_mat[Q_SA_mat$s == state & Q_SA_mat$a == action, "value"])
    )
    
  } 
  
Q_SA_mat  
}
SelActOnPolicySARSA <- function(Q_SA_mat, state) {
  ### Select an on-policy action|state following p. 130 SARSA algo ###
  
  # Extract information for this state 
  Q_sA_info <- Q_SA_mat %>% filter(s == state)
  # Softly select an action following policy 
  action <- SoftSelect(algo_list, Q_sA_info, control_opts)
  
action    
}
## END SARSA ##
######################################################################
########################### INTIALIZATIONS ###########################
## Set control options ##
quiet <- 0 
n_episodes <- 1e2
## Set world options ##
which_algo <- "SARSA"
rows <- 7 # Set rows of gridworld 
grid_world <- CreateWindyGridworld(rows)
# Winds by column given in matrix indices the wind moves us
winds <- c(0, 0, 0, rows, rows, rows, rows+1, rows+1, rows+1, 0)
# Up down left and right in matrix indexing
base_actions <- c(1, -1, rows, -rows)
# Diagonal actions in matrix indexing
diag_actions <- c(-rows+1, -rows-1, rows+1, rows-1)
# Set action type:
# "basic" = base actions only
# "kings" = base + diagonal (king's rules) 
# "kings_plus" = king's rules + same state
action_type <- "basic"
start_state <- 4
goal_state <- 53
noisy_wind <- 0 # Make wind blows stochastic?
# Reward for reaching goal / non-goal state
non_goal_rewards <- -1 
goal_reward <- 0
# TO DO: implement softmax and other soft varieties
soft_policy <- "eps_greedy"
if (soft_policy == "eps_greedy") softness <- .1
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
  "winds"=winds,
  "base_actions"=base_actions,
  "diag_actions"=diag_actions,
  "action_type"=action_type,
  "start_state"=start_state,
  "goal_state"=goal_state,
  "ng_rews"=non_goal_rewards,
  "g_rew"=goal_reward,
  "noisy_wind"=noisy_wind
)
# Package up control and debug options
control_opts <- list("quiet"=quiet, 
                     "n_episodes"=n_episodes,
                     "ts"=1)

SolveWindyGridWorld(world_list, algo_list, control_opts)
############################################
# .. and by by state indices, with column names = upddraft + random string
# windy_states <- setNames(data.frame(grid_world[, wcols]), 
#                          paste0(str_winds, replicate(length(wcols), GenRandString())))
#length(unlist(map(strsplit(names(windy_states), "_"), 1)))
# grid_df <- data.frame()
#ggplot(data.frame(expand.grid(0:5, 0:5)), aes(Var1, Var2)) + geom_tile()
ggplot(data.frame(a=1:10, b=1:5), aes(as.factor(a), as.factor(b))) + geom_tile() + ga + ap

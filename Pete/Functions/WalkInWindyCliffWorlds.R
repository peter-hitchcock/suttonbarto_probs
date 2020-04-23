#### FUNCTIONS TO CREATE WINDY GRIDWORLD (CH 6) AND SOLVE VIA DIFF RL METHODS ####
######################################################################
SolveWindyOrCliffWorlds <- function(world_list,
                                    # Name and specs for RL algorithm to implement,
                                    # defaults to SARSA
                                    algo_list,
                                    control_opts 
) {
  ### Outer environment to call RL solutions to windy gridworld. Returns
  # policy and other information for the algorithm / parameters initialized ###
  
  # Determine set of allowed actions
  if (world_list$action_type=="base") world_list$actions <- base_actions
  if (world_list$action_type=="kings") world_list$actions <- c(base_actions, diag_actions)
  if (world_list$action_type=="kings_plus") world_list$actions <- c(base_actions, diag_actions, 0)
  
  if (algo_list$alg=="SARSA") {
    algo_list[["Q_SA_mat"]] <- InitSARSA_Q_SA(world_list, algo_list$pars)
  }
  
  ep_counter <- 1
  ######################################################################
  outs_list <- list()
  for (ep in seq_along(1:control_opts$n_episodes)) {
    
    if (!control_opts$quiet) { cat('\n##### NEW EPISODE ##########\n'); pause(1) }
    
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
RideTheWind <- function(world_list,
                        put_new_state,
                        windy_states) {
  ### Perturbs actions by stochastic or deterministic wind,
  # outputting wind amount ###
  
  ## Add wind
  # If this is a windy state..
  if (any(put_new_state == windy_states)) {
    # .. find how much wind to add by referencing the column name in 
    # windy_states.. 
    det_wind <- as.numeric(names(windy_states)[which(put_new_state==windy_states, 
                                                     arr.ind=TRUE)[, 2]])
    if (!world_list$noisy_wind) { 
      wind <- det_wind
      if (!quiet) cat("\n** OH HOW HORRID, a gust of --deterministic-- WIND blows us",
                      floor(wind / nrow(world_list$grid_world)) + wind %% nrow(world_list$grid_world), 
                      "units upward **")
    } else {
      # Add stochastic component
      rand_draw <- runif(1, 0, 1)
      if (rand_draw < 1/3) {
        stochastic_comp <- -nrow(world_list$grid_world) # move one more down
      } else if (rand_draw < 2/3) {
        stochastic_comp <- nrow(world_list$grid_world) # move one more up
      } else {
        stochastic_comp <- 0 
      }
      wind <- det_wind + stochastic_comp
      if (!quiet) cat("\n** OH DREAD, a !!stochastic!! WIND blows us",
                      floor(wind / nrow(world_list$grid_world)) + wind %% nrow(world_list$grid_world), 
                      "units upward **")
    }
  } else {
    wind <- 0
  }
wind  
}
EvalMoveAttempt <- function(
                            state,
                            put_new_state,
                            goal_state,
                            world,
                            ng_rew,
                            g_rew,
                            cliff_rew, 
                            is_there_a_cliff, # indicator 
                            cliff, # cliff indices
                            start_state # to return to if fall into the cliff
                            ) {
  ### Returns s'r by determining whether a proposed state transition is 
  # valid ###
  
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
      if (!quiet) cat("\n (S') but that doesn't exist so we're stuck in", s_prime, ".",
                      "\n (R) Our reward is",  reward)
    } else {
      # .. change state and assign reward
      reward <- ng_rew
      s_prime <- put_new_state 
      if (!quiet) cat("\n (S') and that's a valid transition so we are in fact in", s_prime, ".",
                      "\n (R) Our reward is",  reward)
    }
  } 
  
list("s_prime"=s_prime, "reward"=reward)  
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
  cliff_rew <- world_list$cliff_rews
  is_there_a_cliff <- world_list$is_there_a_cliff
  cliff <- world_list$cliff
  start_state <- world_list$start_state
  goal_state <- world_list$goal_state
  gamma <- algo_list$pars$gamma
  alpha <- algo_list$pars$alpha
  reward_list <- list()
  state_list <- list()
  action_list <- list()
  quiet <- control_opts$quiet
  # Extract and reformat winds
  winds <- world_list$winds
  wcols <- which(winds!=0) # columns where its windy
  str_winds <- noquote(as.character(winds[wcols])) # string representation of wind
  # Set column names = upddraft + random string
  windy_states <- setNames(data.frame(grid_world[, wcols]), paste0(str_winds))
  ######################################################################
  ########################### START EPISODE #############################
  state <- start_state 
  # Initialize state and time step 
  t_step <- 1
  state_list[[t_step]] <- state
  if (!control_opts$quiet) cat('\n Time step:', t_step, '\n (S) Our state is:', state)
  
  ## For SARSA, pick an action before looping so we can get our first update
  if (algo_list$alg == "SARSA" & algo_list$pars$on_or_off == "on") {
    action <- SelActOnPolicySARSA(Q_SA_mat, state=state)
    if (!quiet) cat('\n (A) First action:', action)
    action_list[[t_step]] <- action
  }
  
  while (!state == goal_state & t_step < 5e4) {
    
    if (algo_list$alg == "Q_learn") {
      # TO DO: select action
    }
    
    # No chance of wind on first trial
    if (t_step == 1) put_new_state <- state+action
    # Putative new state = state + action + wind..
    if (!quiet) cat("\n (A) We take action A:", action, ".")
    wind <- RideTheWind(world_list,
                        put_new_state,
                        windy_states)
    
    
    put_new_state <- state + action + wind # S <- S' is at bottom of loop
    
    cat("\n Combined with the wind that putatively puts us in state", put_new_state, ".")
    
    #.. but need to evaluate whether this is a valid state transition,
    # returning s',r where s' = s if not
    sp_r <- EvalMoveAttempt(state,
                            put_new_state,
                            goal_state,
                            world,
                            ng_rew,
                            g_rew,
                            cliff_rew,
                            is_there_a_cliff, 
                            cliff, 
                            start_state)
    
    s_prime <- sp_r[["s_prime"]]
    reward <- sp_r[["reward"]]
    
    if (algo_list$alg == "SARSA" & algo_list$pars$on_or_off == "on") {
      # Now that we have s'r we can get A'..
      a_prime <- SelActOnPolicySARSA(Q_SA_mat, state=s_prime)
    }
    if (!quiet) cat("\n (A') We pick next action:", a_prime)
    
    # .. and do appropriate update
    if (algo_list$alg == "SARSA") {
      Q_SA_mat <- SARSAUpdateQSA(Q_SA_mat,
                                 action, state, a_prime, s_prime,
                                 reward, gamma, alpha, control_opts)
    }
    
    if (algo_list$alg == "Q_learn") {
      
    }
    
    if (t_step > 1) {
      state <- s_prime
      action <- a_prime
    }
    ## Remember state advance and reward
    action_list[[t_step]] <- a_prime
    state_list[[t_step]] <- s_prime
    
    if (!control_opts$quiet) cat('\n Time step:', t_step)
    t_step <- t_step+1
    
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
  Q_SA_mat <- expand.grid("s"=1:length(grid_world), "a"=world_list$actions)
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
quiet <- 0 # print trial-by-trial 
visualize <- 1 # output graphs?
n_episodes <- 1e2
# Which algorithm do you want to learn with? (TO DO: only SARSA on-policy implemented so far)
# 1 "SARSA"--and can set on- vs. off- policy below in SARSA's pars
which_algo <- "SARSA"
## Set world options ##
# Which world do you want? (TO DO: only windy grid world constructed so far)
# 1 "windy" for windy grid world
# 2 "cliff_walk" for cliff walk world
# 3 "windy_cliff_walk" if you really want to get crazy
environ <- "windy" 
# Set dimensions of world
if (environ == "windy") { 
  rows <- 7; cols <- 10 
} else {
  rows <- 4; cols <- 12
}
grid_world <- CreateWindyGridworld(rows, cols)
# Winds by column given in matrix indices the wind moves us
winds <- rep(0, cols) # default to no wind
# Where and how much wind do you want in your windy worlds?
if (environ == "windy" | environ == "cliff_walk") {
  winds <- rep(0, cols)
  # Note that wind in the first or second columns won't work for SARSA
  winds[4:9] <- c(rows, rows, rows, rows+1, rows+1, rows)
} 
if (environ == "windy") {
  start_state <- round(rows/2) # start at the middle + 1
  # This is difficult to avoid hard coding, but may need to tweak this depending 
  # on row, col settings
  goal_state <- 53
  cliff <- c() # no cliff in regular windy world
} else {
  start_state <- nrow(grid_world) # start in the bottom left..
  goal_state <- nrow(grid_world) * ncol(grid_world) # ..reach goal in the bottom right..
  # .. alas, The Abyss lies between them
  cliff <- setdiff(grid_world[nrow(grid_world), ], 
                   c(start_state, goal_state))
}
# For windy worlds, set second arg to 1 for stochastic wind blows
noisy_wind <- ifelse(grep("windy", environ), 1, 0)
# Reward for reaching goal / non-goal state
non_goal_rewards <- -1 
goal_reward <- 0
cliff_rewards <- -100
is_there_a_cliff <- 1
# Up down left and right in matrix indexing
base_actions <- c(1, -1, rows, -rows)
# Diagonal actions in matrix indexing
diag_actions <- c(-rows+1, -rows-1, rows+1, rows-1)
# Set action type:
# "basic" = base actions only
# "kings" = base + diagonal (king's rules) 
# "kings_plus" = king's rules + same state
action_type <- "base"
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
}
if (which_algo=="Q_learning") {
  pars <- list(
    "softness"=softness,
    "alpha"=.3, # learning rate
    "gamma"=1, # discount
    "zero_init_values"=1, # how to inialize q values
    "soft_policy"=soft_policy
  )
}
algo_list <- list("alg"=which_algo, "pars"=pars)
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
  "is_there_a_cliff"=is_there_a_cliff,
  "cliff_rews"=cliff_rewards,
  "cliff"=cliff,
  "noisy_wind"=noisy_wind
)
# Package up control and debug options
# TO DO: placeholder for vis options
vis_list <- list()
control_opts <- list("quiet"=quiet, 
                     "n_episodes"=n_episodes,
                     "ts"=1,
                     "vis"=vis_list)

SolveWindyOrCliffWorlds(world_list, algo_list, control_opts)

# Create a visitation graph to print every 1k iters 
# cols <- 10
# row_col <- data.frame(expand_grid("r"=1:rows, "c"=1:cols), "visit"=0)
# inds <- which(grid_world==state, arr.ind = TRUE)
# row_col[row_col$r == inds[1] & row_col$c == inds[2], ]$visit <- 
#   row_col[row_col$r == inds[1] & row_col$c == inds[2], ]$visit+1
# 
# ggplot(row_col, aes(as.factor(c), as.factor(r)), fill=visit/sum(visit)) + geom_tile() + ga + ap
# At 1k visits, normalize over number of visits and print heatmap

# <- 
#   row_col %>% filter(r == as.numeric(state_rc_inds)[1] & c==as.numeric(state_rc_inds)[2])$visit+1
## Notes: seems to basically be working but transitions aren't lining up properly
############################################
# .. and by by state indices, with column names = upddraft + random string
# windy_states <- setNames(data.frame(grid_world[, wcols]), 
#                          paste0(str_winds, replicate(length(wcols), GenRandString())))
#length(unlist(map(strsplit(names(windy_states), "_"), 1)))
# grid_df <- data.frame()
#ggplot(data.frame(expand.grid(0:5, 0:5)), aes(Var1, Var2)) + geom_tile()
#ggplot(data.frame(a=1:10, b=1:5), aes(as.factor(a), as.factor(b))) + geom_tile() + ga + ap

# # Find what state would be if it was only determined by agent move
# agent_move <- state+action
# if (!quiet) cat('\n We try to move to:', agent_move)
# put_new_state <- agent_move + 0 # Can never transition to a windy state from starting state
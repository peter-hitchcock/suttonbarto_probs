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
non_goal_rewards <- -1 # reward on states other than the goal state
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
    "policy"="on", # TO DO: edit, others not yet implemented
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
  "ng_rews"=non_goal_rewards
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
  
  ########################### HELPER FUNCTIONS #########################
  ################################################### END HELPER FUNCTIONS
  
  DoEpisode <- function(world_list,
                        algo_list,
                        debug_opts) {
    ### Runs through a single episode ###
    
    ########################### GENERAL INITS ############################
    world <- world_list$grid_world
    Q_SA_mat <- inits$Q_SA_mat
    ng_rew <- world_list$ng_rews
    goal_state <- world_list$goal_state
    windy_states <- world_list$windy_states
    # Find the windy columns
    windy_columns <- which(winds != 0)
    reward_list <- list()
    ######################################################################
    ########################### START EPISODE #############################
    state <- world_list$start_state 
    
    # Continue episode until end state is reached or hit 1e10 time steps
    while (state != goal_state | ts < 1e10) {
      
      if (algo_list$alg == "SARSA") {
        # Extract information for this state 
        Q_sA_info <- Q_SA_mat %>% filter(s == state) 
        # Softly select an action following policy 
        action <- SoftSelect(algo_list, Q_sA_info, debug_opts)
        # Find new state if was only determined by agent move
        agent_move <- state+action
        if (any(agent_move %in% windy_states)) {
          names(windy_states)[which(agent_move==windy_states, arr.ind=TRUE)[, 2]]
          put_new_state <- agent_move + winds
        }
        
        
        
         
        # If this would take us outside the gridworld..
        if (!put_new_state %in% world) {
          # State stays the same but 
          r
        }
        
        
        
        
      }
      
      ts <- ts+1 # increment time step
      
    }
    ######################################################################
    
    # Notes: ###
  }
    
    

}
########################### WORLD FUNCTIONS ###########################
CreateWindyGridworld <- function(rows=7, columns=10) {
  ### Returns a gridworld incl optional args if want to use 
  # diff dimensions than book ###
  world <- matrix(1:(rows*columns), nrow=rows, ncol=columns)
world  
}
######################################################################
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
    Q_SA_mat$values <- rep(0, nrow(Q_SA_mat)) 
  } else { 
    # .. or as randomly initialized .. #
    Q_SA_mat$values <- runif(nrow(Q_SA_mat), 0, 1) 
    # .. but set terminal state values to 0
    Q_SA_mat[Q_SA_mat$s == world_list$goal_state, ]$values <- 0
  }
  
  # Start with equiprobable policy
  Q_SA_mat$policy <- 1/length(actions)
  
  sarsa_inits <- list(
    "Q_SA_mat"=Q_SA_mat, # Q_SA matrix incl policy
    "action_set"=action_set # set of unique actions
  )
  
sarsa_inits  
}
######################################################################




SolveWindyGridWorld(world_list)



############################################
# .. and by by state indices, with column names = upddraft + random string
# windy_states <- setNames(data.frame(grid_world[, wcols]), 
#                          paste0(str_winds, replicate(length(wcols), GenRandString())))
#length(unlist(map(strsplit(names(windy_states), "_"), 1)))
# grid_df <- data.frame()
#ggplot(data.frame(expand.grid(0:5, 0:5)), aes(Var1, Var2)) + geom_tile()
# ggplot(data.frame(a=1:10, b=1:5), aes(as.factor(a), as.factor(b))) + geom_tile() + ga + ap

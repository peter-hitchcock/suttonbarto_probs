MCSolveRaceTrack <- function(
  gamma=.9,
  n_episodes=50,
  MC_policy="Exploring Starts",
  quiet=0
) {
  ### Solve the racetrack problem with various Monte Carl methods (so far just exploring starts)
  
  # Returns an optimalicy policy fo each starting state ** + other stuff ###

  ############################################    
  #### INITIALIZATIONS ####
  # must be at least 16
  track_height <- 30
  track <- InitTrack(track_height)
  # vertical and horizontal current velocities (can never exceed 5 or both be 0 except at start)
  start_velocity <- data.frame('hor'=0, 'ver'=0)
  # matrix representation of action set (actions correspond to velocity increments)
  possible_actions <- expand.grid('hor'=c(-1, 0, 1), 'ver'=c(-1, 0, 1))
  n_states <- length(which(!is.na(track)))  
  curr_velocity <- start_velocity
  # must be at least 16
  track_height <- 30
  # matrix indices of all start points..
  start_inds <- which(track == 0)
  # .. row column indices of all start points ..
  start_rc_inds <- lapply(start_inds, CMat2RC) %>% bind_rows
  # .. finish line matrix inds and RC inds .. 
  finish_inds <- which(track == 2)
  finish_rc_inds <- lapply(finish_inds, CMat2RC) %>% bind_rows()
  # .. and valid state inds and RC inds 
  state_inds <- which(!is.na(track))
  state_rc_inds <- lapply(state_inds, CMat2RC) %>% bind_rows()
  ############################################  
  
  # Implement early starts policy (p.99). no others yet implemented
  if (MC_policy == "Exploring Starts") {
    
    # Preallocate dataframe with, for each state (referenced by its matrix index representation) +
    # and all actions possible from that state, q_vals and pi.
    # Then episode by episode we'll add returns, derive a new q val and update policy.
    
    state_Q_pi_rets <- data.frame()
    for (s in seq_along(state_inds)) {
      this_row <- data.frame(
        'state'=state_inds[s], 
        poss_actions,
        # initialize arbitrary Q vals between 0 and 1 
        'Q_sa'=runif(nrow(possible_actions), -1, 1), 
        # initialize with equiprobable policy of acting in any state
        'policy'=1/nrow(possible_actions) 
      ) 
      state_Q_pi_rets <- rbind(state_Q_pi_rets, this_row)
    }
    
    ## Estimate q-vals and policies for each state by experiencing n episodes with MC-ES updates after each ep ##
    for (ep in seq_along(n_episodes)) {
      
      # preallocate a returns column for this episode
      state_Q_pi_rets[, paste0("ret_epi_", ep)] <- NA
      
      episode <- GenerateEpisode(state_Q_pi_rets,
                                 states_0=start_inds, 
                                 finish_inds, 
                                 track,
                                 policy)
      
      # Now that we have an episode, work through it backwards, revising Q-val and policy for every novel Q(s,a)
      
      G <- 0 # returns
      
      
      for (t in 1:(episode$time_steps-1)) {
        # cumulative returns to this point in the episode
        G <- gamma * G + reward[t+1]
        
        # ** 
        # search through episode to see if we've already added a return for this state-action pair
        # (if !) 
        # row to update
        update_row <- state_Q_pi_rets[state_Q_pi_rets$state == episode$state[t] &
                                        state_Q_pi_rets$hor == episode$hor_act[t] &
                                        state_Q_pi_rets$ver == episode$ver_act[t], ]
        # append G to returns df
        state_Q_pi_rets[state_Q_pi_rets$state == episode$state[t] &
                          state_Q_pi_rets$hor == episode$hor_act[t] &
                          state_Q_pi_rets$ver == episode$ver_act[t], ][grep(paste0('ret_epi_', ep)), names(update_row)] <- G
        
        # update the Q value
        state_Q_pi_rets[state_Q_pi_rets$state == episode$state[t] &
                          state_Q_pi_rets$hor == episode$hor_act[t] &
                          state_Q_pi_rets$ver == episode$ver_act[t], ]$Q_sa <- 
          sum(update_row %>% select(contains("ret_ep")), na.rm=TRUE)
        
        # reset the policy for the whole state to the max
        state_Q_pi_rets[state_Q_pi_rets$state == episode$state[t], ]$policy <-
          max() # **
        
      } # END TIME STEP LOOP
      
    } # END LOOP THROUGH EPISODES
    
  } # END CONDITIONAL FOR MC ES
  
  optimal_s0_policies #and other stuff
} 
#### HELPER FUNCTIONS ####

## General and Initialization Fxs ## 
CMat2RC <- function(mat_ind, nrows=track_height) {
  ### Convert from matrix index to row, column indices ###
  
  rc <- data.frame("col"=NA, "row"=NA)
  if (mat_ind <= nrows) {
    # then we're in first column
    rc["col"] <- 1
    rc["row"] <- mat_ind
  } else {
    rc["col"] <- ceiling(mat_ind/nrows)
    rc["row"] <- ifelse(mat_ind %% 30==0, 30, mat_ind %% nrows)
  }
  rc  
}
CRC2Mat <- function(rc, nrows=track_height) {
  ### Convert from row, column indices to matrix indices #
  mat_ind <- rc[2] * nrows + rc[1]
  mat_ind  
}
InitTrack <- function(track_height) {
  ### Initialize track where 0s represent starting states, 2s finish states inclusive those 
  # past boundary, 1 intermediate states, and NAs off track and non-finish #
  
  track <- matrix(1, track_height, 19)
  track[track_height, ] <- 0
  track[, 15:19] <- 2
  track[1:10, 1:5] <- NA
  track[11:12, 1:3] <- NA
  track[13:14, 1:2] <- NA
  track[15, 1:2] <- NA
  track[15:track_height, 11:19] <- NA
  
track    
}
## Fxs for Generating an Episode ##
### Randomly pick a starting state index (used to start each ep) ###
GenEpStartState <- function(start_inds) start_inds[round(runif(1, 1, length(start_inds)))]
UpdateEpisode <- function(t,
                          velocity,
                          curr_state,
                          curr_hor_act,
                          curr_ver_act) {
  
  ### Update episode this time step ###
  
  episode <- data.frame('states'=rep(NA, time_steps), 
                        'hor_actions'=rep(NA, time_steps), 
                        'ver_actions'=rep(NA, time_steps),
                        'rewards'=rep(NA, time_steps),
                        # track velocity for informational purposes
                        'velocity'=rep(NA, time_steps))
  
  episode$states[t] <- curr_state
  episode$hor_actions[t] <- curr_hor_act
  epsiode$ver_actions[t] <- curr_ver_act  
  episode$velocity[t] <- velocity
  
episode 
}
TransitionSAR <- function(t,
                          state_df,
                          track, # to find if actions-1|state-1 moved us is an intermed, off track or finish state
                          curr_state,
                          curr_velocity,
                          state_inds,
                          starting_state,
                          noise_velocity=0
) {
  
  ###  Returns next SAR and velocity ###
  
  # Get row, column representation of state
  curr_state_rc <- curr_state_rc <- CMat2RC(curr_state) 
  
  # Generate action and current velocity
  # indicator to increment velocity 0 irrespective action cohosen
  incr_vel_0 <- ifelse(runif(1, 0, 1) < noise_velocity, 1, 0)
  
  if (starting_state == 1) {
    hor_av <- SelActIncVel("hor", curr_velocity$hor, t, incr_vel_0, state_df)
    ver_av <- SelActIncVel("ver", curr_velocity$ver, t, incr_vel_0, state_df)
    curr_actions <- list("curr_hor_act"=hor_av$a, "curr_ver_act"=ver_av$a)
    curr_velocity <- list("curr_hor_vel"=hor_av$v, "curr_ver_vel"=ver_av$v)
    # velocity
  } else {
    # enforce constraint that at least 1 velocity is greater than 0 when we're
    # not in the start state
    while (all(unlist(curr_velocity)==0)) {
      hor_av <- SelActIncVel("hor", curr_velocity$hor, t, state_df)
      ver_av <- SelActIncVel("ver", curr_velocity$ver, t, state_df)
      curr_actions <- list("curr_hor_act"=hor_av$a, "curr_ver_act"=ver_av$a)
      curr_velocity <- list("curr_hor_vel"=hor_av$v, "curr_ver_vel"=ver_av$v)
    }
  }
  
  # Use current velocity to find s'
  # Recalculate the matrix indices ..
  state_inds <- which(!is.na(track))
  # .. and row, column indices of all states
  state_rc_inds <- lapply(state_inds, CMat2RC) %>% bind_rows
  # indicator for if the row then col are on the track
  ot_row <- curr_state_rc$row + curr_velocity$curr_hor_vel %in% state_rc_inds$row
  ot_col <- curr_state_rc$col + curr_velocity$curr_ver_vel %in% state_rc_inds$col
  
  # ** don't think this is right because we want to include going off grid = -1 reward plus
  # go to starting state. for this should fxalize starting state
  # if the putative state transition is on the track find the locations of the next row and column
  if (ot_row & ot_col) {
    next_row <- curr_state_rc$row + curr_velocity$curr_hor_vel
    next_col <- curr_state_rc$col + curr_velocity$curr_ver_vel
  } else if (ot_row | ot_col) {
    # if only one is on track, find its row and column   
    if (!ot_row) next_row <- curr_state_rc$row + curr_velocity$curr_hor_vel
    if (!ot_col) next_col <- curr_state_rc$col + curr_velocity$curr_ver_vel
  }
  # for any not on track, set to max boundary
  if (!ot_col) next_row <- max(state_rc_inds$col)
  if (!ot_row) next_col <- max(state_rc_ind$row)  
  
  next_state <- list("r"=next_row, "c"=next_col)
  
  # Determine reward and if state transition is to a terminal state
  state_type <- track[next_row, next_col]
  
  # finish state
  if (state_type == 2) { reward <- 0; terminal <- 1 }
  # intermed state of start state
  if (state_type == 1 | state_type == 0) { reward <- -1; terminal <- 0 }
  
  list("ns"=next_state, "nha"=next_hor_act, "nva"=next_ver_act, "rew"=reward, "vel"=velocity, "T"=terminal)   
}
SelActIncVel <- function(dir_as_string, 
                         velocity_this_dir,
                         t,
                         state_df,
                         incr_vel_0=0) {
  
  ### Select horizontal / vertical action and increment velocity based on policy 
  # and s.t. constraints (see below) ###
  
  # velocity in any direction be nonnegative and can't exceed 5 and both velocities can't be 0  for
  # any state other than a starting state ###
  browser()
  # Find cumulative probabilities of the 3 actions
  cum_probs <- cumsum(state_df$policy)
  
  # Action selected is the first greater than a random number between 0 and 1
  action <- state_df %>% select(dir_as_string)
  hor_act <- actions[which(cum_probs > runif(1, 0, 1))[1], ]$hor
  ver_act <- actions[which(cum_probs > runif(1, 0, 1))[1], ]$ver
  
  # Only change velocity if this indicator set to 0
  if (!incr_vel_0) {
    velocity_this_dir <- velocity_this_dir + action
    
    # Apply constraints:
    # must be nonnegative..
    if (velocity_this_dir < 0) { action <- 0; velocity_this_dir <- 0 } 
    # .. and less than 5
    if (velocity_this_dir > 5) { action <- 0; velocity_this_dir <- 5 } 
  } 
  
  list("a"=action, "v"=velocity_this_dir)   
}
GenerateEpisode <- function(
  state_Q_pi_rets,
  states_0, 
  finish_inds,
  track, # track in which to find  indices of subsequent states 
  # amount of softness to distribute over the first action
  first_act_softness=.05, 
  noise_velocity=.1, # if nonzero, probability of adding noise to the velocity
  quiet=0
) {
  
  ### Returns episode with SARs, velocities (fyi only), and how long it took to reach finish line ###
  
  # Set current state and actions to S_0 and A_0s
  t <- 1 # time step
  # Pick current state randomly from set of starting states
  curr_state <- states_0[runif(1, 0, length(states_0))]
  # Initialize velocity at 0
  curr_velocity <- list("hor"=0, "ver"=0) 
  
  state_df <- state_Q_pi_rets %>% filter(state == curr_state)
  
  # If there's a unique max, soften the first action so we always have some prob of picking any action
  p <- state_Q_pi_rets %>% filter(state == curr_state) %>% select("policy")
  m <- which(p == max(p)) # max ind or inds
  
  if (length(m) == 1) {
    # decrease the prob of the max
    p[m] <- p[m] - first_act_softness * length(p[setdiff(m, 1:length(p))])
    # .. and increase the probs of the others
    p[setdiff(m, 1:length(p))] <- p[setdiff(m, 1:length(p))] + first_act_softness
    stop(sum(p) != 1) # break if probs don't sum to 1
  }
  
  # ** don't think need this here bc it happens inside SelAct..
  #actions <- state_Q_pi_rets %>% filter(state == curr_state) %>% select(c("hor", "ver"))
  # .. and the following picks the first action pair from this row with cum. prob > a rand # in [0,1]
  #curr_hor_act <- actions[which(cumsum(p) > runif(1, 0, 1))[1], ]$hor
  #curr_ver_act <- actions[which(cumsum(p) > runif(1, 0, 1))[1], ]$ver

  # Continue generating episode until reach the finish line!
  reach_finish_line <- 0
  while (reach_finish_line == 0) {
  
    # Get s', actions, reward, velocities, and if it s' is finish line
    SpAsRVsFl <- TransitionSAR(t,
                               state_df,
                               track, 
                               curr_state,
                               curr_velocity,
                               starting_state,
                               noise_velocity
                               
    )
    
    # the episode time steps continue until the finish line is reached
    t <- t+1
  }
  
  episode <- UpdateEpisode(t, velocity, curr_state, curr_hor_act, curr_ver_act, episode)
  
  # extract all the current state information
  state_df <- state_Q_pi_rets %>% filter(state == episode$curr_state)
  
  episode   
}
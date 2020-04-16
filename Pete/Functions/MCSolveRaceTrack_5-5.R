MCSolveRaceTrack <- function(
  track_height=30,
  gamma=.9,
  n_episodes=50,
  how_to_learn="Exploring Starts",
  quiet=0
) {
  ### Solve the racetrack problem with various Monte Carlo methods (so far just exploring starts)
  
  # Returns an optimal policy fo each starting state ** + other stuff ###

  ############################################    
  #### INITIALIZATIONS ####
  # must be at least 16
  track_height <- 30
  track <- InitTrack(track_height)
  # vertical and horizontal current velocities (can never exceed 5 or both be 0 except at start)
  start_velocity <- data.frame('hor'=0, 'ver'=0)
  # matrix representation of action set (actions correspond to velocity increments)
  poss_actions <- expand.grid('hor'=c(-1, 0, 1), 'ver'=c(-1, 0, 1))
  n_states <- length(which(!is.na(track)))  
  curr_velocity <- start_velocity
  # matrix indices of all start points..
  start_inds <- which(track == 0)
  # .. row column indices of all start points ..
  start_rc_inds <- lapply(start_inds, function(x) CMat2RC(x, nrows=track_height)) %>% bind_rows
  # .. finish line matrix inds and RC inds .. 
  finish_inds <- which(track == 2)
  finish_rc_inds <- lapply(finish_inds, function(x) CMat2RC(x, nrows=track_height)) %>% bind_rows
  # .. and valid state inds and RC inds 
  state_inds <- which(!is.na(track))
  
  state_rc_inds <- lapply(state_inds, function(x) CMat2RC(x, nrows=track_height)) %>% bind_rows
  ############################################  
  
  # Implement MC early starts policy (p.99). no others yet implemented
  if (how_to_learn == "Exploring Starts") {
    
    # Preallocate dataframe with, for each state (referenced by its matrix index representation) +
    # and all actions possible from that state, q_vals and pi.
    # Then episode by episode we'll add returns, derive a new q val and update policy.
    state_Q_pi_rets <- data.frame()
    for (s in seq_along(state_inds)) {
      this_row <- data.frame(
        'state'=state_inds[s], 
                poss_actions,
        # initialize arbitrary Q vals between 0 and 1 
        'Q_sa'=runif(nrow(poss_actions), -1, 1), 
        # initialize with equiprobable policy of acting in any state
        'policy'=1/nrow(poss_actions) 
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
## Fxs for Generating an Episode ##
GenerateEpisode <- function(
  state_Q_pi_rets,
  states_0, 
  finish_inds,
  track, # track in which to find  indices of subsequent states 
  first_act_softness=.05, # amount of softness to distribute over the first action 
  noise_velocity=.1, # if nonzero, probability of adding noise to the velocity
  quiet=0
) {
  
  ### Returns episode with SARs, velocities (fyi only), and how long it took to reach finish line ###
  
  # Set current state and actions to S_0 and A_0s
  t <- 1 # time step
  # Pick current state randomly from set of starting states
  
  curr_state <- states_0[runif(1, 0, length(states_0))]
  
  # Initialize velocity at 0
  curr_velocity <- rep(0, 2)
  
  # Full repr of current state items
  state_df <- state_Q_pi_rets %>% filter(state == curr_state)
  
  # Continue generating episode until reach the finish line!
  reach_finish_line <- 0
  # Initialize empty vectors of the vars that constitute an episode
  ep_vecs <- c("state", "hor_act", "ver_act", "velocity", "t_step")
  for (ev in seq_along(ep_vecs)) assign(ep_vecs[ev], vector())
  
  while (reach_finish_line == 0) {
    
    # Get s', actions, reward, velocities, and if it s' if we're at finish line
    SpAsRVsFl <- TransitionSAR(t,
                               state_df,
                               track, 
                               curr_state,
                               curr_velocity,
                               state_inds,
                               first_act_softness,
                               noise_velocity
                               
    )
    
    t <- t+1
    
    # Extract 
    curr_velocity <- SpAsRVsFl[["vel"]]
    # Store vars in ep vectors
    t_step <- c(t_step, t)
    curr_state <- CRC2Mat(as.numeric(unlist(SpAsRVsFl[["ns"]])), nrow(track)) 
    state <- c(state, curr_state) # store the matrix repr
    hor_act <- c(hor_act, SpAsRVsFl[["nha"]])
    ver_act <- c(ver_act, SpAsRVsFl[["nva"]])
    velocity <- c(velocity, curr_velocity)
    
    reach_finish_line <- as.numeric(SpAsRVsFl[["T"]])
    
    cat("\n curr state"); print(curr_state)
    if (!quiet) cat("\n t", t, "\nstate", curr_state, 
                    "\nacts", c(hor_act, ver_act), 
                    "\nvelocity", curr_velocity, "\nTerminal", reach_finish_line )
    
  }
  
  episode <- data.frame(t_step, state, hor_act, ver_act, velocity)
  
episode   
}
TransitionSAR <- function(t,
                          state_df,
                          track, # to find if actions-1|state-1 moved us is an intermed, off track or finish state
                          curr_state,
                          curr_velocity,
                          state_inds,
                          first_act_softness,
                          noise_velocity=0
) {
  
  ###  Returns next SAR and velocity ###
  
  # Get row, column representation of state
  
  curr_state_rc <- CMat2RC(curr_state, nrows=nrow(track)) 
  
  # Generate action and current velocity
  # indicator to increment velocity 0 irrespective action cohosen
  incr_vel_0 <- ifelse(runif(1, 0, 1) < noise_velocity, 1, 0)
  
  # Exploring starts: use a soft policy on action 1
  if (t == 1) {
    
    # If there's a unique max, soften the action so we always have some prob of picking any action
    p <- state_df %>% select("policy")
    m <- which(p == max(p)) # max ind or inds
    
    if (length(m) == 1) {
      # decrease the prob of the max
      p[m] <- p[m] - first_act_softness * length(p[setdiff(m, 1:length(p))])
      # .. and increase the probs of the others
      p[setdiff(m, 1:length(p))] <- p[setdiff(m, 1:length(p))] + first_act_softness
      stop(sum(p) != 1) # break if probs don't sum to 1
    }
    
    # Replace the policy with the soft one
    state_df$policy <- p
    a_and_v <- SelActIncVel(t, state_df, curr_velocity, incr_vel_0)
    curr_actions <- as.numeric(a_and_v[["a"]])
    curr_velocity <- as.numeric(a_and_v[["v"]])
    # velocity
  } else {
    a_and_v <- SelActIncVel(t, state_df, curr_velocity, incr_vel_0)
    curr_actions <- as.numeric(a_and_v[["a"]])
    curr_velocity <- as.numeric(a_and_v[["v"]])
    # enforce constraint that at least 1 velocity is greater than 0 when we're not in the start state
    while (all(curr_velocity==0)) {
      a_and_v <- SelActIncVel(t, state_df, curr_velocity, incr_vel_0)
      curr_actions <- as.numeric(a_and_v[["a"]])
      curr_velocity <- as.numeric(a_and_v[["v"]])
    }
  }
  
  # Use current velocity to find s'
  # Recalculate the matrix indices ..
  state_inds <- which(!is.na(track))
  # .. and row, column indices of all states
  state_rc_inds <- lapply(state_inds, function(x) CMat2RC(x, nrows=nrow(track))) %>% bind_rows
  
  temp_curr_state_rc <- CMat2RC(curr_state, nrow(track))
  
  unique_rows <- 1:nrow(track)
  unique_cols <- 1:ncol(track)
  # indicator for if the row then col are on the track
  browser(expr=curr_state > 600)
  ot_row <- ifelse((temp_curr_state_rc$row + curr_velocity[1]) %in% unique_rows, 1, 0)
  ot_col <- ifelse((temp_curr_state_rc$col + curr_velocity[2]) %in% unique_cols, 1, 0)
  
  # preallocate RC format next state
  next_state <- rep(NA, 2) 
  
  # if the state transition is on the track..
  if (ot_row & ot_col) {
    # .. find the locations of the next row and column
    next_state <- c(temp_curr_state_rc$row + curr_velocity[1], 
                    temp_curr_state_rc$col + curr_velocity[2])
    
    # and assign reward
    reward <- -1; terminal <- 0 
  } else {
  # if at least one state transition if off the track
    next_state <- rep(NA, 2) # preallocate next state
    # if only one is on track, find its row and column   
    if (ot_row) next_state[1] <- temp_curr_state_rc$row + curr_velocity[1]
    if (ot_col) next_state[2] <- temp_curr_state_rc$col + curr_velocity[2]
    # for any not on track, set to max boundary
    if (!ot_row) next_state[1] <- max(state_rc_inds$row)
    if (!ot_col) next_state[2] <- max(state_rc_inds$col)  
    
    # convert to state index
    next_state_ind <- CRC2Mat(next_state, nrow(track))
    
    if (is.na(track[next_state_ind])) {
      # get start inds again
      start_inds <- which(track==0)
      # and pick one of them
      next_state_tmp <- GenEpStartState(start_inds)
      next_state <- CMat2RC(next_state_tmp, nrow(track))
      # go back to start :/ 
      reward <- -1; terminal <- 0
    } else if (track[next_state_ind] == 2) {
      # reached finish line! 
      reward <- 0; terminal <- 1
    } else {
      browser()
     stop("ERROR in transition SAR: should have exhausted conditionals before this.")
    }
  }
    
list("ns"=next_state, "nha"=curr_actions[1], "nva"=curr_actions[2], "rew"=reward, "vel"=curr_velocity, "T"=terminal)   
}
SelActIncVel <- function(t,
                         state_df,
                         curr_velocity,
                         incr_vel_0=0) {
  
  ### Select horizontal / vertical action and increment velocity based on policy 
  # and s.t. constraints (see below) ###
  
  # Velocity in any direction: 
  # 1 must be nonnegative 
  # 2 can't exceed 5 
  # 3 both velocities can't be 0 for any state other than a starting state ###
  
  # Find cumulative probabilities of the 9 actions
  cum_probs <- cumsum(state_df$policy)
  
  # Action selected is the first greater than a random number between 0 and 1
  actions <- state_df %>% select(c("hor", "ver"))
  
  tmp_sel_actions <- expand.grid('hor'=c(-1, 0, 1), 
                                 'ver'=c(-1, 0, 1))[which(cum_probs > runif(1, 0, 1))[1] , ]
  # find what the velocity would be if not for constraints
  tmp_new_vel <- as.numeric(curr_velocity) + as.numeric(unlist(tmp_sel_actions))

  # Only change velocity if this indicator set to 0
  if (!incr_vel_0) {
    
    for (vel in seq_along(tmp_new_vel)) {
      # Apply constraints:
      # must be nonnegative..
      if (tmp_new_vel[vel] < 0) { tmp_sel_actions[vel] <- 0; tmp_new_vel[vel] <- 0}
      # .. and less than 5
      if (tmp_new_vel[vel] > 5) { tmp_sel_actions[vel] <- 0; tmp_new_vel[vel] <- 5 } 
    }
  }

velocity <- tmp_new_vel
actions <- tmp_sel_actions
    
list("a"=actions, "v"=velocity)   
}
### Randomly pick a starting state index (used to start each ep) ###
GenEpStartState <- function(start_inds) start_inds[round(runif(1, 1, length(start_inds)))]
## General and Initialization Fxs ## 
CMat2RC <- function(mat_ind, nrows) {
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
CRC2Mat <- function(rc, nrows) {
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
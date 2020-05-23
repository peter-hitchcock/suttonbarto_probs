RunTDNStep <- function(world_params,
                       agent_params,
                       controL_params) {
  ### Runs TD n-step, currently set up just to do policy eval not control###
  
  ########################### HELPER FUNCTIONS #########################
  RetAction <- function(pol_s) {
    ### Takes in policy for the current state (val_pol) and returns a random action #
    # proportional to probability given by policy ###
    
    # Take the first action for which the cumulative probablity is greater than a random # from 0 to 1
    action <- pol_s[which(cumsum(pol_s$policy) > runif(1, 0, 1))[1], "action"]
    
    action  
  }
  ######################################################################
  # Notes: Following pseudocode, we now have gamma, alpha, policy, and arbitrarily initialized V(s) for all s in S  ###
  for (ep in 1:n_episodes) {
    
    if (!quiet) { cat("\n *********** NEW EPISODE ********* "); pause(2) }
    
    # Start randomly anywhere but the terminal state (first line in pseudocode after episode start)
    start_state <- ceiling(runif(1, 1, how_long-1)) 
    if(!quiet) cat("\n Starting in state", start_state)
    big_T <- 1e100 # Set T to a huge number
    
    # Not in pseudocode, but I assume we need to assign state to start state
    state <- start_state
    
    # Time step incrementer 
    t <- 0
    
    while (tau != big_T-1) {
      # Preallocate rewards and states this time step = to n states mod 1
      # note: don't understand indexing in these so starting with a big vector
      reward_t <- rep(NA, 100)        # how_long/(n+1) 
      state_t <-  rep(NA, 100)       #how_long/(n+1)
      
      ## First conditional
      if (t < big_T) { # First line under time step line pseudocode
        if(!quiet) cat("\n T is", big_T, "which is bigger than t", t, "\n **entering first conditional**")
        # Take action according to policy..
        action <- RetAction(policy %>% filter(state == state)) 
        next_state <- state+action # .. observe next state
        # Keep at end if action would take us off track
        if (next_state > how_long) next_state <- how_long
        if (next_state < 1) next_state <- 1
        if(!quiet) cat("\n Action is", action, "taking us to next state", next_state)
        
        # Store next state and reward
        reward_t[t] <- rew_locs[next_state]
        state_t[t] <- next_state
        
        if(!quiet) cat("\n Storing next reward", reward_t, "and state", state_t, 
                       "\n in their vectors")
        
        # Change big T if this if we hit the terminal state
        if (next_state == terminal) {
          big_T <- t+1
          if(!quiet) cat("\n This is the terminal state \n T assigned", t+1)
        } 
      } # END if t < T
      
      ## Assign tau: the time whose state estimate is being updated
      tau <- t-n+1
      if(!quiet) cat("\n **Outside first conditonal** \n t is", t, " \n Assign tau:", tau)
      
      ## Second conditional
      if (tau >= 0) {
        # i in sum
        G <- c()
        # Find the part of G that's like Monte Carlo
        seq_for_mc <- tau+1:min(tau+n, big_T)
        if(!quiet) cat("\n i range =", seq_for_mc)
        for (i in seq_for_mc) {
          rew_this_step <- reward[i]
          gamma_this_step <- gamma^(i-tau+1)
          G <- c(G, rew_this_step*gamma_this_step)
        }
        if(!quiet) cat("\n Monte Carlo-like portion of G =", G)
        ## In case of third conditional, the return G is a mixture of the MC-like portion + the discounted 
        # state value tau+n steps from here
        if (tau + n < big_T) {
          cached_value <- gamma^n * 
            # Pull V(S_{tau+n})
            state_vals %>% filter(state == vec_world[tau+n]) %>% select(value)
          G <- G + cached_value
          if(!quiet) cat("\n tau + n is ", tau+n, "which is less than T", big_T, "\n . We add to G
                        the cached value discounted V(S_{tau+n}", cached_value, 
                         "\n Now G =", G)
        } 
        
        # Update the state value at this time step as the discrepancy between its previous value and G
        state_vals[tau] <- state_vals[tau, "value"] + alpha * (G - state_vals[tau]) 
        cat("\n The V(S_tau) update is") print(state_vals[tau, "value"]); cat("+"); print(alpha * (G - state_vals[tau]))
      } # End if tau >0 conditional
      if(!quiet) cat("\n ** Outside second conditional **", )
      
      t <- t+1  
      # This is not in pseudocode, but I think we must now go to next state.. ?
      state <- next_state
      if(!quiet) cat("\n State is now", state)
    } # END 
    
    if (!quiet) cat("# \n ** END TIME STEP LOOP ** 
                    \n Tau is", tau, 
                    "n T is", big_T)
    
  } # END episode loop
  
}
RunGamblersProblem <- function(
                               p_win, # probability of winning bet
                               states, # = capital amounts
                               state_values, # iteratively updated state values
                               theta=1e-3, # iteration stopping criterion
                               gamma=1, # discount on V(s)'
                               quiet=1 # print?
                               ) {
  
  ### Returns optimal policy and value fx (policy and value for each state) #
  
  # 4.9 Gambler's Problem from Sutton & Barto 2nd ed p. 84 ###
  
  #### CREATE HELPER FUNCTIONS ####
  FindBestActionAndValueThereof <- function(state,
                                            state_values, # the current values for all states
                                            gamma=1, # discount on V(s')
                                            p_win=.5) {
    
    ### Finds the best action (or actions) and the value(s) thereof #
    
    # Implements right side of pseudocode for V(s) update in value iter box p. 83 ###
    
    # find all possible stakes
    stakes <- 1:min(state, 100-state)
    
    # get all the outcomes from this state
    outcome_list <- CalcOutcomeAllStakes(state,
                                         stakes,
                                         p_win)
    
    # preallocate vector of action results (= length of actions possible from this state)
    action_expectations <- rep(NA, length(outcome_list)) 
    
    # iterate through the outcomes to find the value of all possible actions
    for (outcome in 1:length(outcome_list)) {
      
      this_outcome <- outcome_list[[outcome]]  
      
      # first find the reward expectation for this state:
      # (since the only state yielding non-zero reward is the winning terminal 
      # state we just need to look in column 4)
      reward_expectation <- this_outcome[4, ]$prob * this_outcome[4, ]$reward
      
      # now find the expected values of the non-terminal s_primes
      s_primes <- data.frame(this_outcome[1:2, ] %>% filter(probs > 0))
      
      if (nrow(s_primes) == 0) { 
        v_of_s_prime_expectations <- 0
      } else {
        v_of_s_prime_expectations <- rep(NA, nrow(s_primes))
        
        for (sp in 1:nrow(s_primes)) v_of_s_prime_expectations[sp] <- 
            gamma * s_primes$prob[sp] * state_values[s_primes$s_prime[sp]+1]
      }
      
      # calculate the total expectations
      action_expectations[outcome] <- reward_expectation + sum(v_of_s_prime_expectations)
      
    } # end outcomes iter
   # browser(expr=state==50)
    # pass out the best stakes (ie actions) and their values
    max_inds <- which(action_expectations == max(action_expectations))
    
    best_actions_and_values <- data.frame('best_action'=stakes[max_inds], # ie stake
                                          'value'=max(action_expectations))
    
  best_actions_and_values    
  }
  CalcOutcomeAllStakes <- function(state,
                                   stakes,
                                   p_win) {
    
    ### Returns a list of outcomes for all actions ie stakes possible from 
    # the current state, state_in ###
    
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
                                          p_win,
                                          terminal_lose_criterion=0,
                                          terminal_win_criterion=100) {
    
    ### Returns values and probabilities of 5 possible s',r pairs #
    
    # Notes: Each state can result in one of 5 (s',r) pairs:
    
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
  #### FIND OPTIMAL POLICY AND VALUE FX THROUGH VALUE ITERATION ####
  ############################################
  
  # if (any(delta_vec > theta)) {
  #   
  # } else {
  #   # value fx
  #   
  #   # policy output
  #   
  # }
  
  # initialize vector of changes from old to current state value
  delta_vec <- 1e5
  
  # iterate until these drop below theta
  while(any(delta_vec > theta)) {
    
    delta_vec <- rep(0, length(states[2:100]))
    # iterate through the non-terminal states 
    for (state in states[2:100]) {
      
      if (!quiet) cat('\n State:', state)
      
      # pull the state's old value before updating 
      old_v_of_s <- state_values[state]
      
      action_value <- FindBestActionAndValueThereof(state,
                                                    state_values, 
                                                    gamma,
                                                    p_win)
      
      delta_vec[state] <- abs(old_v_of_s - action_value$value[1])
      
      if (!quiet) cat('\n V(s) =', action_value$value, '. Old value was ', old_v_of_s,
           '\n Abs. difference =', delta_vec[state],
            '. \n Optimal stake =', action_value$best_action)
      
    }
  }
  
  ############################################
}
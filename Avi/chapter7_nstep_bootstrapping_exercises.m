% chapter 7 exercise 7.2
clear all

% make some states
states = 1:50;

% terminal state
termstate = 25;

% step size parameter
alpha = 0.01;

% discount factor
gamma = 0.9;

% number of steps to boot
n = 5;

% number of episodes
numeps = 5000;

% my policy is to walk to the terminal state and get a reward. I always
% choose an action that takes me to a higher value state, but sometimes
% don't with some epsilon
epsilon = 0.1;

% ## SETTINGS for problems #####

% setting to use observed reward or sum of PEs in error term
obserror = false;

% setting for importance sampling
importance_sampling = false;

% setting for importance sampling algorithm to use
weight_per_decision = false;

% set up steps
steps = zeros(numeps,1);

% initialize state values if not provided
V = zeros(length(states),1);

% run an outer loop to get new state values for each episode and then try
% walking through the maze with a pre-determined state and without decision
% noise to see how many steps it takes to get to the terminal state
for eps = 1:numeps
    %  Run code for training
    [V] = walking_around_without_a_purpose(states,termstate,alpha,gamma,epsilon,n,1,obserror,importance_sampling,weight_per_decision,V,[]);
    % get number of steps taken with epsilon = 0 and set V + start state
    [~,steps(eps)] = walking_around_without_a_purpose(states,termstate,alpha,gamma,0,n,1,obserror,importance_sampling,weight_per_decision,V,1);
end


function [V,steps_taken] = walking_around_without_a_purpose(states,termstate,alpha,gamma,epsilon,n,numeps,obserror,importance_sampling,weight_per_decision,V,state_t)
% learning algorithm with n-step bootstrapping


% initialize steps taken
steps_taken = 0;


for e = 1:numeps
    if isempty(state_t)
        %       initialize in a random state if not provided
        state_t = randi(length(states));
        while state_t == termstate
            state_t = randi(length(states));
        end
    end
    
    %     initialize time steps
    t = 0;
    R = -1;
    S = state_t;
    action_prob_policy = 0;
    rho=1;
    % big T
    T = inf;
    % initialize tau
    tau = NaN;
    
    %     make a fixed state-value matrix that only changes every episode
    Vf = V;
    
    %     time step loop - go until we hit the terminal state
    while tau ~= T-1
        %         increment time step
        t = t + 1;
        G = 0;
        if t < T
            %       here are the states we would go if we go left or right
            if state_t == 1
                leftstate = 1;
            else
                leftstate = state_t-1;
            end
            if state_t == length(states)
                rightstate = length(states);
            else
                rightstate = state_t+1;
            end
            %       roll the dice to decide if we are going to be greedy or explore
            %         choose an action depending on whether right or left state is
            %         better
            if rand > epsilon
                if V(rightstate) > V(leftstate)
                    state_t = rightstate;
                    action_prob_policy(t) = 1;
                elseif V(rightstate) < V(leftstate)
                    state_t = leftstate;
                    action_prob_policy(t) = 1;
                elseif V(rightstate) == V(leftstate)
                    action_prob_policy(t) = 0.5;
                    if rand < 0.5
                        state_t = rightstate;
                    else
                        state_t = leftstate;
                    end
                end
            else
                %         for epsilon actions, choose a random action
                if rand < 0.5
                    state_t = rightstate;
                else
                    state_t = leftstate;
                end
                action_prob_policy(t) = 0.5;
            end
            %         check if we're at the terminal state, if not, rew = -1;
            if state_t == termstate
                R(t+1) = 0;
                T = t+1;
            else
                R(t+1) = -1;
            end
            %         save the state we moved into
            S(t+1) = state_t;
        end
        %         update tau to see if we've had enough time steps to update
        tau = t - n + 1;
        %         update values if we have passed enough time steps
        if tau >= 1
            %       mini loop to compute expected gains based on experienced
            %       rewards
            for i = tau+1:min(tau+n,T)
                %         importance sampling ratio
                if importance_sampling == true && i <= T-1 && i <= tau+n-1
                    if weight_per_decision == false
                        rho(i) = prod(action_prob_policy./0.5);
                    else
                        rho(i) = action_prob_policy(i)./0.5;
                    end
                else
                    rho(i) = 1;
                end
                %                 add discounted rewards to G
                G = G + (gamma^(i-tau-1))*R(i);
            end
            %             add discounted future expectation to G
            if tau + n < T
                %                 use changing value expectations
                if obserror == true
                    G = G + (gamma^n)*V(S(tau+n));
                else
                    %                     use fixed value expectations (sum of TD errors)
                    G = G + (gamma^n)*Vf(S(tau+n));
                end
            end
            %             update V(S-tau) based on past values
            V(S(tau)) = V(S(tau)) + rho(tau)*alpha * (G - V(S(tau)));
        end
    end
    %     save number of time steps per episode
    steps_taken(e) = t;
end
end

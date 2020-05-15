% chapter 7 exercise 7.2

% make some states
states = 1:50;

% terminal state
tstate = 25;

% initialize state values
V = zeros(length(states),1);

% step size parameter
alpha = 0.01;

% discount factor
gamma = 0.9;

% number of steps to boot
n = 5;

% number of episodes
numeps = 1000;

% my policy is to walk to the terminal state and get a reward. I always
% choose an action that takes me to a higher value state, but sometimes
% don't with some epsilon
epsilon = 0;

% ## SETTINGS for problems #####

% setting to use observed reward or sum of PEs in error term
obserror = true;

% setting for importance sampling
importance_sampling = false;

% setting for importance sampling algorithm to use
weight_per_decision = false;

%  ####

% initialize steps taken
steps_taken = 0;

% learning algorithm with n-step bootstrapping
for e = 1:numeps
%     initialize in a random state
    state = randi(length(states)-1);
%     initialize time steps
    t = 0;
    R = -1;
    S = state;
    action_prob_policy = 0;
    rho=0;
    % big T 
    T = inf;
%     time step loop - go until we hit the terminal state
    while tau ~= T-1 && state ~= tstate
        %         increment time step
        t = t + 1;
        G = 0;
%       here are the states we would go if we go left or right
        if state == 1
            leftstate = 1;
        else
            leftstate = state-1;
        end
        if state == length(states)
            rightstate = length(states);
        else
            rightstate = state+1;
        end
%       roll the dice to decide if we are going to be greedy or explore
%         choose an action depending on whether right or left state is
%         better
    if rand > epsilon
        if V(rightstate) > V(leftstate)
            state = rightstate;
            action_prob_policy(t) = 1;
        elseif V(rightstate) < V(leftstate)
            state = leftstate;
            action_prob_policy(t) = 1;
        elseif V(rightstate) == V(leftstate)
            action_prob_policy(t) = 0.5;
            if rand < 0.5
                state = rightstate;  
            else
                state = leftstate;
            end
        end
    else
%         for epsilon actions, choose a random action
        if rand < 0.5
            state = rightstate;
        else
            state = leftstate;
        end
    end
%         check if we're at the terminal state, if not, rew = -1;
        if state == tstate
            R(t+1) = 0;
            T = t+1;
        else
            R(t+1) = -1;
        end
%         save the state we moved into
        S(t+1) = state;
%         update tau to see if we've had enough time steps to update
        tau = t - n + 1; 
%         update values if we have passed enough time steps
        if tau >= 1
%       mini loop to compute expected gains based on experienced
%       rewards
            for i = tau+1:min(tau+n,T)
                %         importance sampling ratio
                if importance_sampling == true && i <= T-1
                    if weight_per_decision == false
                        rho(i) = prod(action_prob_policy./0.5);
                    else
                        rho(i) = action_prob_policy(i)./0.5;
                    end
                else
                    rho(i) = 1;
                end
                if obserror == true
                    G = G + (gamma^(i-tau-1))*R(i);
                else
                    G = G + (gamma^(i-tau-1))*(R(i)-V(S(i)));
                end
            end
            if tau + n < T
                G = G + (gamma^n)*V(S(tau+n));
            end
%             update V(S-tau) based on past values
           V(S(tau)) = V(S(tau)) + rho(tau)*alpha * (G - V(S(tau)));
        end     
    end
%     save number of time steps per episode
    steps_taken(e) = t;
end
    

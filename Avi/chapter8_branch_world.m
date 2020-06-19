%% Initialize a randomly generated task, along with model and Q values
clear all

% chapter 8
% exercise 8.8

% tabular updating w/ uniform or w/ on-policy distribution


% set up number of actions possible at a given state
num_actions = 2;

% set branching factor
b = 1;

% set discounting factor (un-discounted in example)
gamma = 1;

% policy noise (greedy)
epsilon = 0.1;

% probability of everything coming to an end no matter what
termprob = 0.1;

% set up state space as a cell array where layers are on rows and inner
% content of cells maps actions to next states in deeper layers

% also initialize Q(l,s,a) and Model(l,s,a,s') with number of states determined by
% branching factor, number of actions and depth of layers

% loop through a bunch of randomly generated tasks
num_tasks = 200;

for task = 1:num_tasks
    
    % initialize number of starting states
    num_states = 100;
    % loop down layers of the tree
    %     loop through states in each layer
    for k = 1:num_states
        %   loop through actions at each state
        %         initial arbitrary policy in each state
        policy(k) = randi(2);
        for a = 1:num_actions
            states(k,a,:) = randi(num_states,1,b);
            Q(k,a) = 0;
            Model_states(k,a,:) = zeros(1,num_states);
            Model_rewards(k,a,:) = zeros(1,num_states);
            %         make a count for observations of transitions so we can use it in
            %         the denominator for model
            Model_obs(k,a) = 0;
            %             initialize rewards for each transition
            rewards(k,a,:) = randn(1,b);
        end
    end
    
    %% Walk around the branch world and things happen
    % initialize num updates
    updates = 0;
    % info for how often to store new updates and values
    k = 0;
    klast = 0;
    kfreq= 10;
    % set criteria for number of episodes to terminate on
    numeps = 1000;
    % set number of simulations
    num_simulations=1;
    % set how many times we run simulations in the episode
    for e = 1:numeps
        termstate = false;
        %     always start from this state
        curr_state = 1;
        while termstate == false
            %         find out if we are at the end of the road
            if rand < termprob
                termstate = true;
            else
                %         make a decision about where to go with noisy policy
                if rand < epsilon
                    curr_action = randi(2);
                else
                    curr_action = policy(curr_state);
                end
                %             choose transition for action from equiprobable branches
                curr_branch = randi(b,1);
                
                %             find out where we are going
                next_state = states(curr_state,curr_action,curr_branch);
                %             find out what we got
                R = rewards(curr_state,curr_action,curr_branch);
                
                %            update the model
                Model_states(curr_state,curr_action,next_state) = Model_states(curr_state,curr_action,next_state)+1;
                Model_rewards(curr_state,curr_action,next_state) =  R;
                Model_obs(curr_state,curr_action) = Model_obs(curr_state,curr_action)+1;
                
                %            tabular update of Q
                states_hat = squeeze(Model_states(curr_state,curr_action,next_state)./Model_obs(curr_state,curr_action));
                maxQa = max(Q(next_state,:),[],2);
                Q(curr_state,curr_action) = sum(states_hat .* [R + gamma * maxQa]);
                
                %             update number of updates
                updates = updates + 1;
                
                %             run simulations to make some updates
%                 [Q,updates] = dreamland(Q,Model_states,Model_rewards,Model_obs,num_simulations,gamma,updates,'uniform');
                
                %             update policy for current state
                [~,policy(curr_state)] = max(Q(curr_state,:));
                
                %         update current state for next iteration of the loop
                curr_state = next_state;
                
                %             occasionally update tracker of value of policy at start state
                %             and number of updates carried out
                %        determine value of current policy
                %                 if updates > klast+kfreq
                k = k+1;
                V(k,task) = value_policy(Model_states,Model_rewards,Model_obs,policy);
                U(k,task) = updates;
                %                 end
            end
        end
    end
end


%%            SIMULATION
function [Q,updates] = dreamland(Q,Model_states,Model_rewards,Model_obs,num_simulations,gamma,updates,distribution)
if strcmp(distribution,'uniform')
    %         uniform distribution runs through every damn state-action pair
    %         and simulates what might happen based on model and uses that to
    %         update Q - only simulates cases that we've seen before though
    for s = 1:size(Model_states,1)
        for a = 1:size(Model_states,2)
            if Model_obs(s,a) > 0
                for n = 1:num_simulations
                    states_hat = squeeze(Model_states(s,a,:))./Model_obs(s,a);
                    rewards_hat = squeeze(Model_rewards(s,a,:));
                    next_states = [];
                    %                     go to a state
                    while isempty(next_states)
                        eligible_states = find(rand(length(states_hat),1) < states_hat);
                        if ~isempty(eligible_states)
                            next_states = eligible_states(randperm(length(eligible_states),1));
                            R = rewards_hat(next_states);
                        end
                    end
                    %                     get max action values for each state
                    maxQa = max(Q(next_states,:),[],2);
                    %                     use model predictions to make an update
                    Q(s,a) = sum(states_hat .* [R + gamma * maxQa]);
                    %                         update number of computations carried out...
                    updates = updates + 1;
                end
            end
        end
    end
elseif strcmp(distribution,'on-policy')
    for n = 1:num_simulations
    curr_state = randi(size(Model_states,1));
    for n = 1:num_simulations
        states_hat = squeeze(Model_states(curr_state,policy(curr_state),:))./Model_obs(curr_state,policy(curr_state));
        rewards_hat = squeeze(Model_rewards(curr_state,policy(curr_state),:));
        next_states = [];
        %                     go to a state
        if sum(states_hat) == 0
            while isempty(next_states)
                eligible_states = find(rand(length(states_hat),1) < states_hat);
                if ~isempty(eligible_states)
                    next_states = eligible_states(randperm(length(eligible_states),1));
                    R = rewards_hat(next_states);
                end
            end
        else
            break
        end
        %                     get max action values for each state
        maxQa = max(Q(next_states,:),[],2);
        %                     use model predictions to make an update
        Q(s,a) = sum(states_hat .* [R + gamma * maxQa]);
        %                         update number of computations carried out...
        updates = updates + 1;
    end
    end
    
end
end

%% Value calculation
% get value of initial state for current policy using true reward and
% transition structure to evaluate policy from starting state
function V = value_policy(Model_states,Model_rewards,Model_obs,policy)
curr_states = 1;
V=0;
values_future_states=0;
%     get predictions about rewards at current state
for i = 1:length(curr_states)
    states_hat = nansum(squeeze(Model_states(curr_states(i),policy(curr_states(i)),:))./sum(Model_obs(curr_states(i),policy(curr_states(i)))),2);
    reward_hat = squeeze(Model_rewards(curr_states(i),policy(curr_states(i)),:));
    value_curr = sum(states_hat.*reward_hat);
    for j = 1:size(states_hat,1)
        states_hat_future = nansum(squeeze(Model_states(j,policy(j),:))./Model_obs(j,policy(j)),2);
        rewards_hat_future = nansum(squeeze(Model_rewards(j,policy(j),:)),2);
        values_future_states(j) = states_hat(j)*nansum(rewards_hat_future.*states_hat_future);
    end
end
V = V + value_curr + sum(values_future_states);
end

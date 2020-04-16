% racecar problem from chapter 5 of sutton and barto 2018 ? using exploring
% starts algorithm to get optimal policy (pi-star)

clear all
close all

% import a grid world
trackbmp = imread('racetracks/track1.bmp');
track = double(trackbmp);

% values for starts/stops/walls/track
wall = 0;
start = 105;
finish = 175;
road = 255;

% max velocity
mv = 5;

% actions grid
A={[-1,-1],[-1,0],[-1,1];...
   [0,-1],[0,0],[0,1];...
   [1,-1],[1,0],[1,1];};

% initial Q value
% iv = -0.1;

% noise (p(stall))
pStall = 0.1;

% gamma (discount)
gamma = 0.2;

% learning rate 
alpha = 0.3;

% number of episodes to run
numeps = 50000;

% initialize policies, Q's, returns
policy=randi(9,size(track));
Q=struct;
returns = struct;
for x = 1:size(track,2)
    for y = 1:size(track,1)
        policy(y,x)=randi(9,1);
        Q(y,x).a = randn(3)/100;
    end
end

% get list of starting positions
[startY,startX]=find(track==start);

% positive movement actions
aPos = [6,8,9];

% episode path struct
episodePath = struct;

% reward/cost scheme
costDrive = -1;
rewFinish = 1000000;
costCrash = -1000;

% let's race!
% run through an episode using random starting position
for i = 1:numeps
%     randomly select a starting state
    rstart = randperm(length(startY),1);
    epX = startX(rstart);
    epY = startY(rstart);
%     drive car from random starting state
    [t,r,epY,epX,action]=vroom(track,road,wall,finish,start,A,mv,policy,epY,epX,pStall,costDrive,rewFinish,costCrash);
    
%     go back through episode and learn something from what we did
    G = 0;
    for tt = t-1:-1:1
        G = gamma*G+r(tt+1);
        Q(epY(tt),epX(tt)).a(action(tt,1)) = Q(epY(tt),epX(tt)).a(action(tt,1)) + alpha *...
                                            (G-Q(epY(tt),epX(tt)).a(action(tt,1)));
%       update policy                                 
        [~,policy(epY(tt),epX(tt))] = max(Q(epY(tt),epX(tt)).a(:));
    end
%     store trajectory from this episode
    episodePath(i).path = [epX,epY];
    
end

% plot attempts
if numeps < 1000
    figure;
    imagesc(track);hold on;
    for j = 1:numeps
        plot(episodePath(j).path(:,1),episodePath(j).path(:,2))
    end
end
            
% get optimal trajectories
for j = 1:length(startX)
    oX = startX(j);
    oY = startY(j);
    [t,r,oY,oX,action]=vroom(track,road,wall,finish,start,A,mv,policy,oY,oX,pStall,costDrive,rewFinish,costCrash);
     %     store trajectory from this episode
    oPath(j).path = [oX,oY];     
end     

% plot 'optimal' trajectories according to policy
figure;
imagesc(track);hold on;
for j = 1:length(startX)
    plot(oPath(j).path(:,1),oPath(j).path(:,2))
end
    

function [t,r,epY,epX,action]=vroom(track,road,wall,finish,start,A,mv,policy,epY,epX,pStall,costDrive,rewFinish,costCrash)
    drivin = true;
%     initialize velocity in x and y
    vx = 0;
    vy = 0;
%     run through an episode
    t = 1;
    r = costDrive;
    action = randi(9);
    while drivin
        legalaction = false;
%         find out if the chosen action is legal according to problem
%         rules.. If not, cycle through other possible actions and pick one
%         at random
       %       get actions
            ay = A{action(t,1)}(1);
            ax = A{action(t,1)}(2);
        while ~legalaction
            if vy+ay < 0 || vx+ax < 0 || (vy+ay == 0 && vx+ax == 0) ...
                    || vy+ay > mv || vx+ax > mv
                action(t,1) = randi(9);
                ay = A{action(t,1)}(1);
                ax = A{action(t,1)}(2);
            else
                legalaction = true;
            end
        end
%         change velocity
        vy = vy+ay;
        vx = vx+ax;
        
        
% %         roll the dice to add noise by dropping velocity to zero!
        if rand <= pStall
            vx = 0;
            vy = 0;
        end
%         
%         move the car according to current policy
        epY(t+1,1) = epY(t,1)+vy;
        epX(t+1,1) = epX(t,1)+vx;
        
%         make sure we didn't run off the bounds of the track
        if epY(t+1,1) > size(track,1)
            epY(t+1,1) = size(track,1);
        end
        if epY(t+1,1) < 1
            epY(t+1,1) = 1;
        end   
        if epX(t+1,1) > size(track,2)
            epX(t+1,1) = size(track,2);
        end
        if epX(t+1,1) < 1
            epX(t+1,1) = 1;
        end  
        
        %         find out if we hit a wall or the finish line
        if track(epY(t+1),epX(t+1)) == wall
            drivin = false;
            r(t+1,1) = costCrash;
        elseif track(epY(t+1),epX(t+1)) == finish
            r(t+1,1) = rewFinish;
            drivin = false;
        elseif track(epY(t+1),epX(t+1)) == road || track(epY(t+1),epX(t+1)) == start
            r(t+1,1) = costDrive;
            action(t+1,1) = policy(epY(t+1,1),epX(t+1,1));
        end
        
%         up time step
        t = t+1;
    end
end
    

# Part 2. Approximate methods. 

In large state spaces need to give up on optimal policies and find good solutions given limited computational resources. The problem's not just storing really big tables but the time needed to fill them; much of the time, we'll have never encountered the state before so need to generalize from past similar experience. 

For generalization can borrow from other areas that aren't RL per se and most of what we're after can be called *function approximation*, an instance of supervised learning. Chapter 9 is just about value approximation given a policy then ch 10 moves to control where we want to find the optimal policy.  

# On-policy prediction with approximation  
We're back to estimating just $v_\pi$ (not $v_*$) given a known policy. The novelty is that instead of storing values in a table we're representing them with the paramtererized fxal form $w \in \mathbb{R}^d$. We write 
$\hat{v}(s,\bf{w}) \approx v_\pi$.

$\hat{v}$ might be as simple as a linear function of the features of the states with w representing the feature weights; or w could represent all the connection weights in a multilayer neural network.  Typically the # of weights is many fewer than the # of states hence when a single state is updated many states are affected. 

All of the pred methods we've talked about are about shifting value at certain states in the direction of a target value, $s \texttt{->}u$. Until now these've been a trivial shift of a single state value in a table, but now we let that update be arbitrarily complex and affect many other states. This example of getting some start state toward a target state is consistent with the general supervised learning pattern of mimicking input-output exs and when these involve a number = fx approximation. 

A difference from standard supervised learning is that eg. neural networks typically operate on a static training set passed over multiple times, but here we need top operate online while interacting w envir +/ a model. For instance in genrealized policy iteration we updated $q_{\pi}$ while $\pi$ changes and even if policy stays same the approximate values used in eg. TD change over time, so we need methods that can deal with these nonstationarities.  

## 9.2 Prediction objection  
Didn't explicitly set an obj for pred earlier, but now an update of one state affects all others and we know we can't get exact estimates for a state, so updating always involves a tradeoff of updating one state over another, hence we need an objective fx ie. to say which states we care about most, which we do via a state distribution 

$$\displaystyle\sum_{s\in s} \mu(s) = 1$$ (1)

where $\mu(s) \geq 0$, defining how much we care about error each state.  

The mean squared value error can then be defined as an objective fx defined in terms of the $\mu$-weighted sum of squares of $\hat{v}(s,w) - v_\pi(s)$ summed over all states. Often we choose $\mu_s$ based on the time spent in each state.  

It's not clear this always the best objective fx and we'll return to that but it'll do for now.  

The ideal is to find the global optimum for the objective fx; that's often not realistic so we'll then settle for a local optimum where the w's don't change that much; and even that's often not realistic in many realistic cases. 

The space of fx approximators is vast and little definitive is known about many. For now we'll focus on gradient-descent based ones.  

## 9.3 Gradient descent  
We have a column weight vector

$${w_1, w_2, ... w_d}^{\top}$$ 

whose number are finite and real valued and the approx value fx $\hat{v}(s,w)$ is a differential fx of w for all $s \in S$. 

Assume at each step we get a new example of $S_t \texttt{->} v_\pi(S_t)$, a state and its true value under the policy. Although we're assuming we get the exact correct values, still hard bc fx approximator has limited resources specifically no $\bf w$ for all states. 

If we assume all states follow the same distribution as $\mu(s)$ and we're trying to minimize the above defined objective fx then a good strategy is  to try to minimize error in examples. Stochastic gradient descent (SGD) methods do so by incrementing the weights a small amount toward minimizing error in the example so 

$$w_{t+1}=w_t + \alpha [v_\pi(S_t) - \hat{v}(S_t,w_t) ]\nabla \hat{v}(S_t,w_t)$$ (2)

where $\alpha$ is a positive step size param. 

Note that for any fx $f(w)$ that's a fx of a vector (here w), $\nabla \hat{v}(S_t,w_t)$ denotes the partial derivs with respect to the vec components. 

$$\nabla f(w) = (\frac{\partial f(w)}{\partial w_1}, \frac{\partial f(w)}{\partial w_2}, ...\frac{\partial f(w)}{\partial w_d})^{\top}$$ (3)

## Some notes from gradient descent tutorial at
https://machinelearningmastery.com/implement-linear-regression-stochastic-gradient-descent-scratch-python/
(see also tutorials -> grad_descent.py for the code)

GD = min fx by following gradients of the cost fx--so you need an explicit cost fx + its derivative 
so you can move in that direction ie. downhill for descent. Opt works by getting 1 training example at a time and updating the model to reduce error for next pred. In machine learning lingo, find linear regr coefs by updating coefs b = b - learning_rate * error - x , as in eqn 2 above. 

Matt:
Say you're representing a square grid just in terms of x,y position and you assume fx is linear in x and y.  And say you're y is not contributing to value but x is. 

In the tabular case you don't need derivatives because the derivative's 1 for the state you're updating and 0 for everything else.  

Why call the $x_i$ basis fxs? Because basis for representing the value fx. Eg. fourier is basis for rep'ing signal in terms of eg. phase-shifted components.  

## 9.4 Linear Methods  
For linear method, there's a real-valued vec representing every state  

$$x(s) = (x_1(s)...x_d(s))^{\top}$$ (4)

and the state approx fx is the inner product of $w$ and $x(s)$  

$$\hat{v}(s,w) = w^{\top}x(s) = \displaystyle\sum_{i=1}^{d}w_i x_i(s)$$ (5)

x(s) is called a feature vec repr'ing state s. Can think of each component $x_i(x)$ being itself the output a fx $x_i: S \texttt{->} \mathbb{R}$ (that is, mapping the states to a real-valued repr). A feature is the entirety of this fx and the specific value for a state is a feature of s. These are basis fxs in the linear case as they're a linear basis for the approx fxs. 

In this linear case, the gradient's simply $\nabla \hat{v}(s,\bf{w}) = x(s)$ so the update just becomes  

$$w_{t+1}=w_t + \alpha [U_t - \hat{v}(S_t,w_t) ]x(s)$$ (2)

The simplicity makes it mathematically favorable so lots of results assume linearity + there's only a single optimum or at worse if degenerate multiple equally good optimum, so any time there's a local optimum can be assured it's the global.  


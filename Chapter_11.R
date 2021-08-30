# Survival Analysis and Censored Data -------------------------------------
# I.e., time-to-event analysis. Although this might sound like a regression problem
# there is an important complication: many or some of the patients have survived
# until the end of the study. Such a patient's survival time is said to be 
# censored: we know that it is at least five years, but we do not know its true 
# value. However, we do not want to discard this subset of surviving patients, as 
# the fact that they have survived ≥ 5 years amounts to valueable information.

## A Closer Look at Censoring ----------------------------------------------
# In order to analyse survival data, we inevitably have to make a set of 
# assumptions . E.g., suppose that a number of patients drop out of a cancer 
# study early because they are very sick or because of toxicity etc.An analysis
# that does not take into account the reason why patients dropped out of the study
# will likely overestimate the true expected survival time. Similarly, suppose 
# that males who are very sick are more likely to drop out than females who are
# very sick. Then a comparison of male and female survival times may wrongly 
# suggest that makes survive longer than females. 

# In general, we need to assume that the censoring mechanism is *independent*: 
# conditional on the features, event time T is independent of the censoring time
# C. The examples above *violate* the assumption of independent censoring. 
# Typically it is not possible to determine from the data itself whether the 
# censoring mechanism is independent. The usual data sleuthing and casual 
# reasoning applies.

## The Kaplan-Meier Survival Curve -----------------------------------------
# The survival time, or *survival function*, is defined as:

# S(t) = P(T > t)

# This is a decreasing function which quantifies the probability of surviving 
# past time *t*. E.g., suppose that a company is interested in modelling customer
# churn. Let T represent the time that a customer cancels a subscription. Then
# S(t) represents the probability that a customer cancels at a later time than
# t. 

# Further e.g., ,consider the task of estimating the survival curve for S(20) = 
# P(T > 20), the probability that a patient survives for at least t = 20 months.
# It is tempting to simply compute the proportion of people who have survived
# > 20 months. However, Y and T represent different quantities. Importantly, this
# does not account for censoring. But taking into account censoring means that 
# you now do not take into account patients who were not censored.

# To overcome the above, we can let d_1 < d_2 < ... < d_k be the K unique death 
# times among non-censored patients, and we let q_k denote the number of patients
# who died at time d_k. For 1, ..., K we let r_k denote the number of patients 
# alive and in the study just before d_k; these are the *at risk* patients (the 
# set of patients that are at risk at a given time are referred to as the *risk
# set*. By LOTP:

# P(T > d_k) = P(T > d_k |T > d_k-1)P(T > d_k) + P(T > d_k |T <= d_k-1)P(T <= d_k)

# Therefore:
# S(d_k) = P(T > d_k) = P(T > d_k |T > d_k-1)P(T > d_k)
# and by plugging in the survival function again, we see that
# S(d_k) = P(T > d_k | T > d_k-1)S(d_k-1)
# which implies that
# S(d_k) = P(T > d_k |T > d_k-1) * ... * P(T > d_2) | T > d_2)P(T > d_1)
# for all 1...k 

# We can then simply plug in the estimates of each of the terms on the right side
# of the equation above. It is natural to use the estimator:

# P_hat(T > d_j | T > d_j-1) = (r_j - q_j) / r_j

# which is the fraction of the risk set at time d_j who survived past time d_j.
# This leads to the Kaplan-Meier survival curve:

# S_hat(d_k) = Product_(j...k) (r_j - q_j / r_j)

# For times t between d_k and d_k + 1, we set S_hat(t) = S_hat(d_k). Consequently,
# the Kap-Meier estimator has a step-like shape.

## The Log-Rank Test -------------------------------------------------------
# How can we compute a formal test of equality between two survival curves, e.g.,
# between females or males? At first glance, a simple two-sample t-test seems 
# obvious. But again, the presence of censoring creates a hurdle. To overcome this
# we can use the *log-rank test*. The log-rank test examines how the events in 
# each group unfold sequentially in time. 

# Again, denote d_1 < d_2 < ... < d_k for non-censored patients; r_k is number of
# patients at risk at time d_k; q_k is the number of patients who died at time
# d_k. Further, r_j,k and r_j + 1, k denote j groups at time k.

# The main idea behind the log-rank test statistic is as follows. To test H_0: 
# E(X) = 0 for some random variable X, one way is to construct a test-statistic
# of the form:

# W = \frac{X - E(X)}{sqrt(V(X))}

# for X under the H_0. To compute a log-rank statistic, we compute a value that 
# takes exactly the same form and loop X = \sum_(k = 1, K) * q_j, k for each 
# group. In greater detail, under the null hypothesis H_0 of no difference in
# survival between groups, and conditioning on the marginal probabilities, 
# expected value of group 1 for e.g., is

# E(q_1k) = \frac{r_1k}{rk} * q_k

# Further, it can be shown that the variance of q_1k is 

# Var(q_1k) = \frac{q_k(r_1k / r_k)(1 - r_1k / r_k)(r_k - q_k)}{r_k - 1}

# Though q_11, ..., q_1k may be correlated, we nonetheless estimate:

# Var_\sum{k=1}{K} q_1k ≈ \sum{k=1}{K}Var(q_1k) = \sum{k=1}{K} * 
# \frac{q_k(r_1k / r_k)(1 - r_1k / r_k)*(r_k - q_k)}{r_k - 1}

# Therefore, to compute the log-rank statistic, we simply plug in the above to 
# the first test-statistic equation W as:

# W = \frac{\sum{k=1}{K}(q_1k - E(q_1k))}{\sqrt{\sum{k=1}{K}Var(q_1)}}

# When sample size is large, the log-rank statistic W is approx. ~ N(µ, ø);
# this can be used to compute a p-value for the null.

# Regression Model With a Survival Response -------------------------------
# The observations are of the form (Y, ∂), where Y = min(T, C) is the (possibly 
# censored) survival time, and ∂ is an indicator variable that equals 1 if T ≤ C.
# Furthermore, X as an element of R^p is a vector of p features. We wish to 
# predict the survival time T. 

# Since the observed quantity Y is positive and may have a long, right tail, we 
# may be tempted to fit a linear regression of log(Y) on X. But censoring again
# creates a problem since we are actually interested in predicting T, not Y! To
# overcome this difficulty, we make use of sequential construction, similar to 
# the constructions of the Kap-Meier and log-rank test.

### The Hazard Function -----------------------------------------------------
# The hazard function or hazard rate - also known as the force of mortality - 
# is formally defined as 

# h(t) = \lim{\delta t}{tends 0} \frac{P(t < T ≤ t + \delta_T | T > t)}{\delta_t}

# where T is the (unobserved) survival time. It is the death rate in the instant
# after time t, given survival past that time. Interesting to note is that
# h(t) is actually the pdf for T conditional on T > t. It is important to 
# realise that h(t) is closely related to the survival curve and that a key 
# approach for modelling survival data as a function of covariates relies heavily
# on the h(t), i.e. the Cox proportional hazards model. 

# To model survival time as a function of covariates, it is convenient to work
# directly with the survival function, instead of the pdf. One possible approach
# is to assume a functional form for the hazard function h(t | x_i), such as

# h(t | x_i) = exp(\beta{0} + \sum{j=1}{p} \beta_j x_{i}{j})

# where the exponent function guarantees that h(t) is non-negative. However, the
# function is constant with time. 

### Proportional Hazards ----------------------------------------------------
# The Porportional Hazards Assumption: the proportional hazards assumptions 
# states that:

# h(t | x_i) = h_0(t)exp(\sum{j=1}{p} x_{ij}\beta_{j})

# where h_0(t)≥0 is an unspecified function, known as the *baseline hazard*. It
# is the h(t) for an individual with features x_{i}{1} = ... = x_{i}{p} = 0. The
# name "proportional hazards" arises from the fact that the hazard function for 
# an individual with feature vector x_i is some unknown function h_0(t) times 
# the factor exp(\sum{j=1}{p} x_{i}{j} \beta_{j}).

# The quantity exp(\sum{j=1}{p} x_{i}{j} \beta_{j}) is called the *relative risk*
# for the feature vector x_i = (x_{i}{1}, ... , x_{i}{p}) ^ T, relative to that
# for the feature vector x_i = (0, ..., 0) ^ T.

# What does this mean? Basically, it means that we make no assumptions about the
# functional form of h_0(t). We allow the rate of death at time t, given that one
# has survived to at least time t, to take any form. This makes the hazard 
# function very flexible and can model a wide range of relationships between
# the covariates and survival time. Our only assumption is that a one-unit 
# increase in x_{i}{j} corresponds to an increase in h(t | x_i) by a facor of
# exp(\beta_j)

### Cox's Proportional Hazards Model ----------------------------------------
# Because the form of h_0(t) in the proportional hazards assumption is unknown,
# we cannot simply plug h(t | x_i) into the likelihood and then estimate
# \beta = (\beta_1, ..., \beta_p) ^ T by maximum likelihood. However, the magic
# of Cox's proportional hazards model lies in the fact that it is in fact 
# possible to estimate the vector of coefficients without having to specify the
# form h_0(t).

# To do this, we make use of the same "sequential in time" logic that we used for
# Kap-Meier and log-rank statistic. For simplicity, assume that there are no ties
# among failure or death times, i.e., each failure occurs at a distinct time. 
# Assume that \delta_1, i.e., the ith observation is uncensored, and thus y_i is
# its failure time. Then the hazard function for the ith observation at time y_i
# is h(y_i | x_i) = h_0(y_i)exp(\sum_{j=1}{p}x_{i}{j} \beta_j), and the total
# hazard at time y_i for the at risk set is 

# \sum_{i':y_i'≥y_i} h_0(t) exp(\sum_{j=1}{p}x_{i'}{j} \beta_j)

# Therefore, the probability that the ith observation is one to fail at time y_i
# is 

# \frac{h_0(t) exp(\sum_{j=1}{p}x_{i}{j} \beta_j)}{
# \sum_{i':y_i'≥y_i} h_0(t) exp(\sum_{j=1}{p}x_{i'}{j} \beta_j)}
# with the baseline hazards cancelling out...

# The *partial* likelihood is simply the product of these probabilities over all
# uncensored observations. Thus, to estimate the vector of coefficients \beta
# we simply maximise the partial likelihood with respect to \beta. Often no 
# closed form solutions are available and so iterative simulations are required.

#### Connection with the log-rank test ---------------------------------------





















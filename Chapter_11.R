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

















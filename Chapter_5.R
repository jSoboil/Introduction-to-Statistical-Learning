# ====================================================================================
# Libraries ---------------------------------------------------------------
# ====================================================================================

library(ISLR)
library(boot)

# ====================================================================================
# Validation Techniques --------------------------------------------------------------
# ====================================================================================

# Simple Train-Test set validation approach -------------------------------

# We explore the use of the validation set approach in order to estimate the test error
# rates that result from fitting various linear models on the Auto data set.

# Set seed in order to provide replicable results. This function sets a seed for R’s 
# random number generator:
set.seed(1)

# It is generally best practice to set a seed for a stochastic process in order to be
# able to produce replicable results. 

# We begin by using the sample() function to split the set of observations sample() 
# into two halves, by selecting a random subset of 196 observations out of the original 
# 392 observations. We refer to these observations as the training set:
train_set <- sample(392, 196)
train_set

# We then use the subset option in lm() to fit a linear regression using only the 
# observations corresponding to the training set:
lm_mod <- lm(mpg ~ horsepower, data = Auto, subset = train_set)

# We now use the predict() function to estimate the response for all 392 observations,
# and we use the mean() function to calculate the MSE of the 196 observations in the
# validation set. Note that the -train index below selects only the observations that
# are not in the training set:
MSE_1 <- mean((Auto$mpg - predict(lm_mod, Auto))[-train_set]^2)
MSE_1
# Therefore, the estimated test MSE for the linear regression fit is 23.27. We can use 
# the poly() function to estimate the test error for the quadratic and cubic 
# regressions.

# Quadratic model:
lm_mod_2 <- lm(mpg ~ poly(horsepower , 2), data = Auto, subset = train_set)

# Validation set MSE:
MSE_2 <- mean((Auto$mpg - predict(lm_mod_2, Auto))[-train_set]^2)
MSE_2

# Cubic model:
lm_mod_3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train_set)

# Validation set MSE:
MSE_3 <- mean((Auto$mpg - predict(lm_mod_3, Auto))[-train_set]^2)
MSE_3

# These error rates are 18.72 and 18.79, respectively. If we choose a different 
# training set instead, then we will obtain somewhat different errors on the validation
# set.

# Randomise the training set differently:
set.seed(2)

# Create training set:
train_set <- sample(392, 196)

# Generate linear, quadratic, and cubic functions for mpg as a response to horsepower.

# Linear funnction:
lm_mod <- lm(mpg ~ horsepower, data = Auto, subset = train_set)

# Linear validation set MSE:
MSE_1 <- mean((Auto$mpg - predict(lm_mod, Auto))[-train_set]^2)
MSE_1

# Quadratic function:
lm_mod_2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train_set)

# Quadratic validation set MSE:
MSE_2 <- mean((Auto$mpg - predict(lm_mod_2, Auto))[-train_set]^2)
MSE_2

# Cubic function:
lm_mod_3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train_set)

# Cubic validation set MSE:
MSE_3 <- mean((Auto$mpg - predict(lm_mod_3, Auto))[-train_set]^2)
MSE_3

# Using this split of the observations into a training set and a validation set, we 
# find that the validation set error rates for the models with linear, quadratic, and
# cubic terms are 25.73, 20.43, and 20.39, respectively. These results are consistent 
# with our previous findings: a model that predicts mpg using a quadratic function of 
# horsepower performs better than a model that involves only a linear function of 
# horsepower, and there is little evidence in favor of a model that uses a cubic 
# function of horsepower.


# LOOCV -------------------------------------------------------------------

# In this lab, we will perform linear regression using the glm() function rather than
# the lm() function because the former can be used together with cv.glm(). The cv.glm() 
# function is part of the boot library.

# The LOOCV estimate can be automatically computed for any generalized linear model 
# using the glm() and cv.glm() functions. In the lab for Chapter 4, we used the 
# glm() function to perform logistic regression by passing in the family="binomial" 
# argument. But if we use glm() to fit a model without passing in the family argument,
# then it performs linear regression, just like the lm() function.

# So, for example:
glm_mod <- glm(mpg ~ horsepower, data = Auto)

# Cross-Validation error:
CV_err <- cv.glm(data = Auto, glmfit = glm_mod)
CV_err$delta

# The cv.glm() function produces a list with several components. The two numbers in 
# the delta vector contain the cross-validation results. In this case the numbers are 
# identical (up to two decimal places). Below, we discuss a situation in which the two
# numbers differ. Our cross-validation estimate for the test error is approximately
# 24.23.

# We can repeat this procedure for increasingly complex polynomial fits. To automate 
# the process, we use the for() function to initiate a for loop, for(), which 
# iteratively fits polynomial regressions for polynomials of order i = 1 to i = 5, 
# computes the associated cross-validation error, and stores it in the ith element of 
# the vector cv.error. We begin by initializing the vector. This command will likely 
# take a couple of minutes to run:
CV_err <- rep(0, 5)

for (i in 1:5) {
  glm_mod_2 <- glm(mpg ~ poly(horsepower, i), data = Auto)
  CV_err[[i]] <- cv.glm(Auto, glm_mod_2)$delta[1]
}
CV_err

# As in Figure 5.4, we see a sharp drop in the estimated test MSE between the linear 
# and quadratic fits, but then no clear improvement from using higher-order 
# polynomials.

# k-fold Cross-Validation -------------------------------------------------

# The cv.glm() function can also be used to implement k-fold CV. Below we use k = 10, 
# a common choice for k, on the Auto data set. We once again set a random seed and 
# initialize a vector in which we will store the CV errors corresponding to the 
# polynomial fits of orders one to ten.
set.seed(17)

CV_k_10_err <- rep(0, 10)

for (i in 1:10) {
  glm_mod_3 <- glm(mpg ~ poly(horsepower, i), data = Auto)
  CV_k_10_err[[i]] <- cv.glm(Auto, glm_mod_3, K = 10)$delta[1]
}
CV_k_10_err
# Notice that the computation time is much shorter than that of LOOCV. We still see 
# little evidence that using cubic or higher-order polynomial terms leads to lower 
# test error than simply using a quadratic fit.

# We saw that the two numbers associated with delta are essentially the same when 
# LOOCV is performed. When we instead perform k-fold CV, then the two numbers 
# associated with delta differ slightly. The first is the standard k-fold CV estimate. 
# The second is a bias-corrected version. On this data set, the two estimates are very
# similar to each other.
glm_mod_4 <- glm(mpg ~ poly(horsepower, 3), data = Auto)
CV_err_4 <- cv.glm(Auto, glm_mod_4, K = 10)
CV_err_4$delta[1]

# Note that this is the reason why the subset for delta[1] is specified when creating
# a for loop procedure for cross validation; we want the actual CV value and not the 
# adjusted one, which is found in the second column!

# ====================================================================================
# Estimating the Accuracy of a Statistic of Interest ----------------------
# ====================================================================================

# One of the great advantages of the bootstrap approach is that it can be applied in 
# almost all situations. No complicated mathematical calculations are required.

# Performing a bootstrap analysis in R entails only two steps. First, we must create a
# function that computes the statistic of interest. Second, we use the boot() function,
# which is part of the boot library, to perform the bootstrap by repeatedly sampling 
# observations from the data set with replacement.

# In the following section, we will analyse the Portfolio data set in the ISLR package.
# To illustrate the use of the bootstrap on this data, we must first create a function,
# alpha.fn(), which takes as input the (X,Y) data as well as a vector indicating which
# observations should be used to estimate α. The function then outputs the estimate 
# for α based on the selected observations:
alpha <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2*cov(X, Y)))
}
# This function returns, or outputs, an estimate for α, based onthe observations 
# indexed by the argument index. For instance, the following command tells R to 
# estimate α using all 100 observations:
alpha(Portfolio, 1:100)

# The next command uses the sample() function to randomly select 100 observations from
# the range 1 to 100, with replacement. This is equivalent to constructing a new 
# bootstrap data set and recomputing the estimated α based on the new data set:
set.seed(1)

alpha(Portfolio, sample(100, 100, replace = TRUE))

# We can implement a bootstrap analysis by performing this command many times, 
# recording all of the corresponding estimates for α, and computing the resulting 
# standard deviation. However, the boot() function automates this approach. Below we 
# produce R = 1, 000 bootstrap estimates for α:
boot(Portfolio, alpha, R = 1000)
# The final output shows that using the original data, estimated α = 0.5758, and that 
# the bootstrap estimate for SE(αˆ) is 0.0886.

# Estimating the Accuracy of a Linear Regression Model --------------------

# The bootstrap approach can be used to assess the variability of the coefficient 
# estimates and predictions from a statistical learning method. Here we use the 
# bootstrap approach in order to assess the variability of the estimates for β0 and β1,
# the intercept and slope terms for the linear regression model that uses horsepower 
# to predict mpg in the Auto data set. We will compare the estimates obtained using 
# the bootstrap to those obtained using the formulas for SE(βˆ0) and SE(βˆ1).

# We first create a simple function, boot.fn(), which takes in the Auto data set as 
# well as a set of indices for the observations, and returns the intercept and slope 
#estimates for the linear regression model. We then apply this function to the full 
# set of 392 observations in order to compute the esti- mates of β0 and β1 on the 
# entire data set using the usual linear regression coefficient estimate formulas.
boot_fn <- function(data, index) {
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot_fn(Auto, 1:392)

# The boot.fn() function can also be used in order to create bootstrap estimates for 
# the intercept and slope terms by randomly sampling from among the observations with 
# replacement. Here are two examples:
set.seed(1)

boot_fn(Auto, sample(392, 392, replace = TRUE))
boot_fn(Auto, sample(392, 392, replace = TRUE))

# Next, we use the boot() function to compute the standard errors of 1,000 bootstrap
# estimates for the intercept and slope terms:
boot(Auto, boot_fn, 1000)
# This indicates that the bootstrap estimate for estimated SE of β0 is 0.84, and that
# the bootstrap estimate for the estimated SE of β1 is 0.0073. Standard formulas can 
# be used to compute the standard errors for the regression coefficients in a linear 
# model. These can be obtained using the summary() function:
summary(lm(mpg ~ horsepower, data = Auto))$coef

# The standard error estimates for the estimated β0 and estimated β1 obtained using 
# the formulas from Section 3.1.2 are 0.717 for the intercept and 0.0064 for the slope.
# Interestingly, these are somewhat different from the estimates obtained using the 
# bootstrap. Does this indicate a problem with the bootstrap? In fact, it suggests the
# opposite. Recall that the standard formulas given in Equation 3.8 on page 66 rely on
# certain assumptions. For example, they depend on the unknown parameter σ^2 (variance)
# , the noise variance. We then estimate σ^2 using the RSS. Now although the formula 
# for the standard errors do not rely on the linear model being correct, the estimate
# for σ^2 does. We see in Figure 3.8 on page 91 that there is a non-linear relationship
# in the data, and so the residuals from a linear fit will be inflated, and so will the
# estimated σˆ2. Secondly, the standard formulas assume (somewhat unrealistically) 
# that the x[i] are fixed, and all the variability comes from the variation in the 
# errors εi. The bootstrap approach does not rely on any of these assumptions, and so 
# it is likely giving a more accurate estimate of the standard errors of the estimated
# β0 and estimated β1 than is the summary() function.

# Below we compute the bootstrap standard error estimates and the standard linear 
# regression estimates that result from fitting the quadratic model to the data. 
# Since this model provides a good fit to the data, there is now a better 
# correspondence between the bootstrap estimates and the standard estimates of the 
# estimated SE for the estimated β0, and the estimated SE for the estimated β1 and 
# the estimated SE for the estimated β2:
boot_fn <- function(data, index) {
  coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data, 
                  subset = index))
}
set.seed(1)
boot(data = Auto, statistic = boot_fn, R = 1000)

summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef

# End file ----------------------------------------------------------------
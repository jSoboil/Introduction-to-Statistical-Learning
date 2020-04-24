# ======================================================================================
# Load libraries ----------------------------------------------------------
# ======================================================================================

# We will use the glmnet package in order to perform ridge regression and the lasso. The
# main function in this package is glmnet(), which can be used to fit ridge regression 
# models, lasso models, and more. This function has slightly different syntax from other 
# model-fitting functions that we have encountered thus far in this book. In particular, 
# we must pass in an x matrix as well as a y vector, and we do not use the y ∼ x syntax. 
# We will now perform ridge regression and the lasso in order to predict Salary on the 
# Hitters data. Before proceeding ensure that the missing values have been removed from 
# the data.

library(glmnet)

# ======================================================================================
# Clean Hitters data ------------------------------------------------------
# ======================================================================================

# View dimensions of the data:
dim(Hitters)
# Sum the number of NA's:
sum(is.na(Hitters)) # = 0

# ======================================================================================
# Ridge Regression --------------------------------------------------------
# ======================================================================================

# Create x matrix:
xMatrix <- model.matrix(Salary ~ ., Hitters)[, -1]
xMatrix
# The model.matrix() function is particularly useful for creating x; not only does it
# produce a matrix corresponding to the 19 predictors but it also automatically 
# transforms any qualitative variables into dummy variables. The latter property is 
# important because glmnet() can only take numerical, quantitative inputs.

# Create Y vector:
Y_salary <- Hitters$Salary
Y_salary

# The glmnet() function has an alpha argument that determines what type of model is fit. 
# If alpha = 0 then a ridge regression model is fit, and if alpha = 1 then a lasso model
# is fit. We first fit a ridge regression model:
grid <- 10 ^ seq(10, -2, length = 100)
grid

ridge_mod <- glmnet(xMatrix, Y_salary, alpha = 0, lambda = grid)
ridge_mod
# By default the glmnet() function performs ridge regression for an automatically 
# selected range of λ values. However, here we have chosen to implement the function 
# over a grid of values ranging from λ = 10^10 to λ = 10^−2, essentially covering the
# full range of scenarios from the null model containing only the intercept, to the 
# least squares fit. As we will see, we can also compute model fits for a particular 
# value of λ that is not one of the original grid values. Note that by default, the
# glmnet() function standardizes the variables so that they are on the same scale. To 
# turn off this default setting, use the argument standardize = FALSE.

# Associated with each value of λ is a vector of ridge regression coefficients, stored
# in a matrix that can be accessed by coef(). In this case, it is a 20×100 matrix, with 
# 20 rows (one for each predictor, plus an intercept) and 100 columns (one for each 
# value of λ).
dim(coef(ridge_mod))
# We expect the coefficient estimates to be much smaller, in terms of l2 norm, when a 
# large value of λ is used, as compared to when a small value of λ is used. These are 
# the coefficients when λ = 11,498, along with their l2 norm:
ridge_mod$lambda[50]

coef(ridge_mod)[, 50]

sqrt(sum(coef(ridge_mod)[-1, 50]^2))

# In contrast, here are the coefficients when λ = 705, along with their l2 norm. Note 
# the much larger l2 norm of the coefficients associated with this smaller value of λ:
ridge_mod$lambda[60]

coef(ridge_mod)[, 60]

sqrt(sum(coef(ridge_mod)[-1, 60]^2))
# ...it is important to note the exclusion of the models intercept in the equation! You 
# are summing the coefficients, B1 - not B0!

# We can use the predict() function for a number of purposes. For instance, we can 
# obtain the ridge regression coefficients for the first 20 rows for a new value of λ, 
# say 50:
predict(ridge_mod, s = 50, type = "coefficients")[1:20, ]

# We now split the samples into a training set and a test set in order to estimate the 
# test error of ridge regression and the lasso. There are two common ways to randomly
# split a data set. The first is to produce a random vector of TRUE, FALSE elements and
# select the observations corresponding to TRUE for the training data. The second is to
# randomly choose a subset of numbers between 1 and n; these can then be used as the 
# indices for the training observations. The two approaches work equally well. Here we
# demonstrate the latter approach.We first set a random seed so that the results 
# obtained will be reproducible:
set.seed(1)
train_set <- sample(1:nrow(xMatrix), nrow(xMatrix) / 2)
train_set

test_set <- (-train_set)
test_set


Y_test <- Y_salary[test_set]
Y_test

# Next we fit a ridge regression model on the training set, and evaluate its MSE on the
# test set, using λ = 4. Note the use of the predict() function again. This time we get
# predictions for a test set, by replacing type="coefficients" with the newx argument:
ridge_mod <- glmnet(xMatrix[train_set, ], Y_salary[train_set], alpha = 0, 
                    lambda = grid, thresh = 1e-12)
ridge_pred <- predict(ridge_mod, s = 4, newx = xMatrix[test_set, ])

mean((ridge_pred - Y_test)^2)
# The test MSE is 142199.2. Note that if we had instead simply fit a model with just
# an intercept, we would have predicted each test observation using the mean of the 
# training observations. In that case, we could compute the test set MSE like this:
mean((mean(Y_salary[train_set]) - Y_test)^2)

# We could also get the same result by fitting a ridge regression model with a very 
# large value of λ. Note that 1e10 means 10^10:
ridge_pred <- predict(ridge_mod, s = 1e10, newx = xMatrix[test_set, ])
mean((ridge_pred - Y_test)^2)

# In general, instead of arbitrarily choosing λ = 4, it would be better to use 
# cross-validation to choose the tuning parameter λ. We can do this using the built-in
# cross-validation function, cv.glmnet(). By default, the function performs ten-fold 
# cross-validation, though this can be changed using the argument nfolds. Note that we
# set a random seed first so our results will be reproducible, since the choice of the
# cross-validation folds is random:
set.seed(1)
cv_out <- cv.glmnet(xMatrix[train_set, ], Y_salary[train_set], alpha = 0)
plot(cv_out)

best_lam <- cv_out$lambda.min
best_lam
# Therefore, we see that the value of λ that results in the smallest cross-validation 
# error is 326.0828 What is the test MSE associated with this value of λ?
ridge_pred <- predict(ridge_mod, s = best_lam, newx = xMatrix[test_set, ])
mean((ridge_pred - Y_test)^2)

# This represents a further improvement over the test MSE that we got using λ = 4. 
# Finally, we refit our ridge regression model on the full data set, using the value 
# of λ chosen by cross-validation, and examine the coefficient estimates.
ridge_final <- glmnet(xMatrix, Y_salary, alpha = 0)
predict(ridge_final, type = "coefficients", s = best_lam)[1:20, ]
# As expected, none of the coefficients are zero — ridge regression does not perform 
# variable selection!

# ======================================================================================
# The Lasso ---------------------------------------------------------------
# ======================================================================================

# We saw that ridge regression with a wise choice of λ can outperform least squares as 
# well as the null model on the Hitters data set. We now ask whether the lasso can yield
# either a more accurate or a more interpretable model than ridge regression. In order 
# to fit a lasso model, we once again use the glmnet() function; however, this time we 
# use the argument alpha = 1. Other than that change, we proceed just as we did in 
# fitting a ridge model.
lasso_mod <- glmnet(xMatrix[train_set, ], Y_salary[train_set], alpha = 1,
                    lambda = grid)
plot(lasso_mod)
# We can see from the coefficient plot that depending on the choice of tuning parameter, 
# some of the coefficients will be exactly equal to zero. We now perform 
# cross-validation and compute the associated test error:
set.seed(1)
cv_lasso <- cv.glmnet(xMatrix[train_set, ], Y_salary[train_set], alpha = 1)
plot(cv_lasso)

best_lam <- cv_lasso$lambda.min
best_lam

lasso_pred <- predict(lasso_mod, s = best_lam, newx = xMatrix[test_set, ])
mean((lasso_pred - Y_test)^2)
# This is substantially lower than the test set MSE of the null model and of least 
# squares, and very similar to the test MSE of ridge regression with λ chosen by 
# cross-validation.

# However, the lasso has a substantial advantage over ridge regression in that the 
# resulting coefficient estimates are sparse. Here we see that 12 of the 19 coefficient 
# estimates are exactly zero. So the lasso model with λ chosen by cross-validation 
# contains only seven variables:
lasso_final <- glmnet(xMatrix, Y_salary, alpha = 1, lambda = grid)
lasso_coef <- predict(lasso_final, type = "coefficients", s = best_lam)[1:20, ]
lasso_coef
# Hence, lasso performs variable selection.

# End file ----------------------------------------------------------------
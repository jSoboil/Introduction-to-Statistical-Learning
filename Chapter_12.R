# ======================================================================================
# Libraries ---------------------------------------------------------------
# ======================================================================================
library(boot)
library(ISLR)
library(pls)
library(car)
library(glmnet)

# ======================================================================================
# Train/Test Split --------------------------------------------------------
# ======================================================================================

set.seed(1)


train_set <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
train_set

test_set <- (!train_set)
test_set

Y_test <- Hitters$Salary[test_set]

# ======================================================================================
# Principal Components Regression -----------------------------------------
# ======================================================================================

# Principal components regression (PCR) can be performed using the pcr() function,which 
# is part of the pls library. We now apply PCR to the Hitters pcr() data, in order to 
# predict Salary. Again, ensure that the missing values have been removed from the data:
sum(is.na(Hitters))

# Set seed to ensure reproducibility:
set.seed(2)

# Generate PCR model:
PCR_mod <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")
# The syntax for the pcr() function is similar to that for lm(), with a few additional
# options. Setting scale = TRUE has the effect of standardizing each predictor, prior to 
# generating the principal components, so that the scale on which each variable is 
# measured will not have an effect. Setting validation = "CV" causes pcr() to compute 
# the ten-fold cross-validation error for each possible value of M , the number of 
# principal components used. The resulting fit can be examined using summary():
summary(PCR_mod)
# The CV score is provided for each possible number of components, ranging from M = 0 
# onwards. (We have printed the CV output only up to M = 4.) Note that pcr() reports the
# root mean squared error; in order to obtain the usual MSE, we must square this 
# quantity. For instance, a root mean squared error of 342 corresponds to an MSE of:
342^2

# One can also plot the cross-validation scores using the validationplot() function. 
# Using val.type = "MSEP" will cause the cross-validation MSE to be plot() plotted.
validationplot(PCR_mod, val.type = "MSEP")
# One can also plot the cross-validation scores using the validationplot() function. 
# Using val.type = "MSEP" will cause the cross-validation MSE to be plot() plotted.

# The summary() function also provides the percentage of variance explained in the
# predictors and in the response using different numbers of components. Briefly, we can 
# think of this as the amount of information about the predictors or the response that 
# is captured using M principal components. For example, setting M = 1 only captures 
#38.31 % of all the variance, or information, in the predictors. In contrast, 
#$  using M = 6 increases the value to 88.63 %. If we were to use all M = p = 19 
# components, this would increase to 100 %.

# We now perform PCR on the training data and evaluate its test set performance:
set.seed(1)

pcr_mod <- pcr(Salary ~ ., data = Hitters, subset = train_set, scale = TRUE,
               validation = "CV")

summary(pcr_mod)

par(mfrow = c(2, 2))
# Plot RMSE:
validationplot(pcr_mod)
# Plot MSE:
validationplot(pcr_mod, val.type = "MSEP")
# Plot R.sqr:
validationplot(pcr_mod, val.type = "R2")

# Now we find that the lowest cross-validation error occurs when M = 7 component are 
# used. We compute the test MSE as follows:
pcr_pred <- predict(pcr_mod, xMatrix[test_set, ], ncomp = 7)
pcr_MSE <- mean((pcr_pred - Y_test)^2)
pcr_MSE
# This test set MSE is competitive with the results obtained using ridge regression and 
# the lasso. However, as a result of the way PCR is implemented, the final model is more 
# difficult to interpret because it does not perform any kind of variable selection or 
# even directly produce coefficient estimates. 

# Finally, we fit PCR on the full data set, using M = 5, the number of components 
# identified by cross-validation:
pcr_mod <- pcr(Y_salary ~ xMatrix, scale = TRUE, ncomp = 7)
summary(pcr_mod)

dev.off()

# ======================================================================================
# Partial Least Squares ---------------------------------------------------
# ======================================================================================

# Set seed for reproducibility:
set.seed(1)

# Create PLS mod:
pls_mod <- plsr(Salary ~ ., data = Hitters, subset = train_set, scale = TRUE,
                validation = "CV")
summary(pls_mod)
# The lowest cross-validation error occurs when only M = 3 partial least squares 
# directions are used. We now evaluate the corresponding test set MSE:
pls_pred <- predict(pls_mod, xMatrix[test_set, ], ncomp = 2) 
mean((pls_pred - Y_test)^2)
# The test MSE is comparable to, but slightly higher than, the test MSE obtained using 
# ridge regression, the lasso, and PCR.

# Finally, we perform PLS using the full data set, using M = 2, the number of 
# components identified by cross-validation:
pls_fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls_fit)
# Notice that the percentage of variance in Salary that the two-component PLS fit 
# explains, 46.40 %, is almost as much as that explained using the final seven-component
# model PCR fit, 46.69 %. This is because PCR only attempts to maximize the amount of
# variance explained in the predictors, while PLS searches for directions that explain 
# variance in both the predictors and the response.

# ======================================================================================
# Analysing the College data set ------------------------------------------
# ======================================================================================

View(College)

dim(College) # 778 observation rows with 18 column variables 
names(College)

plot(College)

cor(College[, - 1])

# ======================================================================================
# Linear Model ------------------------------------------------------------
# ======================================================================================

# Train/Test Split --------------------------------------------------------
set.seed(1)

college_train <- sample(c(TRUE, FALSE), nrow(College), replace = TRUE)
college_test <- (!college_train)

# Fit a linear model using least squares on the training set, and
# report the test error obtained:

lm_mod_1 <- glm(Apps ~ Top25perc:Top10perc, data = College)
summary(lm_mod_1)

cv_err <- cv.glm(data = College, glmfit = lm_mod_1, K = 10)
cv_err$delta

# Explore significance of polynomial models:
set.seed(1)
CV_k_10 <- rep(0, 10)

for (i in 1:10) {
  lm_mod_2 <- glm(Apps ~ poly(Top25perc, i), data = College)
  CV_k_10[[i]] <- cv.glm(College, lm_mod_2, K = 10)$delta[1]
}
# little evidence thagt a higher order polynomial fit improves the model.

# Reporting test error:
Msqr_err <- mean((College$Apps - predict(lm_mod_1, College))[-college_train]^2)
Msqr_err

# ======================================================================================
# Ridge Model -------------------------------------------------------------
# ======================================================================================

# Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report
# the test error obtained.

# Remember we have to create a matrix for ridge regression!
xMatrix <- model.matrix(Apps ~ ., College)[, -1]
# ... the above notation is to clean the intercept column from the matrix.

# Create the Y (response) vector:
Y_apps <- College$Apps

# Train/Test Split --------------------------------------------------------
set.seed(1)

college_train <- sample(c(TRUE, FALSE), nrow(xMatrix), replace = TRUE)
college_test <- (-college_train)


Y_test <- Y_apps[college_test]

ridge_mod <- glmnet(xMatrix, Y_apps, alpha = 0)

# CV ----------------------------------------------------------------------

set.seed(1)

cv_ridgemod <- cv.glmnet(xMatrix[college_train, ], Y_apps[college_train], alpha = 0)
plot(cv_ridgemod)

best_lam <- cv_ridgemod$lambda.min
best_lam
# Therefore, we see that the value of λ that results in the smallest cross-validation 
# error is 394.2365 What is the test MSE associated with this value of λ?
ridge_pred <- predict(ridge_mod, s = best_lam, newx = xMatrix[college_test, ])
MSE_ridge <- mean((ridge_pred - Y_test)^2)
MSE_ridge

# Finally, we refit our ridge regression model on the full data set, using the value 
# of λ chosen by cross-validation, and examine the coefficient estimates.
ridge_best <- glmnet(xMatrix, Y_apps, alpha = 0)
predict(ridge_best, type = "coefficients", s = best_lam)[1:18, ]

# End file ----------------------------------------------------------------
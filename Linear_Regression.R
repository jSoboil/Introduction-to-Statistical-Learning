# Load Libraries ----------------------------------------------------------

library(MASS)
library(ISLR)

# ===============================================================================================
# Analysing the Boston Dataset --------------------------------------------------------
# ===============================================================================================

View(Boston)
names(Boston)
?Boston

# Simple Regression: medv as response & lstat as predictor ----------------

lm_medv <- lm(medv ~ lstat, data = Boston)
lm_medv
summary(lm_medv)

# We can use the names() function in order to find out what other pieces of info are stored in
# the model; although we can extract these quantities by name:
names(lm_medv)
lm_medv$coefficients

# In order to obtain a confidence interval for the coefficient estimates, we can use the 
# confint() command:
confint(lm_medv)

# The predict() function can be used to produce confidence intervals and prediction intervals 
# for the prediction of medv for a given value of lstat:
predict(lm_medv, data.frame(lstat = c(5, 10, 15)), 
        interval = "confidence")
predict(lm_medv, data.frame(lstat = c(5, 10 , 15)),
        interval = "prediction")

# We will now plot medv and lstat along with the least squares regression line using the 
# plot() and abline() functions:
plot(Boston$lstat, Boston$medv)
abline(lm_medv)
# There is some evidence for non-linearity in the relationship between lstat and medv. We will 
# explore this issue later.

# The abline() function can be used to draw any line, not just the least squares regression 
# line. To draw a line with intercept a and slope b, we type abline(a,b). The lwd = 3 command 
# causes the width of the regression line to be increased by a factor of 3; this works for the 
# plot() and lines() functions also. We can also use the pch option to create different 
# plotting symbols:
abline(lm_medv, lwd = 3)
abline(lm_medv, lwd = 3, col = "red")
plot(Boston$lstat, Boston$medv, col = "red")
plot(Boston$lstat, Boston$medv, pch = 20)
plot(Boston$lstat, Boston$medv, pch = '+')
plot(1:20, 1:20, pch = 1:20)

# Diagnostic Plots --------------------------------------------------------

# Four diagnostic plots are automatically produced by ap- plying the plot() function directly 
# to the output from lm(). In general, this command will produce one plot at a time, and 
# hitting Enter will generate the next plot. However, it is often convenient to view all four 
# plots together. We can achieve this by using the par() function, which tells R to split the 
# display screen into separate panels so that multiple plots can be viewed si- multaneously. 
# For example, par(mfrow=c(2,2)) divides the plotting region into a 2 × 2 grid of panels:
par(mfrow = c(2, 2))
plot(lm_medv)

# Alternatively, we can compute the residuals from a linear regression fit using the 
# residuals() function. The function rstudent() will return the studentized residuals, and we 
# can use this function to plot the residuals against the fitted values:
plot(predict(lm_medv), residuals(lm_medv))
plot(predict(lm_medv), rstudent(lm_medv))
# On the basis of the residual plots, there is some evidence of non-linearity. Leverage 
# statistics can be computed for any number of predictors using the hatvalues() function:
plot(hatvalues(lm_medv))
# The which.max() function identifies the index of the largest element of a vector. In this 
# case, it tells us which observation has the largest leverage statistic:
which.max(hatvalues(lm_medv))

# Multiple Linear Regression ----------------------------------------------

# In order to fit a multiple linear regression model using least squares, we again use the lm()
# function. The syntax lm(y∼x1+x2+x3) is used to fit a model with three predictors, x1, x2, 
# and x3. The summary() function now outputs the regression coefficients for all the predictors.

lm_mod_1 <- lm(medv ~ lstat + age, data = Boston)
summary(mr_medv)

# The Boston data set contains 13 variables, and so it would be cumbersome to have to type all
# of these in order to perform a regression using all of the predictors. Instead, we can use
# the following short-hand:
lm_mod_2 <- lm(medv ~ ., data = Boston)
summary(mr_med)
# We can see the options for summary by typing:
?summary.lm
# Hence:
summary(mr_med)$r.sq
summary(mr_med)$sigma

# The vif() vif() function, part of the car package, can be used to compute variance 
# inflation factors. Most VIF’s are low to moderate for this data:
library(car)
vif(mr_med)

# In the above regression output, age has a high p-value. So we may wish to run a regression
# excluding this predictor. The following syntax results in a regression using all predictors 
# except age:
lm_mod_3 <- lm(medv ~ . - age, data = Boston)
summary(mr_medv2)

# Alternatively, the update() function can be used:
lm_mod_4 <- update(mr_med, ~ . -age )
summary(mr_medv3)

# Creating Interaction Terms & Non-Linear Transformations ------------------------------------

# It is easy to include interaction terms in a linear model using the lm() function. The 
# syntax lstat:black tells R to include an interaction term between lstat and black. The 
# syntax lstat*age simultaneously includes lstat, age, and the interaction term lstat×age as 
# predictors; it is a shorthand for lstat+age+lstat:age:
summary(lm(medv ~ lstat*age, data = Boston))

# For instance, given a predictor X, we can create a predictor X2 using I(X^2). The function 
# I() is needed since the ^ has a special meaning I() in a formula; wrapping as we do allows 
# the standard usage in R, which is to raise X to the power 2. We now perform a regression of 
# medv onto lstat and lstat2:
lm_mod_5 <- lm(medv ~ lstat + I(lstat ^ 2), data = Boston)
summary(lm_fit2)

# The near-zero p-value associated with the quadratic term suggests that it leads to an 
# improved model. We use the anova() function to further quantify the extent to which the 
# quadratic fit is superior to the linear fit:
anova(lm_medv, lm_mod_5)
# Here the F-statistic is 135 and the associated p-value is virtually zero. This provides very
# clear evidence that the model containing the predictors lstat and lstat2 is far superior to 
# the model that only contains the predictor lstat. This is not surprising, since earlier we 
# saw evidence for non-linearity in the relationship between medv and lstat. If we type:
par(mfrow = c(2, 2))
plot(lm_mod_5)
# then we see that when the lstat2 term is included in the model, there is little discernible
# pattern in the residuals.

# In order to create a cubic fit, we can include a predictor of the form I(X^3). However, 
# this approach can start to get cumbersome for higher-order polynomials. A better approach 
# involves using the poly() function to create the polynomial within lm(). For example, the 
# following command produces a fifth-order polynomial fit:
lm_mod_6 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm_mod_6)
confint(lm_mod_6)

par(mfrow = c(2, 2))
plot(lm_mod_6)
# This suggests that including additional polynomial terms, up to fifth order, leads to an 
# improvement in the model fit! However, further investigation of the data reveals that no 
# polynomial terms beyond fifth order have signifi- cant p-values in a regression fit.

#Of course, we are in no way restricted to using polynomial transformations of the predictors.
# Here we try a log transformation:
lm_mod_7 <- lm(medv ~ log(rm), data = Boston)
lm_mod_7
anova(lm_mod_6, lm_mod_7)

# ===============================================================================================
# Analysing the Carseats Dataset -----------------------------------------------------------
# ===============================================================================================

# We will attempt to predict Sales (child car seat sales) in 400 locations based on a number 
# of predictors:
View(Carseats)
names(Carseats)

# The Carseats data includes qualitative predictors such as Shelveloc, an indicator of the 
# quality of the shelving location—that is, the space within a store in which the car seat is 
# displayed—at each location. The predictor Shelveloc takes on three possible values, Bad, 
# Medium, and Good:

# Given a qualitative variable such as Shelveloc, R generates dummy variables automatically!

# Below we fit a multiple regression model that includes some interaction terms:
lm_mod_8 <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm_mod_8)

# The contrasts() function returns the coding that R uses for the dummy variables:
contrasts(Carseats$ShelveLoc)
# Use ?contrasts to learn about other contrasts, and how to set them.

# R has created a ShelveLocGood dummy variable that takes on a value of 1 if the shelving 
# location is good, and 0 otherwise. It has also created a ShelveLocMedium dummy variable 
# that equals 1 if the shelving location is medium, and 0 otherwise. A bad shelving location 
# corresponds to a zero for each of the two dummy variables. The fact that the coefficient for
# ShelveLocGood in the regression output is positive indicates that a good shelving location 
# is associated with high sales (relative to a bad location). And ShelveLocMedium has a 
# smaller positive coefficient, indicating that a medium shelving location leads to higher 
# sales than a bad shelving location but lower sales than a good shelving location.

# ===============================================================================================
# Multiple Linear Regression on Auto Dataset ------------------------------
# ===============================================================================================

View(Auto)

# Produce a scatterplot matrix which includes all of the variables in the data set:
plot(Auto)
# Compute the matrix of correlations between the variables using the function cor(). You will 
# need to exclude the name variable, which is qualitative:
cor(Auto[, - 9])

# Use the lm() function to perform a multiple linear regression with mpg as the response and 
# all other variables except name as the predictors. Use the summary() function to print the 
# results:
lm_mod_9 <- lm(mpg ~ ., data = Auto[, - 9])
# Note: above subsetting excludes 'name' column.
summary(lm_mod_9$coefficients)
# There are singificant relationships between the predictors displacement, weight, year, and 
# origin and the response variable mpg. The current model explains 82% of the variability post-
# regression. The F-statistic suggests that there is a relationship between the response and
# predictors and has a significant probability value. RSE is small. 

plot(lm_mod_9)
# Although the Residual Sum of Squares shows a very slight pattern in the variation error for 
# the model, we judge it to not be significant. There is a observation with high leverage; 
# observation 14...

# The collinearity problem ------------------------------------------------

# The following focuses on the collinearity problem.
set.seed(1)
x1 <- runif(100)
x2 <- .5 * x1 + rnorm(n = 100) / 10
y <- 2 + 2 * x1 + .3 * x2 + rnorm(n = 100)
# The last line corresponds to creating a linear model in which y is a function of x1 and x2.
# Write out the form of the linear model. The form assumes a normal distribution of error. Hence
# the randomised normal distribution of 100. What are the regression coefficients? B0 & B1.

plot(x1, x2)
lm_mod_10 <- lm(y ~ x1 + x2)
summary(lm_mod_10)
lm_mod_11 <- lm(y ~ x1)
summary(lm_mod_11)
lm_mod_12 <- lm(y ~ x2)
summary(lm_mod_12)
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y,6)
lm_mod_13 <- lm(y ~ x1 + x2)
summary(lm_mod_13)
lm_mod_14 <- lm(y ~ x1)
summary(lm_mod_14)
lm_mod_15 <- lm(y ~ x2)
summary(lm_mod_15)
# Note: the collinearity problem whereby a simple linear regression of either variable has a
# significant output.
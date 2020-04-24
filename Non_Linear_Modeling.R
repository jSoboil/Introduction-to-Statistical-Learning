# In this lab, we re-analyze the Wage data considered in the examples through- out this chapter, 
# in order to illustrate the fact that many of the complex non-linear fitting procedures 
# discussed can be easily implemented in R. We begin by loading the ISLR library, which contains
# the data.

# ==============================================================================================
# Load Libraries ----------------------------------------------------------
# ==============================================================================================
library(ISLR)

# ==============================================================================================
# Polynomial regression and Step-wise Functions
# ==============================================================================================

# Here we fit a linear polynomial model utilising the poly() command:
mod_fit_1 <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(mod_fit_1))

# The poly() command allows us to avoid having to write out a long formula with powers of age.
# The function returns a matrix whose columns are a basis of orthogonal polynomials, which 
# essentially means that each column is a linear orthogonal combination of the variables age, 
# age^2, age^3 and age^4.

# There are several other equivalent ways of fitting this model, which showcase the 
# flexibility of the formula language in R. For example:
mod_fit_2 <- lm(wage ~ age + I(age^2) + I(age^3) + I(wage ^ 4), data = Wage)
coef(mod_fit_2)
summary(mod_fit_2)

#  Later we see that this does not affect the model in a meaningful way—though the choice of 
# basis clearly affects the coefficient estimates, it does not affect the fitted values 
# obtained.

# We now create a grid of values for age at which we want predictions, and then call the 
# generic predict() function, specifying that we want standard errors as well:
age_lim <- range(Wage$age)

age_grid <- seq(from = age_lim[1], to = age_lim[2])

preds <- predict(mod_fit_1, newdata = list(age = age_grid), se = TRUE)

se_bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

# Finally, we plot the data and add the fit from the degree-4 polynomial:
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(Wage$age, Wage$wage, xlim = age_lim, cex = .5, col = "darkgrey")
title("Degree -4 Polynomial", outer = TRUE)
lines(age_grid, preds$fit, lwd = 2, col = "blue")
matlines(age_grid, se_bands, lwd = 1, col = "blue", lty = 3)
# Here the mar and oma arguments to par() allow us to control the margins of the plot, and the
# title() function creates a figure title that spans both subplots.

# Validating the best degree of polynomial to use using ANOVA -------------------------

# In performing a polynomial regression we must decide on the degree of the polynomial to use. 
# One way to do this is by using hypothesis tests. We now fit models ranging from linear to a 
# degree-5 polynomial and seek to determine the simplest model which is sufficient to explain 
# the relationship. between wage and age. We use the anova() function, which performs an 
# analysis of variance (ANOVA, using an F-test) in order to test the null hypothesis that a 
# model M1 is sufficient to explain the data against the alternative hypothesis that a more 
# complex model M2 is required. In order to use the anova() function, M1 and M2 must be nested
# models: the predictors in M1 must be a subset of the predictors in M2. In this case, we fit 
# five different models and sequentially compare the simpler model to the more complex model.
mod_fit_1 <- lm(wage ~ age, data = Wage)
mod_fit_2 <- lm(wage ~ poly(age, 2), data = Wage)
mod_fit_3 <- lm(wage ~ poly(age, 3), data = Wage)
mod_fit_4 <- lm(wage ~ poly(age, 4), data = Wage)
mod_fit_5 <- lm(wage ~ poly(age, 5), data = Wage)

anova(mod_fit_1, mod_fit_2, mod_fit_3, mod_fit_4, mod_fit_5)
# The p-value comparing the linear Model 1 to the quadratic Model 2 is essentially zero 
# (<10^-15), indicating that a linear fit is not sufficient. Similarly the p-value comparing 
# the quadratic Model 2 to the cubic Model 3 is very low (0.0017), so the quadratic fit is 
# also insufficient. The p-value comparing the cubic and degree-4 polynomials, Model 3 and
# Model 4, is approximately 5 % while the degree-5 polynomial Model 5 seems unnecessary 
# because its p-value is 0.37. Hence, either a cubic or a quartic polynomial appear to provide
# a reasonable fit to the data, but lower- or higher-order models are not justified.

# In this case, instead of using the anova() function, we could have obtained these p-values
# more succinctly by exploiting the fact that poly() creates orthogonal polynomials:
coef(summary(mod_fit_5))
# Notice that the p-values are the same, and in fact the square of the t-statistics are equal 
# to the F-statistics from the anova() function; for example:
(-11.9830341) ^ 2

# However, the ANOVA method works whether or not we used orthogonal polynomials; it also works
# when we have other terms in the model as well. For example, we can use anova() to compare 
# these three models:
mod_fit_1 <- lm(wage ~ education + age, data = Wage)
mod_fit_2 <- lm(wage ~ education + poly(age, 2), data = Wage)
mod_fit_3 <- lm(wage ~ education + poly(age, 3), data = Wage)

anova(mod_fit_1, mod_fit_2, mod_fit_3)
# As an alternative to using hypothesis tests and ANOVA, we could choose the polynomial 
# degree using cross-validation.

# Next we consider the task of predicting whether an individual earns more than $250,000 per 
# year. We proceed much as before, except that first we create the appropriate response 
# vector, and then apply the glm() function using family="binomial" in order to fit a 
# polynomial logistic regression model:
mod_fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = "binomial")
# Note that we again use the wrapper I() to create this binary response variable on the fly. 
# The expression wage > 250 evaluates to a logical variable containing TRUEs and FALSEs, 
# which glm() coerces to binary by setting the TRUEs to 1 and the FALSEs to 0.

# Once again, we make predictions using the predict() function:
preds <- predict(mod_fit, newdata = list(age = age_grid), se.fit = TRUE)

# However, calculating the confidence intervals is slightly more involved than in the linear 
# regression case. The default prediction type for a glm() model is type = "link", which is
# what we use here. This means we get predictions for the logit, and outcomes transformed into
# log-odds.
pfit <- exp(preds$fit) / (1 + exp(preds$fit))

se_bands_logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

se_bands <- exp(se_bands_logit) / (1 + exp(se_bands_logit))

# Note that we could have directly computed the probabilities by selecting the 
# type = "response" option in the predict() function.
preds <- predict(mod_fit, newdata = list(age = age_grid), type = "response", se.fit = TRUE)
# However, the corresponding confidence intervals would not have been sen- sible because we 
# would end up with negative probabilities!

# Finally, we plot the data:
plot(Wage$age, I(Wage$wage > 250), xlim = age_lim, type = "n", ylim = c(0, .2))
points(jitter(Wage$age), I((Wage$wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey")
lines(age_grid, pfit, lwd = 2, col = "blue")
matlines(age_grid, se_bands, lwd = 1, col = "blue", lty = 3)
# We have drawn the age values corresponding to the observations with wage values above 250 
# as gray marks on the top of the plot, and those with wage values below 250 are shown as 
# gray marks on the bottom of the plot. We used the jitter() function to jitter the age 
# values a bit so that observations with the same age value do not cover each other up. This
# is often called a rug plot.

# Step-wise Functions -----------------------------------------------------
# In order to fit a step function we use the cut() function:
table(cut(Wage$age, 4))

mod_fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(mod_fit))
# Here cut() automatically picked the cutpoints at 33.5, 49, and 64.5 years of age. We could
# also have specified our own cutpoints directly using the breaks option. The function cut() 
# returns an ordered categorical variable; the lm() function then creates a set of dummy 
# variables for use in the regression. The age < 33.5 category is left out, so the intercept 
# coefficient of $94,160 can be interpreted as the average salary for those under 33.5 years 
# of age, and the other coefficients can be interpreted as the average additional salary for 
# those in the other age groups. We can produce predictions and plots just as we did in the 
# case of the polynomial fit.

dev.off()
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
# ==============================================================================================
# Splines, Smoothing-Splines, and Local Regression --------------------------------
# ==============================================================================================
# In order to fit regression splines in R, we use the splines library.
library(splines)

# Regression splines can be fit by constructing an appropriate matrix of basis functions. The
# bs() function generates the entire matrix of bs() basis functions for splines with the 
# specified set of knots. By default, cubic splines are produced. Fitting wage to age using 
# a regression spline is simple:
mod_fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)

pred <- predict(mod_fit, newdata = list(age = age_grid), se.fit = TRUE)

plot(Wage$age, Wage$wage, col = "gray")

lines(age_grid, pred$fit, lwd = 2)

# standard deviation of error positive
lines(age_grid, pred$fit + 2 * pred$se.fit, lty = "dashed")

# standard deviation of error negative
lines(age_grid, pred$fit - 2 * pred$se.fit, lty = "dashed")

# Here we have prespecified knots at ages 25, 40, and 60. This produces a spline with six 
# basis functions. (Recall that a cubic spline with three knots has seven degrees of freedom;
# these degrees of freedom are used up by an intercept, plus six basis functions.) We could 
# also use the df option to produce a spline with knots at uniform quantiles of the data:
dim(bs(Wage$age, knots = c(25, 40, 60)))

dim(bs(Wage$age, df = 6))

attr(bs(Wage$age, df = 6), "knots")
# In this case R chooses knots at ages 33.8, 42.0, and 51.0, which correspond to the 25th, 
# 50th, and 75th percentiles of age. The function bs() also has a degree argument, so we can 
# fit splines of any degree, rather than the default degree of 3 (which yields a cubic 
# spline).

# In order to instead fit a natural spline, we use the ns() function. Here ns() we fit a 
# natural spline with four degrees of freedom:
mod_fit_2 <- lm(wage ~ ns(age, df = 4), data = Wage)

preds_2 <- predict(mod_fit_2, newdata = list(age = age_grid), se.fit = TRUE)

lines(age_grid, preds_2$fit, col = "red", lwd = 2)
# As with the bs() function, we could instead specify the knots directly using the knots 
# option.

# In order to fit a smoothing spline, we use the smooth.spline() function:
plot(Wage$age, Wage$wage, xlim = age_lim, cex = .5, col = "darkgrey")
title("Smoothin Spline")

mod_fit <- smooth.spline(Wage$age, Wage$wage, df = 16)

mod_fit_2 <- smooth.spline(Wage$age, Wage$wage, cv = TRUE)

mod_fit_2$df

lines(mod_fit, col = "red", lwd = 2)
lines(mod_fit_2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)
# Notice that in the first call to smooth.spline(), we specified df = 16. The function then 
# determines which value of λ leads to 16 degrees of freedom. In the second call to 
# smooth.spline(), we select the smoothness level by cross- validation; this results in a 
# value of λ that yields 6.8 degrees of freedom.

# Local Regression --------------------------------------------------------

# In order to perform local regression, we use the loess() function:
plot(Wage$age, Wage$wage, xlim = age_lim, cex = .5, col = "darkgrey")
title("Local Regression")

mod_fit <- loess(wage ~ age, span = .2, data = Wage)
mod_fit_2 <- loess(wage ~ age, span = .5, data = Wage)

lines(age_grid, predict(mod_fit, data.frame(age = age_grid)), 
      col = "red", lwd = 2)
lines(age_grid, predict(mod_fit_2, data.frame(age = age_grid)), 
      col = "blue", lwd = 2)

legend("topright", legend = c("Span = .2", "Span = .5"), col = c("red", "blue"), 
       lty = 1, lwd = 2, cex = .8)
# Here we have performed local linear regression using spans of 0.2 and 0.5: that is, each 
# neighborhood consists of 20 % or 50 % of the observations. The larger the span, the 
# smoother the fit. The locfit library can also be used for fitting local regression models 
# in R.

# ==============================================================================================
# Generalised Additive Models ---------------------------------------------
# ==============================================================================================

# We now fit a GAM to predict wage using natural spline functions of year and age, treating 
# education as a qualitative predictor. Since this is just a big linear regression model 
# using an appropriate choice of basis functions, we can simply do this using the lm() 
# function:
gam_mod <- lm(wage ~ ns(year, 4) + ns(age, 5), education, data = Wage)

# We now fit the model using smoothing splines rather than natural splines. In order to fit
# more general sorts of GAMs, using smoothing splines or other components that cannot be 
# expressed in terms of basis functions and then fit using least squares regression, we will 
# need to use the gam library in R.
library(gam)

# The s() function, which is part of the gam library, is used to indicate that s() we would 
# like to use a smoothing spline. We specify that the function of year should have 4 degrees
# of freedom, and that the function of age will have 5 degrees of freedom. Since education is
# qualitative, we leave it as is, and it is converted into four dummy variables. We use the 
# gam() function in gam() order to fit a GAM using these components. All of the terms are fit
# simultaneously, taking each other into account to explain the response:
gam_mod_3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

# Now we plot the model:
par(mfrow = c(1, 3))
plot(gam_mod_3, se = TRUE, col = "blue")

# The generic plot() function recognizes that gam.m3 is an object of class gam, and invokes 
# the appropriate plot.gam() method. Conveniently,even though gam_mod is not of class gam but
# rather of class lm, we can still use plot.gam() on it:
plot.Gam(gam_mod_3, se = TRUE, col = "red")
# Notice here we had to use plot.gam() rather than the generic plot() function.

# In these plots, the function of year looks rather linear. We can perform a series of ANOVA 
# tests in order to determine which of these three models is best: a GAM that excludes year 
# (Model 1), a GAM that uses a linear function of year (Model 2), or a GAM that uses a 
# spline function of year (Model 3):
gam_mod <- gam(wage ~ s(age , 5) + education, data = Wage)
gam_mod_2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)

anova(gam_mod, gam_mod_2, gam_mod_3, test = "F")

# We find that there is compelling evidence that a GAM with a linear function of year is 
# better than a GAM that does not include year at all (p-value = 0.00014). However, there is 
# no evidence that a non-linear function of year is needed (p-value = 0.349). In other words,
# based on the results of this ANOVA, M2 is preferred.

# The summary() function produces a summary of the gam fit:
summary(gam_mod_3)
# The p-values for year and age correspond to a null hypothesis of a linear relationship 
# versus the alternative of a non-linear relationship. The large p-value for year reinforces 
# our conclusion from the ANOVA test that a linear function is adequate for this term. 
# However, there is very clear evidence that a non-linear term is required for age.

# We can make predictions from gam objects, just like from lm objects, using the predict() 
# method for the class gam. Here we make predictions on the training set:
preds <- predict(gam_mod_2, newdata = Wage)

# We can also use local regression fits as building blocks in a GAM, using the lo() function:
gam_lo <- gam(wage ~ s(year, df = 4) + lo(age, span = .7) + education, 
              data = Wage)
plot.Gam(gam_lo, se = TRUE, col = "green")
# Here we have used local regression for the age term, with a span of 0.7. We can also use 
# the lo() function to create interactions before calling the gam() function. For example:
gam_lo_i <- gam(wage ~ lo(year, age, span = .5) + education, data = Wage)
# fits a two-term model, in which the first term is an interaction between year and age, fit 
# by a local regression surface. We can plot the resulting two-dimensional surface if we 
# first install the akima package:
library(akima)
plot(gam_lo_i)

# In order to fit a logistic regression GAM, we once again use the I() function in 
# constructing the binary response variable, and set family = binomial:
gam_logit <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = "binomial",
                 data = Wage)
par(mfrow = c(1, 3))
plot(gam_logit, se = TRUE, col = "green")

# It is easy to see that there are no high earners in the < HS category:
table(Wage$education, I(Wage$wage > 250))

# Hence, we fit a logistic regression GAM using all but this category. This provides more 
# sensible results:
gam_logit_noHS <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = "binomial",
                      data = Wage, subset = (education != "1. < HS Grad"))
plot(gam_logit_noHS, se = TRUE, col = "green")    
# No we see the spread better between the quantiles for the education predictor variable!

# End file ----------------------------------------------------------------
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ISLR)
library(caret)

# Explore Smarket data ----------------------------------------------------

# View the names of the variables:
names(Smarket)

# View the number of row x column dimensions
dim(Smarket)

# View a quintile summary of each column
summary(Smarket)

# Visually explore the data set:
pairs(Smarket)

# View the correlations between the variables:
cor(Smarket)
# the error message occurs because the direction variable is qualitative...
typeof(Smarket$Direction)

cor(Smarket[, -9])
# The only substantial correlation is between Year and Volume. 

plot(Smarket$Volume)
# by plotting the data we see that Volume is increasing over time. In other words, the average 
# number of shares traded daily increased from 2001 to 2005.

# Run Logistic Regression Model -------------------------------------------

# Fit a logistic regression model in order to predict Direction using Lag1 through Lag5 and 
# Volume. The glm() function fits generalized glm() linear models, a class of models that 
# includes logistic regression. The syntax generalized of the glm() function is similar to 
# that of lm(), except that we must pass in linear model the argument family=binomial in order
# to tell R to run a logistic regression rather than some other type of generalized linear
# model.

glm_mod_1 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                 data = Smarket, family = binomial)
summary(glm_mod_1)
# The p-value are large, and so there is no clear evidence of a real association between any 
# predictor variables.  The smallest p-value is for Lag1 and Direction.

# We use the coef() function in order to access just the coefficients for this fitted model. 
# We can also use the summary() function to access particular aspects of the fitted model, 
# such as the p-values for the coefficients:
coef(glm_mod_1) 
summary(glm_mod_1)$coef[, 4]

# The type = "response" option tells R to output probabilities of the form P(Y = 1|X), as 
# opposed to other information such as the logit. If no data set is supplied to the predict()
# function, then the probabilities are computed for the training data that was used to fit the
# logistic regression model. Here we have printed only the first ten probabilities. We know 
# that these values correspond to the probability of the market going up, rather than down, 
# because the contrasts() function indicates that R has created a dummy variable with a 1 for 
# Up:
glm_probs_1 <- predict(glm_mod_1, type = "response")
glm_probs_1[1:10]

contrasts(Smarket$Direction)

# In order to make a prediction as to whether the market will go up or down on a particular 
# day, we must convert these predicted probabilities into class labels, Up or Down. The 
# following two commands create a vector of class predictions based on whether the predicted 
# probability of a market increase is greater than or less than 0.5:
glm_pred_1 <- rep("Down", 1250)
glm_pred_1[glm_probs_1 > .5] <- "Up"

# The first command creates a vector of 1,250 Down elements. The second line transforms to Up 
# all of the elements for which the predicted probability of a market increase exceeds 0.5. 
# Given these predictions, the table() function can be used to produce a confusion matrix in 
# order to determine how many observations were correctly or incorrectly classified:
cMatrix_mod_1 <- table(glm_pred_1, Smarket$Direction)
cMatrix_mod_1

# At first glance, it appears that the logistic regression model is working a little better 
# than random guessing. However, this result is misleading because we trained and tested the 
# model on the same set of 1, 250 observations. In other words, 100 − 52.2 = 47.8 % is the 
# training error rate. As we have seen previously, the training error rate is often overly 
# optimistic — it tends to underestimate the test error rate. In order to better assess the 
# accuracy of the logistic regression model in this setting, we can fit the model using part
# of the data, and then examine how well it predicts the held out data.

# To implement this strategy, we will first create a vector corresponding to the observations
# from 2001 through 2004. We will then use this vector to create a held out data set of 
# observations from 2005:
train <- Smarket$Year < 2005
train

# Create a test set for predicting 2005:
Smarket_2005_test <- Smarket[!train, ]
dim(Smarket_2005)

# Create test set for Direction variable:
Direction_test <- Smarket$Direction[!train]

# We now fit a logistic regression model using only the subset of the observations that 
# correspond to dates before 2005, using the subset argument. We then obtain predicted 
# probabilities of the stock market going up for each of the days in our test set—that is, 
# for the days in 2005:
glm_mod_2 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                 data = Smarket, family = binomial, subset = train)

glm_probs_2 <- predict(glm_mod_2, Smarket_2005_test, type = "response")

# Finally, we compute the predictions for 2005 and compare them to the actual movements of 
# the market over that time period:
glm_pred_2 <- rep("Down", 252)
glm_pred_2[glm_probs_2 > .5] <- "Up"

table(glm_pred_2, Direction_test)

mean(glm_pred_2 == Direction_test)
mean(glm_pred_2 != Direction_test)

# The != notation means not equal to, and so the last command computes the test set error 
# rate. The results are rather disappointing: the test error rate is 52%, which is worse than
# random guessing! Of course this result is not all that surprising, given that one would not
# generally expect to be able to use previous days’ returns to predict future market 
# performance.

# We recall that the logistic regression model had very underwhelming p-values associated 
# with all of the predictors, and that the smallest p-value, though not very small, 
# corresponded to Lag1. Perhaps by removing the variables that appear not to be helpful in 
# predicting Direction, we can obtain a more effective model. After all, using predictors 
# that have no relationship with the response tends to cause a deterioration in the test
# error rate (since such predictors cause an increase in variance without a corresponding 
# decrease in bias), and so removing such predictors may in turn yield an improvement. Below
# we have refit the logistic regression using just Lag1 and Lag2, which seemed to have the 
# highest predictive power in the original logistic regression model:
glm_mod_3 <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, 
                 subset = train)

glm_probs_3 <- predict(glm_mod_3, Smarket_2005_test, type = "response")

glm_pred_3 <- rep("Down", 252)
glm_pred_3[glm_probs_3 > .5] <- "Up"

table(glm_pred, Direction_2005)

mean(glm_pred_3 == Direction_2005)
# Now the results appear to be a little better: 56% of the daily movements have been 
# correctly predicted. It is worth noting that in this case, a much simpler strategy of 
# predicting that the market will increase every day will also be correct 56% of the time! 
# Hence, in terms of overall error rate, the logistic regression method is no better than
# the näıve approach. However, the confusion matrix shows that on days when logistic
# regression predicts an increase in the market, it has a 58% accuracy rate. This suggests a
# possible trading strategy of buying on days when the model predicts an increasing market,
# and avoiding trades on days when a decrease is predicted. Of course one would need to 
# investigate more carefully whether this small improvement was real or just due to random 
# chance.

# Suppose that we want to predict the returns associated with particular values of Lag1 and 
# Lag2. In particular, we want to predict Direction on a day when Lag1 and Lag2 equal 1.2 and
# 1.1, respectively, and on a day when they equal 1.5 and −0.8. We do this using the 
# predict() function:
predict(glm_mod_3, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)),
        type = "response")


# Linear Discriminant Analysis --------------------------------------------

library(MASS)

# In R, we fit an LDA model using the lda() function, which is part of the MASS library. 
# Notice that the syntax for the lda() function is identical to that of lm(), and to that of
# glm() except for the absence of the family option. We fit the model using only the 
# observations before 2005:
lda_mod_1 <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda_mod_1

# The plot() function produces plots of the linear discriminants, obtained by computing
# − 0.642 × Lag1 − 0.514 × Lag2 for each of the training observations:
plot(lda_mod_1)

# The predict() function returns a list with three elements. 
lda_pred_1 <- predict(lda_mod_1, Smarket_2005_test)
names(lda_pred_1)
# The first element, class, contains LDA’s predictions about the movement of the market. 
# The second element, posterior, is a matrix whose kth column contains the posterior 
# probability that the corresponding observation belongs to the kth class. Finally, x 
# contains the linear discriminants, described earlier.

# The LDA and logistic regression predictions are almost identical:
lda_class <- lda_pred_1$class

table(lda_class, Direction_test)
mean(lda_class == Direction_test)

# Applying a 50 % threshold to the posterior probabilities allows us to recreate the 
# predictions contained in lda.pred$class:
sum(lda_pred_1$posterior[, 1] >= .5)
sum(lda_pred_1$posterior[, 1] < .5)

# Notice that the posterior probability output by the model corresponds to the probability 
# that the market will decrease:
lda_pred_1$posterior[1:20, 1]
lda_class[1:20]

# If we wanted to use a posterior probability threshold other than 50% in order to make 
# predictions, then we could easily do so. For instance, suppose that we wish to predict a 
# market decrease only if we are very certain that the market will indeed decrease on that 
# day—say, if the posterior probability is at least 90 %:
sum(lda_pred_1$posterior[, 1] > .9)
# Note we select column 1, which is the 'Down' column.

# No days in 2005 meet that threshold! In fact, the greatest posterior prob- ability of 
# decrease in all of 2005 was 52.02 %.


# Quadratic Discriminant Analysis -----------------------------------------

# The QDA function is also found in the MASS library.
qda_mod_1 <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda_mod_1

# The output contains the group means. But it does not contain the coefficients of the linear
# discriminants, because the QDA classifier involves a quadratic, rather than a linear, 
# function of the predictors. The predict() function works in exactly the same fashion as for
# LDA:
qda_class <- predict(qda_mod_1, Smarket_2005_test)$class

cMatrix_qda_mod <- table(qda_class, Direction_test)
cMatrix_qda_mod

mean(qda_class == Direction_test)
# Interestingly, the QDA predictions are accurate almost 60% of the time, even though the 
# 2005 data was not used to fit the model. This level of accuracy is quite impressive for 
# stock market data, which is known to be quite hard to model accurately. This suggests that 
# the quadratic form assumed by QDA may capture the true relationship more accurately than 
# the linear forms assumed by LDA and logistic regression. However, it is recommend evaluating
# this method’s performance on a larger test set before betting that this approach will 
# consistently beat the market!


# KNN ---------------------------------------------------------------------

# We will now perform KNN using the knn() function, which is part of the class library. This 
# function works rather differently from the other model-fitting functions that we have 
# encountered thus far. Rather than a two-step approach in which we first fit the model and 
# then we use the model to make predictions, knn() forms predictions using a single command. 
# The function requires four inputs:

# 1. A matrix containing the predictors associated with the training data
# 2. A matrix containing the predictors associated with the data for which we wish to make
#    predictions.
# 3. A vector containing the class labels for the training observations.
# 4. A value for K, the number of nearest neighbors to be used by the classifier.

# We use the cbind() function, short for column bind, to bind the Lag1 and Lag2 variables 
# together into two matrices, one for the training set and the other for the test set:
# we first need to load the class package...
library(class)
train_X <- cbind(Smarket$Lag1, Smarket$Lag2)[train, ]
train_X

test_X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train, ]
test_X

train_Direction <- Smarket$Direction[train]

# Now the knn() function can be used to predict the market’s movement for the dates in 2005. 
# We set a random seed before we apply knn() because if several observations are tied as 
# nearest neighbors, then R will randomly break the tie. Therefore, a seed must be set in 
# order to ensure reproducibility of results:
library(caret)
set.seed(1)
knn_pred <- knn(train = train_X, test = test_X, train_Direction, k = 1)

# Create confusion matrix:
cMatrix_knn <- table(knn_pred, Direction_test)
cMatrix_knn

PPV <- (43 + 83) / 252
PPV

# The results using K = 1 are not very good, since only 50 % of the observations are 
# correctly predicted. Of course, it may be that K = 1 results in an overly flexible fit to 
# the data. Below, we repeat the analysis using K = 3:
knn_pred_2 <- knn(train = train_X, test = test_X, train_Direction, k = 3)

# Create confusion matrix:
cMatrix_knn_2 <- table(knn_pred_2, Direction_test)
cMatrix_knn_2

PPV <- mean(knn_pred_2 == Direction_test)
PPV
# The results have improved slightly. But increasing K further turns out to provide no 
# further improvements. It appears that for this data, QDA provides the best results of the
# methods that we have examined so far.

# KNN: Caravan data set ---------------------------------------------------

dim(Caravan)

summary(Caravan$Purchase)

# True positive rate:
348 / 5822
# ...only 6% of people purchased caravan insurance.

# Because the KNN classifier predicts the class of a given test observation by identifying the
# observations that are nearest to it, the scale of the variables matters. Any variables that
# are on a large scale will have a much larger effect on the distance between the observations
# and hence on the KNN classifier, than variables that are on a small scale. For instance, 
# imagine a data set that contains two variables, salary and age (measured in dollars and 
# years, respectively). As far as KNN is concerned, a difference of $1,000 in salary is 
# enormous compared to a difference of 50 years in age. Consequently, salary will drive the 
# KNN classification results, and age will have almost no effect. This is contrary to our 
# intuition that a salary difference of $1, 000 is quite small compared to an age difference 
# of 50 years. Furthermore, the importance of scale to the KNN classifier leads to another 
# issue: if we measured salary in Japanese yen, or if we measured age in minutes, then we’d 
# get quite different classification results from what we get if these two variables are 
# measured in dollars and years.

# A good way to handle this problem is to standardize the data so that all variables are 
# given a mean of zero and a standard deviation of one. Then all variables will be on a 
# comparable scale. The scale() function does just this. In standardizing the data, we 
# exclude column 86, because that is the qualitative Purchase variable:
standardised_X <- scale(Caravan[, -86])

# Variation between variables prior to standardisation:
var(Caravan[, 1])
var(Caravan[, 2])

# Variation between variables post standardisation:
var(standardised_X[, 1])
var(standardised_X[, 2])
# Now every column of standardized.X has a standard deviation of one and a mean of zero.

# We now split the observations into a test set, containing the first 1,000 observations, and
# a training set, containing the remaining observations. We fit a KNN model on the training
# data using K = 1, and evaluate its performance on the test data.

# Create test reference to leave out the first 1000 rows:
test <- 1:1000

# Create training set that exlcudes first 1000 rows of each column:
train_X <- standardised_X[-test, ]
train_X

# Create test set that is made up of the first 1000 rows of each column
test_X <- standardised_X[test, ]
test_X

# Create training response variable:
train_Y <- Caravan$Purchase[-test]
train_Y

test_Y <- Caravan$Purchase[test]
test_Y

# Set seed for replicability:
set.seed(1)

knn_pred <- knn(train = train_X, test = test_X, train_Y, k = 1)

# Compute test error rate:
mean(test_Y != knn_pred)

# But since only 6% of customers purchased insurance, we could get the error rate down
# to 6 % by always predicting No regardless of the values of the predictors:
mean(test_Y != "No")
# it is not a very good error rate then...

# However, suppose that there is some non-trivial cost to trying to sell insurance to a given
# individual. For instance, perhaps a salesperson must visit each potential customer. If the
# company tries to sell insurance to a random selection of customers, then the success rate 
# will be only 6%, which may be far too low given the costs involved. Instead, the company 
# would like to try to sell insurance only to customers who are likely to buy it. So the 
# overall error rate is not of interest. Instead, the fraction of individuals that are 
# correctly predicted to buy insurance is of interest. It turns out that KNN with K = 1 does 
# far better than random guessing among the customers that are predicted to buy insurance. 
# Among 77 such customers, 9, or 11.7 %, actually do purchase insurance. This is double the 
# rate that one would obtain from random guessing:
cMatrix_knn <- table(knn_pred, test_Y)
cMatrix_knn

# Using K = 3, the success rate increases to 19 %:
knn_pred <- knn(train_X, test_X, train_Y, k = 3)
plot(knn_pred)

cMatrix_knn <- table(knn_pred, test_Y)
cMatrix_knn

# Accuracy:
sum(diag(cMatrix_knn)) / sum(cMatrix_knn)

# Sensitivty:
5/26
# The model has low specificty of 19.2%; likley the model has a high type II error rate.

# Specificity:
920/(920 + 54)
# The model has high specificty and therefore a low type I error rate.

# With K = 5 the rate is 26.7 %. This is over four times the rate that results from random 
# guessing. It appears that KNN is finding some real patterns in a difficult data set:
knn_pred <- knn(train_X, test_X, train_Y, k = 5)
plot(knn_pred)

cMatrix_knn <- table(knn_pred, test_Y)
cMatrix_knn

# Sensitivity:
4 / 15
# with five kernals, we see an improvement in sensitivity to 26%. Therefore, a reduced type II
# error rate.

# Specificity:
930 / (930 + 55)
# a high specificty rate, indicating a low type I error rate.


# End file ----------------------------------------------------------------
# ==============================================================================================
# Fitting Classification Trees --------------------------------------------
# ==============================================================================================
# The tree library is used to construct classification and regression trees:
library(tree)

# We first use classification trees to analyze the Carseats data set:
library(ISLR)

# In these data, Sales is a continuous variable, and so we begin by recoding it as a binary 
# variable. We use the ifelse() function to create a variable, called High, which takes on a 
# value of Yes if the Sales variable exceeds 8, and takes on a value of No otherwise:
High <- ifelse(Carseats$Sales <= 8, "No", "Yes")

# Finally, we use the data.frame() function to merge High with the rest of the Carseats data:
Carseats <- data.frame(Carseats, High)
summary(Carseats)

# We now use the tree() function to fit a classification tree in order to predict High using 
# all variables but Sales. The syntax of the tree() function is quite similar to that of the 
# lm() function:
tree_carseats <- tree(High ~ . - Sales, data = Carseats)
# The summary() function lists the variables that are used as internal nodes in the tree, the
# number of terminal nodes, and the (training) error rate;
summary(tree_carseats)
# A small deviance indicates a tree that provides a good fit to the (training) data.

# One of the most attractive properties of trees is that they can be graphically displayed. We
# use the plot() function to display the tree structure, and the text() function to display 
# the node labels. The argument pretty = 0 instructs R to include the category names for any 
# qualitative predictors, rather than simply displaying a letter for each category:
plot(tree_carseats)
text(tree_carseats, pretty = 0)
# The most important indicator of Sales appears to be shelving location, since the first 
# branch differentiates Good locations from Bad and Medium locations.

# If we just type the name of the tree object, R prints output corresponding to each branch of
# the tree. R displays the split criterion (e.g. Price < 92.5), the number of observations in
# that branch, the deviance, the overall prediction for the branch (Yes or No), and the 
# fraction of observations in that branch that take on values of Yes and No. Branches that 
# lead to terminal nodes are indicated using asterisks:
tree_carseats

# In order to properly evaluate the performance of a classification tree on these data, we 
# must estimate the test error rather than simply computing the training error. We split the 
# observations into a training set and a test set, build the tree using the training set, and 
# evaluate its performance on the test data. The predict() function can be used for this 
# purpose. In the case of a classification tree, the argument type = "class" instructs R to 
# return the actual class prediction. This approach leads to correct predictions for around 
# 71.5 % of the locations in the test data set:
set.seed(2)

Car_train_set <- sample(1:nrow(Carseats), 200)

Car_test_set <- Carseats[-Car_train_set, ]

High_test <- High[-Car_train_set]

tree_carseats <- tree(High ~ . - Sales, data = Carseats, subset = Car_train_set)
tree_pred <- predict(tree_carseats, newdata = Car_test_set, type = "class")

# Confusion matrix:
cMatrix <- table(tree_pred, High_test)
# Accuracy:
Accuracy <- sum(diag(cMatrix)) / sum(cMatrix)
Accuracy

# Next, we consider whether pruning the tree might lead to improved results.

# Pruning Classification Trees --------------------------------------------

# The function cv.tree() performs cross-validation in order to determine the optimal level of 
# tree complexity; cost complexity pruning is used in order to select a sequence of trees for 
# consideration. We use the argument FUN = prune.misclass in order to indicate that we want 
# the classification error rate to guide the cross-validation and pruning process, rather than
# the default for the cv.tree() function, which is deviance. The cv.tree() function reports 
# the number of terminal nodes of each tree considered (size) as well as the corresponding 
# error rate and the value of the cost-complexity parameter used (k, which corresponds to α):
set.seed(3)
cv_tree_carseats <- cv.tree(tree_carseats, FUN = prune.misclass)
names(cv_tree_carseats)

cv_tree_carseats$size
cv_tree_carseats$dev
cv_tree_carseats$k
cv_tree_carseats$method
attr(cv_tree_carseats, "class")

# Note that, despite the name, dev corresponds to the cross-validation error rate in this 
# instance. The tree with 9 terminal nodes results in the lowest cross-validation error rate,
# with 50 cross-validation errors. We plot the error rate as a function of both size and k:
par(mfrow = c(1, 2))
plot(cv_tree_carseats$size, cv_tree_carseats$dev, type = "b")
plot(cv_tree_carseats$k, cv_tree_carseats$dev, type = "b")

# We now apply the prune.misclass() function in order to prune the tree to obtain the 
# nine-node tree, and then plot the output:
prune_carseats <- prune.misclass(tree_carseats, best = 9)
plot(prune_carseats)
text(prune_carseats, pretty = 0)

# How well does this pruned tree perform on the test data set? Once again, we apply the 
# predict() function:
tree_pred <- predict(prune_carseats, newdata = Car_test_set, type = "class")

# cMatrix:
cMatrix <- table(tree_pred, High_test)
# Classification Accuracy:
Accuracy <- sum(diag(cMatrix)) / sum(cMatrix)
Accuracy

# Now 77 % of the test observations are correctly classified, so not only has the pruning 
# process produced a more interpretable tree, but it has also improved the classification 
# accuracy.

# If we increase the value of best, we obtain a larger pruned tree with lower classification
# accuracy:
prune_carseats <- prune.misclass(tree_carseats, best = 15)
plot(prune_carseats)
text(prune_carseats, pretty = 0)

tree_pred <- predict(prune_carseats, newdata = Car_test_set, type = "class")

cMatrix <- table(tree_pred, High_test)
#  Accuracy:
Accuracy <- sum(diag(cMatrix)) / sum(cMatrix)
Accuracy

# ==============================================================================================
# Fitting Regression Trees ------------------------------------------------
# ==============================================================================================
# Here we fit a regression tree to the Boston data set. First, we create a training set, and 
# fit the tree to the training data;
library(MASS)

set.seed(1)
train_set <- sample(1:nrow(Boston), nrow(Boston) / 2)

tree_boston <- tree(medv ~ ., data = Boston, subset = train_set)
summary(tree_boston)

# Notice that the output of summary() indicates that only four of the variables have been 
# used in constructing the tree. In the context of a regression tree, the deviance is simply 
# the sum of squared errors for the tree. We now plot the tree:
plot(tree_boston)
text(tree_boston, pretty = 0)

# Now we use the cv.tree() function to see whether pruning the tree will improve performance:
cv_boston <- cv.tree(tree_boston)
plot(cv_boston$size, cv_boston$dev, type = "b")

# In this case, the most complex tree is selected by cross-validation. However, if we wish to
# prune the tree, we could do so as follows, using the prune.tree() function:
prune_boston <- prune.tree(tree_boston, best = 5)

plot(prune_boston)
text(prune_boston, pretty = 0)

# In keeping with the cross-validation results, we use the unpruned tree to make predictions 
# on the test set:
yhat_tree_boston <- predict(tree_boston, newdata = Boston[-train_set, ])

boston_test <- Boston[-train_set, "medv"]

plot(yhat_tree_boston, boston_test)
abline(0, 1)

# Mean Squared Error:
MSE <- mean((yhat_tree_boston - boston_test) ^ 2)
MSE
sqrt(MSE)
# In other words, the test set MSE associated with the regression tree is 35.28688. The square 
# root of the MSE is therefore around 5.940, indicating that this model leads to test 
# predictions that are within around $5, 940 of the true median home value for the suburb.

# ==============================================================================================
# Bagging and Random Forests ----------------------------------------------
# ==============================================================================================
# Here we apply bagging and random forests to the Boston data, using the randomForest package
# in R. Note that results may vary according to the version of R you have installed...
library(randomForest)

# Recall that bagging is simply a special case of a random forest with m = p. Therefore, the 
# randomForest() function can be used to perform both random forests and bagging. We perform 
# bagging Forest() as follows:
set.seed(1)
bag_boston <- randomForest(medv ~ ., data = Boston, subset = train_set, mtry = 13,
                           importance = TRUE)
bag_boston

# The argument mtry = 13 indicates that all 13 predictors should be considered for each split
# of the tree — in other words, that bagging should be done. How well does this bagged model 
# perform on the test set?
yhat_bag <- predict(bag_boston, newdata = Boston[-train_set, ])

plot(yhat_bag, boston_test)
abline(0, 1)

# Mean Squared Error:
MSE_bag <- mean((yhat_bag - boston_test) ^ 2)
MSE_bag

# The test set MSE associated with the bagged regression tree is 23.6, almost half that 
# obtained using an optimally-pruned single tree. 

# We could change the number of trees grown by randomForest() using the ntree argument:
bag_boston <- randomForest(medv ~ ., data = Boston, subset = train_set, mtry = 13, 
                           ntree = 25)
yhat_bag <- predict(bag_boston, newdata = Boston[-train_set, ])

# Mean Squared Error:
MSE_bag <- mean((yhat_bag - boston_test) ^ 2)
MSE_bag

# Growing a random forest proceeds in exactly the same way, except that we use a smaller 
# value of the mtry argument. By default, randomForest() uses p/3 variables when building a 
# random forest of regression trees, and sqrt(p) variables when building a random forest of 
# classification trees. Here we use mtry = 6:
set.seed(1)
rforest_boston <- randomForest(medv ~ ., data = Boston, subset = train_set, mtry = 6, 
                              importance = TRUE)
yhat_rforest <- predict(rforest_boston, newdata = Boston[-train_set, ])
# Mean Squared Error:
MSE_rforest <- mean((yhat_rforest - boston_test) ^ 2)
MSE_rforest
# The test set MSE is 19.62; this indicates that random forests yielded an improvement over 
# bagging in this case.

# Note that using the importance() function, we can view the importance of each variable:
importance(rforest_boston)

# Two measures of variable importance are reported. The former is based upon the mean 
# decrease of accuracy in predictions on the out of bag samples when a given variable is 
# excluded from the model. The latter is a measure of the total decrease in node impurity 
# that results from splits over that variable, averaged over all trees. In the case of 
# regression trees, the node impurity is measured by the training RSS, and for classification
# trees by the deviance. Plots of these importance measures can be produced using the 
# varImpPlot() function:
varImpPlot(rforest_boston)

# The results indicate that across all of the trees considered in the random forest, the 
# wealth level of the community (lstat) and the house size (rm) are by far the two most 
# important variables.

# ==============================================================================================
# Boosting ----------------------------------------------------------------
# ==============================================================================================
# Here we use the gbm package,and within it the gbm() function, to fit boosted regression 
# trees to the Boston data set. 
library(gbm)

# We run gbm() with the option distribution = "gaussian" since this is a regression problem; 
# if it were a binary classification problem, we would use distribution = "bernoulli". The 
# argument n.trees = 5000 indicates that we want 5000 trees, and the option 
# interaction.depth = 4 limits the depth of each tree.
set.seed(1)

# Note that the gmb() function does not have a subset argument, and therefore it must be 
# explicitly supplied via subsetting the original Boston data set:
boost_boston <- gbm(medv ~ ., data = Boston[train_set, ], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4)

# The summary() function produces a relative influence plot and also outputs the relative 
# influence statistics:
summary(boost_boston)

# We see that lstat and rm are by far the most important variables. We can also produce 
# partial dependence plots for these two variables. These plots illustrate the marginal 
# effect of the selected variables on the response after integrating out the other variables.
# In this case, as we might expect, median house prices are increasing with rm and decreasing
# with lstat:
par(mfrow = c(1, 2))
plot(boost_boston, i.var = "rm")
plot(boost_boston, i.var = "lstat")

# We now use the boosted model to predict medv on the test set:
yhat_boost <- predict(boost_boston, newdata = Boston[-train_set, ], n.trees = 5000)

# Mean Squared Error:
boost_MSE <- mean((yhat_boost - boston_test) ^ 2)
boost_MSE
# The test MSE obtained is 18.84; similar to the test MSE for random forests and superior to 
# that for bagging. If we want to, we can perform boosting with a different value of the 
# shrinkage parameter λ. The default value is 0.001, but this is easily modified. Here we
# take λ = 0.2:
boost_boston <- gbm(medv ~ ., data = Boston[train_set, ], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, 
                    verbose = FALSE)
yhat_boost <- predict(boost_boston, newdata = Boston[-train_set, ], n.trees = 5000)

# Mean Squared Error;
boost_MSE_2 <- mean((yhat_boost - boston_test) ^ 2)
boost_MSE_2
# In this case, using λ = 0.2 leads to a slightly higher test MSE than λ = 0.001.

# End file ----------------------------------------------------------------
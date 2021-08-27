# We use the e1071 library in R to demonstrate the support vector classifier and the SVM. 
# Another option is the LiblineaR library, which is useful for very large linear problems.
library(e1071)

# ==============================================================================================
# Support Vector Classifier -----------------------------------------------
# ==============================================================================================
# The e1071 library contains implementations for a number of statistical learning methods. In
# particular, the svm() function can be used to fit a support vector classifier when the 
# argument kernel = "linear" is used.

# A cost argument allows us to specify the cost of a violation to the margin. When the cost 
# argument is small, then the margins will be wide and many support vectors will be on the
# margin or will violate the margin. When the cost argument is large, then the margins will be
# narrow and there will be few support vectors on the margin or violating the margin.

# We now use the svm() function to fit the support vector classifier for a given value of the 
# cost parameter. Here we demonstrate the use of this function on a two-dimensional example 
# so that we can plot the resulting decision boundary. We begin by generating the 
# observations, which belong to two classes:
set.seed(1)
X <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
X[y == 1, ] <-  X[y == 1, ] + 1

# We begin by checking whether the classes are linearly separable:
plot(X, col = (3 - y))
# They are not. Next, we fit the support vector classifier. Note that in order for the svm() 
# function to perform classification (as opposed to SVM-based regression), we must encode the 
# response as a factor variable. We now create a data frame with the response coded as a 
# factor:
df <- data.frame(X = X, y = as.factor(y))

svm_fit <- svm(y ~ ., data = df, kernel = "linear", cost = 10,
               scale =  FALSE)
# The argument scale=FALSE tells the svm() function not to scale each feature to have mean 
# zero or standard deviation one; depending on the application, one might prefer to use 
# scale = TRUE.

# We can now plot the support vector classifier obtained:
plot(svm_fit, df)

# Note that the two arguments to the plot.svm() function are the output of the call to svm(), 
# as well as the data used in the call to svm(). The region of feature space that will be 
# assigned to the −1 class is shown in light cream, and the region that will be assigned to 
# the +1 class is shown in red. The decision boundary between the two classes is linear 
# (because we used the argument kernel = "linear"), though due to the way in which the 
# plotting function is implemented in this library the decision boundary looks somewhat jagged
# in the plot. We see that in this case only one observation is misclassified. (Note that 
# here the second feature is plotted on the x-axis and the first feature is plotted on the 
# y-axis, in contrast to the behavior of the usual plot() function in R.) The support vectors 
# are plotted as crosses and the remaining observations are plotted as circles; we see here 
# that there are seven support vectors. We can determine their identities as follows:
svm_fit$index

# We can obtain some basic information about the support vector classifier fit using the 
# summary() command:
summary(svm_fit)
# This tells us, for instance, that a linear kernel was used with cost = 10, and that there 
# were 3 support vectors, 2 in one class and 1 in the other.

# What if we instead used a smaller value of the cost parameter?
svm_fit <- svm(y ~ ., data = df, kernel = "linear", cost = .1, 
               scale = FALSE)
plot(svm_fit, df)
svm_fit$index
# Now that a smaller value of the cost parameter is being used, we obtain a larger number of
# support vectors, because the margin is now wider. Unfortunately, the svm() function does 
# not explicitly output the coefficients of the linear decision boundary obtained when the 
# support vector classifier is fit, nor does it output the width of the margin.

# The e1071 library includes a built-in function, tune(), to perform cross- validation. By 
# default, tune() performs ten-fold cross-validation on a set of models of interest. In order
# to use this function, we pass in relevant information about the set of models that are 
# under consideration. The following command indicates that we want to compare SVMs with a 
# linear kernel, using a range of values of the cost parameter:
set.seed(1)
tune_out <- tune(svm, y ~ ., data = df, kernel = "linear", 
                 ranges = list(cost = c(.001, .01, .1, 1, 5, 10, 100)))
# We can easily access the cross-validation errors for each of these models using the 
# summary() command:
summary(tune_out)

# We see that cost = 0.1 & .1 result in the lowest cross-validation error rate. It is probably
#  best to choose the less complex model. The tune() function stores the best model obtained,
# which can be accessed as follows:
best_mod <- tune_out$best.model
summary(best_mod)

# The predict() function can be used to predict the class label on a set of test observations,
# at any given value of the cost parameter. We begin by generating a test data set:
X_test <- matrix(rnorm(20 * 2), ncol = 2)
y_test <- sample(c(-1, 1), 20, rep = TRUE)
X_test[y == 1, ] <- X_test[y == 1] + 1
test_df <- data.frame(X = X_test, y = as.factor(y_test))

# Now we predict the class labels of these test observations. Here we use the best model 
# obtained through cross-validation in order to make predictions:
yhat <- predict(best_mod, test_df)
# cMatrix
cMatrix <- table(predict = yhat, truth = test_df$y)
cMatrix
# Thus, with this value of cost, 12 of the test observations are correctly classified. 

# What if we had instead used cost=0.01?
svm_fit <- svm(y ~ ., data = df, kernel = "linear", cost = .01, 
               scale = FALSE)
yhat <- predict(svm_fit, test_df)
# cMatrix
cMatrix <- table(predict = yhat, truth = test_df$y)
cMatrix
# In this case two additional observation are accurately classified.

# Now consider a situation in which the two classes are linearly separable. Then we can find 
# a separating hyperplane using the svm() function. We first further separate the two classes
# in our simulated data so that they are linearly separable:
X[y == 1] <- X[y == 1] + .5
plot(X, col = (y + 5) / 2, pch = 19)
# Now the observations are just barely linearly separable. We fit the support vector
# classifier and plot the resulting hyperplane, using a very large value of cost so that no
# observations are misclassified:
df <- data.frame(X = X, y = as.factor(y))
svm_fit <- svm(y ~ ., data = df, kernel = "linear", cost = 1e5)

summary(svm_fit)
svm_fit$index

plot(svm_fit, df)
# No training errors were made and only three support vectors were used. However, we can see 
# from the figure that the margin is very narrow (because the observations that are not 
# support vectors, indicated as circles, are very close to the decision boundary). It seems
# likely that this model will perform poorly on test data. We now try a smaller value of 
# cost:
svm_fit <- svm(y ~ ., data = df, kernel = "linear", cost = 1)
summary(svm_fit)
svm_fit$index

plot(svm_fit, df)

# Using cost = 1, we misclassify a training observation, but we also obtain a much wider 
# margin and make use of seven support vectors. It seems likely that this model will perform
# better on test data than the model with cost = 1e5.

# ==============================================================================================
# Support Vector Machine --------------------------------------------------
# ==============================================================================================
# In order to fit an SVM using a non-linear kernel, we once again use the svm() function. 
# However, now we use a different value of the parameter kernel. To fit an SVM with a 
# polynomial kernel we use kernel = "polynomial", and to fit an SVM with a radial kernel we 
# use kernel = "radial". In the former case we also use the degree argument to specify a 
# degree for the polynomial kernel, and in the latter case we use gamma to specify a value of
# γ for the radial basis kernel.

# We first generate some data with a non-linear class boundary, as follows:
set.seed(1)
X <- matrix(rnorm(200 * 2), ncol = 2)
X[1:100, ] <- X[1:100, ] + 2
X[101:150, ] <- X[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
df <- data.frame(X = X, y = as.factor(y))

# Plotting the data makes it clear that the class boundary is indeed non- linear:
plot(X, col = y)

# The data is randomly split into training and testing groups. We then fit the training data
# using the svm() function with a radial kernel and γ = 1:
train_set <- sample(200, 100)

svm_fit <- svm(y ~ ., data = df[train_set, ], kernel = "radial", gamma = 1,
                                cost = 1)
plot(svm_fit, data = df[train_set, ])
# The plot shows that the resulting SVM has a decidedly non-linear boundary. The summary() 
# function can be used to obtain some information about the SVM fit:
summary(svm_fit)
svm_fit$index

# We can see from the figure that there are a fair number of training errors in this SVM fit.
# If we increase the value of cost, we can reduce the number of training errors. However, 
# this comes at the price of a more irregular decision boundary that seems to be at risk of 
# overfitting the data:
svm_fit <- svm(y ~ ., data = df[train_set, ], kernel = "radial", gamma = 1, 
               cost = 1e5)
plot(svm_fit, data = df[train_set, ])

# We can perform cross-validation using tune() to select the best choice of γ and cost for an
# SVM with a radial kernel:
set.seed(1)
tune_out <- tune(svm, y ~ ., data = df[train_set, ], kernel = "radial", 
                 ranges = list(cost = c(.1, 1, 10, 100, 1000),
                               gamma = c(.5, 1, 2, 3, 4)))
summary(tune_out)
# Therefore, the best choice of parameters involves cost = 1 and gamma = .5. 

# We can view the test set predictions for this model by applying the predict() function to 
# the data. Notice that to do this we subset the dataframe df using -train as an index set:
cMatrix <- table(true = df[-train_set, "y"], pred = predict(tune_out$best.model, 
                                                 newdata = df[-train_set, ]))
# Accuracy:
Accuracy <- sum(diag(cMatrix)) / sum(cMatrix)
Accuracy
# 12 % of test observations are misclassified by this SVM.

# ==============================================================================================
# Reciever Operating Characteristic (ROC) curves --------------------------------
# ==============================================================================================
# The gplots package can be used to produce ROC curves. 
library(gplots)

# We first write a short function to plot an ROC curve given a vector containing a numerical 
# score for each observation, pred, and a vector containing the class label for each 
# observation, truth.
rocplot <- function(pred, truth, ...) {
  predob <-  prediction (pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}
# SVMs and support vector classifiers output class labels for each observation. However, it 
# is also possible to obtain fitted values for each observation, which are the numerical 
# scores used to obtain the class labels. For instance, in the case of a support vector 
# classifier, the fitted value for an observation X = (X1,X2,...,Xp)^T takes the form 
# β0 + β1X1 + β2X2 + ... + βpXp.

# In essence, the sign of the fitted value determines on which side of the decision boundary
# the observation lies. Therefore, the relationship between the fitted value and the class 
# prediction for a given observation is simple: if the fitted value exceeds zero then the 
# observation is assigned to one class, and if it is less than zero than it is assigned to 
# the other. In order to obtain the fitted values for a given SVM model fit, we use 
# decision.values = TRUE when fitting svm(). Then the predict() function will output the 
# fitted values:
svmfit_opt <- svm(y ~ ., data = df[train_set, ], kernel = "radial", gamma = .5,
                  cost = 1, decision.values = TRUE)
summary(svmfit_opt)
plot(svm_fit, df[train_set, ])

fitted <- attributes(predict(svmfit_opt, df[train_set, ], 
                             decision.values = TRUE))$decision.values

# Now we can produce the ROC plot:
par(mfrow = c(1, 2))
rocplot(fitted, df[train_set, "y"], main = "Training Data")

# SVM appears to be producing accurate predictions. By increasing γ we can produce a more 
# flexible fit and generate further improvements in accuracy:
svmfit_flex <- svm(y ~ ., data = df[train_set, ], kernel = "radial", gamma = 50, 
                   cost = 1, decision.values = TRUE)

fitted <- attributes(predict(svmfit_flex, df[train_set, ], 
                             decision.values = TRUE))$decision.values

rocplot(fitted, df[train_set, "y"], add = TRUE, col = "red")

# However, these ROC curves are all on the training data. We are really more interested in 
# the level of prediction accuracy on the test data. When we compute the ROC curves on the 
# test data, the model with γ = 2 appears to provide the most accurate results:
fitted <- attributes(predict(svmfit_opt, df[-train_set, ], 
                             decision.values = TRUE))$decision.values
rocplot(fitted, df[-train_set, "y"], main = "Test Data")

fitted <- attributes(predict(svmfit_flex, df[-train_set, ], 
                             decision.values = TRUE))$decision.values
rocplot(fitted, df[-train_set, "y"], add = TRUE, col = "red")

# ==============================================================================================
# SVM with Multiple Classes -----------------------------------------------
# ==============================================================================================
# If the response is a factor containing more than two levels, then the svm() function will 
# perform multi-class classification using the one-versus-one approach. We explore that 
# setting here by generating a third class of observations:
set.seed(1)
X <- rbind(X, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
X[y == 0, 2] <- X[y == 0, 2] + 2
df <- data.frame(X = X, y = as.factor(y))
par(mfrow = c(1, 1))
plot(X, col = (y + 1))

# We now fit an SVM to the data:
svm_fit <- svm(y ~ ., data = df, kernel = "radial", cost = 10, gamma = 1)
plot(svm_fit, df)

# The e1071 library can also be used to perform support vector regression, if the response 
# vector that is passed in to svm() is numerical rather than a factor.

# ==============================================================================================
# Application to Gene Expression ------------------------------------------
# ==============================================================================================
# We now examine the Khan data set, which consists of a number of tissue samples 
# corresponding to four distinct types of small round blue cell tu- mors. For each tissue 
# sample, gene expression measurements are available. The data set consists of training data,
# xtrain and ytrain, and testing data, xtest and ytest.
library(ISLR)

# We examine the dimension of the data:
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
# This data set consists of expression measurements for 2,308 genes. The training and test 
# sets consist of 63 and 20 observations respectively.
table(Khan$ytrain)
table(Khan$ytest)

# We will use a support vector approach to predict cancer subtype using gene expression
# measurements. In this data set, there are a very large number of features relative to the
# number of observations. This suggests that we should use a linear kernel, because the 
# additional flexibility that will result from using a polynomial or radial kernel is 
# unnecessary.

df <- data.frame(X = Khan$xtrain, y = as.factor(Khan$ytrain))
svm_fit <- svm(y ~ ., data = df, kernel = "linear", cost = 10)
summary(svm_fit)

# cMatrix:
cMatrix <- table(svm_fit$fitted, df$y)
cMatrix
# Accuracy:
Accuracy <- sum(diag(cMatrix)) / sum(cMatrix)
Accuracy
# We see that there are no training errors. In fact, this is not surprising, because the 
# large number of variables relative to the number of observations implies that it is easy to
# find hyperplanes that fully separate the classes. 

# However, we are most interested not in the support vector classifier’s performance on the 
# training observations, but rather its performance on the test observations:
df_test <- data.frame(X = Khan$xtest, y = as.factor(Khan$ytest))
pred_test <- predict(svm_fit, newdata = df_test)

# test_cMatrix:
cMatrix_test <- table(pred_test, df_test$y)
cMatrix_test
# test_Accuracy:
Accuracy_test <- sum(diag(cMatrix_test)) / sum(cMatrix_test)
Accuracy_test
# We see that using cost=10 yields two test set errors on this data, with a 90% accuracy rate.

# End file ----------------------------------------------------------------
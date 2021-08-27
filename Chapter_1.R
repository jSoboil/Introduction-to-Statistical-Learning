# ==============================================================================================
# Intro to R --------------------------------------------------------------
# ==============================================================================================

# Basic Commands ----------------------------------------------------------

x <- c(1, 2, 3, 4, 5)
x

y <- c(6, 7, 8, 9, 10)
y

length(x)
length(y)

# Adds the correpsonding values together; like linear matrix addition:
x+y
# Note:
z <- c(1, 2, 3)
x+z

# ls() function returns all objects within the working environment:
ls()

# It’s also possible to remove all objects at once:
rm(list = ls())

# The matrix() function can be used to create a matrix of numbers:
?matrix
# The help file reveals that the matrix() function takes a number of inputs, but for now we 
# focus on the first three: the data (the entries in the matrix), the number of rows, and the 
# number of columns:
x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
# Note that we could just as well omit typing data=, nrow=, and ncol= in the matrix() command 
# above: that is, we could just type:
x <- matrix(c(1, 2, 3, 4), 2, 2)
x
# However, better practice to explicitly label commands to make code clear and precise; avoid
# errors this way.

#  The sqrt() function returns the square root of each element of a vector or matrix:
sqrt(x)

# The rnorm() function generates a vector of random normal variables, with first argument n the
# sample size. Each time we call this function, we will get a different answer. Here we create 
# two correlated sets of numbers, x and y, 
x <- rnorm(50)
y <- rnorm(50, mean = 50, sd = .1)
x
y
# and use the cor() function to compute the correlation between them:
cor(x, y)

# By default, rnorm() creates standard normal random variables with a mean of 0 and a standard
# deviation of 1. However, the mean and standard deviation can be altered using the mean and sd
# arguments.

# Sometimes we want our code to reproduce the exact same set of random numbers; we can use the 
# set.seed() function to do this. The set.seed() function takes an (arbitrary) integer argument:
set.seed(1303)
rnorm(50)
set.seed(1303)
rnorm(50)


# The mean() and var() functions can be used to compute the mean and variance of a vector of 
# numbers. Applying sqrt() to the output of var() will give the standard deviation. Or we can 
# simply use the sd() function:
set.seed(3)
y <- rnorm(n = 100)
mean(y)
var(y)
sqrt(var(y))


# Graphics ----------------------------------------------------------------

x <- rnorm(n = 100)
y <- rnorm(n = 100)
plot(x, y, xlab = "x-axis", ylab = "y-axis", main = "Scatter of X vs Y")

# The command to save an output:
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

# The function dev.off() indicates to R that we are done creating the plot; indicating to R 
# that the previous output must be saved.

# The function seq() can be used to create a sequence of numbers. For instance, seq(a,b) makes
# a vector of integers between a and b. There are many other options: for instance, 
# seq(0,1,length=10) makes a sequence of 10 numbers that are equally spaced between 0 and 1. 
# Typing 3:11 is a shorthand for seq(3,11) for integer arguments:
x <- seq(1, 10)
x
x <- 1:10
x
x <- seq(0, 1, length.out = 10)
x
x <- seq(-pi, pi, length.out = 50)
x

# The contour() function produces a contour plot in order to represent three-dimensional data:

# It takes three arguments:
#   1. A vector of the x values (the first dimension),
#   2. A vector of the y values (the second dimension), and
#   3. A matrix whose elements correspond to the z value (the third dimen- sion) for each pair
#      of (x,y) coordinates.

?contour
y <- x
f <- outer(x, y, function(x, y)cos(y) / (1 + x ^ 2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = TRUE)

# The image() function works the same way as contour(), except that it produces a color-coded 
# plot whose colors depend on the z value.  This is often referred to as a heatmap:
fa <- (f-t(f))/2

# Alternatively,persp() can be used to produce a three-dimensional plot. The arguments theta 
# and phi control the angles at which the plot is viewed:
image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)


# Indexing Data -----------------------------------------------------------

# We often wish to examine part of a set of data. Suppose that our data is stored in the 
# matrix A, where A is a 4x4 matrix of a sequence of the numbers 1-16:
A <- matrix(data = 1:16, nrow = 4, ncol = 4)
# We can subset to get the ith value of the jth column
A[2, 3]
# will select the element corresponding to the second row and the third column.

# We can also select multiple rows and columns at a time, by providing vectors as the indices:
A[c(1, 3), c(2, 4)]
# Note remember linear rules for matrix addition/subtraction and multiplication... The 
# subset returns a new matrix with corresponding values of the ith row and jth column.
A[1:3, 2:4]

# If you just want to select specific rows & all columns:
A[1:2, ]
# If you want to select all rows but specific colums:
A[, 1:2]

# R treats a single row or column of a matrix as a vector, like usual matrix rules:
A[1, ]
# The use of a negative sign - in the index tells R to keep all rows or columns except those 
# indicated in the index.
A[-c(1, 3), ]
A[-c(1, 3), c(1, 3, 4)]

# The dim() function outputs the number of rows followed by the number of columns of a given 
# matrix.
dim(A)


# Loading Data ------------------------------------------------------------
library(ISLR)
# An illustrative example of loading a text table is shown below: 
# Auto <- read.table("Auto.data")

# Once the data has been loaded, the fix() function can be used to view it in a spreadsheet
# like window:
fix(Auto)
?fix
# I prefer View()
View(Auto)
dim(Auto)

# Additional Graphical & Numerical Summaries ------------------------------

# To refer to a variable, we must type the data set and the variable name joined with a $ 
# symbol:
plot(Auto$cylinders, Auto$mpg)
# Since there are only a small number of possible values for cylinders, one may prefer to 
# treat it as a qualitative variable. The as.factor() function converts quantitative variables 
# into qualitative variables:
library(tidyverse)
Auto <- Auto %>% 
  select(mpg:name) %>% 
  mutate(cyl.factor = as.factor(Auto$cylinders))
Auto
# If the variable plotted on the x-axis is categorial, then boxplots will automatically be
# produced by the plot() function:
plot(Auto$cyl.factor, Auto$mpg)
# As mentioned, various options are available:
plot(Auto$cyl.factor, Auto$mpg, col = "red", varwidth = TRUE)

# The hist() function can be used to plot a histogram. Note that col=2 has the same effect as 
# col="red" and breaks are a command for the number of bins
hist(Auto$mpg, col = 2, breaks = 15)

# The pairs() function creates a scatterplot matrix i.e. a scatterplot for every pair of 
# variables for any given data set. We can also produce scatterplots for just a subset of the 
# variables:
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, data = Auto)

# Summary function:
summary(Auto)
summary(Auto$mpg)














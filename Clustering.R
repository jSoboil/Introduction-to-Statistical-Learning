# ============================================================================================
# K-Means Clustering ------------------------------------------------------
# ============================================================================================
# The function kmeans() performs K-means clustering in R. We begin with a simple simulated 
# example in which there truly are two clusters in the data: the first 25 observations have a 
# mean shift relative to the next 25 observations.
set.seed(2)
X <- matrix(rnorm(50 * 2), ncol = 2)
X[1:25, 1] <- X[1:25, 1] + 3
X[1:25, 2] <- X[1:25, 2] - 4
X

# We now perform K-means clustering with K = 2:
knn <- kmeans(X, 2, nstart = 20)

# The cluster assignments of the 50 observations are contained in km.out$cluster:
knn$cluster
# The K-means clustering perfectly separated the observations into two clusters even though 
# we did not supply any group information to kmeans(). 

# We can plot the data, with each observation colored according to its cluster assignment:
plot(X, col = (knn$cluster + 1), main = "K-Mean CLustering for K = 2", 
     xlab = "", ylab = "", pch = 20, cex = 2)
# Here the observations can be easily plotted because they are two-dimensional. If there were 
# more than two variables then we could instead perform PCA and plot the first two principal
# components score vectors.

# In this example, we knew that there really were two clusters because we generated the data. 
# However, for real data, in general we do not know the true number of clusters. We could 
# instead have performed K-means clustering on this example with K = 3.
set.seed(4)
knn_2 <- kmeans(X, 3, nstart = 20)
knn_2
# When K = 3, K-means clustering splits up the two clusters.

# To run the kmeans() function in R with multiple initial cluster assign- ments, we use the 
# nstart argument. If a value of nstart greater than one is used, then K-means clustering 
# will be performed using multiple random assignments. Here we compare using nstart = 1 to 
# nstart = 20:
set.seed(3)
knn_3 <- kmeans(X , 3, nstart = 1)
knn_3$tot.withinss

knn_4 <- kmeans(X, 3, nstart = 20)
knn_4$tot.withinss
# The individual within-cluster sum-of-squares are contained in the vector km.out$withinss.

# When performing K-means clustering, in addition to using multiple initial cluster 
# assignments, it is also important to set a random seed using the set.seed() function. This 
# way, the initial cluster assignments in Step 1 can be replicated, and the K-means output 
# will be fully reproducible.

# ============================================================================================
# Hierarchical Clustering -------------------------------------------------
# ============================================================================================
# The hclust() function implements hierarchical clustering in R. We begin by clustering 
# observations using complete linkage. The dist() function is used to compute the 50 × 50 
# inter-observation Euclidean distance matrix.

hc_complete <- hclust(dist(X), method = "complete")
# We could just as easily perform hierarchical clustering with average or single linkage 
# instead:
hc_ave <- hclust(dist(X), method = "average")
hc_single <- hclust(dist(X), method = "single")

# We can now plot the dendrograms obtained using the usual plot() function. The numbers at 
# the bottom of the plot identify each observation:
par(mfrow = c(1, 3))

plot(hc_complete, main = "Complete Linkeage", xlab = "", sub = "", cex = .9)
plot(hc_ave, main = "Average Linkeage", xlab = "", sub = "", cex = .9)
plot(hc_single, main = "Single Linkeage", xlab = "", sub = "", cex = .9)

# To determine the cluster labels for each observation associated with a given cut of the 
# dendrogram, we can use the cutree() function:
cutree(hc_complete, 2)
cutree(hc_ave, 2)
cutree(hc_single, 2)
# For this data, complete and average linkage generally separate the observations into their
# correct groups. However, single linkage identifies one point as belonging to its own 
# cluster. A more sensible answer is obtained when four clusters are selected, although there
# are still two singletons:
cutree(hc_single, 4)

# To scale the variables before performing hierarchical clustering of the observations, we 
# use the scale() function:
X_scaled <- scale(X)
plot(hclust(dist(X_scaled), method = "complete"), 
     main = "Hierarchical Clustering with Scaled Features")
# Correlation-based distance can be computed using the as.dist() function, which converts an
# arbitrary square symmetric matrix into a form that the hclust() function recognizes as a 
# distance matrix. 

# However, this only makes sense for data with at least three features since the absolute 
# correlation between any two observations with measurements on two features is always 1. 
# Hence, we will cluster a three-dimensional data set:
X <- matrix(rnorm(30 * 3), ncol = 3)
cordist <- as.dist(1 - cor(t(X)))
plot(hclust(cordist, method = "complete"), 
     main = "Complete Linkeage with Correlation-Based Distance", xlab = "", sub = "")

# ============================================================================================
# NCI60 Data Example ------------------------------------------------------
# ============================================================================================
# Unsupervised techniques are often used in the analysis of genomic data. In particular, 
# PCA and hierarchical clustering are popular tools. We illustrate these techniques on the 
# NCI60 cancer cell line microarray data, which consists of 6,830 gene expression 
# measurements on 64 cancer cell lines.
library(ISLR)

nci_labs <- NCI60$labs
nci_data <- NCI60$data

# Each cell line is labeled with a cancer type. We do not make use of the cancer types in 
# performing PCA and clustering, as these are unsupervised techniques. But after performing
# PCA and clustering, we will check to see the extent to which these cancer types agree with
# the results of these unsupervised techniques.

# The data has 64 rows and 6,830 columns:
dim(nci_data)

# We begin by examining the cancer types for the cell lines:
nci_labs[1:4]
table(nci_labs)

# Performing PCA on the NCI60 Data ---------------------------------------------------
# We first perform PCA on the data after scaling the variables (genes) to have standard 
# deviation one, although one could reasonably argue that it is better not to scale the 
# genes.
pca_out <- prcomp(nci_data, scale. = TRUE)
# We now plot the first few principal component score vectors, in order to visualize the 
# data. The observations (cell lines) corresponding to a given cancer type will be plotted 
# in the same color, so that we can see to what extent the observations within a cancer type
# are similar to each other. 

# We first create a simple function that assigns a distinct color to each element of a 
# numeric vector. The function will be used to assign a color to each of the 64 cell lines, 
# based on the cancer type to which it corresponds:
Cols <- function(vec) {
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
# Note that the rainbow() function takes as its argument a positive integer, and returns a 
# vector containing that number of distinct colors. 

# We now can plot the principal component score vectors:
par(mfrow = c(1, 2))

plot(pca_out$x[, 1:2], col = Cols(nci_labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pca_out$x[, c(1, 3)], col = Cols(nci_labs), pch = 19, xlab = "Z1", ylab = "Z3")

# On the whole, cell lines corresponding to a single cancer type do tend to have similar 
# values on the first few principal component score vectors. This indicates that cell lines 
# from the same cancer type tend to have pretty similar gene expression levels.

# We can obtain a summary of the proportion of variance explained (PVE) of the first few 
# principal components using the summary() method for a prcomp object:
summary(pca_out)

# Using the plot() function, we can also plot the variance explained by the first few 
# principal components:
plot(pca_out)
# Note that the height of each bar in the bar plot is given by squaring the corresponding 
# element of pr.out$sdev. However, it is more informative to plot the PVE of each principal 
# component (i.e. a scree plot) and the cumulative PVE of each principal component. This can
# be done with just a little work:
pve <- 100 * pca_out$sdev ^ 2 / sum(pca_out$sdev ^ 2)

par(mfrow = c(1, 2))

plot(pve, type = "o", ylab = "Proportion of Variance Explained", 
     xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative Proportion of Variance Explained",
     xlab = "Principal Component", col = "brown3")
# We see that together, the first seven principal components explain around 40 % of the 
# variance in the data. This is not a huge amount of the variance. However, looking at the 
# scree plot, we see that while each of the first seven principal components explain a 
# substantial amount of variance, there is a marked decrease in the variance explained by 
# further principal components. That is, there is an elbow in the plot after approximately 
# the seventh principal component. This suggests that there may be little benefit to 
# examining more than seven or so principal components (though even examining seven 
# principal components may be difficult).

# Clustering the Observations of the NCI60 Data ---------------------------

# We now proceed to hierarchically cluster the cell lines in the NCI60 data, with the goal
# of finding out whether or not the observations cluster into distinct types of cancer. To 
# begin, we standardize the variables to have mean zero and standard deviation one. As 
# mentioned earlier, this step is optional and should be performed only if we want each 
# gene to be on the same scale.
sd_data <- scale(nci_data)

# We now perform hierarchical clustering of the observations using complete, single, and 
# average linkage. Euclidean distance is used as the dissimilarity measure:
par(mfrow = c(1, 3))

data_dist <- dist(sd_data)

plot(hclust(data_dist, method = "complete"), labels = nci_labs, 
     main = "Complete Linkeage", xlab = "", sub = "", ylab = "")

plot(hclust(data_dist, method = "average"), labels = nci_labs, 
     main = "Average Linkeage", xlab = "", sub = "", ylab = "")

plot(hclust(data_dist, method = "single"), 
     main = "Single Linkeage", xlab = "", sub = "", ylab = "")
# We see that the choice of linkage certainly does affect the results obtained. Typically, 
# single linkage will tend to yield trailing clusters: very large clusters onto which 
# individual observations attach one-by-one. On the other hand, complete and average linkage
# tend to yield more balanced, attractive clusters. For this reason, complete and average 
# linkage are generally preferred to single linkage. Clearly cell lines within a single 
# cancer type do tend to cluster together, although the clustering is not perfect. We will 
# use complete linkage hierarchical clustering for the analysis that follows.

# We can cut the dendrogram at the height that will yield a particular number of clusters, 
# say four:
hc_out <- hclust(dist(sd_data))
hc_clusters <- cutree(hc_out, 4)
table(hc_clusters, nci_labs)
# There are some clear patterns. All the leukemia cell lines fall in cluster 3, while the 
# breast cancer cell lines are spread out over three different clusters. We can plot the cut
# on the dendrogram that produces these four clusters:
par(mfrow = c(1, 1))

plot(hc_out, labels = nci_labs)
abline(h = 139, col = "red")
# The abline() function draws a straight line on top of any existing plot in R. The argument
# h=139 plots a horizontal line at height 139 on the den- drogram; this is the height that
# results in four distinct clusters. It is easy to verify that the resulting clusters are 
# the same as the ones we obtained using cutree(hc_out, 4).

# Printing the output of hclust gives a useful brief summary of the object:
hc_out

# K-means clustering and hierarchical clustering with the dendrogram cut to obtain the same 
# number of clusters can yield very different results. How do these NCI60 hierarchical 
# clustering results compare to what we get if we perform K-means clustering with K = 4?
set.seed(2)

kmeans_out <- kmeans(sd_data, 4, nstart = 20)
kmeans_clusters <- kmeans_out$cluster
table(kmeans_clusters, hc_clusters)
# We see that the four clusters obtained using hierarchical clustering and K- means 
# clustering are somewhat different. Cluster 2 in K-means clustering is identical to 
# cluster 3 in hierarchical clustering. However, the other clusters differ: for instance, 
# cluster 4 in K-means clustering contains a portion of the observations assigned to cluster
# 1 by hierarchical clustering, as well as all of the observations assigned to cluster 2 by 
# hierarchical clustering.

# Rather than performing hierarchical clustering on the entire data matrix, we can simply 
# perform hierarchical clustering on the first few principal component score vectors, as 
# follows:
hc_out <- hclust(dist(pca_out$x[, 1:5]))

plot(hc_out, labels = nci_labs, 
     main = "Hierarchical Clusterin on First Five Score Vectors")

table(cutree(hc_out, 4), nci_labs)
# Not surprisingly, these results are different from the ones that we obtained when we 
# performed hierarchical clustering on the full data set. Sometimes performing clustering on
# the first few principal component score vectors can give better results than performing 
# clustering on the full data. In this situation, we might view the principal component step
# as one of denoising the data. We could also perform K-means clustering on the first few 
# principal component score vectors rather than the full data set.

# End file ----------------------------------------------------------------
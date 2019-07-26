#clustering

#clear lists
rm(list=ls())

# Install factoextra
install.packages("factoextra")
install.packages("NbClust")

library(factoextra) #used to visualize cluster analysis
library(cluster) #cluster algorithms included: Gaussian mixture models, k-means, mini-batch-kmeans, k-medoids and affinity propagation
library(NbClust) #this provides 30 indexes for determining the optimal number of clusters in a dataset

# Load data
data("USArrests")
my_data <- USArrests

# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)

# Scale variables
my_data <- scale(my_data)

# View the firt 3 rows
head(my_data, n = 3)


#get_dist(): for computing a distance matrix between the rows of a data matrix. Compared to the standard dist() function, 
# it supports correlation-based distance measures including "pearson", "kendall" and "spearman" methods.

#fviz_dist(): for visualizing a distance matrix with a heat map graph
res_dist <- get_dist(USArrests, stand = TRUE, method = "pearson")
fviz_dist(res_dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

###################### How to determine the amount of clusters needed for analysis #######################################
df <- scale(USArrests)

#NbClust() this will run through 30 different indicies to determine the amount of clusters the user should use 
#a full list of index types, proposed clusters, critical values, type of clustering with a number of clusters and value index,
# and a partition breakdown of which state goes into which cluster based on the conclusion for best number of clusters
NbClust(data = df, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")

#conclusion of the best number of clusters
set.seed(123)
res.nbclust <- NbClust(data = df, distance = "euclidean",
                       min.nc = 2, max.nc = 10, 
                       method = "complete", index ="all")

#the next methods will show the amount of clusters visually
#elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 will keep compute time to a minimum
# nboot= 500 should be more accurate 
# Use verbose = FALSE to hide computing progression
set.seed(123) #set seed to duplicate results
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")



#K-means clustering the number that follows the data "df" is the amount of clusters
km_clust <- kmeans(df, 4, nstart = 25)
km_clust

#visualize using the fviz_cluster function
fviz_cluster(km_clust, data = my_data, ellipse.type = "convex")+
  theme_minimal()

#PAM algorithm is an alternative to k-means but this algorithm may be slow and require a lot of memory to process
pam_clust <- pam(my_data, 2) #2 clusters
pam_clust

pam_clust1 <- pam(my_data, 4) #4 clusters
pam_clust1

#visualize using the fviz_cluster function
fviz_cluster(pam_clust) #graph with 2 clusters

fviz_cluster(pam_clust1) #graph with 4 clusters






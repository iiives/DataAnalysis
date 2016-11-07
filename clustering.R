# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)
str(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

df <- scale(wine[-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest? 
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

# Ans
# -- 3, has the biggest bend
#
# -- The wssplot is setup with nc = max number of clusters to consider and 
# -- a fixed seed number such that all iterations will give the same random output.
#

# -- The premise of the curve is to compare the total intra-error distances  
# -- for the particular cluster number arrangement. Then joining these 
# -- will show at which place the arrangement is met by a big variation in distance 
# -- and be the recommended cluster number and appropriate level of cluster to 
# -- belance cluster formations that are too sparse or too narrow.
#


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

# Ans
# -- 3, has the highest count with 26 criterias

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, 3, nstart = 5) 
# nstart adjusted to 5 or 25 make values have less errors
#str(fit.km)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(wine$Type, fit.km$cluster)

#Confusion Matrix
#   1  2  3
#1 59  0  0
#2  3 65  3
#3  0  0 48

# 6 errors
Overall_accuracy = 59 + 65 +48 / (59 + 65 +48 + 3 +3)
#                 = 172 / 178
Overall_accuracy# = 96.63%
# Has a high-overall accuracy 96.63% hence it is a good clustering

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

#http://www.statmethods.net/advstats/cluster.html
#install.packages("cluster")
library(cluster)
clusplot( df, fit.km$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0 )
#There is some overlap thdoes not make it ideal, as then the datapoint will be ambigious

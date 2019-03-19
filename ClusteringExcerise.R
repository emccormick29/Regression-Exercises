# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df = wine[-1]
df = scale(data)

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

##This method suggests 3 clusters as there is a distinct drop when moving from 1 to 3, but
##not as significant

##First, this method sets a number of clusters, in this case, 15. Then, for each number
##of clusters from 2..15, we create a sum of squares for each cluster. For example
## in the graph we get a sum of squares value for two clusters, a sum of squares 
##for 3 clusters, etc. We then plot these sum of square values against the number
##of clusters the values are associated with so that we can analyze what number
## of clusters is associated with the biggest drops in the sum of squares.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

##This method also suggets 3 clusters

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, centers = 3, iter.max = 1000)
str(fit.km)
# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
healthyclusters = fit.km$cluster
winetable = table(wine$Type, fit.km$cluster)
winetable

##I would consider this a good clustering because each of the clusters almost perfectly groups into 
##a different type of wine.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
clusplot(fit.km)
clusplot(pam(df,3))

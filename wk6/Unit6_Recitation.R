# Unit 6 - Recitation

# Video 2

flower = read.csv("flower.csv", header=FALSE)
str(flower)

# Change the data type to matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

# Turn matrix into a vector
flowerVector = as.vector(flowerMatrix)
str(flowerVector)

flowerVector2 = as.vector(flower) #gives 50 vars
str(flowerVector2)

#after csv read into df, need as.vector(as.matrix(df))
#if values in df are values at posns on grid

# Compute distances
distance = dist(flowerVector, method = "euclidean")



# Video 3

# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward.D")

# Plot the dendrogram
plot(clusterIntensity)

# Select 3 clusters
rect.hclust(clusterIntensity, k = 3, border = "red")
flowerClusters = cutree(clusterIntensity, k = 3)
flowerClusters #flowerClusters is vector of values
#1,2, or 3 (depending on cluster that observation is
#assigned to)

# Find mean intensity values
tapply(flowerVector, flowerClusters, mean)

# Plot the image and the clusters
dim(flowerClusters) = c(50,50) #forces flowerClusters
#into a 2D 50x50 vector
image(flowerClusters, axes = FALSE)

# Original image
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256))) #pic
#of original data (without clustering)

#reminder: image resolution is measured in ppi

# Video 4

#QUESTION: WHAT IS A T2-WEIGHTED MRI?
#ANSWER: T1-weighted MRI makes structures with
#lots of fat appear bright, while T2-weighted MRI
#makes structures with lots of water appear bright.
#T1 vs T2 weighting as something to do with 
#'relaxation times' and 'excitation times', both of
#which are short in T1 and long in T2.
#Inflammation is dark on T1. Water is dark on T1.
#Flowing fluid is dark on both T1 and T2. Blood
#is bright on T1. Fat is bright on both T1 and T2.
#Water is bright on T2. Inflammation is bright on T2.
#

# Let's try this with an MRI image of the brain

healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# Hierarchial clustering
healthyVector = as.vector(healthyMatrix)
#DO NOT run below commented out dist() command;
#R will run out of memory as (365636*365635)/2 values
#must be computed; this is order 100 billion
# distance = dist(healthyVector, method = "euclidean")

# We have an error - why?
str(healthyVector)

#Note: K-Means clustering much more judicious
#when clustering a large number of datapoints

# Video 5

# Specify number of clusters
k = 5

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
#Note: KMC is a 'kmeans' object
str(KMC)

# Extract clusters
healthyClusters = KMC$cluster
KMC$centers[2]

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyClusters, axes = FALSE, col=rainbow(k))



# Video 6

# Apply to a test image
 
tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Apply clusters from before to new image, using the flexclust package
install.packages("flexclust") #flexclust package
#contains as.kcca function, which takes as argument
#a kmeans object and a training dataset, and then effectively
#builds a kmeans clustering model that can be used 
#to predict the cluster to which each observation
#in a test dataset belongs. 
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector) #the KMC.kcca
#object is of type "kcca"; it's effectively a clustering model
#that can now be used to assign observations of a test dataset
#each to a cluster

KMC.kcca

tumorClusters = predict(KMC.kcca, newdata = tumorVector)

# Visualize the clusters
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col=rainbow(k))


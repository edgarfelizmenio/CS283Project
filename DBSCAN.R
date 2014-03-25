library("fpc")
library("data.table")
library("plyr")

source("modified_dbscan.R")
#Edit 
#setwd("./RandomPieces_10000")


# Load training data and test data
kddtrain <- read.table("./RandomPieces_10000/ids10000_0.data",header=FALSE,sep=",")
kddtest <- read.table("./RandomPieces_10000/ids10000_1.data",header=FALSE,sep=",")

#Remove the labels
traindata <- kddtrain[, -42]
testdata <- kddtest[, -42]

classes <- kddtrain[42]
names(classes) <- NULL

#Normalization Code
minmaxnorm <- function(x) {
  maximum <- max(x, na.rm=TRUE)
  minimum <- min(x, na.rm=TRUE)
  if (maximum == minimum) {
    return(rep(0, length(x)))
  } else {
    return(((x - minimum)/(maximum - minimum)))
  }
}


normtraindata <- apply(traindata,2,minmaxnorm)
normtraindata <- as.data.frame(normtraindata)

#GENERATE MODEL
numclust <- matrix(nrow=11, ncol=9)
rownames(numclust) <- seq(0,1,by=0.1)
colnames(numclust) <- seq(2,10)
i <- 1
for (eps in seq(0,1, by=0.1)) {
  j <- 1
  for(minpts in 2:10) {
    model <- mydbscan(normtraindata, 9, eps=eps, MinPts=minpts, method = "raw")
    clusters <- predict(model, normtraindata)
    
    #print(model)
    
    #cat("eps: ")
    #cat(model$eps)
    #cat("\nMinPts: ")
    #cat(model$MinPts)
    #cat("\nnumber of clusters: ")
    #cat(length(unique(model$cluster)))
    #cat("\n")
    numclust[i,j] <- length(unique(model$cluster))
    j <- j + 1
    
    clusterAssignments <- cbind(clusters, classes)
    clusterAssignments <- as.data.frame(clusterAssignments, col.names=c("clusters", "classes"))
    clusterAssignments <- clusterAssignments[with(clusterAssignments, order(clusters,classes)), ]
    
    cat("\n=================================================\n")
    cat("eps: ")
    cat(model$eps)
    cat("\tMinPts: ")
    cat(model$MinPts)
    cat("\n=================================================\n")
    cat('\nCluster summary:\n')
    clustsum <- count(clusterAssignments, c("clusters","classes"))
    print(clustsum)


    cat('\n\nPredicted Classes:\n')
    trainingclusters <- aggregate(freq ~ clusters, clustsum,max)
    oldnames <- colnames(trainingclusters)
    trainingclusters <- merge(trainingclusters, clustsum)
    colnames(trainingclusters) <- c(oldnames, "class")
    trainingclusters <- trainingclusters[,c("clusters", "class", "freq")]

    print(trainingclusters)
  }
  i <- i + 1
  
}
print(numclust)

#Identify cluster assignments based on model

#Define Cluster Labels
#--to implement
#--compare clusterAssignments with traindata[,42]
#--

#Predict assignments of testdata
#clusterAssignmentsTest <- predict(model, traindata, testdata)

#Evaluate Accuracy/Precision/Recall
#--compare clusterAssignmentsTest to cluster labels.

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
eps <- 0.1
for(minpts in 2:100) {
  model <- mydbscan(normtraindata, 9, eps=eps, MinPts=minpts, method = "raw", showplot=1)
  clusters <- predict(model, normtraindata)
  
  print(model)

  clusterAssignments <- cbind(clusters, classes)
  cat(dim(clusterAssignments))
  clusterAssignments <- as.data.frame(clusterAssignments, col.names=c("clusters", "classes"))
  clusterAssignments <- clusterAssignments[with(clusterAssignments, order(clusters,classes)), ]
  clusterAssignments <- count(clusterAssignments, c("clusters","classes"))
  #print(clusterAssignments)
  trainingclusters <- aggregate(freq ~ clusters, clustsum,max)

  #cat('\n\nPredicted Classes:\n')
  #print(trainingclusters)
  oldnames <- colnames(trainingclusters)
  trainingclusters <- merge(trainingclusters, clustsum)
  colnames(trainingclusters) <- c(oldnames, "class")
  trainingclusters <- trainingclusters[,c("clusters", "class", "freq")]
  cat('\n\nPredicted Classes:\n')
  print(trainingclusters)
}

#Identify cluster assignments based on model

#Define Cluster Labels
#--to implement
#--compare clusterAssignments with traindata[,42]
#--

#Predict assignments of testdata
#clusterAssignmentsTest <- predict(model, traindata, testdata)

#Evaluate Accuracy/Precision/Recall
#--compare clusterAssignmentsTest to cluster labels.

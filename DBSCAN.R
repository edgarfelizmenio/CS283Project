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

normtestdata <- apply(testdata, 2, minmaxnorm)
normtestdata <- as.data.frame(normtestdata)

#GENERATE MODEL
numclust <- matrix(nrow=11, ncol=9)
rownames(numclust) <- seq(0,1,by=0.1)
colnames(numclust) <- seq(2,10)
i <- 1
for (eps in seq(1,1, by=0.1)) {
  j <- 1
  for(minpts in 10:10) {
    model <- mydbscan(normtraindata, 9, eps=eps, MinPts=minpts, method = "raw")
    clusters <- predict(model, normtraindata)
    
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
    cat("\tnumber of clusters: ")
    cat(length(unique(model$cluster)))
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
cat('\n\nNumber of clusters based on epsilon and MinPts:\n')
print(numclust)
write.table(numclust, "number of clusters.csv", row.names=TRUE, col.names=TRUE)

# predict classes of test data using model with eps=1 and MinPts=2

model <- mydbscan(normtraindata, 9, eps=1, MinPts=2, method = "raw")

#predict trainingdata
clusters <- predict(model, normtraindata)

clusterAssignments <- cbind(clusters, classes)
clusterAssignments <- as.data.frame(clusterAssignments, col.names=c("clusters", "classes"))
clusterAssignments <- clusterAssignments[with(clusterAssignments, order(clusters,classes)), ]
   
cat("\n=================================================\n")
cat("eps: ")
cat(model$eps)
cat("\tMinPts: ")
cat(model$MinPts)
cat("\tnumber of clusters: ")
cat(length(unique(model$cluster)))
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

cat('train clusters; done')

# predict test data
testclasses <- kddtest[42]
clusters <- predict(model, normtraindata, normtestdata)

clusterAssignments <- cbind(clusters, testclasses)
clusterAssignments <- as.data.frame(clusterAssignments, col.names=c("clusters", "classes"))
clusterAssignments <- clusterAssignments[with(clusterAssignments, order(clusters,testclasses)), ]
   
cat("\n=================================================\n")
cat("eps: ")
cat(model$eps)
cat("\tMinPts: ")
cat(model$MinPts)
cat("\tnumber of clusters: ")
cat(length(unique(model$cluster)))
cat("\n=================================================\n")
cat('\nCluster summary:\n')

clustsum <- count(clusterAssignments, c("clusters","V42"))
colnames(clustsum) <- c("clusters", "class", "freq")
print(clustsum)

traintestsum <- merge(clustsum, trainingclusters, by=c("clusters","class"))

names(traintestsum) <- c("clusters", "class", "test_freq", "max_train_freq")
cat("\nSUMMARY:\n")
print(traintestsum)

write.table(traintestsum, "traintestsum.csv", row.names=TRUE, col.names=TRUE)


#Identify cluster assignments based on model

#Define Cluster Labels
#--to implement
#--compare clusterAssignments with traindata[,42]
#--

#Predict assignments of testdata
#clusterAssignmentsTest <- predict(model, traindata, testdata)

#Evaluate Accuracy/Precision/Recall
#--compare clusterAssignmentsTest to cluster labels.

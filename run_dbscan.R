

source("modified_dbscan.R")
#Edit 
#setwd("./RandomPieces_10000")


# Load training data and test data
traindata <- read.table("./RandomPieces_1000/ids1000_322.data",header=FALSE,sep=",")
testdata <- read.table("./RandomPieces_1000/ids1000_643.data",header=FALSE,sep=",")



classes <- traindata[42]
testclasses <- testdata[42]
names(classes) <- NULL

#Remove the labels
traindata <- traindata[, -42]
testdata <- testdata[, -42]

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

#NormalizeData
normtraindata <- apply(traindata,2,minmaxnorm)
normtraindata <- as.data.frame(normtraindata)

normtestdata <- apply(testdata, 2, minmaxnorm)
normtestdata <- as.data.frame(normtestdata)


#source("findOptimalParameters.R")

#GENERATE MODEL
# predict classes of test data using model with eps=1 and MinPts=2

model <- mydbscan(normtraindata, 9, eps=1, MinPts=20, method = "hybrid")

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
cat('\nCTRAIN CLUSTER SUMMARY:\n')

clustsum <- count(clusterAssignments, c("clusters","classes"))
print(clustsum)

write.table(clustsum, "Training_Cluster_Summary.csv", row.names=TRUE, col.names=TRUE)

######################################
#    INSERT CODE FOR PRECISION AND RECALL FOR clustsum
######################################

cat('\n\nPredicted Classes:\n')
trainingclusters <- aggregate(freq ~ clusters, clustsum,max)
oldnames <- colnames(trainingclusters)
trainingclusters <- merge(trainingclusters, clustsum)
colnames(trainingclusters) <- c(oldnames, "class")
trainingclusters <- trainingclusters[,c("clusters", "class", "freq")]
#print(trainingclusters)


write.table(trainingclusters, "Training_Cluster_Assignment.csv", row.names=TRUE, col.names=TRUE)

#cat('train clusters; done')
#trainingclusterSize <- aggregate(freq ~ clusters, clustsum,sum)
#trainingclusterPurity <- merge( trainingclusters,trainingclusterSize, by=c("clusters"))
#trainingclusterPurity <-cbind(trainingclusterPurity,trainingclusterPurity[,3]/trainingclusterPurity[,4])
#names(trainingclusterPurity) <- c("Cluster", "Class", "Max", "Size","Purity")
#averagePurity <- mean(trainingclusterPurity$Purity)
#print(trainingclusterPurity)
#write.table(trainingclusterPurity, "Training_Cluster_Purity.csv", row.names=TRUE, col.names=TRUE)


# predict test data

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
cat('\nTEST CLUSTER SUMMARY:\n')

clustsum <- count(clusterAssignments, c("clusters","V42"))
colnames(clustsum) <- c("clusters", "class", "freq")
print(clustsum)

######################################
#    INSERT CODE FOR PRECISION AND RECALL FOR clustsum
######################################



traintestsum <- merge( trainingclusters,clustsum, by=c("clusters"))
names(traintestsum) <- c("Cluster" ,"ClusterClass","TrainFreq", "ActualClass","Test_Freq")
traintestsum <- traintestsum[c("Cluster" ,"ClusterClass", "ActualClass","Test_Freq")]
cat("\nTEST DATA ASSIGNMENTS:\n")
print(traintestsum)

write.table(traintestsum, "Test_Cluster_Assginment.csv", row.names=TRUE, col.names=TRUE)


######################################
#    INSERT CODE FOR CLASSIFICATION (true postitives, true negatives)
######################################

traintestAccuracy <- merge( trainingclusters,clustsum, by=c("clusters","class"))
names(traintestAccuracy) <- c("Cluster", "ClusterClass", "Train_freq", "CorrectAssginments")
cat("\nTEST DATA CORRECT:\n")
print(traintestAccuracy)

write.table(traintestsum, "Test_Cluster_Assginment_CorrectOnly.csv", row.names=TRUE, col.names=TRUE)

testAccuracy <- sum(traintestsum$CorrectAssginments) / nrow(classes)
cat("Accuracy: ")
print(testAccuracy)
nrow(classes)

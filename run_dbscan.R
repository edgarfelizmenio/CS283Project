

source("modified_dbscan.R")
#Edit 
#setwd("./RandomPieces_10000")


# Load training data and test data
#Unit test Variables
unitTest <- FALSE
if(unitTest == TRUE){
  traindata <- read.table("./RandomPieces_1000/ids1000_322.data",header=FALSE,sep=",")
  testdata <- read.table("./RandomPieces_1000/ids1000_643.data",header=FALSE,sep=",")
  totalPrecisionTrain <-0
  totalRecallTrain <-0
  totalPrecisionTest <-0
  totalRecallTest <-0
  totalAccuracy <-0
  datapercent <- 1
  runNumber <-0
  
  rundirectory <- paste("datasize_",datapercent,"xval_",runNumber,sep="")
  dir.create(rundirectory)
}

#MINPOINTS <- 20
#EPSILON <- 1

## End of Unit test variables

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

model <- mydbscan(normtraindata, 9, eps=EPSILON, MinPts=MINPOINTS, method = "hybrid")

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

write.table(clustsum, paste(rundirectory, "/Training_Cluster_Summary_",runNumber,".csv",sep=""), row.names=TRUE, col.names=TRUE)

######################################
#    INSERT CODE FOR PRECISION AND RECALL FOR clustsum
######################################
set_default <-function(val) {
  if (is.nan(val)) {
    val <- 0
  }
  return(val)
}

compute_sci <-function(data) {
  sci <- (data$freq-1)/(data$size-1)*(data$freq)
  sci <- sapply(sci, set_default)
  return(sci)
}

#PRECISION CODE
clustsize <- aggregate(freq ~ clusters, clustsum,sum)
precision <- merge( clustsum,clustsize, by=c("clusters"))
names(precision) <- c("clusters","classes","freq","size")
precision <- cbind(precision,compute_sci(precision) )
names(precision) <- c("clusters","classes","freq","Clustersize","Sci")
numericPrecision <- sum(precision$Sci) / nrow(classes)
totalPrecisionTrain <- totalPrecisionTrain + numericPrecision

#RECALL CODE
classsize <- aggregate(freq ~ classes, clustsum,sum)
recall <- merge( clustsum,classsize, by=c("classes"))
names(recall) <- c("classes","clusters","freq","size")
recall <- cbind(recall, compute_sci(recall) )
names(recall) <- c("classes","clusters","freq","classsize","Sci")
numericRecall <- sum(recall$Sci) / nrow(classes)
totalRecallTrain  <- totalRecallTrain +numericRecall



cat('\n\nPredicted Classes:\n')
trainingclusters <- aggregate(freq ~ clusters, clustsum,max)
oldnames <- colnames(trainingclusters)
trainingclusters <- merge(trainingclusters, clustsum)
colnames(trainingclusters) <- c(oldnames, "class")
trainingclusters <- trainingclusters[,c("clusters", "class", "freq")]
#print(trainingclusters)


write.table(trainingclusters, paste(rundirectory, "/Training_Cluster_Assignment_",runNumber,".csv",sep=""), row.names=TRUE, col.names=TRUE)

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

#PRECISION CODE
clustsize <- aggregate(freq ~ clusters, clustsum,sum)
precision <- merge( clustsum,clustsize, by=c("clusters"))
names(precision) <- c("clusters","classes","freq","size")
precision <- cbind(precision,compute_sci(precision) )
names(precision) <- c("clusters","classes","freq","Clustersize","Sci")
numericPrecisionTest <- sum(precision$Sci) / nrow(classes)
totalPrecisionTest <- totalPrecisionTest + numericPrecisionTest

#RECALL CODE
classsize <- aggregate(freq ~ class, clustsum,sum)
recall <- merge( clustsum,classsize, by=c("class"))
names(recall) <- c("classes","clusters","freq","size")
recall <- cbind(recall, compute_sci(recall) )
names(recall) <- c("classes","clusters","freq","classsize","Sci")
numericRecallTest <- sum(recall$Sci) / nrow(classes)
totalRecallTest  <- totalRecallTest +numericRecallTest


traintestsum <- merge( trainingclusters,clustsum, by=c("clusters"))
names(traintestsum) <- c("Cluster" ,"ClusterClass","TrainFreq", "ActualClass","Test_Freq")
traintestsum <- traintestsum[c("Cluster" ,"ClusterClass", "ActualClass","Test_Freq")]
cat("\nTEST DATA ASSIGNMENTS:\n")
print(traintestsum)

write.table(traintestsum, paste(rundirectory, "/Test_Cluster_Assignment_",runNumber,".csv",sep=""), row.names=TRUE, col.names=TRUE)


######################################
#    INSERT CODE FOR CLASSIFICATION (true postitives, true negatives) use on testtrainsum
######################################

traintestAccuracy <- merge( trainingclusters,clustsum, by=c("clusters","class"))
names(traintestAccuracy) <- c("Cluster", "ClusterClass", "Train_freq", "CorrectAssginments")
cat("\nTEST DATA CORRECT:\n")
print(traintestAccuracy)

write.table(traintestsum,  paste(rundirectory, "/Test_Cluster_Assignment_Correct_",runNumber,".csv",sep=""), row.names=TRUE, col.names=TRUE)

testAccuracy <- sum(traintestAccuracy$CorrectAssginments) / nrow(classes)
totalAccuracy <- testAccuracy + totalAccuracy
cat("Accuracy: ")
print(testAccuracy)
nrow(classes)

runstats <-cbind(numericPrecision,numericRecall,numericPrecisionTest,numericRecallTest,testAccuracy)
runstats

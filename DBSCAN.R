library("fpc")

#Edit 
setwd("CS283_MiniProj/RandomPieces_10000")


# Load training data and test data
kddtrain <- read.table("ids10000_0.data",header=FALSE,sep=",")
kddtest <- read.table("ids10000_1.data",header=FALSE,sep=",")

#Remove the labels
traindata <- kddtrain[, -42]
testdata <- kddtest[, -42]

#Normalization Code
minmaxnorm <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))}
# use lapply to apply minmaxnorm() to every column in a data frame



#GENERATE MODEL
model <- dbscan(traindata, 100, method = "raw")

#Identify cluster assignments based on model
clusterAssignments <- predict(model, traindata)

#Define Cluster Labels
#--to implement
#--compare clusterAssignments with traindata[,42]
#--

#Predict assignments of testdata
clusterAssignmentsTest <- predict(model, traindata, testdata)

#Evaluate Accuracy/Precision/Recall
#--compare clusterAssignmentsTest to cluster labels.

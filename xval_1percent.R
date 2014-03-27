#load libraries

library("fpc")
library("data.table")
library("plyr")

build_pathname <- function(filename, path) {
  return(paste(path, filename, sep="/"))
}


num_datasets <- 11
filepath <- "./RandomPieces_10000" 

#filepath <- "./RandomPieces_1000" 

files <- list.files(filepath)

sampledata <- sample(files, num_datasets, replace=FALSE)
file_paths <- lapply(sampledata, build_pathname, path=filepath)
names(file_paths) <- NULL

datasets <- sapply(file_paths, read.table, header=FALSE, sep=",")

#PERFORMANCE VARIABLES
totalPurity <- 0


for (i in 1:num_datasets) {
  
  cat(paste("validation #",i,":\n",sep=""))
  #create trainingset and test set
  
  traindata <- data.frame()
  for (j in 1:num_datasets) {
    if (j != i) {
      traindata <- rbind(traindata, data.frame(datasets[,j]))
    }
  }
  testdata <- data.frame(datasets[,i])
  
  # INSERT TRAINING AND PREDICTION CODE HERE
  source("run_dbscan.R")
  
}


#get 10 files randomly

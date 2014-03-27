#load libraries

library("fpc")
library("data.table")
library("plyr")

build_pathname <- function(filename, path) {
  return(paste(path, filename, sep="/"))
}




num_datasets <- 10
#change filepath per size
filepath <- "./RandomPieces_100" 
#1000 for 1 percent
#100 for 10 percent


files <- list.files(filepath)
sampledata <- sample(files, num_datasets, replace=FALSE)
file_paths <- lapply(sampledata, build_pathname, path=filepath)
names(file_paths) <- NULL

datasets <- sapply(file_paths, read.table, header=FALSE, sep=",")




runstatsKFoldMeanForDataSize <- c(0,0,0,0,0)
for (EPSILON in seq(0.1,1, by=0.1)) {  
  for(MINPOINTS in seq(5,200, by=5)){
    
    runstatsKFold <- c(0,0,0,0,0)
    #PERFORMANCE VARIABLES
    totalPrecisionTrain <-0
    totalRecallTrain <-0
    totalPrecisionTest <-0
    totalRecallTest <-0
    totalAccuracy <-0
    datapercent <- 1
    runNumber <-0
    
    
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
      
      
      
          
          
          runNumber <- i
          rundirectory <- paste("params_Minpts_",MINPOINTS,"_Eps_",EPSILON,sep="")
          dir.create(rundirectory)
      
      # INSERT TRAINING AND PREDICTION CODE HERE
      source("run_dbscan.R")
      runstatsKFold <- rbind(runstatsKFold,runstats)
    }
    
    ##END OF KFOLD - Summary Here
    runstatsKFoldMean <-sapply(runstats,mean)
    
    runstatsKFoldMeanForDataSize <- rbind(runstatsKFoldMeanForDataSize,runstatsKFoldMean)
    
    
    
    write.table(runstatsKFold,paste("params_Minpts_",MINPOINTS,"_Eps_",EPSILON,"_stats.csv",sep=""))
  }#end of epsilon  
}#end of minpts

runstatsKFoldMeanForDataSize
write.table(runstatsKFoldMeanForDataSize,paste("SUMMARY_OF_WHOLE_DATA_stats.csv",sep=""))

#get 10 files randomly

#TODO put the cross validation code here
#dump all important output in csv files

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

for (i in 1:num_datasets) {
  
  cat(paste("validation #",i,":\n",sep=""))
  #create trainingset and test set
  
  train <- data.frame()
  for (j in 1:num_datasets) {
    if (j != i) {
      train <- rbind(train, data.frame(datasets[,j]))
    }
  }
  test <- data.frame(datasets[,i])
  
}



#kddtrain <- read.table("./RandomPieces_1000/ids1000_322.data",header=FALSE,sep=",")



#get 10 files randomly

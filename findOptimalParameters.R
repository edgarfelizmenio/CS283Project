
#Find optimal parameters


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
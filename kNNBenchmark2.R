library("e1071")
library("SparseM")

cat("reading the dataset...\n")
if (!file.exists("test_20_percent.csv")) {
  input <- read.csv("test-remapped.csv", header=TRUE)
  input <- input[-1]
  #numrows <- ceiling(length(input) * 0.20)
  numrows <- 10
  write.csv(input[1: numrows],"test_20_percent.csv")
}

cat("scanning the matrix...")
dataset <- e1071::read.matrix.csr("test_20_percent.csv")
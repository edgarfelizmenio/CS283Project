#Coding Conventions:
# in functions, separate words with an '_'
# function definitions should appear first

library('hash')

get_vector <- function(line) {
  parsed <- strsplit(line, ",")
  return(unlist(parsed)[2])
}

# get the feature-value pais from the line
get_FV_Pairs <- function(line) {
  parsed <- strsplit(line, " ")
  
  fvPairs <- unlist(parsed)[-1]
  return(fvPairs)
}

termOccurences <- hash()
get_IDFs <- function(dataset) {
  get_Occurences <- function(line, termOccurences) {
    
    
    get_Feature <- function(pairs) {
      parsed <- strsplit(pairs, ":")
      feature <- unlist(parsed)[1]
      return(feature)
    }
    
    update_Table <- function(feature, termOccurences) {
      #cat("length(feature): ",paste(as.character(length(feature)),"\n",collapse=""))
      if (hash::has.key(feature, termOccurences)) {
        termOccurences[feature] <- termOccurences[[feature]] + 1
      } else {
        termOccurences[feature] <- 1
      }
      return(NULL)
    }
    
    fvPairs <- get_FV_Pairs(line)
    features <- sapply(fvPairs, get_Feature)
    #cat("length(features): ",paste(as.character(length(features)),"\n",collapse=""))
    invisible(sapply(features, update_Table, termOccurences=termOccurences))
    return(NULL)
  }
  
  compute_IDFs <- function(term, termOccurences, termIDFs, Log2N) {
    termIDFs[term] <- Log2N - log2(termOccurences[[term]])
    return(NULL)
  }
  
  N <- length(dataset)
  Log2N <- log2(N)
  
  #cat("computing term occurences...\n")

  invisible(sapply(dataset, get_Occurences, termOccurences=termOccurences))

  #cat("computing IDFs...\n")
  #cat(paste("N: ", N,"\n",collapse=""))
  termIDFs <- hash()
  invisible(sapply(keys(termOccurences), compute_IDFs, termOccurences=termOccurences, termIDFs=termIDFs, Log2N=Log2N))
  return(termIDFs)
}

term <- NULL
frequency <- NULL
parsed <- NULL
get_Characteristic_Terms <- function(dataset, IDFs) {
  get_Characteristic_Terms_Per_Line <- function(line, IDFs) {
    split_Pairs <- function(pair) {
      return(unlist(strsplit(pair,":")))
    }
    
    map_IDFs <- function(term, IDFs) {
      #cat(term)
      #cat("\n")
      #cat(IDFs[[term]])
      #cat("\n")
      return(IDFs[[term]])
    }
    
    fvPairs <- get_FV_Pairs(line)
    #cat(paste("fvPairs: ", fvPairs, collapse=""))
    #cat(paste("\n\nlength(fvPairs): ", length(fvPairs), "\n\n", collapse=""))

    
    
    
    parsed <- sapply(fvPairs, split_Pairs)
    term <- parsed[1,]
    #cat(term)
    frequency <- as.integer(parsed[2,])
    #cat("\n")
    #cat(parsed[1,])
    #cat("\n")
    #cat(parsed[2,])
    #cat(paste("\n\n\n",max(frequency), "\n\n\n"))
    frequency <- frequency / max(frequency)
    term_IDFs <- sapply(term, map_IDFs, IDFs=IDFs)
    #cat("\nterm_TFs:\n")
    #cat(frequency)
    #cat("\nterm_IDFs:\n")
    #cat(term_IDFs)
    TF_IDFs <- frequency*term_IDFs
    #cat("\nTF_IDFs:\n")
    #cat(TF_IDFs)
    cand_index <- which(TF_IDFs == max(TF_IDFs))
    #cat("\ncand_index:\n")
    #cat(cand_index)
    #cat("\n")
    int_term <- as.integer(term)
    df <- data.frame(int_term, frequency, term_IDFs, TF_IDFs)
    #print(df[with(df, order(-TF_IDFs, -frequency, int_term))[1:3],])
    return(df[with(df, order(-TF_IDFs, -frequency, int_term))[1:3],]$int_term)
  }
  
  
  return(sapply(dataset, get_Characteristic_Terms_Per_Line, IDFs=IDFs))
}

getTFs <- function(dataset) {
    get_TFs <- function(line) {
      parsed <- strsplit(pairs, ":")
      parsed <- strsplit(line, " ")
      fvPairs <- unlist(parsed)  
      return(fvPairs)
    }
    
}

# Read the Data
if (!exists("dataset")) {
  cat("reading data...\n")
  input <- readLines("test.csv")
  dataset <- input[-1]
  #dataset <- dataset[1:20]
}

if (!exists("vectors")) {
  cat("parsing data...\n")
  #for each line,
  #split the line on "," and discard the first part.
  vectors <- sapply(dataset, get_vector)
  names(vectors) <- NULL
}

#1: get the inverse document frequency
cat("getting the IDFs\n")
names(vectors) <- NULL
IDFs <- get_IDFs(vectors)
cat("getting the characteristic terms...\n")
x <- get_Characteristic_Terms(vectors, IDFs)
x <- unname(x)
x <- t(x)
##For each testing instance t:
##
##a) Find the 3 features with the highest TF/IDF (using only the training instances for the IDF computation). [DONE]
##b) From the set T of all the training instances compute set S, subset of T, for which: each instance of S contains at least one of the 3 top features of step a.
##c) Find the 5 nearest instances of S to t. Return the 3 most frequent categories of the 5 nearest instances. In case of they are less than 3, return less than 3 answers.
##In case the most frequent answers are more than 3 (tie breaking cases) return more than 3 answers.The distance between two instances is computed using Boolean attributes and the following formula:
##(numberOfFeaturesOfInst1 + numberOfFeaturesOfInst2 - 2 * commonFeaturesOf(inst1,inst2) ) / (numberOfFeaturesOfInst1 + numberOfFeaturesOfInst2 - commonFeaturesOf(inst1,inst2)),

##where numberOfFeaturesOfInstN are the number of features of instance N (without taking into account frequency) and  commonFeaturesOf(inst1,inst2) are the number of features that appear in both instances.
# init
libs <- c("tm", "plyr", "class", "RTextTools", "randomForest")
lapply(libs, require, character.only = TRUE)

# set options
options(stringsAsFactors = FALSE)

# set parameters
labels <- read.table('labels.txt')
pathtrain <- paste(getwd(), "/newtrain", sep="")
pathtest <- paste(getwd(), "/test", sep="")

# build TDM
generateTDM <- function(label, path) {
  s.dir <- sprintf("%s/%s", path, label)
  s.fnames <- list.files(s.dir, pattern = "*.txt", full.names = TRUE)
  s.fnames2 <- list.files(s.dir, pattern = "*.txt", full.names = FALSE)
  s.data <- lapply(s.fnames, function(x) paste(readLines(x), collapse = " "))
  s.tdm <- create_matrix(s.data, language="english", removeNumbers = TRUE, removePunctuation = TRUE, 
                         removeSparseTerms = 0.7, removeStopwords = TRUE, stemWords = TRUE, stripWhitespace = TRUE, toLower = TRUE)
  #return(list(name = label, tdm = s.tdm))
  return(list(flist = s.fnames2, clabel = label, tdm = s.tdm))
}

#tdm <- lapply(labels, generateTDM, path = pathtrain)
generateDF <- function(label, path) { 
  k <- generateTDM(label, path)
  tdm <- k$tdm
  tdm$dimnames$Docs <- k$flist
  s.mat <- t(data.matrix(tdm))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  s.df <- cbind(s.df, rep(k$clabel, nrow(s.df)), row.names = NULL)
  colnames(s.df)[ncol(s.df)] <- "targetlabel"
  return(s.df)
}

df <- lapply(labels, generateDF, path = pathtrain)

# stack
tdm.stack <- do.call(rbind.fill, df)
tdm.stack[is.na(tdm.stack)] <- 0

# hold-out
train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.7))
test.idx <- (1:nrow(tdm.stack)) [- train.idx]

# model - KNN
tdm.lab <- tdm.stack[, "targetlabel"]
tdm.stack.nl <- tdm.stack[, !colnames(tdm.stack) %in% "targetlabel"]
knn.pred <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.lab[train.idx])

# accuracy
conf.mat <- table("Predictions" = knn.pred, Actual = tdm.lab[test.idx])
(accuracy <- sum(diag(conf.mat)) / length(test.idx) * 100)
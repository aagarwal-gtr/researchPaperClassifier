# init
libs <- c("tm", "plyr", "class", "randomForest", "lda", "MASS", "party") # load requred R packages
lapply(libs, require, character.only = TRUE)

# set options
options(stringsAsFactors = FALSE)

# set parameters
labels <- read.table('labels.txt') # read classes
path <- paste(getwd(), "/data", sep="") # get directory path of data

# clean text
cleanCorpus <- function(corpus) {
  corpus.tmp <- tm_map(corpus, removePunctuation) # remove punctuation
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers) # remove numbers
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace) # strip extra whitespace
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower)) # convert all text to lowercase
  corpus.tmp <- tm_map(corpus.tmp, stemDocument, language = "english") # stem words using porter's stemming algorithm
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english")) # remove stopwords
  corpus.tmp <- tm_map(corpus.tmp, function(x) iconv(x, "latin1", "ASCII", sub="")) # remove non ASCII characters
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument) # ensure that corpus is plaintextdocument
  return(corpus.tmp)
}

# build TDM
generateTDM <- function(label, path) {
  s.dir <- sprintf("%s/%s", path, label) # create directory path by appending label to master path
  s.cor <- Corpus(DirSource(directory = s.dir), readerControl = list(language = "en")) # create corpus of all docs in directory
  s.cor.cl <- cleanCorpus(s.cor) # call cleancorpus function to clean text
  s.tdm <- TermDocumentMatrix(s.cor.cl) # create tdm
  s.tdm <- removeSparseTerms(s.tdm, 0.7) # remove sparsely occuring words
  return(list(name = label, tdm = s.tdm))
}

tdm <- lapply(labels, generateTDM, path = path) # fetch list of TDMs for all classes

# attach name
bindLabelToTDM <- function(tdm) { # binds class name to appropriate TDM
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)), row.names = NULL)
  colnames(s.df)[ncol(s.df)] <- "targetlabel"
  return(s.df) 
}

labelTDM <- lapply(tdm, bindLabelToTDM) # calls binding function

# stack
tdm.stack <- do.call(rbind.fill, labelTDM) # stacks similar TDMs together
tdm.stack[is.na(tdm.stack)] <- 0 # replace NA values with zero

# hold-out
train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.75)) # split data into test and train
test.idx <- (1:nrow(tdm.stack)) [- train.idx]

tdm.lab <- tdm.stack[, "targetlabel"]
tdm.stack.nl <- tdm.stack[, !colnames(tdm.stack) %in% "targetlabel"] # same dataset without class values
train <- tdm.stack[train.idx, ] # train dataset
test <- tdm.stack[test.idx, ] # test dataset with class values
testnl <- tdm.stack.nl[test.idx, ] # test dataset without class values

# set.seed(1234)

# Random FOrrest
rf.train <- train
rf.trainnl <- trainnl
rf.test <- test
rf.testnl <- testnl
rf.formula <- as.formula("targetlabel ~ .")
environment(rf.formula) <- environment()
rf.train$targetlabel <- as.factor(rf.train$targetlabel)
tdm.lab <- as.data.frame(tdm.lab)
rf <- randomForest(x = tdm.stack.nl, y = tdm.lab, ntree = 5000, mtry = 15, importance = TRUE)
#rf <- randomForest(rf.formula, data = rf.train, ntree = 5000, mtry = 15, importance = TRUE)
rf.testnl$targetlabel = predict(rf, rf.testnl, type = "response")
table(rf.test$targetlabel, rf.testnl$targetlabel)
prop.table(table(rf.test$targetlabel, rf.testnl$targetlabel), 1)
 
# # Conditional Inference Tree
# ctree.train <- train
# ctree.testnl <- testnl
# ctree.train$targetlabel <- as.factor(ctree.train$targetlabel)
# ctree <- ctree(targetlabel ~ ., data = ctree.train)
# ctree.testnl$targetlabel <- predict(ctree, ctree.testnl, type = "response")
# ctree.conf.mat <- table(ctree.test$targetlabel, ctree,testnl$targetlabel)
# ctree.prop.table <- prop.table(table(ctree.test$targetlabel, ctree.testnl$targetlabel), 1)
# 
# # Linear Discriminant Analysis
# lda.train <- train
# lda.testnl <- testnl
# lda.test <- test
# lda <- lda(targetlabel ~ ., data = lda.train)
# k <- as.data.frame(predict(lda, lda.testnl, type = "response"))
# lda.testnl$targetlabel = k
# lda.testnl$targetlabel = k$class
# lda.conf.mat <- table(lda.test$targetlabel, lda.testnl$targetlabel)
# lda.prop.table <- prop.table(table(lda.test$targetlabel, lda.testnl$targetlabel), 1)

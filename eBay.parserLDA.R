# TODO: convert the raw click data into LDA model format
# 
# Author: Jun Yu
# Version: June, 2012
###############################################################################

rm(list=ls())

#setwd("/Users/junyu/Documents/workspace/eBay.R")
setwd("/Users/junyu/Documents/eBay/code/eBay.R")

library("hash")

source("./eBay.Utility.R")

set.seed(8675309)

######################
# experiment settings
######################
queries <- c("hello kitty", "fossil", "fossils", "basketball", "keyboard", "iphone", "ipod", "coach")
stopWordFreq <- 1.0
rareWordFreq <- 0.01

## load queries
#queryFile <- "../../data/testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query

# queryStats
queryStats <- array(0, dim=c(length(queries),3))
colnames(queryStats) <- c("query", "vocabSize", "numDocs")

for (n in 1:length(queries)) {
	query <- queries[n]
	cat("Query:",query,"...\n")
	
	# load in click data
	clickDataFile <- paste("../../data/train/",query,"_train.tsv",sep="")
	clickData <- LoadLogData(clickDataFile)
	clicks <- as.character(clickData$ItemID)
	clickDataItemTerms <- clickData$Terms
	clickDataItemTitle <- clickData$Title
	
	# find all the terms in the title
	allWords <- unlist(clickDataItemTerms)

	# get unique words
	uniqueWords <- unique(allWords)
	uniqueWordsFreq <- table(allWords) / length(clicks)
	
	# remove stop words
	#stopWords <- uniqueWords[which(uniqueWordsFreq > stopWordFreq)]
	#nonStopWords <- uniqueWords[which(uniqueWordsFreq <= stopWordFreq)]
	
	# remove rare words
	uniqueWordsFreq <- sort(uniqueWordsFreq)
	uniqueWordsFreq <- uniqueWordsFreq[which(uniqueWordsFreq > rareWordFreq)]

	# construct the dictionary
	dic <- hash(names(uniqueWordsFreq), as.numeric(uniqueWordsFreq))
	vocab <- keys(dic)
	vocabSize <- length(vocab)
	vocabIndex <- hash(vocab, 0:(vocabSize-1))
	
	# construct LDA documents
	documents.title <- list()
	documents.origtitle <- list()
	lda.documents <- list()
	mblda.documents <- list()
	for (i in 1:length(clicks)) {
		if (i %% 5000 == 0) { cat("Process", i, "data...\n") } 
		
		terms <- unlist(clickDataItemTerms[i])
		termIndex <- unlist(sapply(terms, function(x) vocabIndex[[x]]))
		documents.title[[i]] <- clickDataItemTitle[i]
		
		# save lda document data
		lda.documents[[i]] <- matrix(as.integer(1), nrow=2, ncol=length(termIndex))
		lda.documents[[i]][1,] <- termIndex
		
		# save mblda document data
		mblda.documents[[i]] <- matrix(as.integer(0), nrow=2, ncol=vocabSize)
		mblda.documents[[i]][1,] <- as.integer(0:(vocabSize-1))
		mblda.documents[[i]][2,(termIndex + 1)] <- as.integer(1) 
	}	
	
	# save data
	outputDataFile <- paste("../../data/train/",query,"_train.RData",sep="")
	save(query, vocab, lda.documents, mblda.documents, clickData, dic, file=outputDataFile)
	
	# store stats
	queryStats[n,] <- c(query, vocabSize, length(clicks))
	cat(paste(query, vocabSize, length(clicks), "\n", sep=" ")) 
}

# save query stats
outputDataFile <- "../../data/query_stat_lda.tsv"
write.table(queryStats, file=outputDataFile, sep="\t", row.names=FALSE, quote=FALSE)


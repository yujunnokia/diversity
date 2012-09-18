# TODO: subsample the raw data and use 10K clicked items to training LDA
#
# Author: Jun Yu
# Version: June, 2012
###############################################################################

rm(list=ls())

#setwd("/Users/junyu/Documents/workspace/eBay.R")
setwd("/Users/junyu/Documents/eBay/code/eBay.R")

source("./eBay.Utility.R")

set.seed(8675309)


######################
# experiment settings
######################
queries <- c("hello kitty", "fossil", "fossils", "basketball", "keyboard", "iphone", "ipod", "coach")
numClicks <- 10000
modes <- c("train", "validation")

## load queries
#queryFile <- "../testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query

for (query in queries) {
	cat("Query:",query,"...\n")
	
	# load in click data
	clickDataFile <- paste("../../data/raw/",query,"_raw.tsv",sep="");
	clickData <- read.table(clickDataFile, head=TRUE, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE)
	clicks <- clickData$ItemID
	
	# sub-sample the data
	#sampleClicksIndex <- sample(1:length(clicks), numClicks*2, replace=FALSE)
	sampleClicksIndex <- sample(1:length(clicks), numClicks*2, replace=TRUE)
	sampleClicks <- data.frame(Index=sampleClicksIndex, Mode=c(rep("train",numClicks), rep("validation",numClicks)))
	
	for (mode in modes) {
		index <- sampleClicks$Index[which(sampleClicks$Mode == mode)]

		# get sample data
		sampleData <- clickData[index,]
		
		# save sample data
		sampleClickFile <- paste("../../data/",mode,"/",query,"_",mode,".tsv",sep="")
		write.table(sampleData, file=sampleClickFile, sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
	} # mode
} # query



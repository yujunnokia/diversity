# TODO: Evaluate Cat-Div model
#
# Author: Jun Yu
# Version: July, 2012
##################################################################

rm(list=ls())

#setwd("/Users/junyu/Documents/workspace/eBay.R")
setwd("/Users/junyu/Documents/eBay/code/eBay.R")

source("./eBay.Utility.R")

#######################
## experiment settings
#######################
Tdver <- "Cosine"
queries <- c("hello kitty", "fossil", "fossils", "basketball", "keyboard", "coach", "iphone", "ipod")
topKs <- 1:5

## load queries
#queryFile <- "../../data/testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query

# run evaluation
results <- matrix(0, nrow=length(topKs), ncol=length(queries))
rownames(results) <- topKs
colnames(results) <- queries
for (query in queries) {
	print (query)
	
	# load in TRv4 for item similarity comparison
	TRv4ModelFile <- paste("../../data/model/TRv4/",query,"_TRv4.tsv",sep="")
	TRv4Model <- LoadTRv4Model(TRv4ModelFile)
	
	# load in click data
	validationDataFile <- paste("../../data/validation/",query,"_validation.tsv",sep="")
	validationData <- LoadLogData(validationDataFile) 
	
	# load in rank data
	rankDataFile <- paste("../../data/catdiv/",query,"_catdiv.tsv",sep="")
	rankData <- LoadRankData(rankDataFile) 
	
	# evaluate on top K items of the rank data
	results[,query] <- EvaluateRankList(TRv4Model, Tdver, validationData, rankData, topKs)
} # queries

# save the results
outputFile <- paste("../../result/catdiv_",Tdver,".tsv",sep="")
write.table(results, outputFile, quote=TRUE, sep="\t", row.names=FALSE)



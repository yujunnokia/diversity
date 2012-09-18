# TODO: Evaluate LDA models
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
lambdas <- c("0", "0.2", "0.4", "0.6", "0.8", "1")
ldaVersion <- "lda"  # lda or mblda
K <- 5  # number of topics

args <- commandArgs(trailingOnly = TRUE)
ldaVersion <- as.character(args[2])
if (ldaVersion != "lda" && ldaVersion != "mblda") {
	stop("lda version is invalid.\n
					please run 'Rscript eBay.evalLDA.R -args lda' or \n
					'Rscript eBay.evalLDA.R -args mblda'")
} 

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
	
	maxAUC <- 0
	for (lambda in lambdas) {
		lambda <- as.numeric(lambda)
		cat("lambda is",lambda,"\n")
		
		# load in rank data
		rankDataFile <- paste("../../result/lda/",query,"_",ldaVersion,"_K",K,"_",lambda,".tsv",sep="")
		rankData <- LoadRankData(rankDataFile) 
		
		# evaluate on top K items of the rank data
		result <- EvaluateRankList(TRv4Model, Tdver, validationData, rankData, topKs)
		
		# use the lambda value that gives that best area under the curve
		AUC <- sum(sapply(topKs[-length(topKs)], function(x) mean(result[x] + result[x+1])))
		if (AUC > maxAUC) {
			maxAUC <- AUC
			results[,query] <- result
		}
	} # lambda
} # queries

# save the result
outputFile <- paste("../../result/",ldaVersion,"_",Tdver,".tsv",sep="")
write.table(results, outputFile, quote=TRUE, sep="\t", row.names=FALSE)



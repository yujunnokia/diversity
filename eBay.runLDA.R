# TODO: rank all items according to topic model and show the most relevant item 
#       from each topic
# 
# Author: Jun Yu
# Version: July, 2012
###############################################################################

rm(list=ls())

#setwd("/Users/junyu/Documents/workspace/eBay.R")
setwd("/Users/junyu/Documents/eBay/code/eBay.R")

source("./eBay.Utility.R")

#######################
## experiment settings
#######################
ldaVersion <- "lda"  # lda or mblda
queries <- c("hello kitty", "fossil", "fossils", "basketball", "keyboard", "iphone", "ipod", "coach")
lambdas <- c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")
K <- 4  # number of topics

args <- commandArgs(trailingOnly = TRUE)
ldaVersion <- as.character(args[2])
if (ldaVersion != "lda" && ldaVersion != "mblda") {
	stop("lda version is invalid.\n
					please run 'Rscript eBay.runLDA.R -args lda' or \n
					'Rscript eBay.runLDA.R -args mblda'")
} 

## load queries
#queryFile <- "../../data/testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query

for (query in queries) {
	print(query)
	
	# load in TRv4 model
	TRv4ModelFile <- paste("../../data/model/TRv4/",query,"_TRv4.tsv",sep="")
	TRv4Model <- LoadTRv4Model(TRv4ModelFile)
	
	# load live eBay data
	rankDataFile <- paste("../../data/rank/",query,"_rank.tsv",sep="")
	rankData <- LoadRankData(rankDataFile) 
	rankDataTerms <- rankData$Terms

	# load weight
	weightFile <- paste("../../data/model/lda/",query,"_K",K,"_",ldaVersion,"_weights.tsv",sep="")
	weightData <- read.table(weightFile, head=TRUE, sep="\t", check.names=FALSE)
	weightTerms <- colnames(weightData)
	
#	# normalize
#	weightData <- apply(weightData, 2, function(x) x * (log(x + 1e-05) - sum(log(x + 1e-05))/length(x)))
#	weightData <- data.frame(weightData)
#	colnames(weightData) <- weightTerms
	
	# load topic popularity data
	topicPopFile <- paste("../../data/model/lda/",query,"_K",K,"_",ldaVersion,"_topicpop.tsv",sep="")
	topicPopData <- read.table(topicPopFile, head=TRUE, sep="\t", check.names=FALSE)
	topicPopOrder <- order(topicPopData[,2], decreasing=TRUE)
	
	# score each item in each topics
	itemScores <- ScoreItemsLDA(TRv4Model, rankDataTerms, weightTerms)
	itemIDs <- names(rankDataTerms)
	
	for (lambda in lambdas) {
		lambda <- as.numeric(lambda)
		cat("lambda is",lambda,"\n")
		
		# rank all the topics according to MMR
		notSelectedTopics <- topicPopOrder
		rankedTopics <- c(notSelectedTopics[1])
		notSelectedTopics <- notSelectedTopics[-1]
		for (k in 2:K) {
			maxMMR <- -1
			maxMMRItem <- notSelectedTopics[1]
			for (i in notSelectedTopics) {
				maxSim <- -1
				
				# compute maximum similarity
				for (j in rankedTopics) {
					selectedTopic <- as.numeric(weightData[i,])
					notSelectedTopic <- as.numeric(weightData[j,])
					sim <- crossprod(selectedTopic, notSelectedTopic)/sqrt(crossprod(selectedTopic) * crossprod(notSelectedTopic))
					if (sim > maxSim) { maxSim <- sim}
				}
				
				# compute MMR value
				MMR <- lambda * (topicPopData[i,2] - min(topicPopData[,2])) / (max(topicPopData[,2]) - min(topicPopData[,2])) - (1 - lambda) * maxSim 
				#MMR <- lambda * topicPopData[i,2] - (1 - lambda) * maxSim 
				if (MMR > maxMMR) { 
					maxMMR <- MMR
					maxMMRItem <- i
				}
			} # k
			
			rankedTopics <- c(rankedTopics, maxMMRItem)
			notSelectedTopics <- notSelectedTopics[-which(notSelectedTopics == maxMMRItem)]
		}
		
		# output the item with the maximum score from each topic
		selectedItemIDs <- rep(0, K)
		selectedItemTitle <- rep(0, K)
		cat1 <- rep(0, K)
		cat2 <- rep(0, K)
		for (i in 1:K) {
			k <- rankedTopics[i]
			count <- 1
			itemIndex <- order(itemScores[,k], decreasing = TRUE)[count]
			itemID <- itemIDs[itemIndex]
			while (sum(selectedItemIDs == itemID)) {
				count <- count + 1
				itemIndex <- order(itemScores[,k], decreasing = TRUE)[count]
				itemID <- itemIDs[itemIndex]
			}
			selectedItemIDs[i] <- itemID
			selectedItemTitle[i] <- as.character(rankData$Title[which(itemIDs == itemID)])
			cat1[i] <- as.numeric(rankData$Cat1[which(itemIDs == itemID)])
			cat2[i] <- as.numeric(rankData$Cat2[which(itemIDs == itemID)])
		} # i
		
		# save output
		outputData <- data.frame(ItemID=selectedItemIDs, Score=rep(0,length(selectedItemIDs)), 
				Title=selectedItemTitle, Cat1=cat1, Cat2=cat2)
		outputFile <- paste("../../result/lda/",query,"_",ldaVersion,"_K",K,"_",lambda,".tsv",sep="")
		write.table(outputData, file=outputFile, row.names=FALSE, sep="\t", quote=FALSE)
	} # lambda
} # query

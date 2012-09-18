# TODO: Run MMR model with different lambda values
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
queries <- c("hello kitty", "fossil", "fossils", "basketball", "keyboard", "iphone", "ipod", "coach")
lambdas <- c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0")
topK <- 10 # number of items output from MMR model

## load queries
#queryFile <- "../../data/testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query

# run evaluation
for (query in queries) {
	print(query)
	
	# load in TRv4 model
	TRv4ModelFile <- paste("../../data/model/TRv4/",query,"_TRv4.tsv",sep="")
	TRv4Model <- LoadTRv4Model(TRv4ModelFile)

	# load live eBay data
	rankDataFile <- paste("../../data/rank/",query,"_rank.tsv",sep="")
	rankData <- LoadRankData(rankDataFile) 
	rankDataTerms <- rankData$Terms
	
	# score items
	rankDataScores <- ScoreItems(TRv4Model, rankDataTerms)
	
	# sort rank data
	sortedItems <- names(rankDataScores)[order(unlist(rankDataScores), decreasing=TRUE)]
	
	# run MMR with different lambda value
	for (lambda in lambdas) {
		lambda <- as.numeric(lambda)
		cat("lambda is",lambda,"\n")
		
		# re-rank items according to MMR
		notSelectedItems <- sortedItems
		selectedItems <- c(notSelectedItems[1])
		notSelectedItems <- notSelectedItems[-1]
		score <- array(0,topK)
		score[1] <- as.numeric(rankDataScores[selectedItems[1]])
		index <- which(rankData$ItemID == as.numeric(selectedItems[1]))
		cat1 <- array(0,topK)
		cat1[1] <- rankData$Cat1[index]
		cat2 <- array(0,topK)
		cat2[1] <- rankData$Cat2[index]
		title <- array(0,topK)
		title[1] <- rankData$Title[index]
		for (k in 2:topK) {
			maxMMR <- -1
			maxMMRItem <- notSelectedItems[1]
			maxMMRItemRelScore <- -1
			for (i in notSelectedItems) {
				maxSim <- -1
				notSelectedItemTerms <- rankDataTerms[[i]]
				
				# compute maximum similarity
				for (j in selectedItems) {
					selectedItemTerms <- rankDataTerms[[j]]
					
					sim <- CosineSimilarity(TRv4Model, notSelectedItemTerms, selectedItemTerms)
					if (sim > maxSim) { maxSim <- sim }
				}
				
				# compute MMR value
				MMR <- lambda * rankDataScores[[i]] - (1 - lambda) * maxSim 
				if (MMR > maxMMR) { 
					maxMMR <- MMR
					maxMMRItem <- i
					maxMMRItemRelScore <-  as.numeric(rankDataScores[i])
				}
			} # notSelectedItems
			
			selectedItems <- c(selectedItems, maxMMRItem)
			notSelectedItems <- notSelectedItems[-which(notSelectedItems == maxMMRItem)]
			
			index <- which(rankData$ItemID == as.numeric(maxMMRItem))
			score[k] <- maxMMRItemRelScore
			cat1[k]  <- rankData$Cat1[index] 
			cat2[k]  <- rankData$Cat2[index] 
			title[k] <- rankData$Title[index]
		} # k

		# save MMR ranking results
		outputData <- data.frame(ItemID=selectedItems, Score=score, Title=title, Cat1=cat1, Cat2=cat2)
		outputFile <- paste("../../result/mmr/",query,"_MMR_",lambda,".tsv",sep="")
		write.table(outputData, file=outputFile, row.names=FALSE, sep="\t", quote=FALSE)
	} # lambda
} # queries




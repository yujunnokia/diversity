# TODO: Utility functions
#
# Author: Jun Yu
# Version: June, 2012
##################################################################

#setwd("/Users/junyu/Documents/workspace/eBay.R")

#library("hash")

#
# normalize title
#
NormalizeTitle <- function(str)
{
	# [0] convert to lower-case
	str <- tolower(str)
	
	# [1] processDotRule:
	#       Preserve (STRING-START | DIGIT | SPACE) DOT DIGIT+;
	#       else Replace DOT with replaceTo;
	str <- gsub("[^a-z0-9\\.]", " ", str, perl=TRUE)
	str <- paste(" ", str, sep="")
	str <- gsub("([0-9\\.])\\.([0-9])", "\\1D\\2", str, perl=TRUE)
	
	# [2] processAdjacantNumAlphaRule:
	#       DIGIT ALPHA  =>  DIGIT replaceTo ALPHA;
	#       ALPHA DIGIT  =>  ALPHA replaceTo DIGIT;
	str <- gsub("([a-z])([0-9])", "\\1 \\2", str, perl=TRUE)
	str <- gsub("([0-9])([a-z])", "\\1 \\2", str, perl=TRUE)
	
	str <- gsub("[^a-z0-9D]", " ", str, perl=TRUE)
	str <- gsub("D", ".", str, perl=TRUE)
	
	str <- gsub("[\\s]+", " ", str, perl=TRUE)
	str <- gsub("^[\\s]+", "", str, perl=TRUE)
	str <- gsub("[\\s]+$", "", str, perl=TRUE)
	
	return(str) 
}


#
# load in TRv4 model
#
LoadTRv4Model <- function(TRv4ModelFile) 
{
	TRv4Data <- read.table(TRv4ModelFile, header = TRUE, sep = "\t", stringsAsFactors=FALSE)

	TRv4Model <- list()
	for (i in 1:nrow(TRv4Data)) {
		TRv4Model[[as.character(TRv4Data[i,1])]] <- as.numeric(TRv4Data[i,2])
	}
	
	return(TRv4Model)
}


#
# load in rank data
#
LoadRankData <- function(rankDataFile, topRankData = -1) 
{	
	terms <- list()
#	rankDataTitles <- list()
	
	rankData <- read.table(rankDataFile, head=TRUE, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE)
	for (i in 1:nrow(rankData)) {
		itemID <- as.character(rankData$ItemID[i])
		title <- NormalizeTitle(rankData$Title[i])
		cat1 <- paste("cat-",rankData$Cat1[i],sep="")
		cat2 <- paste("cat-",rankData$Cat2[i],sep="")
		
		if (cat2 == "cat-0") {
			terms[[itemID]] <- unique(c(unlist(strsplit(title," ")), cat1))
#			rankDataTitles[[itemID]] <- paste(title, cat1, sep="\t")
		} else {
			terms[[itemID]] <- unique(c(unlist(strsplit(title," ")), cat1, cat2))
#			rankDataTitles[[itemID]] <- paste(title, cat1, cat2, sep="\t")
		}
	}
	
	return(list(ItemID=rankData$ItemID, Score=rankData$BackendScore, Terms=terms, 
					Title=rankData$Title, Cat1=rankData$Cat1, Cat2=rankData$Cat2))
}


#
# load in log data
#
LoadLogData <- function(logDataFile) 
{
	# load in log data
	logData <- read.table(logDataFile, head=TRUE, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE)
	
	# normalize title
	titleTerms <- sapply(logData$Title, function(x) strsplit(NormalizeTitle(x), split=" "))
	
	# add category as one term in title
	terms <- sapply(1:nrow(logData), function(x) if (logData$Cat2[x] == 0) { c(titleTerms[[x]], paste("cat-",logData$Cat1[x],sep="")) } 
				else { c(titleTerms[[x]], paste("cat-",logData$Cat1[x],sep=""), paste("cat-",logData$Cat2[x],sep="")) } )
	names(terms) <- logData$ItemID
	
	return(list(ItemID=logData$ItemID, Terms=terms, Title=logData$Title, Cat1=logData$Cat1, Cat2=logData$Cat2))
}


#
# load in log data from click and item files
#
LoadLogData2 <- function(logDataFile, logDataItemFile) 
{
	# load in click itemIDs
	logs <- read.table(logDataFile, header=TRUE, sep="\t", stringsAsFactors=FALSE)
	ItemIDs <- logs$ItemID
	
	# load in loged item information
	Terms <- list()
	Count <- list()
	Title <- list()
	logItems <- read.table(logDataItemFile, head=TRUE, sep="\t", comment.char="", quote="", stringsAsFactors=FALSE)
	for (i in 1:nrow(logItems)) {
		itemID <- as.character(logItems$ItemID[i])
		title <- NormalizeTitle(logItems$Title[i])
		cat1 <- paste("cat-",logItems$Cat1[i],sep="")
		cat2 <- paste("cat-",logItems$Cat2[i],sep="")
		count <- logItems$Count[i]
		if (cat2 == "cat-0") {
			Terms[[itemID]] <- unique(c(unlist(strsplit(title," ")), cat1))
		} else {
			Terms[[itemID]] <- unique(c(unlist(strsplit(title," ")), cat1, cat2))
		}
		Count[[itemID]] <- as.numeric(count)
		Title[[itemID]] <- logItems$Title[i]
	}
	
	return(list(ItemID=ItemIDs, Terms=Terms, Count=Count, Title=Title))
}


#
# Compute cosine similarity between two items
#
CosineSimilarity <- function(TRv4Model, item1, item2) 
{
	# remove duplicated terms
	item1 <- unique(item1)
	item2 <- unique(item2)
	
	# keep the terms in TRv4 model
	item1 <- intersect(item1, names(TRv4Model))
	item2 <- intersect(item2, names(TRv4Model))
	
	# find intersect sets
	isect <- intersect(item1, item2)

	num <- denom1 <- denom2 <- 0
	if (length(isect) > 0) { 
		num <- sum(sapply(isect, function(x) TRv4Model[[x]]^2)) 
	} 
	if (length(item1)) {
		denom1 <- sum(sapply(item1, function(x) TRv4Model[[x]]^2))
	}
	if (length(item2)) {
		denom2 <- sum(sapply(item2, function(x) TRv4Model[[x]]^2))
	}

	if (denom1 == 0 || denom2 == 0) {
		return(0)
	} else {
		return(num/(denom1^0.5 * denom2^0.5))
	}
}


#
# Jaccard Index similarity
#
JaccardSimilarity <- function(qbrModel, item1, item2) 
{
	# remove duplicated terms
	item1 <- unique(item1)
	item2 <- unique(item2)
	
	# keep the terms in qbr model
	item1 <- intersect(item1, keys(qbrModel))
	item2 <- intersect(item2, keys(qbrModel))
	
	# find intersect and union sets
	union <- union(item1, item2)
	isect <- intersect(item1, item2)

	# compute Jaccard Index
	num <- denom <- 0
	if (length(isect) > 0) {
		num <- sum(sapply(isect, function(x) abs(qbrModel[[x]])))
	} 
	if (length(union)) {
		denom <- sum(sapply(union, function(x) abs(qbrModel[[x]])))
	}
	
	if (denom == 0) {
		return(0)
	} else {
		return(num/denom)
	}
}


#
# evaluate rank list based on validation data
#
EvaluateRankList <- function(TRv4Model, Tdver, validationData, rankData, topKs)
{
	validationDataItemID <- as.character(validationData$ItemID)
	validationDataTerms  <- validationData$Terms

	rankDataItemID <- as.character(rankData$ItemID)
	rankDataTerms  <- rankData$Terms
	
	# compute similarity between rank data and validation data
	sims <- matrix(0, nrow=max(topKs), ncol=length(validationDataItemID))
	for (i in 1:max(topKs)) {
		if (i %% 5 == 0) {
			cat(paste("compute similarity for top",i,"item on list\n",sep=" ")) 
		}
		
		rankItemTerms <- rankDataTerms[[rankDataItemID[i]]]
		sims[i,] <- sapply(validationDataItemID, 
				function(x) CosineSimilarity(TRv4Model, validationDataTerms[[x]], rankItemTerms))
	}
	
	# compute mean satisfaction at each top K
	scores <- rep(0,length(topKs))
	for (i in 1:length(topKs)) {
		topK <- topKs[i]
		if (topK == 1) {
			maxSims <- sims[1,]
		} else {
			maxSims <- apply(sims[1:topK,], 2, max)
		}
		scores[i] <- sum(maxSims)
	}
	
	return(scores/length(validationDataItemID))
}


#
# score each item according to TRv4
#
ScoreItems <- function(TRv4Model, rankDataTerms) {
	itemIDs <- names(rankDataTerms)
	
	# compute relevance score
	itemScores <- list()
	for (itemID in itemIDs) {
		itemTerms <- rankDataTerms[[itemID]]
		itemTerms <- intersect(itemTerms, names(TRv4Model))
		
		# sum the weights of all terms
		itemScore <- 0
		if (length(itemTerms) > 0) {
			itemScore <- sum(sapply(itemTerms, function(x) TRv4Model[[x]]))
		}
		
		# normalize
		normalizedLength <- max(TRv4Model[["@AVG_TITLE_LENGTH"]], length(rankDataTerms[[itemID]]))
		itemScores[[itemID]] <- (itemScore / normalizedLength - TRv4Model[["MIN"]]) * 1.00/ (TRv4Model[["MAX"]] - TRv4Model[["MIN"]])
	}
	
	return(itemScores)
}


#
#
#
ScoreItemsLDA <- function(TRv4Model, rankDataItemTerms, weightTerms) {
	
	itemIDs <- names(rankDataItemTerms)
	nItems <- length(itemIDs)
	itemScores <- matrix(0, nrow=nItems, ncol=K)
	row.names(itemScores) <- itemIDs
	for (itemID in itemIDs) {
		itemTerms <- rankDataItemTerms[[itemID]]
		itemTerms <- intersect(itemTerms, weightTerms)
		if (length(itemTerms) > 1) {
			itemScores[itemID,] <- rowSums(sapply(itemTerms, function(x) weightData[[x]]))
		} 
	}
	
	# normalize the score by the length of the item
	itemNormalizedLength <- rep(0, nItems)
	for (i in 1:nItems) {
		terms <- rankDataItemTerms[[itemIDs[i]]]
		if ("cat-0" %in% terms) {
			terms <- terms[-which(terms == "cat-0")]
		}
		itemNormalizedLength[i] <- max(TRv4Model[["@AVG_TITLE_LENGTH"]], length(terms))
	}
	itemScores <- itemScores / matrix(rep(itemNormalizedLength, K), ncol=K)
	
	return(itemScores)
}


#
# summary fn
#
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
	require(plyr)
	
	# New version of length which can handle NA's: if na.rm==T, don't count them
	length2 <- function (x, na.rm=FALSE) {
		if (na.rm) sum(!is.na(x))
		else       length(x)
	}
	
	# This is does the summary; it's not easy to understand...
	datac <- ddply(data, groupvars, .drop=.drop,
			.fun= function(xx, col, na.rm) {
				c( N    = length2(xx[,col], na.rm=na.rm),
						mean = mean   (xx[,col], na.rm=na.rm),
						sd   = sd     (xx[,col], na.rm=na.rm)
				)
			},
			measurevar,
			na.rm
	)
	
	# Rename the "mean" column    
	datac <- rename(datac, c("mean"=measurevar))
	
	datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
	
	# Confidence interval multiplier for standard error
	# Calculate t-statistic for confidence interval: 
	# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
	ciMult <- qt(conf.interval/2 + .5, datac$N-1)
	datac$ci <- datac$se * ciMult
	
	return(datac)
}


#
# LDA version of top topic words
#
lda.top.topic.words <- function (topics, num.words = 20, by.score = FALSE) 
{
	if (by.score) {
		normalized.topics <- topics/(rowSums(topics) + 1e-05)
		scores <- apply(normalized.topics, 2, function(x) x * 
							(log(x + 1e-05) - sum(log(x + 1e-05))/length(x)))
		apply(scores, 1, function(x) colnames(scores)[order(x, 
									decreasing = TRUE)[1:num.words]])
	}
	else {
		apply(topics, 1, function(x) colnames(topics)[order(x, 
									decreasing = TRUE)[1:num.words]])
	}
}


#
# MBLDA version of top topic words
#
mblda.top.topic.words <- function (topics_present, topics_absent, num.words = 20, by.score = FALSE) 
{
	normalized.topics <- topics_present/(topics_present + topics_absent + 1e-05)
	if (by.score) {
		scores <- apply(normalized.topics, 2, function(x) x * 
							(log(x + 1e-05) - sum(log(x + 1e-05))/length(x)))
		apply(scores, 1, function(x) colnames(scores)[order(x, 
									decreasing = TRUE)[1:num.words]])
	}
	else {
		apply(normalized.topics, 1, function(x) colnames(normalized.topics)[order(x, 
									decreasing = TRUE)[1:num.words]])
	}
}

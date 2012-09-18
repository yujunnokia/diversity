# TODO: Get the site data sorted by MLR
#
# Author: Jun Yu
# Version: July, 2012
##################################################################

rm(list=ls())

#setwd("/Users/junyu/Documents/workspace/eBay.R")
setwd("/Users/junyu/Documents/eBay/code/eBay.R")

queries <- c("hello kitty", "fossil", "fossils", "basketball", "keyboard", "coach", "iphone", "ipod")

## load queries
#queryFile <- "../../data/testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query
		
for (query in queries) {
	print(query)
	
	# replace whitespace with '+' in query
	q <- gsub(" ","+", query)
	
	# get the default ranker data
	url <- paste("http://dev-ss01.arch.ebay.com:9080/iquery?query=",q,"&fields=ItemID+BackendScore+ScrapeProfileID+LeafCats+Title+eBaySaleType&num_items=2000",sep="")
	data <- read.table(url, sep="\t", comment.char="", quote="", header=TRUE)
	if (sum(is.na(data$eBaySaleType)) > 0) {
		data <- data[-which(is.na(data$eBaySaleType)),]
	}
	if (sum(data$eBaySaleType == 1) > 0) {
		data <- data[-which(data$eBaySaleType == 1),]
	}
	cat("Number of BIN items is",nrow(data),"\n")
	
	# process category information
	cat1 <- matrix(0,nrow=nrow(data),ncol=1)
	cat2 <- matrix(0,nrow=nrow(data),ncol=1)
	for (i in 1:nrow(data)) {
		cats <- strsplit(as.character(data$LeafCats[i]),"\v")[[1]]
		cat1[i] <- as.numeric(cats[1])
		cat2[i] <- 0
		if (length(cats) == 2) {
			cat2[i] <- as.numeric(cats[2])
		} 
	}
	rankData <- data.frame(ItemID=data$ItemID, BackendScore=data$BackendScore, Title=data$Title,
			Cat1=cat1, Cat2=cat2)
	
	# save ranker data
	outputFile <- paste("../../data/rank/",query,"_rank.tsv",sep="")
	write.table(rankData, file=outputFile, sep="\t", row.names=FALSE, quote=FALSE)
}
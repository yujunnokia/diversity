# TODO: Convert Category-based diversity data
#
# Author: Jun Yu
# Version: June, 2012
##################################################################

rm(list=ls())

#setwd("/Users/junyu/Documents/workspace/eBay.R")
setwd("/Users/junyu/Documents/eBay/code/eBay.R")

queries <- c("hello kitty", "fossil", "fossils", "basketball", "keyboard", "iphone", "ipod", "coach")

## load queries
#queryFile <- "../../data/testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query

for (query in queries) {
	print(query)
	
	# replace whitespace with %20 in query
	q <- gsub(" ","+", query)
	
	# load in original category-based diversity data
	url <- paste("http://d-sjc-smohan1.corp.ebay.com/Test/ebutils/diversity_cats.pl?Query=",q,"&SortOrder=12&output=text&divcount=10",sep="")
	
	result <- try(data <- read.table(url, sep="\t", comment.char="", quote="", header=FALSE))
	if(class(result) == "try-error") next;
	
	# keep itemID, title and category information
	result <- try(data <- data[,c(1,2,3)])
	if(class(result) == "try-error") next;
	
	# process category information
	cat1 <- matrix(0, nrow=nrow(data), ncol=1)
	cat2 <- matrix(0, nrow=nrow(data), ncol=1)
	for (i in 1:nrow(data)) {
		cats <- strsplit(as.character(data[i,3]),",")[[1]]
		
		cat1[i] <- as.numeric(cats[1])
		cat2[i] <- 0
		if (length(cats) == 2) {
			cat2[i] <- as.numeric(cats[2])
		} 
	}
	catDivData <- data.frame(ItemID=data[,1], Title=data[,2],
			Cat1=cat1, Cat2=cat2)
	
	# save cat-div output data
	outputFile <- paste("../../data/catdiv/",query,"_catdiv.tsv",sep="")
	write.table(catDivData, file=outputFile, sep="\t", row.names = FALSE, quote=FALSE)
}
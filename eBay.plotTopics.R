# TODO: plot lda model for individual query
#
# Author: Jun Yu
# Version: July, 2012
##################################################################

rm(list=ls())

#setwd("/Users/junyu/Documents/workspace/eBay.R")
setwd("/Users/junyu/Documents/eBay/code/eBay.R")

library(reshape2)
library(ggplot2)

source("./eBay.Utility.R")

#######################
## experiment settings
#######################
queries <- c("hello kitty", "fossil", "fossils", "basketball", "keyboard", "coach", "iphone", "ipod")
Ks <- c(4)  # number of topics
ldaVersion <- "lda" 
N <- 5  # number of documents to show

args <- commandArgs(trailingOnly = TRUE)
ldaVersion <- as.character(args[2])
if (ldaVersion != "lda" && ldaVersion != "mblda") {
	stop("lda version is invalid.\n
					please run 'Rscript eBay.plotTopics.R -args lda' or \n
					'Rscript eBay.plotTopics.R -args mblda'")
} 

## load queries
#queryFile <- "../../data/testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query

for (query in queries) {
	print(query)
	
	# load data
	load(paste("../../data/train/",query,"_train.RData",sep=""))
	
	for (K in Ks) {
		# load in LDA model
		load(paste("../../data/model/lda/",query,"_K",K,"_",ldaVersion,".RData",sep=""))
		
		# compute topic proportion for each doc
		topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
		sample.docs <- 1:N #sample(1:length(lda.documents),N)
		topic.proportions <- topic.proportions[sample.docs,]
		topic.proportions[is.na(topic.proportions)] <-  1 / K
		
		# compute top words for each topic
		if (ldaVersion == "lda") {
			top.words <- lda.top.topic.words(result$topics, 10, by.score=TRUE)
		} else if (ldaVersion == "mblda") {
			top.words <- mblda.top.topic.words(result$topics_present, result$topics_absent, 10, by.score=TRUE)
		} else {
			stop("ldaVersion is invalid...")
		}
		
		# prepare data.frame for plot
		colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")
		topic.proportions.df <- melt(cbind(data.frame(topic.proportions),
						document=factor(1:N)),	#document=sample.docs.terms), #document=factor(1:N)),
				variable_name="topic",
				id.vars = "document")  
		names(topic.proportions.df) <- c("document", "topic", "value")
		
		# plot
		pdf(paste("../../result/plot/",query,"_K",K,"_",ldaVersion,".pdf",sep=""), height=6, width=8) 
		
		print(qplot(topic, value, fill=document, ylab="proportion", main = query,
						data=topic.proportions.df, geom="bar") +
				opts(axis.text.x = theme_text(angle=90, hjust=1)) +  
				coord_flip() +
				opts(axis.text.y=theme_text(size=12)) +
				facet_wrap(~ document, ncol=5))
		
		#this saves png in your current directory
		dev.off()
	} # k
} # query

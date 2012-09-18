# TODO: plot individual query
#
# Author: Jun Yu
# Version: July, 2012
##################################################################

rm(list=ls())

#setwd("/Users/junyu/Documents/workspace/eBay.R")
setwd("/Users/junyu/Documents/eBay/code/eBay.R")

library(ggplot2)

source("./eBay.Utility.R")

#######################
## experiment settings
#######################
Tdver <- "Cosine"
topKs <- 1:5

## load queries
#queryFile <- "../../data/testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query
	
# load default ranker
rankerFile <- paste("../../result/defaultranker_Cosine.tsv",sep="")
ranker <- read.table(rankerFile, sep="\t", head=TRUE)
ranker <- ranker[c(topKs,50),]
queries <- names(ranker)

# load MMR
CatDivFile <- paste("../../result/catdiv_Cosine.tsv",sep="")
CatDivResult <- read.table(CatDivFile, sep="\t", head=TRUE)
CatDivResult <- CatDivResult[topKs,queries]

# load MMR
MMRFile <- paste("../../result/mmr_Cosine.tsv",sep="")
MMRResult <- read.table(MMRFile, sep="\t", head=TRUE)
MMRResult <- MMRResult[topKs,queries]

# load LDA
LDAFile <- paste("../../result/lda_Cosine.tsv",sep="")
LDAResult <- read.table(LDAFile, sep="\t", head=TRUE)
LDAResult <- LDAResult[topKs,queries]

# load MBLDA
MBLDAFile <- paste("../../result/mblda_Cosine.tsv",sep="")
MBLDAResult <- read.table(MBLDAFile, sep="\t", head=TRUE)
MBLDAResult <- MBLDAResult[topKs,queries]

for (query in queries) {
    print(query)
	q <- gsub("\\."," ",query)
	
	df <- matrix(0,nrow=length(1:(5*length(topKs))), ncol=3)
	colnames(df) <- c("model","topK","measure")
	for (i in topKs) {
		df[(i-1)*5+1,] <- c("Default Ranker", i, ranker[i,query])
		df[(i-1)*5+2,] <- c("CatDiv", i, CatDivResult[i,query])
		df[(i-1)*5+3,] <- c("MMR", i, MMRResult[i,query])
		df[(i-1)*5+4,] <- c("LDA", i, LDAResult[i,query])
		df[(i-1)*5+5,] <- c("MBLDA", i, MBLDAResult[i,query])
	}
#	df <- rbind(df, c("Default Ranker", 50, ranker[11,query]))
	
	df <- as.data.frame(df)
	df$model <- factor(as.character(df$model), levels=c("Default Ranker", "CatDiv", "MMR", "LDA", "MBLDA"))
#	df$topK <- factor(as.character(df$topK), levels=c(1:10,50))
	df$topK <- factor(as.character(df$topK), levels=c(1:10))
	df$measure <- as.numeric(as.character(df$measure))
	
	# plot
	pdf(paste("../../result/plot/",q,".pdf",sep=""), height=6, width=8) 
	
	pd <- position_dodge(.2) # move them .05 to the left and right
	p <- ggplot(df, aes(x=topK, y=measure, group=model, colour=model, shape=model)) + 
			ylim(min(df$measure)-0.1,max(df$measure)+0.1) +
			geom_line(position=pd) +
			geom_point(position=pd, size=4, fill="white") + 
			xlab("top K") +
			ylab("Mean Satisfaction") +
#			scale_colour_hue(#name="Query Type", # Legend label, use darker colors
#					#labels=c("All", "Frequent", "Infrequent"),
#					l=40) +                  # Use darker colors, lightness=40
			opts(title=q, plot.title=theme_text(family="Times", face="bold", size=20)) +
			#theme_bw() +
			opts(legend.justification=c(1,0), legend.position=c(1,0)) # Position legend in bottom right
	print(p)
	
	#this saves png in your current directory
	dev.off()
}



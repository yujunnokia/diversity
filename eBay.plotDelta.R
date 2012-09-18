# TODO: Plot Delta mean satisfaction across all queries
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
queries <- c("hello kitty", "fossil", "fossils", "basketball", "keyboard", "coach", "iphone", "ipod")
topKs <- 1:5
models <- c("catdiv", "mmr", "lda", "mblda")
K <- 5 # number of topics for lda

## load queries
#queryFile <- "../../data/testQueries.tsv"
#queryData <- read.table(queryFile, head=TRUE, sep="\t", stringsAsFactors=FALSE)
#queries <- queryData$query

queries <- make.names(queries)

# load default ranker
rankerFile <- paste("../../result/defaultranker_Cosine.tsv",sep="")
ranker <- read.table(rankerFile, sep="\t", head=TRUE)
ranker <- ranker[topKs,queries]

# build df
df <- matrix(0,nrow=length(1:(length(topKs)*length(models))), ncol=4)
colnames(df) <- c("model","topK","measure","se")
for (i in 1:length(models)) {
	model <- models[i]
	
	# load model 
	modelFile <- paste("../../result/",model,"_Cosine.tsv",sep="")
	modelResult <- read.table(modelFile, sep="\t", head=TRUE)
	modelResult <- as.matrix(modelResult[,queries])
	
	# compute delta mean and se
	delta <- (modelResult - ranker) * 100 
	deltaMean <- apply(delta, 1, mean)
	deltaSE <- apply(delta, 1, function(x) sd(x)/sqrt(length(x)))
	
	df[((i-1)*length(topKs)+topKs),] <- cbind(rep(model,length(topKs)), topKs, deltaMean, deltaSE)
}
df <- as.data.frame(df)
df$topK <- as.numeric(as.character(df$topK))
df$measure <- as.numeric(as.character(df$measure))
df$se <- as.numeric(as.character(df$se))

## plot
pdf(paste("../../result/plot/allModel.pdf",sep=""), height=6, width=8) 

pd <- position_dodge(.2) # move them .05 to the left and right
p <- ggplot(df, aes(x=topK, y=measure, group=model, colour=model, shape=model)) + 
		#ylim(-0.07,0.13) +
		ylim(min(df$measure)-max(df$se)-2,max(df$measure)+max(df$se)) +
		geom_errorbar(aes(ymin=measure-se, ymax=measure+se), width=.5, position=pd) +
		geom_line(position=pd) +
		geom_point(position=pd, size=4, fill="white") + 
		xlab("top K") +
		ylab("Delta(Mean Satisfaction)") +
		scale_colour_hue(#name="Query Type", # Legend label, use darker colors
				#labels=c("All", "Frequent", "Infrequent"),
				l=40) +                  # Use darker colors, lightness=40
		opts(title="Evaluation across all queries", plot.title=theme_text(family="Times", face="bold", size=20)) +
		#theme_bw() +
		opts(legend.justification=c(1,0), legend.position=c(1,0)) # Position legend in bottom right
print(p)

#this saves png in your current directory
dev.off()

#!/usr/bin/env Rscript
library(stm) # Process data using func called text processor
library(tm)
library(quanteda)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(reshape2)
#library(gofastr)
library(tokenizers)
library(wordcloud)
library(optparse)

option_list <- list(
  make_option(c("-i", "--csv_file"), type = "character", default = NULL, metavar = "path", help = "CSV file with NYT cleaned data."),
  make_option(c("-t", "--titleordesc"), type = "character", default = "desc", metavar = "integer", help = "Desc or Title"),
  make_option(c("-k", "--numcluster"), type = "integer", default = "3", metavar = "integer", help = "Number of clusters"),
  make_option(c("-o", "--outdir"), type = "character", default = "./", metavar = "integer", help = "Output directory."),
  make_option(c("-c", "--cores"), type = "integer", default = 1, metavar = "integer", help = "Number of cores."))

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

#https://github.com/dondealban/learning-stm
#Use function called textprocessor to create obj called LargeTextProcessor
# Prep data
data <- read.csv(opt$csv_file,encoding = "UTF-8")
data$year1 <- as.character(data$items__volumeInfo__publishedDate)
#data$year2 <- as.character(data$year2)

stopwordsbestseller<- readLines("bestsellers.stopwords.txt", encoding = "UTF-8")
type = opt$titleordesc
if (type == "title"){
  corp = data$items__volumeInfo__title}

if (type == "desc"){
  corp = data$items__volumeInfo__description}

processed1 <- textProcessor(corp,metadata = data, stem=FALSE, customstopwords = stopwordsbestseller)
out1 <- prepDocuments(processed1$documents, processed1$vocab, processed1$meta)
docs <- out1$documents
vocab <- out1$vocab
meta <- out1$meta


descfit <- stm(out1$documents, out1$vocab, K=opt$numcluster, prevalence=~(year1),
                       max.em.its=75, data=out1$meta, init.type="Spectral",
                       seed=8458159)



plot.new()
jpeg(file = paste0(opt$texttype, "numberofclusters:", opt$numcluster, ".stmmeta.jpg"))
plot(descfit)
dev.off()
plot.new()

for (i in 1:opt$numcluster) {
  plot.new()
  jpeg(file = paste0(opt$texttype, "clusternum:", i, ".stmbyyear.jpg"))
  prep <- estimateEffect(1:(opt$numcluster)~(Published_Date), descfit, meta=out1$meta,
                         uncertainty="Global")
  yearseq <- seq(from=as.Date("2012-01-01"), to=as.Date("2022-01-01"), by="year")
  yearnames <- format(as.Date(yearseq, format="%d/%m/%Y"),"%Y")
  par(bty="n",lwd=2,xaxt="n")  # Get rid of the box around the plot, make the lines thicker,
  # and tell R to get rid of the x axis.
  plot.estimateEffect(prep, covariate = "year", model=descfit,topics=i,
                      method="continuous", xlab = "Year", ylab = "Expected Proportions",
                      main = paste0("Topic", i) ,
                      printlegend=FALSE, xaxt="n", xlim=c(1978,2021))
  abline(h=0,lty=4,lwd=1,col="grey45")  # Put a dotted line on the y axis at 0.
  abline(v=yearnames,lty=2,lwd=1,col="grey45")  # Put dotted lines
  par(xaxt="s")
  axis(1,at=yearnames,labels=yearnames,las=2)
  dev.off()
}




dev.off()
graphics.off()
plot.new()
jpeg(file = paste0(opt$texttype, "numberofclusters:", opt$numcluster, ".topiccorr.jpg"))
plot(topicCorr(descfit))
dev.off()

for (i in 1:opt$numcluster) {
  plot.new()
  jpeg(file = paste0(opt$texttype, "clusternum:", i, ".wordcloud.jpg"))
  cloud(descfit, topic=i)
  dev.off()
  }


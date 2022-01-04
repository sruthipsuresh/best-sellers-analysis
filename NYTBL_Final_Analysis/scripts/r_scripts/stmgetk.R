library(stm) # Process data using func called text processor
library(tm)
library(quanteda)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gofastr)
library(tokenizers)
#library(igraph)
#library(wordcloud)
library(optparse)
library(stringr)

option_list <- list(
  make_option(c("-i", "--csv_file"), type = "character", default = NULL, metavar = "path", help = "CSV file with NYT cleaned data."),
  make_option(c("-o", "--outdir"), type = "character", default = "/home/sruthipsuresh/nytanalysis/best-sellers-analysis/NYTBL_Final_Analysis/results/database1", metavar = "character", help = "Output directory."),
  make_option(c("-c", "--cores"), type = "integer", default = 1, metavar = "integer", help = "Number of cores."))

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

data <- read.csv(opt$csv_file,encoding = "UTF-8")
data$year1 <- as.character(data$year1)
data$year2 <- as.character(data$year2)

stopwordsbestseller<- readLines("/home/sruthipsuresh/nytanalysis/best-sellers-analysis/NYTBL_Final_Analysis/scripts/r_scripts/bestsellers.stopwords.txt", encoding = "UTF-8")

processed1 <- textProcessor(data$results__books__description,metadata = data, stem=FALSE, customstopwords = stopwordsbestseller)
out1 <- prepDocuments(processed1$documents, processed1$vocab, processed1$meta)
docs <- out1$documents
vocab <- out1$vocab
meta <- out1$meta
findingk <- searchK(out1$documents, out1$vocab, data = out1$meta, prevalence = ~(year1), K = c(3:10), seed = 12345)
jpeg(file = paste0("description", ".year.searchk.jpg"))

plot(findingk)
dev.off()
plot.new()

findingk <- searchK(out1$documents, out1$vocab, K = c(3:10),
                    data = meta, verbose=FALSE)
jpeg(file = paste0("description", ".nometa.searchk.jpg"))

plot(findingk)
dev.off()


processed1 <- textProcessor(data$results__books__title,metadata = data, stem=FALSE, customstopwords = stopwordsbestseller)
out1 <- prepDocuments(processed1$documents, processed1$vocab, processed1$meta)
docs <- out1$documents
vocab <- out1$vocab
meta <- out1$meta


findingk <- searchK(out1$documents, out1$vocab, data = out1$meta, prevalence = ~(year1), K = c(3:10), seed = 12345)
jpeg(file = paste0("title", ".year.searchk.jpg"))

plot(findingk)
dev.off()
plot.new()

findingk <- searchK(out1$documents, out1$vocab, K = c(2:10),
                    data = meta, verbose=FALSE)
jpeg(file = paste0("title", ".nometa.searchk.jpg"))

plot(findingk)
dev.off()


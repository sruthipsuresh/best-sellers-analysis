#!/usr/bin/env Rscript

library(stm)
library(tm)
library(quanteda)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(reshape2)
#library(gofastr)
library(tokenizers)
library(optparse)

option_list <- list(
  make_option(c("-i", "--csv_file"), type = "character", default = NULL, metavar = "path", help = "CSV file with NYT cleaned data."),
  make_option(c("-t", "--titleordesc"), type = "character", default = "title", metavar = "integer", help = "Desc or Title"),
  make_option(c("-m", "--thresh"), type = "integer", default = ".01", metavar = "integer", help = "thresh"),
  make_option(c("-n", "--ngram"), type = "integer", default = "1", metavar = "integer", help = "Ngram number"),
  make_option(c("-k", "--numcluster"), type = "integer", default = "3", metavar = "integer", help = "Number of clusters"),
  make_option(c("-o", "--outdir"), type = "character", default = "./", metavar = "integer", help = "Output directory."),
  make_option(c("-c", "--cores"), type = "integer", default = 1, metavar = "integer", help = "Number of cores."))

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Prep data
data <- read.csv(opt$csv_file,encoding = "UTF-8")
title <- data["items__volumeInfo__title"]
desc <- data["items__volumeInfo__description"]
names(title)[1] <- "text"
title$doc_id <- 1:nrow(title)
names(desc)[1] <- "text"
desc$doc_id <- 1:nrow(desc)

# Get title and desc
# Make title tokens
type = opt$titleordesc
if (type == "title"){
  corp = title}

if (type == "desc"){
  corp = desc}

title_toks <- tokens(corpus(Corpus(DataframeSource(corp))), remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)

#Remove bestseller unique words
stopwordsbestseller<- readLines("bestsellers.stopwords.txt", encoding = "UTF-8")
print("stop")


#Remove standard stop words
title_toks <- tokens_remove(title_toks, pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst"))
title_toks <- tokens_remove(title_toks, pattern = stopwordsbestseller)
# Generate ngrams from 1:4
title_toks <- tokens_ngrams(title_toks, n = opt$ngram)

dfm_title <- dfm(title_toks) %>%
  dfm_trim(termfreq_type = "quantile",
           docfreq_type = "prop")
#min_docfreq = 0.1
processeddesc <- quanteda::convert(dfm_title, to = "stm")

#, lower.thresh = 1
outdesc <- prepDocuments(processeddesc$documents, processeddesc$vocab, processeddesc$meta, lower.thresh = opt$thresh)
print("prep")
#WITHOUT META
nometadesc <- stm(documents = outdesc$documents,
                  vocab = outdesc$vocab,
                  K = opt$numcluster,
                  max.em.its = 500,
                  data = outdesc$meta,
                  init.type = "Spectral")

jpeg(file = paste0(opt$titleordesc, "ngram:", opt$ngram, "cluster:", opt$numcluster, ".stmnometa.jpg"), res=150, width = 1000, height = 1000)

plot(nometadesc)

dev.off()


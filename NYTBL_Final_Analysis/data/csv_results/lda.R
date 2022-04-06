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
library(seededlda)
library(optparse)

option_list <- list(
  make_option(c("-i", "--csv_file"), type = "character", default = "/home/sruthipsuresh/nytanalysis/best-sellers-analysis/NYTBL_Final_Analysis/data/csv_results/database1/data/gone.csv", metavar = "path", help = "CSV file with NYT cleaned data."),
  make_option(c("-t", "--titleordesc"), type = "character", default = "desc", metavar = "integer", help = "Desc or Title"),
  make_option(c("-n", "--ngram"), type = "integer", default = "1", metavar = "integer", help = "Ngram number"),
  make_option(c("-k", "--minfreq"), type = "integer", default = ".05", metavar = "integer", help = "Minfreq"),
  make_option(c("-l", "--numcluster"), type = "integer", default = "3", metavar = "integer", help = "Number of clusters"),
  make_option(c("-o", "--outdir"), type = "character", default = "./home/sruthipsuresh/nytanalysis/best-sellers-analysis/NYTBL_Final_Analysis/results/database1", metavar = "character", help = "Output directory."),
  make_option(c("-c", "--cores"), type = "integer", default = 1, metavar = "integer", help = "Number of cores."))

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

#Prep data
data <- read.csv(opt$csv_file,encoding = "UTF-8")
data <- unique(data, by = "items__volumeInfo__title")
data <- unique(data, by = "items__volumeInfo__industryIdentifiers__identifier")

title <- data["items__volumeInfo__title"]
desc <- data["items__volumeInfo__description"]
# Prepare for dfm and each document one row
names(title)[1] <- "text"
title$doc_id <- 1:nrow(title)
names(desc)[1] <- "text"
desc$doc_id <- 1:nrow(desc)
print("done proc")
type = opt$titleordesc
if (type == "title"){
  corp = title}

if (type == "desc"){
  corp = desc}
# Make title tokens
title_toks <- tokens(corpus(Corpus(DataframeSource(corp))), remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)

#Remove bestseller unique words
stopwordsbestseller<- readLines("bestsellers.stopwords.txt", encoding = "UTF-8")
print("unique")


#Remove standard stop words
title_toks <- tokens_remove(title_toks, pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst"))
title_toks <- tokens_remove(title_toks, pattern = stopwordsbestseller)
# Generate ngrams from 2
title_toks <- tokens_ngrams(title_toks, n = opt$ngram)

dfm_title <- dfm(title_toks) %>%
  dfm_trim(termfreq_type = "quantile",
           min_docfreq = opt$minfreq, docfreq_type = "prop")
print("dfm")

title_lda_topic_model <- textmodel_lda(dfm_title, k = opt$numcluster)
title_lda_topics <- terms(title_lda_topic_model,opt$numcluster)
title_lda_topic_model$topic <- topics(title_lda_topic_model)
tableop <- table(title_lda_topic_model$topic)
write.table(tableop, file = paste0(opt$titleordesc, "ngram:", opt$ngram, "cluster:", opt$numcluster, ".ldatopics.txt"))

dtmtitle <- convert(dfm_title, to = "topicmodels")
print("lmao table")

raw.sum=apply(dtmtitle,1,FUN=sum) #sum by raw each raw of the table
dtmtitle=dtmtitle[raw.sum!=0,]
print("erm plot")
plot.new()

jpeg(file = paste0(opt$titleordesc, "ngram:", opt$ngram, "cluster:", opt$numcluster, ".ldaclusters.jpg"))

title_lda_topic_model<-LDA(dtmtitle, k=opt$numcluster)

title_lda_topics <- tidy(title_lda_topic_model, matrix = "beta")
lda_title_top_terms <-
  title_lda_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


lda_title_top_terms %>%
  dplyr::mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



dev.off()

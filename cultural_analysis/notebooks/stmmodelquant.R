library(stm)
library(tm)
library(quanteda)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gofastr)
library(tokenizers)
# Prep data
data <- read.csv("specific_books_ver3_final_full.csv",encoding = "UTF-8")
title <- data["items__volumeInfo__title"]
desc <- data["items__volumeInfo__description"]
names(title)[1] <- "text"
title$doc_id <- 1:nrow(title)
names(desc)[1] <- "text"
desc$doc_id <- 1:nrow(desc)

# Get title and desc
# Make title tokens
title_toks <- tokens(corpus(Corpus(DataframeSource(desc))), remove_punct = TRUE, remove_numbers = TRUE, remove_symbol = TRUE)

#Remove bestseller unique words
stopwordsbestseller<- readLines("bestsellers.stopwords.txt", encoding = "UTF-8")



#Remove standard stop words
title_toks <- tokens_remove(title_toks, pattern = c(stopwords("en"), "*-time", "updated-*", "gmt", "bst"))
title_toks <- tokens_remove(title_toks, pattern = stopwordsbestseller)
# Generate ngrams from 1:4
title_toks <- tokens_ngrams(title_toks, n = 1:4)

dfm_title <- dfm(title_toks) %>%
  dfm_trim(termfreq_type = "quantile",
           min_docfreq = 0.1, docfreq_type = "prop")

processeddesc <- quanteda::convert(dfm_title, to = "stm")


outdesc <- prepDocuments(processeddesc$documents, processeddesc$vocab, processeddesc$meta, lower.thresh = 1)

#WITHOUT META
nometadesc <- stm(documents = outdesc$documents,
                             vocab = outdesc$vocab,
                             K = 7,
                             max.em.its = 500,
                             data = outdesc$meta,
                             init.type = "Spectral")

plot(nometadesc)


# WITH METADATA

stmdescmeta <- stm(documents = outdesc$documents,
                               vocab = outdesc$vocab,
                               K = 10,
                               prevalence =~ s(year),
                               max.em.its = 500,
                               data = outdesc$meta,
                               init.type = "Spectral",
                               verbose=FALSE)
plot(stmdescmeta)

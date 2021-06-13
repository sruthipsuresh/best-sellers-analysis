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
library(quanteda.corpora)
library(seededlda)

#Prep data
data <- read.csv("specific_books_ver3_final_full.csv",encoding = "UTF-8")
title <- data["items__volumeInfo__title"]
desc <- data["items__volumeInfo__description"]
# Prepare for dfm and each document one row
names(title)[1] <- "text"
title$doc_id <- 1:nrow(title)
names(desc)[1] <- "text"
desc$doc_id <- 1:nrow(desc)

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
           min_docfreq = 0.01, docfreq_type = "prop")



title_lda_topic_model <- textmodel_lda(dfm_title, k = 7)
title_lda_topics <- terms(title_lda_topic_model ,5)
title_lda_topic_model$topic <- topics(title_lda_topic_model)
tableop <- table(title_lda_topic_model$topic)
write.table(tableop, "ldatopicsbydoc.txt")

dtmtitle <- convert(dfm_title, to = "topicmodels")


raw.sum=apply(dtmtitle,1,FUN=sum) #sum by raw each raw of the table
dtmtitle=dtmtitle[raw.sum!=0,]


title_lda_topic_model<-LDA(dtmtitle, k=7)


title_lda_topics <- tidy(title_lda_topic_model, matrix = "beta")
lda_title_top_terms <-
  title_lda_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


lda_title_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()




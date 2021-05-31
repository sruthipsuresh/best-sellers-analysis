library(stm)
library(tm)
library(quanteda)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gofastr)

data <- read.csv("pc_terms_title_clean.csv",encoding = "UTF-8")
title <- data["items__volumeInfo__title"]
desc <- data["items__volumeInfo__description"]
names(title)[1] <- "text"
title$doc_id <- 1:nrow(title)
names(desc)[1] <- "text"
desc$doc_id <- 1:nrow(desc)


dtmtitle <- DocumentTermMatrix(Corpus(DataframeSource(title)), control = list(removeNumbers = TRUE, stopwords = TRUE, stemming = FALSE, stripWhitespace = TRUE,
                                                                              removePunctuation = TRUE,
                                                                              removeSeparators = TRUE, minWordLength = 3, removeWords = stopwords("en", source = "smart")))
dtmtitle = removeSparseTerms(dtmtitle, 0.99) # AT least 1% of documents

raw.sum=apply(dtmtitle,1,FUN=sum) #sum by raw each raw of the table
dtmtitle=dtmtitle[raw.sum!=0,]


title_lda_topic_model<-LDA(dtmtitle, k=10)


title_lda_topics <- tidy(title_lda_topic_model, matrix = "beta")
lda_title_top_terms <-
  title_lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


lda_title_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()




dtmdesc <- DocumentTermMatrix(Corpus(DataframeSource(desc)), control = list(removeNumbers = TRUE, stopwords = TRUE, stemming = FALSE, stripWhitespace = TRUE,
                                                                              removePunctuation = TRUE,
                                                                              removeSeparators = TRUE, minWordLength = 3, removeWords = stopwords("en", source = "smart")))
dtmdesc = removeSparseTerms(dtmdesc, 0.90) # AT least 10% of documents

raw.sum=apply(dtmdesc,1,FUN=sum) #sum by raw each raw of the table
dtmdesc=dtmdesc[raw.sum!=0,]


desc_lda_topic_model<-LDA(dtmdesc, k=10)

desc_lda_topics <- tidy(desc_lda_topic_model, matrix = "beta")
lda_desc_top_terms <-
  desc_lda_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


lda_desc_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



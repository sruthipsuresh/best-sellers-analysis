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

title <- as.data.frame(t(title))
desc <- as.data.frame(t(desc))
processeddesc <- textProcessor(desc, metadata = data, removestopwords = TRUE,
                                removenumbers = TRUE,
                                removepunctuation = TRUE,
                                stem = FALSE,
                                wordLengths = c(3,Inf),
                                sparselevel = 1,
                                language = "en",
                                verbose = TRUE,
                                onlycharacter = TRUE)
outdesc <- prepDocuments(processeddesc$documents, processeddesc$vocab, processeddesc$meta, lower.thresh = 10)

#WITHOUT META
nometadesc <- stm(documents = outdesc$documents,
                             vocab = outdesc$vocab,
                             K = 10,
                             max.em.its = 500,
                             data = outdesc$meta,
                             init.type = "Spectral")

plot(nometadesc)


# WITH METADATA

stmdescmeta <- stm(documents = outdesc$documents,
                               vocab = outdesc$vocab,
                               K = 10,
                               prevalence =~ outdesc$meta,
                               max.em.its = 500,
                               data = outdesc$meta,
                               init.type = "Spectral",
                               verbose=FALSE)

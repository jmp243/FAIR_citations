# Jung Mee Park
# Wichita State University
# 2026-03-06

# open alex api
# install.packages("openalexR")
remotes::install_github("ropensci/openalexR")

# INstall packages
library(usethis)
library(openalexR)
library(tidyverse)

paper_data <- oa_fetch(
  identifier = "W2302501749", # OpenAlex ID for the orignal paper is W2302501749
  entity = "works",
  verbose = TRUE
)

# View the data structure
dplyr::glimpse(paper_data)

###### do not share #################################
usethis::edit_r_environ()

options(openalexR.apikey = "")

Sys.getenv("OPENALEX_API_KEY")
#####################################################

citing_works <- oa_fetch(
  entity = "works",
  cites = "W2302501749"
)

library(readr)

citing_works <- read_csv("works-csv-3PQvKsrgTre7x5jHfqjVq3.csv")

library(tidytext)
library(textmineR)
library(stopwords)

# Subset abstract data
abstract_data <- citing_works$abstract

dtm <- CreateDtm(doc_vec = abstract_data,
                 doc_names = citing_works$id,
                 ngram_window = c(1, 2), # Using unigrams and bigrams
                 stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart")),
                 lower = TRUE,
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 verbose = FALSE,
                 cpus = 1)

# identify the term frequency-inverse docuemnt frequency
tf_mat <- TermDocFreq(dtm)

tfidf_mat <- as.matrix(tf_mat$idf)

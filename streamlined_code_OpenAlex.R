# Wichita State University
# 2026-03-06
# last update 2026-04-23


# load packages
library(usethis)
library(openalexR)
library(tidyverse)
library(readr)
library(tidytext)
library(textstem)
library(textmineR)
library(stopwords)
library(dplyr)
library(tidyr)
library(stringr)
library(dplyr)



#### authoritative data source ####
# read in CSV of the works citing FAIR2016
citing_works <- read_csv("ten_yr_openalex_citation_corpus_2026-03-16.csv") # define to 3/15/2026

# rename display_name as title
citing_works$title <- citing_works$display_name

# remove http and other signifiers to make doi's compatible
citing_works <- citing_works %>% 
  mutate(doi_clean = str_remove(doi, "https://doi.org/"))

# unique doi
alex_doi <- citing_works %>% 
  filter(!is.na(doi) & doi != "") %>% 
  unique()

# filter out works that were published before 2015
alex_doi_new <- alex_doi %>% 
  filter(publication_year > 2015)

dups <- alex_doi_new %>%
  group_by(title) %>%
  filter(n() > 1) %>%
  ungroup()

table(dups$type)

dups_api <- alex_doi %>%
  group_by(title) %>%
  filter(n() > 1) %>%
  mutate(
    has_accepted = any(version == "acceptedVersion"),
    has_cites = any(cited_by_count > 0)
  ) %>%
  filter(has_accepted, has_cites) %>%
  ungroup()

## anti-join take out the dupes that are not the acceptedVersion
anti_dups <- anti_join(dups_api, dups)

# remove the anti_dups from the larger dataset alex_doi_new
alex_doi_new <- alex_doi_new %>% anti_join(anti_dups, by = "id")

#### Section: Country Analysis using csv ####
### parse out authorship countries and split the columns
# unique row idenifier
alex_doi_new <- alex_doi_new  %>%
  mutate(.row_id = row_number())

country_wide <- alex_doi_new %>%
  # Keep only the identifier and the countries column
  select(.row_id, authorships.countries) %>%
  # Split rows on "|" and clean
  separate_rows(authorships.countries, sep = "\\|") %>%
  mutate(
    country = str_trim(authorships.countries),
    country = toupper(country),         # normalize to upper case
    country = na_if(country, "")        # drop empties
  ) %>%
  filter(!is.na(country)) %>%
  distinct(.row_id, country) %>%        # avoid duplicate country per citing work
  mutate(has_country = 1L) %>%          # indicator column
  pivot_wider(
    names_from = country,
    values_from = has_country,
    values_fill = 0
  )

library(countrycode)
# create a country code dictionary
country_codes <- names(country_wide)

# remove non‑country columns like ".row_id"
country_codes <- country_codes[country_codes != ".row_id"]

# create a country code dictionary using  ISO‑3166‑1 country code 
country_dict <- data.frame(
  code = country_codes,
  country = countrycode(country_codes, origin = "iso2c", destination = "country.name")
)

country_dict$country[country_dict$code == "XK"] <- "Kosovo"

# Join back to original if you want the rest of the columns:
alex_doi_new_wide <- alex_doi_new %>%
  left_join(country_wide, by = ".row_id") %>%
  select(-.row_id)

# fixing years on publication dates
library(lubridate)
alex_doi_new_wide$publication_date <- as.Date(alex_doi_new_wide$publication_date,
                                              format = "%m/%d/%Y")

alex_doi_new_wide$publication_year <- as.integer(alex_doi_new_wide$publication_year)

# -----------------------------------------------------------------------------
#### Section 1: Build topic data frame ####
# -----------------------------------------------------------------------------

alex_doi_topic <- alex_doi_new %>%
  filter(
    publication_date >= as.Date("2016-03-15"),
    publication_date <= as.Date("2026-03-16"),
    doi_clean != "10.1038/sdata.2016.18"
  ) %>%
  mutate(
    journal_name = primary_location.source.display_name,
    topic        = primary_topic.display_name
  )

# -----------------------------------------------------------------------------
#### Section 4: Domain stopwords #### 
# (defined once)
# -----------------------------------------------------------------------------

domain_stopwords <- c(
  "study", "analysis", "research", "approach",
  "method", "methods", "using", "based",
  "advanced", "advances"
)

# -----------------------------------------------------------------------------
#### Section 5: Lemmatize and build DTM ####
# -----------------------------------------------------------------------------

topic_data_alex_lemma <- lemmatize_strings(topic_data_alex)

dtm_topic <- CreateDtm(
  doc_vec      = topic_data_alex_lemma,
  doc_names    = alex_doi_topic$doi_clean,
  ngram_window = c(1, 2),
  stopword_vec = c(
    stopwords::stopwords("en"),
    stopwords::stopwords(source = "smart"),
    domain_stopwords
  ),
  lower             = TRUE,
  remove_punctuation = TRUE,
  remove_numbers    = TRUE,
  verbose           = FALSE,
  cpus              = 1
)


# -----------------------------------------------------------------------------
#### Section 6: TermDocFreq #### 
# stored as tf_mat to avoid shadowing the function
# -----------------------------------------------------------------------------

tf_mat_topic <- TermDocFreq(dtm_topic)


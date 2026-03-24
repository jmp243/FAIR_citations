# Jung Mee Park
# Wichita State University
# 2026-03-06
# last update 2026-03-13

# open alex api
# install.packages("openalexR")
remotes::install_github("ropensci/openalexR")

# load packages
library(usethis)
library(openalexR)
library(tidyverse)
library(readr)
library(tidytext)
library(textmineR)
library(stopwords)
library(dplyr)
library(tidyr)
library(stringr)

# fetch paper info
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

citing_works_api <- oa_fetch(
  entity = "works",
  cites = "W2302501749"
) #9:56 to 


# identify the unique doi
alex_doi_api <- citing_works_api %>% 
  filter(!is.na(doi) & doi != "") %>% 
  unique()

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

# # check for preprint redundancies 
### but preprints are getting citations
# subset works with the exact same titles
dups <- alex_doi_new %>% 
  group_by(title) %>% 
  filter(n() > 1) %>% 
  ungroup()

table(dups$type)

# dups_version <- dups %>% 
#   filter(version == "acceptedVersion")

dups_api <- alex_doi_api %>% 
  group_by(title) %>% 
  filter(n() > 1) %>% 
  mutate(
    has_accepted = any(version == "acceptedVersion"),
    has_cites = any(cited_by_count > 0)
  ) %>% 
  filter(has_accepted, has_cites) %>% 
  ungroup()

# dups <- alex_doi_api %>% 
#   group_by(title) %>% 
#   filter(n() > 1) %>% 
#   filter(!is.na(title), title != "") %>% 
#   filter(!is.na(abstract), abstract != "") %>% 
#   mutate(has_accepted = any(version == "acceptedVersion")) %>% 
#   filter(!has_accepted | version == "acceptedVersion") %>% 
#   ungroup()

# dups_no_abstract <- dups %>% 
#   group_by(title) %>% 
#   filter(n() > 1) %>% 
#   ungroup() %>% 
#   filter(!is.na(abstract), abstract != "")

#### Section: Country of Origin #### 
## distribution of citations by author country of origin 
## (insight into global reach)

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

# citations by year
library(lubridate)
alex_doi_new_wide$publication_date <- as.Date(alex_doi_new_wide$publication_date,
                                         format = "%m/%d/%Y")

alex_doi_new_wide$publication_year <- as.integer(alex_doi_new_wide$publication_year)

# graph 
citations_by_year_line <- alex_doi_new_wide %>% 
  mutate(publication_year = as.integer(publication_year)) %>% 
  group_by(publication_year) %>% 
  filter(publication_year > 2015) %>% 
  summarise(doi_count = n_distinct(doi_clean)) %>% 
  ggplot(aes(x = publication_year, y = doi_count)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(
    title = "Unique DOIs by Publication Year",
    x = "Publication Year",
    y = "Number of Unique DOIs"
  ) +
  theme_minimal()

citations_by_year_line

library(RColorBrewer)
# type of article (primary article, review, editorial, etc)

citations_by_bar <- alex_doi_new_wide %>% 
  mutate(publication_year = as.integer(publication_year)) %>% 
  filter(publication_year > 2015) %>% 
  group_by(publication_year, type) %>% 
  summarise(doi_count = n_distinct(doi_clean), .groups = "drop") %>% 
  ggplot(aes(x = factor(publication_year), y = doi_count, fill = type)) +
  geom_col(position = "stack") +
  # scale_color_grey()+
  # scale_fill_brewer(palette = "PuOr") +
  labs(
    title = "Unique DOIs by Publication Year and Type",
    x = "Publication Year",
    y = "Number of Unique DOIs"
  ) +
  theme_minimal()

citations_by_bar

# build an interactive version with plotly
library(dplyr)
library(plotly)
library(RColorBrewer)

df <- alex_doi_new_wide %>% 
  mutate(publication_year = as.integer(publication_year)) %>% 
  filter(publication_year > 2015) %>% 
  group_by(publication_year, type) %>% 
  summarise(doi_count = n_distinct(doi_clean), .groups = "drop") %>% 
  group_by(publication_year) %>% 
  mutate(year_total = sum(doi_count)) %>% 
  ungroup()

# Choose a palette strategy (pick ONE of the following)
# A) Larger Brewer (up to 12)
# pal <- brewer.pal(max(3, min(12, dplyr::n_distinct(df$type))), "Set3")

# B) Viridis discrete (scales as needed)
# In ggplot use scale_fill_viridis_d; for plotly, build a manual vector:
type_levels <- sort(unique(df$type))
pal <- viridis::viridis(length(type_levels), option = "plasma")
type_colors <- setNames(pal, type_levels)

p <- ggplot(df, aes(
  x = factor(publication_year), y = doi_count, fill = type,
  text = paste0(
    "Year: ", publication_year, "\n",
    "Type: ", type, "\n",
    "Unique DOIs: ", doi_count, "\n",
    "Year total: ", year_total
  )
)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = type_colors) +   # swap for your chosen strategy
  labs(
    title = "Unique DOIs by Publication Year and Type",
    x = "Publication Year", y = "Number of Unique DOIs", fill = "Article Type"
  ) +
  theme_minimal()

ggplotly(p, tooltip = "text")

#### Section: Distribution of Primary Topics ####
# distribution of citations by primary topic and journal name (insight into disciplines)
alex_doi_topic <- alex_doi_new %>% 
  filter(publication_date >= as.Date("2016-03-15"),
  publication_date <= as.Date("2026-03-15")) %>%
  filter(doi_clean!= "10.1038/sdata.2016.18") %>% 
  mutate(journal_name = primary_location.source.display_name) %>% 
  mutate(topic = primary_topic.display_name)

table(alex_doi_topic$topic)

# subset topic words
topic_data_alex <- alex_doi_topic$topic

# domain-specific academic stopwords
domain_stopwords <- c(
  "study", "analysis", "research", "approach",
  "method", "methods", "using", "based",
  "advanced", "advances"
)

dtm_topic <- CreateDtm(
  doc_vec   = topic_data_alex,
  doc_names = alex_doi_topic$doi_clean,
  ngram_window = c(1, 2),
  stopword_vec = c(
    stopwords::stopwords("en"),
    stopwords::stopwords(source = "smart"),
    domain_stopwords          # add domain stopwords
  ),
  lower = TRUE,
  remove_punctuation = TRUE,
  remove_numbers = TRUE,
  verbose = FALSE,
  cpus = 1
)

# TF–IDF-style output
tf_mat_topic <- TermDocFreq(dtm_topic)

# Subset abstract data
topic_data_alex <- alex_doi_topic$topic

# domain-specific academic stopwords
domain_stopwords <- c(
  "study", "analysis", "research", "approach",
  "method", "methods", "using", "based",
  "advanced", "advances"
)

dtm_topic <- CreateDtm(
  doc_vec   = topic_data_alex,
  doc_names = alex_doi_topic$doi_clean,
  ngram_window = c(1, 2),
  stopword_vec = c(
    stopwords::stopwords("en"),
    stopwords::stopwords(source = "smart"),
    domain_stopwords          # ✅ add them here
  ),
  lower = TRUE,
  remove_punctuation = TRUE,
  remove_numbers = TRUE,
  verbose = FALSE,
  cpus = 1
)

# TF–IDF-style output
tf_mat_topic <- TermDocFreq(dtm_topic)

# tfidf_mat_topic <- as.matrix(tf_mat_topic$idf)

# how do I trim this further
tf_mat_topic

str(tf_mat_topic)

n_docs <- length(unique(alex_doi_topic$doi_clean))

tf_mat_topic_trimmed <- tf_mat_topic %>%
  dplyr::filter(
    doc_freq >= 2,
    doc_freq <= 0.5 * n_docs
  )
# n_docs <- length(unique(alex_doi_topic$doi_clean))

tf_mat_topic_trimmed <- tf_mat_topic %>%
  dplyr::filter(
    doc_freq >= 2,
    doc_freq <= 0.5 * n_docs
  )
tf_mat_topic_trimmed

# trim academic stopwords
domain_stopwords <- c(
  "study", "analysis", "research", "approach",
  "method", "methods", "using", "based", "advanced", "advances"
)

tf_mat_topic_trimmed <- tf_mat_topic %>%
  dplyr::filter(!term %in% domain_stopwords)

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

# identify the term frequency-inverse document frequency
tf_mat <- TermDocFreq(dtm)

tfidf_mat <- as.matrix(tf_mat$idf)

## remove like words
library(textstem)

topic_data_alex_lemma <- lemmatize_strings(topic_data_alex)

######### dimensions data ###############
Dimensions_Publication_2026_a <- read_csv("dimensions_data/Dimensions-Publication-2026-03-06_19-01-28.csv", skip = 1)
Dimensions_Publication_2026_b <- read_csv("dimensions_data/Dimensions-Publication-2026-03-06_19-02-00.csv", skip = 1)
Dimensions_Publication_2026_c <- read_csv("dimensions_data/Dimensions-Publication-2026-03-06_19-02-32.csv", skip = 1)
Dimensions_Publication_2026_d <- read_csv("dimensions_data/Dimensions-Publication-2026-03-06_19-02-49.csv", skip = 1)

Dimensions_data <- bind_rows(Dimensions_Publication_2026_a, Dimensions_Publication_2026_b, 
                         Dimensions_Publication_2026_c, Dimensions_Publication_2026_d)

# pub id unique
dimensions_unique_doi <- Dimensions_data %>% 
  filter(!is.na(DOI) & DOI != "") %>% unique()

# new dataset keeping only those citations with doi
# dimensions_pub_id <- Dimensions_data %>% 
#   filter(!is.na(`Publication ID`) & `Publication ID` != "") %>% 
#   unique()


# Subset abstract data
abstract_data_dim <- dimensions_unique_doi$Abstract

dtm_dim <- CreateDtm(doc_vec = abstract_data_dim,
                 doc_names = dimensions_unique_doi$DOI,
                 ngram_window = c(1, 2), # Using unigrams and bigrams
                 stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart")),
                 lower = TRUE,
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 verbose = FALSE,
                 cpus = 1)

# identify the term frequency-inverse document frequency
tf_mat_dim <- TermDocFreq(dtm_dim)

tfidf_mat_dim <- as.matrix(tf_mat_dim$idf)


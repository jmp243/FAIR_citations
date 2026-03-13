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

Sys.getenv("OPENALEX_API_KEY") # half the time with Sam's computer
#####################################################

citing_works_api <- oa_fetch(
  entity = "works",
  cites = "W2302501749"
) # run from 11:30 


# identify the unique doi
alex_doi_api <- citing_works_api %>% 
  filter(!is.na(doi) & doi != "") %>% 
  unique()

# read in CSV of the works citing FAIR2016
citing_works <- read_csv("works-csv-March11.csv") # define to 3/15/2026

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


## distribution of citations by author country of origin 
## (insight into global reach)
### parse out authorship countries and split the columns


# If you *don't* have a stable unique per-row id, create one:
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

citations_by_bar <- alex_doi_new_wide %>% 
  mutate(publication_year = as.integer(publication_year)) %>% 
  filter(publication_year > 2015) %>% 
  group_by(publication_year, type) %>% 
  summarise(doi_count = n_distinct(doi_clean), .groups = "drop") %>% 
  ggplot(aes(x = factor(publication_year), y = doi_count, fill = type)) +
  geom_col(position = "stack") +
  # scale_color_grey()+
  # scale_color_brewer(palette = "PuOr") +
  labs(
    title = "Unique DOIs by Publication Year and Type",
    x = "Publication Year",
    y = "Number of Unique DOIs"
  ) +
  theme_minimal()

citations_by_bar
# type of article (primary article, review, editorial, etc)

# distribution of citations by primary topic and journal name (insight into disciplines)


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


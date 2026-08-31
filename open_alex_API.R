# Wichita State University
# 2026-03-06
# last run 2026-08-31

# open alex api
# install.packages("openalexR")
# remotes::install_github("ropensci/openalexR")

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

# # fetch paper info
# paper_data <- oa_fetch(
#   identifier = "W2302501749", # OpenAlex ID for the orignal paper is W2302501749
#   entity = "works",
#   verbose = TRUE
# )

# View the data structure
# dplyr::glimpse(paper_data)

###### do not share #################################
# usethis::edit_r_environ()
# 
# options(openalexR.apikey = "")
# 
# Sys.getenv("OPENALEX_API_KEY") 
#####################################################
# 
# citing_works_api <- oa_fetch(
#   entity = "works",
#   cites = "W2302501749"
# ) #9:20am to 9:34am
# 
# ## rename
# # identify the unique doi which reduces the number of citations from the API call
# alex_doi_api <- citing_works_api %>% 
#   filter(!is.na(doi) & doi != "") %>% 
#   unique()

#### authoritative data source ####
# read in CSV of the works citing FAIR2016
citing_works <- read_csv("ten_yr_openalex_citation_corpus_2026-03-16.csv") # define to 3/15/2026

names(citing_works) # 29 columns 
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

# # remove the article itself
# alex_doi_new <- alex_doi_new %>% 
#   filter(doi_clean != "10.1038/sdata.2016.18"
# )



# # check for preprint redundancies 
### but preprints are getting citations
# subset works with the exact same titles
dups <- alex_doi_new %>%
  group_by(title) %>%
  filter(n() > 1) %>%
  ungroup()
# 1564 duplicates based on title alone

table(dups$type)
# names(dups)
# dups_version <- dups %>% 
#   filter(version == "acceptedVersion")

# dups_api <- alex_doi_api %>%
dups_api <- alex_doi %>%
  group_by(title) %>%
  filter(n() > 1) %>% 
  mutate(
    # has_accepted = any(version == "acceptedVersion"),
    has_cites = any(cited_by_count > 0)
  ) %>%
  filter(has_cites) %>%
  ungroup()

# 1170 had at least one citation

# remove has cites variable
dups_api <- dups_api %>% 
  select(-has_cites)

names(dups_api)

## anti-join take out the dupes that are not cited
anti_dups <- anti_join(dups, dups_api)

# remove the anti_dups from the larger dataset alex_doi_new
alex_doi_new <- alex_doi_new %>% anti_join(anti_dups, by = "id")

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
names(alex_doi_new)

# bring in new dataframe with the topics 
works_csv_August7_sourceID <- read_csv("works-csv-August7-sourceID.csv")
names(works_csv_August7_sourceID) 

# rename the alike column names
# works_csv_August7_sourceID$title <- works_csv_August7_sourceID$Title
works_csv_August7_sourceID$doi <- works_csv_August7_sourceID$DOI
# works_csv_August7_sourceID$publication_year <- works_csv_August7_sourceID$Year
# works_csv_August7_sourceID$cited_by_count <- works_csv_August7_sourceID$`Citation count`

works_csv_August7 <- works_csv_August7_sourceID %>% 
  select(-c(Title, DOI, Year, `Citation count` ))

# how many empty cells for domain
sum(works_csv_August7$Domain == "") # this is zero

# left_join this to the alex_doi_new
alex_doi_new1 <- alex_doi_new %>% 
  left_join(works_csv_August7, by = c("doi" = "doi"))

names(alex_doi_new1)

# find dupes
dups_doi <- alex_doi_new1 %>%
  group_by(doi) %>%
  filter(n() > 1) %>% 
  filter(`Primary accepted` != "primary accepted") %>% 
  ungroup() # remove primary not accepted

# remove the unaccepted ones 
alex_doi_new1 <- alex_doi_new1 %>% anti_join(dups_doi, by = "doi")

# remove the originating article Wilkerson et al
alex_doi_new1 <-alex_doi_new1 %>% 
  filter(doi_clean != "10.1038/sdata.2016.18")

#### Section: Country of Origin #### 
## distribution of citations by author country of origin 
## (insight into global reach)

#### Section: Country Analysis using csv ####
### parse out authorship countries and split the columns
# unique row idenifier
alex_doi_new1 <- alex_doi_new1  %>%
  mutate(.row_id = row_number())

# save csv of new input data
write.csv(alex_doi_new1, file = "output_data/alex_doi_new1.csv")


country_wide <- alex_doi_new1 %>%
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
alex_doi_new_wide <- alex_doi_new1 %>%
  left_join(country_wide, by = ".row_id") %>%
  select(-.row_id)

# citations by year
library(lubridate)
alex_doi_new_wide$publication_date <- as.Date(alex_doi_new_wide$publication_date,
                                              format = "%m/%d/%Y")

alex_doi_new_wide$publication_year <- as.Date(alex_doi_new_wide$publication_year)

### check if these two variables are the same ###
# 
# identical(as.character(alex_doi_new_wide$Concept), as.character(alex_doi_new_wide$Keyword))
# 
# alex_doi_new_wide[as.character(alex_doi_new_wide$Concept) != as.character(alex_doi_new_wide$Keyword),
#                   c("Concept", "Keyword")]
# 
# # See if leading/trailing whitespace is causing the issue
# identical(trimws(alex_doi_new_wide$Concept), trimws(alex_doi_new_wide$Keyword))

#remove the original article

# write to csv
write.csv(alex_doi_new_wide, file = "output_data/alex_doi_new_wide.csv")
# move onto the topic_words file
######################################################
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
# library(dplyr)
library(plotly)
# library(RColorBrewer)

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

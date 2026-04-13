# =============================================================================
# open_alex_topic_words_clean.R
# Topic-word frequency analysis using OpenAlex data
# =============================================================================

library(dplyr)
library(stringr)
library(textstem)
library(textmineR)
library(stopwords)
library(readr)

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
#### Section 2: Collapse map  (variants → canonical term) ####
# -----------------------------------------------------------------------------

collapse_map <- list(

  # behavior
  "behavior"         = c("behaviour", "behavioral", "behavioural"),

  # biology family
  "biology"          = c("biological"),
  "microbiology"     = c("microbial", "microbe", "microorganism"),
  "neurobiology"     = c("neurobiological"),
  "immunology"       = c("immunological", "immune", "immunity"),
  "pharmacology"     = c("pharmacological", "pharmaceutical", "pharmacokinetics",
                         "pharmacogenetics", "pharmacovigilance"),
  "toxicology"       = c("toxic", "toxicity", "toxin"),
  "epidemiology"     = c("epidemiological"),
  "physiology"       = c("physiological"),
  "morphology"       = c("morphological"),
  "pathology"        = c("pathological", "pathogenesis", "pathophysiology",
                         "pathogen"),
  "ecology"          = c("ecological"),
  "psychology"       = c("psychological", "psychosocial", "psychometric",
                         "psychosomatic"),
  "sociology"        = c("sociological", "societal"),
  "geology"          = c("geological", "geologic"),
  "geophysics"       = c("geophysical"),
  "geochemistry"     = c("geochemical"),
  "biochemistry"     = c("biochemical"),
  "astronomy"        = c("astronomical"),
  "astrophysics"     = c("astrophysical"),
  "archaeology"      = c("archaeological"),
  "anthropology"     = c("anthropological"),
  "musicology"       = c("musicological"),

  # economy
  "economy"          = c("economic", "economics"),
  "finance"          = c("financial"),

  # technology
  "technology"       = c("technological"),
  "nanotechnology"   = c("nanomaterials", "nanoparticle", "nanostructure",
                         "nanofabrication", "nanocluster", "nanonetworks",
                         "nanoplatforms"),

  # chemistry / physics
  "chemistry"        = c("chemical", "chemometric"),
  "physics"          = c("physical"),
  "thermodynamics"   = c("thermodynamic", "thermography"),

  # medicine / clinical
  "medicine"         = c("medical", "medicinal"),
  "diagnosis"        = c("diagnostic", "diagnostics"),
  "surgery"          = c("surgical"),
  "therapy"          = c("therapeutic", "theranostics"),
  "nutrition"        = c("nutritional", "nutrient"),
  "vaccination"      = c("vaccine"),
  "infection"        = c("infectious", "infective"),
  "inflammation"     = c("inflammatory", "neuroinflammation"),
  "cancer"           = c("carcinoma", "carcinogen", "carcinogenesis", "oncology"),
  "neuroscience"     = c("neurological", "neurology", "neurodegeneration",
                         "neurodegenerative", "neurogenesis", "neuroplasticity",
                         "neuropharmacology", "neuroethics", "neuroimaging",
                         "neurovascular", "neurotransmitter", "neurodevelopmental",
                         "neurogenetic"),
  "cardiovascular"   = c("cardiac", "coronary", "cardiology"),
  "pulmonary"        = c("respiratory"),
  "genetics"         = c("genetic", "genetically", "genomics", "genomic"),
  "metabolism"       = c("metabolic", "metabolomics"),
  "rehabilitation"   = c("rehab"),

  # environment / earth science
  "environment"      = c("environmental"),
  "hydrology"        = c("hydrological", "hydrogeology"),
  "sustainability"   = c("sustainable"),
  "climate"          = c("climatic"),

  # social sciences
  "education"        = c("educational"),
  "politics"         = c("political"),
  "statistics"       = c("statistical"),
  "mathematics"      = c("mathematical"),
  "philosophy"       = c("philosophical"),
  "law"              = c("legal", "judicial"),

  # engineering / computing
  "engineering"      = c("engineer"),
  "computation"      = c("computational", "compute"),
  "automation"       = c("automate"),
  "optimization"     = c("optimal"),
  "simulation"       = c("simulate"),
  "manufacture"      = c("manufacturing", "fabrication"),

  # agriculture
  "agriculture"      = c("agricultural", "agronomic", "agronomy"),

  # other
  "development"      = c("developmental"),
  "regulation"       = c("regulatory"),
  "reproduction"     = c("reproductive"),
  "organization"     = c("organizational"),
  "communication"    = c("communications"),
  "innovation"       = c("innovative"),
  "integration"      = c("integrate"),
  "distribution"     = c("distribute"),
  "production"       = c("produce"),
  "analysis"         = c("analytic", "analytics"),
  "spectroscopy"     = c("spectrometry"),
  "radiotherapy"     = c("radiopharmaceutical", "radiomics", "radiology",
                         "radiography")
)

# Build flat lookup: old term → collapsed_term
lookup <- stack(collapse_map)
names(lookup) <- c("term_canonical", "collapsed_term")
lookup$term_canonical <- as.character(lookup$term_canonical)
lookup$collapsed_term  <- as.character(lookup$collapsed_term)

# -----------------------------------------------------------------------------
#### Section 3: Apply collapse to alex_doi_topic, then extract topic text ####
# -----------------------------------------------------------------------------

alex_doi_topic <- alex_doi_topic %>%
  left_join(lookup, by = c("topic" = "term_canonical")) %>%
  mutate(
    collapsed_term = coalesce(collapsed_term, topic)
  )

# Use collapsed_term as the document text for TermDocFreq
topic_data_alex <- alex_doi_topic$collapsed_term

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

# -----------------------------------------------------------------------------
#### Section 7: Apply fine-grained term grouping ####
# (bigram collapse + lookup)
# -----------------------------------------------------------------------------
tf_mat_topic_grouped <- tf_mat_topic %>%
  mutate(
    term_canonical = case_when(
      term %in% c("amphibian", "amphibian_reptile")        ~ "amphibian",
      term %in% c("acid", "acid_chemistry")                ~ "acid",
      term %in% c("adipokines", "adipokines_inflammation")  ~ "adipokines",
      term %in% c("fracture", "fractures", "fracture_mechanics") ~ "fracture",
      TRUE ~ term
    )
  ) %>%
  left_join(lookup, by = "term_canonical") %>%
  mutate(
    collapsed_term = coalesce(collapsed_term, term_canonical)  # keep as its own column
  )
# tf_mat_topic_grouped <- tf_mat_topic %>%
#   mutate(
#     term_canonical = case_when(
#       term %in% c("amphibian", "amphibian_reptile")       ~ "amphibian",
#       term %in% c("acid", "acid_chemistry")               ~ "acid",
#       term %in% c("adipokines", "adipokines_inflammation") ~ "adipokines",
#       term %in% c("fracture", "fractures", "fracture_mechanics") ~ "fracture",
#       TRUE ~ term
#     )
#   ) %>%
#   left_join(lookup, by = "term_canonical") %>%
#   mutate(
#     term_canonical = coalesce(collapsed_term, term_canonical)
#   ) %>%
#   select(-collapsed_term)

# -----------------------------------------------------------------------------
#### Section 8: Trim by document frequency ####
# -----------------------------------------------------------------------------
n_docs <- length(unique(alex_doi_topic$doi_clean))

tf_mat_topic_trimmed <- tf_mat_topic_grouped %>%
  filter(!term %in% domain_stopwords) %>%
  group_by(collapsed_term) %>%
  summarise(
    term_freq = sum(term_freq),
    doc_freq  = sum(doc_freq),
    idf       = mean(idf),        # average IDF across collapsed variants
    .groups   = "drop"
  ) %>%
  filter(
    doc_freq >= 2,
    doc_freq <= 0.5 * n_docs
  ) %>%
   arrange(desc(term_freq))
# n_docs <- length(unique(alex_doi_topic$doi_clean))
# 
# tf_mat_topic_trimmed <- tf_mat_topic_grouped %>%
#   filter(
#     doc_freq >= 2,
#     doc_freq <= 0.5 * n_docs,
#     !term %in% domain_stopwords
#   )

# -----------------------------------------------------------------------------
#### Section 9: Save output ####
# -----------------------------------------------------------------------------

# write_csv(tf_mat_topic_trimmed, "TermDocFreq_long_collapsed.csv")

# -----------------------------------------------------------------------------
#### Section 10: Journal title counts ####
# (reference / visualization)
# -----------------------------------------------------------------------------

alex_doi_unique <- alex_doi_new %>%
  mutate(
    journal_name = primary_location.source.display_name,
    doi_clean    = str_remove(doi, "https://doi.org/"),
    journal_name_clean = journal_name %>%
      str_replace("^\\d{4}\\s+", "") %>%
      str_replace_all("[[:cntrl:]]", "") %>%
      str_replace_all("[^[:alnum:] [:space:]]", "") %>%
      str_squish()
  ) %>%
  filter(
    !is.na(journal_name_clean),
    !is.na(doi_clean),
    publication_date >= as.Date("2016-03-15"),
    publication_date <= as.Date("2026-03-15"),
    doi_clean != "10.1038/sdata.2016.18"
  ) %>%
  mutate(publication_year = as.integer(publication_year)) %>%
  group_by(publication_year, type)

journal_doi_counts <- alex_doi_topic %>%
  filter(!is.na(journal_name), !is.na(doi_clean)) %>%
  count(journal_name, doi_clean) %>%
  count(journal_name, name = "n_unique_doi")

alex_doi_journal <- alex_doi_topic %>%
  select(journal_name, abstract, topic, collapsed_term, type) 

# Save to CSV
# write_csv(alex_doi_journal, "alex_doi_journal.csv")

alex_doi_topic %>%
  count(journal_name, sort = TRUE) %>%
  slice_head(n = 30) %>%
  ggplot(aes(x = reorder(journal_name, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 30 Journals", x = "Journal", y = "Count")

# more cleaning steps for the journals
# # scraping data from JSTOR for organized journal discipline lists

# read a TXT file
# Source - https://stackoverflow.com/a/61525923
# Posted by linog, modified by community. See post 'Timeline' for change history
# Retrieved 2026-04-07, License - CC BY-SA 4.0

jstor <- readr::read_delim("JSTOR_Global_Archive_Primary_Source_Collection_2026-04-07.txt", delim = "\t")
# JSTOR's coverage is not great in the sciences

#### Domain Data ####
# OPEN Alex has 4 domains
## read in 4 csv files 
phys_sci_domain <- read_csv("domain_data/physical_science_domain_openAlex.csv")
soci_sci_domain <- read_csv("domain_data/social_sciences_domain_openAlex.csv")
health_sci_domain <- read_csv("domain_data/health_sciences_domain_openAlex.csv")
life_sci_domain <- read_csv("domain_data/life_sciences_domain_openAlex.csv")

# create a column for domain for each of them
phys_sci_domain$domain <- "phys sci"
soci_sci_domain$domain <- "soci sci"
health_sci_domain$domain <- "health sci"
life_sci_domain$domain <- "life sci"

# combine into one dataframe
alex_doi_domain <- rbind(phys_sci_domain, soci_sci_domain, health_sci_domain, life_sci_domain)

# recombine it to the larger alex_doi_new_wide dataframe
alex_doi_domain <- alex_doi_domain %>% 
  select(domain, doi, primary_location.source.id)

alex_doi_new <- alex_doi_new %>% 
  left_join(alex_doi_domain)

#### Field Data ####
# read in field dataset
comp_sci_field <- read_csv("field_data/comp_sci_field_openAlex.csv")
biochem_field <- read_csv("field_data/biochem_field_openAlex.csv")
env_sci_field <- read_csv("field_data/env_sci_field_openAlex.csv")
decision_sci_field <- read_csv("field_data/decision_sci_field_openAlex.csv")
medicine_field <- read_csv("field_data/medicine_field_openAlex.csv")
planet_sci_field <- read_csv("field_data/planet_sci_field_openAlex.csv")
engineering_field <- read_csv("field_data/engineering_field_openAlex.csv")
soci_sci_field <- read_csv("field_data/soci_sci_field_openAlex.csv")
mat_sci_field <- read_csv("field_data/mat_sci_field_openAlex.csv")
agri_bio_sci_field <- read_csv("field_data/agri_bio_sci_field_openAlex.csv")

# create a column for field for each of them
comp_sci_field$field <- "comp sci"
soci_sci_field$field <- "soci sci"
mat_sci_field$field <- "material sci"
engineering_field$field <- "engineering"
medicine_field$field <- "medicine"
planet_sci_field$field <- "planetary spacesci"
biochem_field$field <- "biochem genetics molecular"
agri_bio_sci_field$field <- "agricultural bio sci"
decision_sci_field$field <- "decision sci"
env_sci_field$field <- "environmental sci"

# combine fields into one data frame
alex_doi_field <- rbind(comp_sci_field, mat_sci_field,
                        env_sci_field, decision_sci_field,
                        engineering_field, medicine_field,
                        soci_sci_field, agri_bio_sci_field,
                        planet_sci_field, biochem_field)

# recombine field to the larger alex_doi_new_wide dataframe
alex_doi_field <- alex_doi_field %>% 
  select(field, doi, primary_location.source.id)

alex_doi_new <- alex_doi_new %>% 
  left_join(alex_doi_field)

#### SubField Data ####
# read in subfield dataset
AI_subfield <- read_csv("subfield_data/AI_subfield_openAlex.csv")
eco_model_subfield <- read_csv("subfield_data/eco_model_subfield_openAlex.csv")
ecology_subfield <- read_csv("subfield_data/ecology_subfield_openAlex.csv")
global_planet_change_subfield <- read_csv("subfield_data/global_planet_change_subfield_openAlex.csv")
info_sys_manage_subfield <- read_csv("subfield_data/info_sys_manage_subfield_openAlex.csv")
info_sys_subfield <- read_csv("subfield_data/info_sys_subfield_openAlex.csv")
mat_chem_subfield <- read_csv("subfield_data/mat_chem_subfield_openAlex.csv")
molec_bio_subfield <- read_csv("subfield_data/molec_bio_subfield_openAlex.csv")
pub_health_subfield <- read_csv("subfield_data/pub_health_subfield_openAlex.csv")
radi_nuclear_med_subfield <- read_csv("subfield_data/radi_nuclear_med_subfield_openAlex.csv")

# create a column for subfield for each of them
AI_subfield$subfield <- "AI"
eco_model_subfield$subfield <- "ecolog model"
ecology_subfield$subfield <- "ecology"
global_planet_change_subfield$subfield <- "global planet change"
info_sys_subfield$subfield <- "info systems"
info_sys_manage_subfield$subfield <- "info sys management"
mat_chem_subfield$subfield <- "material chemistry"
molec_bio_subfield$subfield <- "molecular biology"
pub_health_subfield$subfield <- "public health"
radi_nuclear_med_subfield$subfield <- "radiation nuclear medicine"


# combine subfields into one data frame
alex_doi_subfield <- rbind(AI_subfield, eco_model_subfield,
                           ecology_subfield, global_planet_change_subfield,
                           info_sys_manage_subfield, info_sys_subfield,
                           mat_chem_subfield, molec_bio_subfield,
                           pub_health_subfield, radi_nuclear_med_subfield)

# recombine subfield to the larger alex_doi_new_wide dataframe
alex_doi_subfield <- alex_doi_subfield %>% 
  select(subfield, doi, primary_location.source.id)

alex_doi_new <- alex_doi_new %>% 
  left_join(alex_doi_subfield)

# move the columns to topics, domain, field, subfield 
alex_doi_new <- alex_doi_new %>% 
  relocate(domain, .after = title) %>% 
  relocate(field, .after = domain) %>% 
  relocate(subfield, .after = field)

# -----------------------------------------------------------------------------
# Section 11: Create DTM for term frequencies using domain and (sub)field
# -----------------------------------------------------------------------------

# Term frequency per domain label
tf_domain <- alex_doi_new %>%
  filter(!is.na(domain)) %>%
  count(domain, name = "tf") %>%
  arrange(desc(tf))

# IDF: log(N / df) where N = total docs, df = docs containing that domain
N <- n_distinct(alex_doi_new$doi)

idf_domain <- alex_doi_new %>%
  filter(!is.na(domain)) %>%
  distinct(doi, domain) %>%          # one row per doc-domain pair
  count(domain, name = "df") %>%     # how many docs mention each domain
  mutate(
    idf    = log(N / df),
    tf_idf = tf_domain$tf[match(domain, tf_domain$domain)] / N * idf
  )

idf_domain


# Term frequency per field label
tf_field <- alex_doi_new %>%
  filter(!is.na(field)) %>%
  count(field, name = "tfield") %>%
  arrange(desc(tfield))

# IDF: log(N / df) where N = total docs, df = docs containing that domain
N <- n_distinct(alex_doi_new$doi)

idf_field <- alex_doi_new %>%
  filter(!is.na(field)) %>%
  distinct(doi, field) %>%          
  count(field, name = "df") %>%     
  mutate(
    idf    = log(N / df),
    tf_idf = tf_field$tfield[match(field, tf_field$field)] / N * idf  # tfield not tf
  )

idf_field

# Term frequency per subfield label
tf_subfield <- alex_doi_new %>%
  filter(!is.na(subfield)) %>%
  count(subfield, name = "tsubfield") %>%
  arrange(desc(tsubfield))

# IDF: log(N / df) where N = total docs, df = docs containing that domain
N <- n_distinct(alex_doi_new$doi)

idf_subfield <- alex_doi_new %>%
  filter(!is.na(subfield)) %>%
  distinct(doi, subfield) %>%          
  count(subfield, name = "df") %>%     
  mutate(
    idf    = log(N / df),
    tf_idf = tf_subfield$tsubfield[match(subfield, tf_subfield$subfield)] / N * idf  # tfield not tf
  )

idf_subfield

# Year by year breakdown of journals
library(DT)

journal_hierarchy <- alex_doi_new %>%
  filter(!is.na(domain), 
         # !is.na(field), !is.na(subfield),
         !is.na(publication_year),
         !is.na(primary_location.source.display_name)) %>%
  distinct(publication_year, domain, field, subfield,
           primary_location.source.display_name) %>%
  count(publication_year, domain, field, subfield,
        name = "n_unique_journals") %>%
  arrange(publication_year, domain, field, subfield)

datatable(
  journal_hierarchy,
  filter   = "top",           # per-column filter boxes
  rownames = FALSE,
  colnames = c("Year", "Domain", "Field", "Subfield", "Unique Journals"),
  options  = list(
    pageLength  = 20,
    scrollX     = TRUE,
    autoWidth   = TRUE,
    columnDefs  = list(list(className = "dt-center", targets = c(0, 4)))
  )
)

# Plotly Sunburst visual
library(plotly)
library(dplyr)

sunburst_data <- alex_doi_new %>%
  filter(!is.na(domain), 
         # !is.na(field), !is.na(subfield),
         !is.na(primary_location.source.display_name)) %>%
  distinct(domain, field, subfield,
           primary_location.source.display_name) %>%
  count(domain, field, subfield, name = "n_unique_journals")

# Build ids/labels/parents for plotly sunburst
domains  <- sunburst_data %>%
  group_by(domain) %>%
  summarise(n = sum(n_unique_journals)) %>%
  transmute(ids = domain, labels = domain, parents = "", values = n)

fields   <- sunburst_data %>%
  group_by(domain, field) %>%
  summarise(n = sum(n_unique_journals), .groups = "drop") %>%
  transmute(ids = paste(domain, field, sep = " - "),
            labels = field, parents = domain, values = n)

subfields <- sunburst_data %>%
  transmute(ids     = paste(domain, field, subfield, sep = " - "),
            labels  = subfield,
            parents = paste(domain, field, sep = " - "),
            values  = n_unique_journals)

sb <- bind_rows(domains, fields, subfields)

plot_ly(
  sb,
  ids     = ~ids,
  labels  = ~labels,
  parents = ~parents,
  values  = ~values,
  type    = "sunburst",
  branchvalues = "total"
)
# -----------------------------------------------------------------------------
# Section 12: Dimensions data
# -----------------------------------------------------------------------------

Dimensions_Publication_2026_a <- read_csv("dimensions_data/Dimensions-Publication-2026-04-07_19-44-57.csv", skip = 1)
Dimensions_Publication_2026_b <- read_csv("dimensions_data/Dimensions-Publication-2026-04-07_19-45-51.csv", skip = 1)
Dimensions_Publication_2026_c <- read_csv("dimensions_data/Dimensions-Publication-2026-04-07_19-46-14.csv", skip = 1)
Dimensions_Publication_2026_d <- read_csv("dimensions_data/Dimensions-Publication-2026-04-07_19-47-22.csv", skip = 1)

Dimensions_data <- bind_rows(
  Dimensions_Publication_2026_a,
  Dimensions_Publication_2026_b,
  Dimensions_Publication_2026_c,
  Dimensions_Publication_2026_d
)

dimensions_unique_doi <- Dimensions_data %>%
  filter(!is.na(DOI) & DOI != "") %>%
  unique()

abstract_data_dim <- dimensions_unique_doi$Abstract

dtm_dim <- CreateDtm(
  doc_vec      = abstract_data_dim,
  doc_names    = dimensions_unique_doi$DOI,
  ngram_window = c(1, 2),
  stopword_vec = c(
    stopwords::stopwords("en"),
    stopwords::stopwords(source = "smart")
  ),
  lower             = TRUE,
  remove_punctuation = TRUE,
  remove_numbers    = TRUE,
  verbose           = FALSE,
  cpus              = 1
)

tf_mat_dim <- TermDocFreq(dtm_dim)

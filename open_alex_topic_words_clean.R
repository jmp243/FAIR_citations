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
    publication_date <= as.Date("2026-03-15"),
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

write_csv(tf_mat_topic_trimmed, "TermDocFreq_long_collapsed.csv")

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


write_csv(alex_doi_journal, "alex_doi_journal.csv")

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

# OPEN Alex has 4 domains
## read in 4 csv files 
phys_sci_domain <- read_csv("physical_science_domain_openAlex.csv")
soci_sci_domain <- read_csv("social_sciences_domain_openAlex.csv")
health_sci_domain <- read_csv("health_sciences_domain_openAlex.csv")
life_sci_domain <- read_csv("life_sciences_domain_openAlex.csv")

# create a column for domain for each of them
phys_sci_domain$domain <- "phys_sci"
soci_sci_domain$domain <- "soci_sci"
health_sci_domain$domain <- "health_sci"
life_sci_domain$domain <- "life_sci"

# combine into one dataframe
alex_doi_domain <- rbind(phys_sci_domain, soci_sci_domain, health_sci_domain, life_sci_domain)

# recombine it to the larger alex_doi_new_wide dataframe
alex_doi_domain <- alex_doi_domain %>% 
  select(domain, doi, primary_location.source.id)

alex_doi_new_wide <- alex_doi_new_wide %>% 
  left_join(alex_doi_domain)


# install.packages("rvest")
# library(rvest)
# # library(tidyverse)
# 
# # read JSTOR subject
# url <- "https://www.jstor.org/subjects"
# page <- read_html(url)
# html_structure(page)

# library(dplyr)
# library(stringr)
# 
# alex_doi_journal <- read.csv("alex_doi_journal.csv", stringsAsFactors = FALSE)
# 
# categorize <- function(journal_name, topic) {
#   journal <- tolower(journal_name)
#   t <- tolower(topic)
#   combined <- paste(t, journal)
# 
#   # IEEE → Engineering
#   if (str_detect(journal, "\\bieee\\b")) return("Engineering")
# 
#   # Oncology
#   if (str_detect(combined, "cancer|oncol|tumor|leukemia|lymphoma|melanoma|glioma|sarcoma|carcinoma|neoplasm|metastas|radiother|chemotherapy|breast cancer|lung cancer|prostate cancer|colorectal cancer")) return("Oncology")
# 
#   # Neuroscience & Psychology
#   if (str_detect(combined, "neuro|brain|cogniti|psychol|psychiatr|mental health|alzheimer|parkinson|epilepsy|stroke|dementia|autism|schizophrenia|depression|anxiety|bipolar|traumatic brain|eeg|brain.computer|fmri|neuroimag|ptsd|mindfulness|migraine|sleep.*disorder|circadian|opioid|addiction")) return("Neuroscience & Psychology")
# 
#   # Bioinformatics & Genomics
#   if (str_detect(combined, "bioinform|genomic|genome|proteom|transcript|sequenc|phylogen|single.cell|epigenetic|dna methyl|chromatin|chromosome|gwas|gene regulat|gene express|rna.seq|mrna|microrna|crispr|gene edit|metabolom|omics")) return("Bioinformatics & Genomics")
# 
#   # AI & Machine Learning
#   if (str_detect(combined, "machine learn|deep learn|neural network|artificial intelligen|natural language process|computer vision|large language|generative adversarial|reinforcement learn|explainable ai|few.shot|recommender system|sentiment analysis|topic model|text classif|image classif|object detect|speech recognit|graph neural")) return("AI & Machine Learning")
# 
#   # Data Science & Informatics
#   if (str_detect(combined, "data sci|data min|big data|data analyt|data quality|data manag|research data|open.*data|metadata|data.*driven|bibliometric|scientometric|open access|library.*information|information.*literacy|semantic web|ontology|scholarly|digital.*humanit")) return("Data Science & Informatics")
# 
#   # Medicine & Health
#   if (str_detect(combined, "medicine|medical|clinic|health|hospital|patient|disease|epidemi|public health|nursing|pharmac|drug|therapeut|vaccin|immunol|infect|pathol|surgery|cardio|dermatol|radiol|pediatr|obstet|gynecol|dental|ophthalm|orthop|urol|pulmon|endocrin|rheumatol|gastro|hepat|nephrol|hematol|anesthes|emergency|rehabilit|diagnos|telemedicine|telehealth|covid|sars|hiv|tuberculosis|diabetes|obesity|pregnancy|maternal|neonatal|pathol")) return("Medicine & Health")
# 
#   # Biology & Life Sciences
#   if (str_detect(combined, "biology|biotech|biotechnol|cell bio|molecular bio|biochem|biophys|microbiol|virol|ecolog|evolution|species.*distribut|animal.*behav|marine.*ecol|forest.*ecol|plant.*bio|plant.*ecol|mycorrhiz|enzyme|autophagy|apoptosis|cytokine|receptor.*signal|immune.*cell|t.cell|b.cell|mitochond|pluripotent|photosynthes|dna.*repair|rna.*splicing|aquatic.*ecosys|freshwater|fish.*ecol|aquaculture|coral|wildlife|conservation.*bio|biodiversit|invasive.*species|animal.*genetic|veterinary|avian|amphibian|reptile")) return("Biology & Life Sciences")
# 
#   # Chemistry & Materials
#   if (str_detect(combined, "chemi|polymer|catalys|molecule|nanomater|nanoparticle|nanotechnol|surface science|colloid|crystal|spectroscop|synthesis|organic.*chemistry|inorganic|analytical.*chem|chromatograph|nmr|electrochemi|thermodynam|metal.organic framework|zeolite|graphene|perovskite|mxene|supercapacitor|hydrogel|photocatalysis|ionic liquid")) return("Chemistry & Materials")
# 
#   # Physics & Astronomy
#   if (str_detect(combined, "physics|quantum|astro|exoplanet|astrophysic|cosmolog|dark matter|galaxi|particle.*physics|nuclear.*physics|plasma.*physics|atomic.*physics|particle.*accelerat|semiconductor.*device|superconductor|cold.*atom|bose.einstein|fluid dynamics|turbulent|aerodynam|radio.*astronom|radioactive.*decay|radioactivity")) return("Physics & Astronomy")
# 
#   # Engineering
#   if (str_detect(combined, "engineer|manufactur|mechanical.*design|civil.*engineer|structural.*engineer|construction|aerospace|robotics|control.*system|fault.*detection|uav|drone|electric.*motor|electric.*vehicle|power.*system|smart.*grid|microwave|wireless.*sensor|iot|embedded.*system|signal.*processing|additive.*manufactur|3d.*print|welding|fatigue.*fracture|geotechnical|reservoir.*engineering|drilling|oil.*gas|mining|infrastructure")) return("Engineering")
# 
#   # Environment & Sustainability
#   if (str_detect(combined, "ecosystem|biodiversit|land.*use|deforestation|carbon.*footprint|greenhouse.*gas|emission|air.*quality|atmospheric.*chem|microplastic|plastic.*pollution|environmental.*toxicol|ecotoxicol|heavy metal.*environ|wastewater|water.*pollution|oil.*spill|toxic.*pollutant|recycling|waste.*management|environmental.*impact|life.*cycle.*assessment|green.*it|sustainable.*urban|sustain|climate.*change|ecolog")) return("Environment & Sustainability")
# 
#   # Earth & Geosciences
#   if (str_detect(combined, "geograph|geolog|geosci|earth.*science|seismol|earthquake|tectonic|geochemistry|geomorphol|geophysic|hydrogeol|karst|groundwater|geothermal|volcan|paleoclimat|sediment|rock.*mechanic|geomagnet|gnss|lidar|remote.*sensing|gis|cartograph|oceanograph|ocean.*acidification|precipitation|flood.*risk|hydrology|cryosphere|ice.*dynamics|polar|landslide")) return("Earth & Geosciences")
# 
#   # Agriculture & Food Science
#   if (str_detect(combined, "agri|food(?!.*security.*health)|crop|farm|plant.*pathog|plant.*breed|wheat|barley|rice|soybean|maize|potato|cassava|banana|sugarcane|livestock|dairy|milk|poultry|ruminant|aquaculture|fisheries|postharvest|food.*qualit|food.*safety|food.*process|pesticide.*residue|organic.*food|agroforest|silvopastor|horticult|viticultur|irrigation|medicinal.*plant")) return("Agriculture & Food Science")
# 
#   # Education
#   if (str_detect(combined, "\\beducation\\b|teaching|pedagog|curriculum|e.learning|online.*learning|higher.*education|doctoral.*education|early.*childhood.*education|educational.*game|gamification|intelligent.*tutoring|learning.*analytic|problem.*based.*learning|academic.*integrity|educational.*assessment|educational.*leadership|digital.*literacy.*education|vocational.*education")) return("Education")
# 
#   # Social Sciences
#   if (str_detect(combined, "sociolog|anthropolog|demographic|migration|refugee|diaspora|indigenous.*studies|gender.*diversit|gender.*inequal|lgbtq|critical.*race|disability.*education|work.family|job.*satisf|organizational.*behav|survey.*method|qualitative.*method|ethnograph|oral.*history|discourse.*analysis|cultural.*identity|opinion.*dynamics|misinformation|electoral.*system|political.*participat|indigenous.*health|sex.*work")) return("Social Sciences")
# 
#   # Business & Economics
#   if (str_detect(combined, "\\beconom|financ|\\bbusiness\\b|\\bmanagement\\b|\\bmarket\\b|accounting|supply.*chain|trade|commerce|entrepreneurship|fintech|crowdfunding|corporate.*governance|corporate.*social.*responsib|corporate.*taxation|risk.*management.*financial|stock.*market|private.*equity|venture.*capital|innovation.*policy|digital.*economy")) return("Business & Economics")
# 
#   # Law & Policy
#   if (str_detect(combined, "\\blaw\\b|\\blegal\\b|justice|regulation|governance|\\bpolicy\\b|administrative.*law|constitutional|judicial|international.*law|maritime.*law|environmental.*law|intellectual.*property.*law|ai.*law|electoral.*system|political.*influence")) return("Law & Policy")
# 
#   # Arts & Humanities
#   if (str_detect(combined, "histor|philosophy|humanities|literature|cultural.*studies|archaeology|\\bmusic\\b|theatre|performance.*studies|heritage|digital.*humanities|medieval|ancient|classical|literary|poetry|fiction|narrative|rhetoric|translation.*studies|lexicography")) return("Arts & Humanities")
# 
#   # Mathematics & Statistics
#   if (str_detect(combined, "\\bmath|\\bstatistic|probabilit|\\boptim|algebra|calculus|graph.*theory|topology|combinatorics|number.*theory|differential.*equation|numerical.*analysis|bayesian|markov.*chain|monte.*carlo|time.*series.*analysis|complex.*network.*analysis|rough.*set|fuzzy.*logic")) return("Mathematics & Statistics")
# 
#   # Energy
#   if (str_detect(combined, "\\benergy\\b|solar|wind.*power|photovoltaic|fuel cell|nuclear.*energy|renewable.*energy|energy.*storage|smart.*grid|electric.*vehicle.*infrastructure|biofuel.*energy|hydrogen.*energy|carbon.*capture|electrocatalyst.*energy")) return("Energy")
# 
#   return("Other")
# }
# 
# alex_doi_journal <- alex_doi_journal %>%
#   mutate(topic_category = mapply(categorize, journal_name, topic))
# 
# # Review results
# alex_doi_journal %>% count(topic_category) %>% arrange(desc(n))
# 
# # write.csv(alex_doi_journal_categorized, "alex_doi_journal_categorized.csv", row.names = FALSE)
# 

# -----------------------------------------------------------------------------
# Section 11: Dimensions data
# -----------------------------------------------------------------------------

Dimensions_Publication_2026_a <- read_csv("dimensions_data/Dimensions-Publication-2026-03-06_19-01-28.csv", skip = 1)
Dimensions_Publication_2026_b <- read_csv("dimensions_data/Dimensions-Publication-2026-03-06_19-02-00.csv", skip = 1)
Dimensions_Publication_2026_c <- read_csv("dimensions_data/Dimensions-Publication-2026-03-06_19-02-32.csv", skip = 1)
Dimensions_Publication_2026_d <- read_csv("dimensions_data/Dimensions-Publication-2026-03-06_19-02-49.csv", skip = 1)

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

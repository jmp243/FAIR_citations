# collapse_terms.R
# Collapses similar canonical terms in the Term column of a term-document
# frequency data frame in long format.
#
# Input:  TermDocFreq_long.csv  (columns: term, term_freq, doc_freq, idf, term_canonical)
# Output: same structure with term_canonical replaced by a collapsed version

library(dplyr)

df <- read.csv("TermDocFreq_long.csv", stringsAsFactors = FALSE)

# ------------------------------------------------------------------
# 1. Collapse map
#    Format: "canonical_to_keep" = c("term1", "term2", ...)
#    The left-hand name is what every listed term will be renamed to.
# ------------------------------------------------------------------
collapse_map <- list(
  
  # --- behaviour / behavior -----------------------------------------
  "behavior"        = c("behaviour", "behavioral", "behavioural"),
  
  # --- biology family -----------------------------------------------
  "biology"         = c("biological"),
  "microbiology"    = c("microbial", "microbe", "microorganism"),
  "neurobiology"    = c("neurobiological"),
  "immunology"      = c("immunological", "immune", "immunity"),
  "pharmacology"    = c("pharmacological", "pharmaceutical", "pharmacokinetics",
                        "pharmacogenetics", "pharmakovigilance"),
  "toxicology"      = c("toxic", "toxicity", "toxin"),
  "epidemiology"    = c("epidemiological"),
  "physiology"      = c("physiological"),
  "morphology"      = c("morphological"),
  "pathology"       = c("pathological", "pathogenesis", "pathophysiology",
                        "pathogen"),
  "ecology"         = c("ecological"),
  "psychology"      = c("psychological", "psychosocial", "psychometric",
                        "psychosomatic"),
  "sociology"       = c("sociological", "societal"),
  "geology"         = c("geological", "geologic"),
  "geophysics"      = c("geophysical"),
  "geochemistry"    = c("geochemical"),
  "biochemistry"    = c("biochemical"),
  "astronomy"       = c("astronomical"),
  "astrophysics"    = c("astrophysical"),
  "archaeology"     = c("archaeological"),
  "anthropology"    = c("anthropological"),
  "musicology"      = c("musicological"),
  
  # --- economy family -----------------------------------------------
  "economy"         = c("economic", "economics"),
  "finance"         = c("financial"),
  
  # --- technology family --------------------------------------------
  "technology"      = c("technological"),
  "nanotechnology"  = c("nanomaterials", "nanoparticle", "nanostructure",
                        "nanofabrication", "nanocluster", "nanonetworks",
                        "nanoplatforms"),
  
  # --- chemistry / physics ------------------------------------------
  "chemistry"       = c("chemical", "chemometric"),
  "physics"         = c("physical"),
  "thermodynamics"  = c("thermodynamic", "thermographical", "thermography"),
  
  # --- medicine / clinical ------------------------------------------
  "medicine"        = c("medical", "medicinal"),
  "diagnosis"       = c("diagnostic", "diagnostics"),
  "surgery"         = c("surgical"),
  "therapy"         = c("therapeutic", "theranostics"),
  "treatment"       = c("treatments"),
  "rehabilitation"  = c("rehab"),
  "nutrition"       = c("nutritional", "nutrient"),
  "vaccination"     = c("vaccine"),
  "infection"       = c("infectious", "infective"),
  "inflammation"    = c("inflammatory", "neuroinflammation"),
  "cancer"          = c("carcinoma", "carcinogen", "carcinogenesis", "oncology"),
  "neuroscience"    = c("neurological", "neurology", "neurodegeneration",
                        "neurodegenerative", "neurogenesis", "neuroplasticity",
                        "neuropharmacology", "neuroethics", "neuroimaging",
                        "neurovascular", "neurotransmitter", "neurodevelopmental",
                        "neurogenetic"),
  "cardiovascular"  = c("cardiac", "coronary", "cardiology"),
  "pulmonary"       = c("respiratory"),
  "genetics"        = c("genetic", "genetically", "genomics", "genomic"),
  "metabolism"      = c("metabolic", "metabolomics"),
  
  # --- environment / earth science ----------------------------------
  "environment"     = c("environmental"),
  "hydrology"       = c("hydrological", "hydrogeology"),
  "sustainability"  = c("sustainable"),
  "climate"         = c("climatic"),
  "geology"         = c("geological", "geologic"),
  
  # --- social sciences ----------------------------------------------
  "education"       = c("educational"),
  "politics"        = c("political"),
  "statistics"      = c("statistical"),
  "mathematics"     = c("mathematical"),
  "philosophy"      = c("philosophical"),
  "law"             = c("legal", "judicial"),
  
  # --- engineering / computing --------------------------------------
  "engineering"     = c("engineer"),
  "computation"     = c("computational", "compute"),
  "automation"      = c("automate"),
  "optimization"    = c("optimal"),
  "simulation"      = c("simulate"),
  "manufacture"     = c("manufacturing", "fabrication"),
  "machine learning"= c("learn"),   # context-dependent — review before keeping
  
  # --- agriculture --------------------------------------------------
  "agriculture"     = c("agricultural", "agronomic", "agronomy"),
  
  # --- other --------------------------------------------------------
  "development"     = c("developmental"),
  "regulation"      = c("regulatory"),
  "reproduction"    = c("reproductive"),
  "organization"    = c("organizational"),
  "communication"   = c("communications"),
  "innovation"      = c("innovative"),
  "integration"     = c("integrate"),
  "distribution"    = c("distribute"),
  "production"      = c("produce"),
  "analysis"        = c("analytic", "analytics"),
  "spectroscopy"    = c("spectrometry"),
  "radiotherapy"    = c("radiopharmaceutical", "radiomics", "radiology",
                        "radiography")
)

# ------------------------------------------------------------------
# 2. Build a flat lookup table from the map
# ------------------------------------------------------------------
lookup <- stack(collapse_map)            # col 1: values (old terms), col 2: ind (new term)
names(lookup) <- c("term_canonical", "collapsed_term")
lookup$term_canonical  <- as.character(lookup$term_canonical)
lookup$collapsed_term  <- as.character(lookup$collapsed_term)

# ------------------------------------------------------------------
# 3. Apply the collapse
# ------------------------------------------------------------------
df <- df |>
  left_join(lookup, by = "term_canonical") |>
  mutate(
    term_canonical = coalesce(collapsed_term, term_canonical)
  ) |>
  select(-collapsed_term)

# ------------------------------------------------------------------
# 4. (Optional) Re-aggregate frequencies after collapsing
#    Uncomment if you want one row per collapsed term.
# ------------------------------------------------------------------
# df_collapsed <- df |>
#   group_by(term_canonical) |>
#   summarise(
#     term_freq = sum(term_freq),
#     doc_freq  = sum(doc_freq),
#     idf       = mean(idf),      # or re-compute from scratch
#     .groups   = "drop"
#   )

# ------------------------------------------------------------------
# 5. Inspect results
# ------------------------------------------------------------------
cat("Top 30 canonical terms after collapsing:\n")
print(
  df |>
    count(term_canonical, sort = TRUE) |>
    slice_head(n = 30)
)

# ------------------------------------------------------------------
# 6. Save
# ------------------------------------------------------------------
write.csv(df, "TermDocFreq_long_collapsed.csv", row.names = FALSE)
cat("\nSaved to TermDocFreq_long_collapsed.csv\n")


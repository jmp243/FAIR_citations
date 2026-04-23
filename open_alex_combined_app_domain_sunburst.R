# =============================================================================
# open_alex_combined_app.R
# Multi-page Shiny app combining:
#   - Countries explorer
#   - Journals & topics explorer
#   - Publication type explorer
#
# Prerequisites: run open_alex_topic_words_clean.R first so that
#   alex_doi_new, alex_doi_new_wide, country_dict, collapse_map,
#   and domain_stopwords are all in your environment.
# =============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(viridis)
library(DT)
library(tibble)
library(textmineR)
library(stopwords)
# connect to posit cloud connect
library(rsconnect)
library(countrycode)
library(readr)
library(openalexR)

# read in CSV of the works citing FAIR2016
citing_works <- read_csv("ten_yr_openalex_citation_corpus_2026-03-16.csv") # define to 3/15/2026

# rename display_name as title
citing_works$title <- citing_works$display_name

# remove http and other signifiers to make doi's compatible
citing_works <- citing_works %>% 
  mutate(doi_clean = str_remove(doi, "https://doi.org/"))
# remove the anti_dups from the larger dataset alex_doi_new
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


# -----------------------------------------------------------------------------
# Shared: collapsed term matching
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

pattern_lookup <- bind_rows(
  tibble(
    pattern        = names(collapse_map),
    collapsed_term = names(collapse_map)
  ),
  stack(collapse_map) %>%
    rename(collapsed_term = ind, pattern = values) %>%
    mutate(across(everything(), as.character))
)

match_collapsed <- function(topic_str, patterns, targets) {
  topic_lower <- tolower(topic_str)
  idx <- which(str_detect(topic_lower, paste0("\\b", patterns, "\\b")))[1]
  if (is.na(idx)) topic_str else targets[idx]
}

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
# Shared: date filter constants
# -----------------------------------------------------------------------------

DATE_MIN   <- as.Date("2016-03-15")
DATE_MAX   <- as.Date("2026-03-16")
EXCL_DOI   <- "10.1038/sdata.2016.18"

# -----------------------------------------------------------------------------
# Page 1 data: Countries
# -----------------------------------------------------------------------------

code_col <- intersect(c("code", "iso2", "iso2c", "country_code"), names(country_dict))
name_col <- intersect(c("name", "country_name", "country", "label"),  names(country_dict))

if (length(code_col) == 0) stop("country_dict must have a code column.")
country_dict_std <- if (length(name_col) == 0) {
  country_dict %>% rename(code = !!code_col[1]) %>% mutate(name = code)
} else {
  country_dict %>% rename(code = !!code_col[1], name = !!name_col[1])
}

country_codes <- names(alex_doi_new_wide)[str_detect(names(alex_doi_new_wide), "^[A-Z]{2}$")]

df_long <- alex_doi_new_wide %>%
  filter(publication_date >= DATE_MIN, publication_date <= DATE_MAX,
         doi_clean != EXCL_DOI) %>%
  mutate(publication_year = as.integer(publication_year)) %>%
  filter(!is.na(publication_year), publication_year > 2015) %>%
  select(publication_year, doi_clean, all_of(country_codes)) %>%
  pivot_longer(cols = all_of(country_codes),
               names_to = "country_code", values_to = "value") %>%
  filter(!is.na(value), value > 0) %>%
  left_join(country_dict_std, by = c("country_code" = "code")) %>%
  mutate(country_name = coalesce(name, country_code)) %>%
  select(publication_year, doi_clean, country_code, country_name, value)

summary_full <- df_long %>%
  group_by(publication_year, country_code, country_name) %>%
  summarise(count = n_distinct(doi_clean), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(year_total = sum(count)) %>%
  ungroup()

all_countries <- sort(unique(summary_full$country_name))

# -----------------------------------------------------------------------------
# Page 2 data: Journals & Domain / Field / Subfield
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
  # bring domain / field / subfield to the front (after title)
  relocate(domain,   .after = title) %>%
  relocate(field,    .after = domain) %>%
  relocate(subfield, .after = field) %>%
  filter(!is.na(journal_name_clean), !is.na(doi_clean),
         publication_date >= DATE_MIN, publication_date <= DATE_MAX,
         doi_clean != EXCL_DOI) %>%
  mutate(publication_year = as.integer(publication_year)) %>%
  group_by(publication_year, type, domain, field, subfield) %>%
  distinct(journal_name_clean, doi_clean, publication_year)

domain_choices   <- sort(unique(alex_doi_unique$domain))
field_choices    <- sort(unique(alex_doi_unique$field))
subfield_choices <- sort(unique(alex_doi_unique$subfield))

# -----------------------------------------------------------------------------
# Page 3 data: Publication Types
# -----------------------------------------------------------------------------

df_full <- alex_doi_new_wide %>%
  filter(publication_date >= DATE_MIN, publication_date <= DATE_MAX,
         doi_clean != EXCL_DOI) %>%
  mutate(publication_year = as.integer(publication_year)) %>%
  group_by(publication_year, type) %>%
  summarise(doi_count = n_distinct(doi_clean), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(year_total = sum(doi_count)) %>%
  ungroup()

all_types <- sort(unique(df_full$type))

# =============================================================================
# UI
# =============================================================================

ui <- navbarPage(
  title = "OpenAlex Explorer",
  theme = NULL,

  # ---------------------------------------------------------------------------
  # Tab 1: Countries
  # ---------------------------------------------------------------------------
  tabPanel(
    "Countries",
    sidebarLayout(
      sidebarPanel(
        sliderInput("c_year_range", "Publication year range:",
                    min = min(summary_full$publication_year),
                    max = max(summary_full$publication_year),
                    value = range(summary_full$publication_year),
                    step = 1, sep = ""),
        sliderInput("c_top_n", "Top N countries:",
                    min = 3, max = 40, value = 15),
        tags$hr(),
        selectizeInput("c_country_select", "Select countries:",
                       choices  = all_countries,
                       multiple = TRUE,
                       options  = list(placeholder     = "Search countries",
                                       plugins = list("remove_button"))),
        fluidRow(
          column(6, actionButton("c_select_all", "Select all")),
          column(6, actionButton("c_clear_all",  "Clear all"))
        ),
        tags$br(),
        radioButtons("c_country_mode", "Country selection mode:",
                     choices  = c("Top N only"      = "top",
                                  "Selected only"   = "selected",
                                  "Top N + Selected" = "both"),
                     selected = "top"),
        checkboxInput("c_include_other", "Include 'Other' group", value = TRUE),
        radioButtons("c_stack_mode", "Y-axis:",
                     choices = c("Counts"               = "count",
                                 "Percent (within year)" = "percent"),
                     selected = "count", inline = TRUE),
        tags$hr(),
        downloadButton("c_export_csv", "Export to CSV")
      ),
      mainPanel(
        plotlyOutput("c_stacked", height = "650px")
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # Tab 2: Journals & Domain / Field / Subfield
  # ---------------------------------------------------------------------------
  tabPanel(
    "Journals & Topics",
    sidebarLayout(
      sidebarPanel(
        sliderInput("j_top_n", "Top N Journals",
                    min = 5, max = 50, value = 20, step = 5),
        sliderInput("j_year_range", "Publication Year",
                    min   = min(alex_doi_unique$publication_year, na.rm = TRUE),
                    max   = max(alex_doi_unique$publication_year, na.rm = TRUE),
                    value = range(alex_doi_unique$publication_year, na.rm = TRUE),
                    sep   = ""),
        selectizeInput("j_domain_choice", "Domain (select one or more)",
                       choices  = domain_choices,
                       selected = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "All domains")),
        selectizeInput("j_field_choice", "Field (select one or more)",
                       choices  = field_choices,
                       selected = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "All fields")),
        selectizeInput("j_subfield_choice", "Subfield (select one or more)",
                       choices  = subfield_choices,
                       selected = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "All subfields")),
        tags$hr(),
        downloadButton("j_export_csv", "Export to CSV")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Top Journals",
                   plotOutput("j_journal_plot"),
                   DTOutput("j_journal_table")),
          tabPanel("Publications by Year",
                   plotOutput("j_year_plot")),
          tabPanel("Hierarchy",
                   plotlyOutput("j_sunburst_plot", height = "650px"))
        )
      )
    )
  ),
  # ---------------------------------------------------------------------------
  # Tab 3: Publication Types
  # ---------------------------------------------------------------------------
  tabPanel(
    "Publication Types",
    sidebarLayout(
      sidebarPanel(
        sliderInput("t_year_range", "Publication year range:",
                    min   = min(df_full$publication_year),
                    max   = max(df_full$publication_year),
                    value = range(df_full$publication_year),
                    step = 1, sep = ""),
        selectizeInput("t_type_select", "Select article types:",
                       choices  = all_types,
                       selected = all_types,
                       multiple = TRUE,
                       options  = list(plugins     = list("remove_button"),
                                       placeholder = "Select article types")),
        checkboxInput("t_include_other", "Include 'Other' group", value = FALSE),
        radioButtons("t_y_mode", "Y-axis:",
                     choices  = c("Counts"               = "count",
                                  "Percent (within year)" = "percent"),
                     selected = "count", inline = TRUE),
        tags$hr(),
        downloadButton("t_export_csv", "Export to CSV")
      ),
      mainPanel(
        plotlyOutput("t_stacked_plot", height = "650px")
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Tab 1: Countries
  # ---------------------------------------------------------------------------

  observeEvent(input$c_select_all, {
    updateSelectizeInput(session, "c_country_select", selected = all_countries)
  })
  observeEvent(input$c_clear_all, {
    updateSelectizeInput(session, "c_country_select", selected = character(0))
  })

  c_summary_filtered <- reactive({
    summary_full %>%
      filter(publication_year >= input$c_year_range[1],
             publication_year <= input$c_year_range[2])
  })

  c_topN_summary <- reactive({
    dat <- c_summary_filtered()

    top_tbl    <- dat %>%
      group_by(country_code, country_name) %>%
      summarise(total_count = sum(count), .groups = "drop") %>%
      arrange(desc(total_count))
    top_codes  <- head(top_tbl$country_code, input$c_top_n)

    selected_codes <- dat %>%
      filter(country_name %in% input$c_country_select) %>%
      pull(country_code) %>% unique()

    keep_codes <- switch(input$c_country_mode,
      "top"      = top_codes,
      "selected" = selected_codes,
      "both"     = union(top_codes, selected_codes)
    )

    dat2 <- dat %>%
      mutate(country_group = if_else(country_code %in% keep_codes,
                                     country_name, "Other"))
    if (!isTRUE(input$c_include_other))
      dat2 <- dat2 %>% filter(country_group != "Other")

    dat_grouped <- dat2 %>%
      group_by(publication_year, country_group) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      group_by(publication_year) %>%
      mutate(year_total = sum(count)) %>%
      ungroup()

    if (input$c_stack_mode == "percent") {
      dat_grouped %>%
        mutate(value   = if_else(year_total > 0, 100 * count / year_total, 0),
               y_title = "Percent of yearly total")
    } else {
      dat_grouped %>% mutate(value = count, y_title = "Count")
    }
  })

  output$c_export_csv <- downloadHandler(
    filename = function() paste0("country_summary_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(c_topN_summary(), file, row.names = FALSE)
  )

  output$c_stacked <- renderPlotly({
    dat       <- c_topN_summary()
    groups    <- unique(dat$country_group)
    color_map <- setNames(viridis(length(groups), option = "plasma"), groups)

    plot_ly(dat,
            x         = ~factor(publication_year),
            y         = ~value,
            type      = "bar",
            color     = ~country_group,
            colors    = color_map,
            hoverinfo = "text",
            text      = ~paste0("Year: ", publication_year, "<br>",
                                "Country: ", country_group, "<br>",
                                "Value: ", round(value, 2), "<br>",
                                "Year total: ", year_total)) %>%
      layout(barmode = "stack",
             xaxis   = list(title = "Publication Year"),
             yaxis   = list(title = unique(dat$y_title)),
             legend  = list(title = list(text = "Country")))
  })

  # ---------------------------------------------------------------------------
  # Tab 2: Journals & Topics
  # ---------------------------------------------------------------------------

  j_filtered <- reactive({
    df <- alex_doi_unique %>%
      filter(publication_year >= input$j_year_range[1],
             publication_year <= input$j_year_range[2])
    if (length(input$j_domain_choice) > 0)
      df <- df %>% filter(domain %in% input$j_domain_choice)
    if (length(input$j_field_choice) > 0)
      df <- df %>% filter(field %in% input$j_field_choice)
    if (length(input$j_subfield_choice) > 0)
      df <- df %>% filter(subfield %in% input$j_subfield_choice)
    df
  })

  output$j_export_csv <- downloadHandler(
    filename = function() paste0("journals_domain_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(j_filtered(), file, row.names = FALSE)
  )

  # Consistent domain colour palette shared across all Tab 2 plots
  j_domain_pal <- reactive({
    domains <- sort(unique(j_filtered()$domain))
    setNames(viridis(length(domains), option = "plasma"), domains)
  })

  j_counts <- reactive({
    # Top-N journals; break down by domain for stacked colouring
    top_journals <- j_filtered() %>%
      group_by(journal_name_clean) %>%
      summarise(n_unique_doi = n_distinct(doi_clean), .groups = "drop") %>%
      arrange(desc(n_unique_doi)) %>%
      slice_head(n = input$j_top_n) %>%
      pull(journal_name_clean)

    j_filtered() %>%
      filter(journal_name_clean %in% top_journals) %>%
      group_by(journal_name_clean, domain) %>%
      summarise(n_unique_doi = n_distinct(doi_clean), .groups = "drop")
  })

  output$j_journal_plot <- renderPlot({
    dat <- j_counts()
    pal <- j_domain_pal()

    # Order journals by total DOIs descending (bottom = largest on flipped axis)
    journal_order <- dat %>%
      group_by(journal_name_clean) %>%
      summarise(total = sum(n_unique_doi), .groups = "drop") %>%
      arrange(total) %>%
      pull(journal_name_clean)

    dat %>%
      mutate(journal_name_clean = factor(journal_name_clean, levels = journal_order)) %>%
      ggplot(aes(x = journal_name_clean, y = n_unique_doi, fill = domain)) +
      geom_col() +
      scale_fill_manual(values = pal, name = "Domain") +
      coord_flip() +
      labs(title = "Top Journals by Unique DOI", x = "Journal", y = "Unique DOI Count") +
      theme(legend.position = "bottom")
  })

  output$j_journal_table <- renderDT({
    datatable(j_counts(), options = list(pageLength = 10))
  })

  output$j_year_plot <- renderPlot({
    dat <- j_filtered() %>%
      distinct(doi_clean, publication_year, domain) %>%
      count(publication_year, domain)
    pal <- j_domain_pal()

    dat %>%
      ggplot(aes(x = publication_year, y = n, fill = domain)) +
      geom_col() +
      scale_fill_manual(values = pal, name = "Domain") +
      labs(title = "Publications by Year", x = "Year", y = "Unique DOIs") +
      theme(legend.position = "bottom")
  })

  output$j_sunburst_plot <- renderPlotly({

    sunburst_data <- alex_doi_new %>%
      filter(!is.na(domain),
             !is.na(primary_location.source.display_name)) %>%
      distinct(domain, field, subfield,
               primary_location.source.display_name) %>%
      count(domain, field, subfield, name = "n_unique_journals")

    domains <- sunburst_data %>%
      group_by(domain) %>%
      summarise(n = sum(n_unique_journals)) %>%
      transmute(ids = domain, labels = domain, parents = "", values = n)

    fields <- sunburst_data %>%
      group_by(domain, field) %>%
      summarise(n = sum(n_unique_journals), .groups = "drop") %>%
      transmute(ids     = paste(domain, field, sep = " - "),
                labels  = field,
                parents = domain,
                values  = n)

    subfields <- sunburst_data %>%
      transmute(ids     = paste(domain, field, subfield, sep = " - "),
                labels  = subfield,
                parents = paste(domain, field, sep = " - "),
                values  = n_unique_journals)

    sb <- bind_rows(domains, fields, subfields)

    plot_ly(
      sb,
      ids          = ~ids,
      labels       = ~labels,
      parents      = ~parents,
      values       = ~values,
      type         = "sunburst",
      branchvalues = "total"
    )
  })

  # ---------------------------------------------------------------------------
  # Tab 3: Publication Types
  # ---------------------------------------------------------------------------

  t_filtered <- reactive({
    dat <- df_full %>%
      filter(publication_year >= input$t_year_range[1],
             publication_year <= input$t_year_range[2]) %>%
      mutate(type_group = if_else(type %in% input$t_type_select, type, "Other"))

    if (!isTRUE(input$t_include_other))
      dat <- dat %>% filter(type_group != "Other")

    dat_grouped <- dat %>%
      group_by(publication_year, type_group) %>%
      summarise(doi_count = sum(doi_count), .groups = "drop") %>%
      group_by(publication_year) %>%
      mutate(year_total = sum(doi_count)) %>%
      ungroup()

    if (input$t_y_mode == "percent") {
      dat_grouped %>%
        mutate(value   = if_else(year_total > 0, 100 * doi_count / year_total, 0),
               y_title = "Percent of yearly total")
    } else {
      dat_grouped %>% mutate(value = doi_count, y_title = "Number of unique DOIs")
    }
  })

  output$t_stacked_plot <- renderPlotly({
    dat   <- t_filtered()
    types <- unique(dat$type_group)
    pal   <- setNames(viridis(length(types), option = "plasma"), types)

    plot_ly(dat,
            x         = ~factor(publication_year),
            y         = ~value,
            type      = "bar",
            color     = ~type_group,
            colors    = pal,
            text      = ~paste0("Year: ", publication_year, "<br>",
                                "Type: ", type_group, "<br>",
                                "DOIs: ", doi_count, "<br>",
                                "Year total: ", year_total),
            hoverinfo = "text") %>%
      layout(barmode = "stack",
             xaxis   = list(title = "Publication Year"),
             yaxis   = list(title = unique(dat$y_title)),
             legend  = list(title = list(text = "Article Type")))
  })

  output$t_export_csv <- downloadHandler(
    filename = function() paste0("doi_by_year_type_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(t_filtered(), file, row.names = FALSE)
  )
}

# =============================================================================
shinyApp(ui, server)

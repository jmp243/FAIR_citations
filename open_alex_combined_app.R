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

# -----------------------------------------------------------------------------
# Shared: collapsed term matching
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# Shared: date filter constants
# -----------------------------------------------------------------------------

DATE_MIN   <- as.Date("2016-03-15")
DATE_MAX   <- as.Date("2026-03-15")
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
# Page 2 data: Journals & Topics
# -----------------------------------------------------------------------------

alex_doi_unique <- alex_doi_new %>%
  mutate(
    journal_name = primary_location.source.display_name,
    topic        = primary_topic.display_name,
    doi_clean    = str_remove(doi, "https://doi.org/"),
    journal_name_clean = journal_name %>%
      str_replace("^\\d{4}\\s+", "") %>%
      str_replace_all("[[:cntrl:]]", "") %>%
      str_replace_all("[^[:alnum:] [:space:]]", "") %>%
      str_squish()
  ) %>%
  filter(!is.na(journal_name_clean), !is.na(doi_clean),
         publication_date >= DATE_MIN, publication_date <= DATE_MAX,
         doi_clean != EXCL_DOI) %>%
  mutate(
    publication_year = as.integer(publication_year),
    collapsed_term   = mapply(
      match_collapsed,
      topic,
      MoreArgs = list(
        patterns = pattern_lookup$pattern,
        targets  = pattern_lookup$collapsed_term
      )
    )
  ) %>%
  group_by(publication_year, type, collapsed_term) %>%
  distinct(journal_name_clean, doi_clean, publication_year)

collapsed_choices <- sort(unique(alex_doi_unique$collapsed_term))

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
  # Tab 2: Journals & Topics
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
        selectizeInput("j_topic_choice", "Topic (select one or more)",
                       choices  = collapsed_choices,
                       selected = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "All topics"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Top Journals",
                   plotOutput("j_journal_plot"),
                   DTOutput("j_journal_table")),
          tabPanel("Publications by Year",
                   plotOutput("j_year_plot"))
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
    if (length(input$j_topic_choice) > 0)
      df <- df %>% filter(collapsed_term %in% input$j_topic_choice)
    df
  })

  j_counts <- reactive({
    j_filtered() %>%
      group_by(journal_name_clean) %>%
      summarise(n_unique_doi = n_distinct(doi_clean), .groups = "drop") %>%
      arrange(desc(n_unique_doi)) %>%
      slice_head(n = input$j_top_n)
  })

  output$j_journal_plot <- renderPlot({
    j_counts() %>%
      ggplot(aes(x = reorder(journal_name_clean, n_unique_doi), y = n_unique_doi)) +
      geom_col() + coord_flip() +
      labs(title = "Top Journals by Unique DOI", x = "Journal", y = "Unique DOI Count")
  })

  output$j_journal_table <- renderDT({
    datatable(j_counts(), options = list(pageLength = 10))
  })

  output$j_year_plot <- renderPlot({
    j_filtered() %>%
      distinct(doi_clean, publication_year) %>%
      count(publication_year) %>%
      ggplot(aes(x = publication_year, y = n)) +
      geom_col() +
      labs(title = "Publications by Year", x = "Year", y = "Unique DOIs")
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

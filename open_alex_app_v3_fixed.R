# =============================================================================
# open_alex_app_v3.R
# FAIR Data Citation Explorer — Shiny App
#
# Tabs:
#   1. Countries         — citation counts by country over time
#   2. Journals & Topics — top journals filtered by domain/field/subfield/SJR
#   3. Discipline Hierarchy — static sunburst (domain > field > subfield)
#   4. Journal Trends    — track selected journals over time with SJR context
#   5. Publication Types — article type breakdown over time
#
# Data: load three pre-built CSVs (no raw API corpus needed):
#   alex_doi_new.csv         — core works + domain/field/subfield
#   alex_doi_new_journal.csv — core works + ScimagoJR 2025 merge
#   alex_doi_new_wide.csv    — core works + country indicator columns
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
library(readr)
library(scales)
library(countrycode)

# =============================================================================
# 0. Load CSVs
# =============================================================================

alex_doi_new         <- read_csv("output_data/alex_doi_new.csv",         show_col_types = FALSE)
alex_doi_new_journal <- read_csv("output_data/alex_doi_new_journal.csv", show_col_types = FALSE)
alex_doi_new_wide    <- read_csv("output_data/alex_doi_new_wide.csv",    show_col_types = FALSE)

# Normalise date & year columns across all three frames
fix_dates <- function(df) {
  df %>% mutate(
    publication_date = as.Date(publication_date),
    publication_year = as.integer(publication_year)
  )
}

alex_doi_new         <- fix_dates(alex_doi_new)
alex_doi_new_journal <- fix_dates(alex_doi_new_journal)
alex_doi_new_wide    <- fix_dates(alex_doi_new_wide)

# =============================================================================
# 1. Shared constants
# =============================================================================

DATE_MIN <- as.Date("2016-03-15")
DATE_MAX <- as.Date("2026-03-16")
EXCL_DOI <- "10.1038/sdata.2016.18"

QUARTILE_PAL <- c(
  Q1       = "#1a9641",
  Q2       = "#a6d96a",
  Q3       = "#fdae61",
  Q4       = "#d7191c",
  Unranked = "#bdbdbd"
)

# =============================================================================
# 2. ScimagoJR lookup  (one row per journal, best-ranked record)
# =============================================================================

scimago_lookup <- alex_doi_new_journal %>%
  select(
    journal_name    = primary_location.source.display_name,
    h_index         = `H index`,
    sjr_quartile    = `SJR Best Quartile`,
    sjr_rank        = Rank,
    cites_per_doc   = `Citations / Doc. (2years)`,
    categories      = Categories,
    areas           = Areas
  ) %>%
  filter(!is.na(journal_name)) %>%
  group_by(journal_name) %>%
  arrange(sjr_rank) %>%
  slice(1) %>%
  ungroup()

# =============================================================================
# 3. Tab 1 data — Countries
# =============================================================================

# Detect ISO-2 country columns (exactly 2 uppercase letters)
country_codes_cols <- names(alex_doi_new_wide)[str_detect(names(alex_doi_new_wide), "^[A-Z]{2}$")]

country_dict <- data.frame(
  code    = country_codes_cols,
  country = countrycode(country_codes_cols, origin = "iso2c",
                        destination = "country.name"),
  stringsAsFactors = FALSE
) %>%
  mutate(country = if_else(code == "XK", "Kosovo", country))

df_long <- alex_doi_new_wide %>%
  filter(publication_date >= DATE_MIN, publication_date <= DATE_MAX,
         doi_clean != EXCL_DOI,
         !is.na(publication_year), publication_year > 2015) %>%
  select(publication_year, doi_clean, all_of(country_codes_cols)) %>%
  pivot_longer(cols = all_of(country_codes_cols),
               names_to = "country_code", values_to = "value") %>%
  filter(!is.na(value), value > 0) %>%
  left_join(country_dict, by = c("country_code" = "code")) %>%
  mutate(country_name = coalesce(country, country_code)) %>%
  select(publication_year, doi_clean, country_code, country_name, value)

summary_full <- df_long %>%
  group_by(publication_year, country_code, country_name) %>%
  summarise(count = n_distinct(doi_clean), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(year_total = sum(count)) %>%
  ungroup()

all_countries <- sort(unique(summary_full$country_name))

# =============================================================================
# 4. Tab 2 data — Journals & Topics
# =============================================================================

alex_doi_unique <- alex_doi_new %>%
  filter(!is.na(primary_location.source.display_name),
         !is.na(doi_clean),
         publication_date >= DATE_MIN,
         publication_date <= DATE_MAX,
         doi_clean != EXCL_DOI) %>%
  mutate(
    journal_name_clean = primary_location.source.display_name %>%
      str_replace("^\\d{4}\\s+", "") %>%
      str_replace_all("[[:cntrl:]]", "") %>%
      str_replace_all("[^[:alnum:] [:space:]]", "") %>%
      str_squish()
  ) %>%
  # Attach SJR quartile for filtering
  left_join(
    scimago_lookup %>% select(journal_name, sjr_quartile),
    by = c("primary_location.source.display_name" = "journal_name")
  )

domain_choices   <- sort(na.omit(unique(alex_doi_unique$domain)))
field_choices    <- sort(na.omit(unique(alex_doi_unique$field)))
subfield_choices <- sort(na.omit(unique(alex_doi_unique$subfield)))

# =============================================================================
# 5. Tab 3 data — Discipline Hierarchy (pre-built; fully static)
# =============================================================================

sunburst_raw <- alex_doi_new %>%
  filter(!is.na(domain),
         !is.na(primary_location.source.display_name)) %>%
  distinct(domain, field, subfield, primary_location.source.display_name) %>%
  count(domain, field, subfield, name = "n_unique_journals")

sb_domains <- sunburst_raw %>%
  group_by(domain) %>%
  summarise(n = sum(n_unique_journals), .groups = "drop") %>%
  transmute(ids = domain, labels = domain, parents = "", values = n)

sb_fields <- sunburst_raw %>%
  group_by(domain, field) %>%
  summarise(n = sum(n_unique_journals), .groups = "drop") %>%
  transmute(ids     = paste(domain, field, sep = " - "),
            labels  = field,
            parents = domain,
            values  = n)

sb_subfields <- sunburst_raw %>%
  transmute(ids     = paste(domain, field, subfield, sep = " - "),
            labels  = subfield,
            parents = paste(domain, field, sep = " - "),
            values  = n_unique_journals)

sb_data <- bind_rows(sb_domains, sb_fields, sb_subfields)

# =============================================================================
# 6. Tab 4 data — Journal Trends
# =============================================================================

journal_year_counts <- alex_doi_new_journal %>%
  filter(!is.na(primary_location.source.display_name),
         publication_date >= DATE_MIN,
         publication_date <= DATE_MAX,
         doi_clean != EXCL_DOI) %>%
  distinct(doi_clean, publication_year, primary_location.source.display_name) %>%
  count(primary_location.source.display_name, publication_year, name = "n_papers") %>%
  rename(journal_name = primary_location.source.display_name)
  # NOTE: no SJR join here — scimago_lookup is attached once in jt_filtered()

journal_totals <- journal_year_counts %>%
  group_by(journal_name) %>%
  summarise(total_papers = sum(n_papers), .groups = "drop") %>%
  left_join(scimago_lookup, by = "journal_name") %>%
  arrange(desc(total_papers)) %>%
  mutate(
    quartile_label = if_else(!is.na(sjr_quartile),
                             as.character(sjr_quartile), "Unranked")
  )

# Selectize choice labels:  "Journal (N papers | Q1 | h=42)"
jt_choices <- setNames(
  journal_totals$journal_name,
  paste0(
    journal_totals$journal_name,
    " (", journal_totals$total_papers, " papers",
    if_else(!is.na(journal_totals$sjr_quartile),
            paste0(" | ", journal_totals$sjr_quartile), ""),
    if_else(!is.na(journal_totals$h_index),
            paste0(" | h=", journal_totals$h_index), ""),
    ")"
  )
)

jt_default  <- head(journal_totals$journal_name, 5)
jt_year_min <- min(journal_year_counts$publication_year, na.rm = TRUE)
jt_year_max <- max(journal_year_counts$publication_year, na.rm = TRUE)

# =============================================================================
# 7. Tab 5 data — Publication Types
# =============================================================================

df_pub_types <- alex_doi_new_wide %>%
  filter(publication_date >= DATE_MIN, publication_date <= DATE_MAX,
         doi_clean != EXCL_DOI) %>%
  group_by(publication_year, type) %>%
  summarise(doi_count = n_distinct(doi_clean), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(year_total = sum(doi_count)) %>%
  ungroup()

all_types <- sort(unique(df_pub_types$type))

# =============================================================================
# UI
# =============================================================================

ui <- navbarPage(
  title = "FAIR Citation Explorer",

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
        sliderInput("c_top_n", "Top N countries:", min = 3, max = 40, value = 15),
        tags$hr(),
        selectizeInput("c_country_select", "Pin additional countries:",
                       choices  = all_countries,
                       multiple = TRUE,
                       options  = list(placeholder = "Search countries…",
                                       plugins = list("remove_button"))),
        fluidRow(
          column(6, actionButton("c_select_all", "Select all")),
          column(6, actionButton("c_clear_all",  "Clear all"))
        ),
        tags$br(),
        radioButtons("c_country_mode", "Country selection mode:",
                     choices  = c("Top N only"       = "top",
                                  "Selected only"    = "selected",
                                  "Top N + Selected" = "both"),
                     selected = "top"),
        checkboxInput("c_include_other", "Include 'Other' group", value = TRUE),
        radioButtons("c_stack_mode", "Y-axis:",
                     choices  = c("Counts"               = "count",
                                  "Percent (within year)" = "percent"),
                     selected = "count", inline = TRUE),
        tags$hr(),
        downloadButton("c_export_csv", "Export to CSV")
      ),
      mainPanel(plotlyOutput("c_stacked", height = "650px"))
    )
  ),

  # ---------------------------------------------------------------------------
  # Tab 2: Journals & Topics
  # ---------------------------------------------------------------------------
  tabPanel(
    "Journals & Topics",
    sidebarLayout(
      sidebarPanel(
        sliderInput("j_top_n", "Top N journals:",
                    min = 5, max = 50, value = 20, step = 5),
        sliderInput("j_year_range", "Publication year range:",
                    min   = min(alex_doi_unique$publication_year, na.rm = TRUE),
                    max   = max(alex_doi_unique$publication_year, na.rm = TRUE),
                    value = range(alex_doi_unique$publication_year, na.rm = TRUE),
                    sep   = ""),
        selectizeInput("j_domain_choice", "Domain:",
                       choices  = domain_choices, selected = NULL, multiple = TRUE,
                       options  = list(placeholder = "All domains")),
        selectizeInput("j_field_choice", "Field:",
                       choices  = field_choices, selected = NULL, multiple = TRUE,
                       options  = list(placeholder = "All fields")),
        selectizeInput("j_subfield_choice", "Subfield:",
                       choices  = subfield_choices, selected = NULL, multiple = TRUE,
                       options  = list(placeholder = "All subfields")),
        selectInput("j_sjr_quartile", "SJR Best Quartile:",
                    choices  = c("All", "Q1", "Q2", "Q3", "Q4", "Unranked"),
                    selected = "All"),
        tags$hr(),
        downloadButton("j_export_csv", "Export to CSV")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Top Journals",
                   br(),
                   plotOutput("j_journal_plot", height = "550px"),
                   br(),
                   DTOutput("j_journal_table")),
          tabPanel("Publications by Year",
                   br(),
                   plotOutput("j_year_plot", height = "500px"))
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # Tab 3: Discipline Hierarchy  (static — no sidebar)
  # ---------------------------------------------------------------------------
  tabPanel(
    "Discipline Hierarchy",
    fluidPage(
      br(),
      helpText(
        "Static sunburst showing unique journals by OpenAlex domain > field > subfield.",
        "Click a segment to drill down; click the centre to return."
      ),
      plotlyOutput("j_sunburst_plot", height = "720px")
    )
  ),

  # ---------------------------------------------------------------------------
  # Tab 4: Journal Trends
  # ---------------------------------------------------------------------------
  tabPanel(
    "Journal Trends",
    sidebarLayout(
      sidebarPanel(
        width = 3,

        selectizeInput(
          "jt_journals",
          label    = "Select journals to track:",
          choices  = jt_choices,
          selected = jt_default,
          multiple = TRUE,
          options  = list(placeholder = "Search and add journals…",
                          plugins     = list("remove_button"),
                          maxItems    = 20)
        ),
        helpText("Labels: total FAIR papers | SJR quartile | h-index."),

        tags$hr(),

        selectInput("jt_quartile_filter", "Filter journal list to quartile:",
                    choices  = c("All", "Q1", "Q2", "Q3", "Q4", "Unranked"),
                    selected = "All"),
        helpText("Narrows the picker above without removing already-selected journals."),

        tags$hr(),

        sliderInput("jt_year_range", "Publication year range:",
                    min = jt_year_min, max = jt_year_max,
                    value = c(jt_year_min, jt_year_max),
                    step = 1, sep = ""),

        radioButtons("jt_chart_type", "Chart type:",
                     choices  = c("Line chart" = "line", "Stacked bar" = "bar"),
                     selected = "line", inline = TRUE),
        radioButtons("jt_y_mode", "Y-axis:",
                     choices  = c("Counts" = "count", "% of yearly total" = "percent"),
                     selected = "count", inline = TRUE),
        radioButtons("jt_color_by", "Colour by:",
                     choices  = c("Journal" = "journal", "SJR Quartile" = "quartile"),
                     selected = "journal", inline = TRUE),
        checkboxInput("jt_show_points", "Show data points on lines", value = TRUE),

        tags$hr(),
        downloadButton("jt_export_csv", "Export to CSV")
      ),

      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Trend Chart",
                   br(),
                   plotlyOutput("jt_trend_plot", height = "520px")),
          tabPanel("Journal Profiles",
                   br(),
                   helpText("ScimagoJR 2025 metadata for selected journals."),
                   DTOutput("jt_profile_table")),
          tabPanel("Impact vs. FAIR Papers",
                   br(),
                   helpText("Bubble size = total FAIR-citing papers. Colour = SJR quartile."),
                   plotlyOutput("jt_scatter_plot", height = "500px")),
          tabPanel("Data Table",
                   br(),
                   DTOutput("jt_data_table"))
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # Tab 5: Publication Types
  # ---------------------------------------------------------------------------
  tabPanel(
    "Publication Types",
    sidebarLayout(
      sidebarPanel(
        sliderInput("t_year_range", "Publication year range:",
                    min   = min(df_pub_types$publication_year),
                    max   = max(df_pub_types$publication_year),
                    value = range(df_pub_types$publication_year),
                    step = 1, sep = ""),
        selectizeInput("t_type_select", "Select article types:",
                       choices  = all_types, selected = all_types, multiple = TRUE,
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
      mainPanel(plotlyOutput("t_stacked_plot", height = "650px"))
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

  c_display <- reactive({
    dat <- c_summary_filtered()

    top_codes <- dat %>%
      group_by(country_code, country_name) %>%
      summarise(total_count = sum(count), .groups = "drop") %>%
      arrange(desc(total_count)) %>%
      head(input$c_top_n) %>%
      pull(country_code)

    selected_codes <- dat %>%
      filter(country_name %in% input$c_country_select) %>%
      pull(country_code) %>% unique()

    keep_codes <- switch(input$c_country_mode,
      top      = top_codes,
      selected = selected_codes,
      both     = union(top_codes, selected_codes)
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

  output$c_stacked <- renderPlotly({
    dat       <- c_display()
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

  output$c_export_csv <- downloadHandler(
    filename = function() paste0("country_summary_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(c_display(), file, row.names = FALSE)
  )

  # ---------------------------------------------------------------------------
  # Tab 2: Journals & Topics
  # ---------------------------------------------------------------------------

  j_filtered <- reactive({
    df <- alex_doi_unique %>%
      filter(publication_year >= input$j_year_range[1],
             publication_year <= input$j_year_range[2])

    if (length(input$j_domain_choice)   > 0) df <- df %>% filter(domain   %in% input$j_domain_choice)
    if (length(input$j_field_choice)    > 0) df <- df %>% filter(field    %in% input$j_field_choice)
    if (length(input$j_subfield_choice) > 0) df <- df %>% filter(subfield %in% input$j_subfield_choice)

    if (!is.null(input$j_sjr_quartile) && input$j_sjr_quartile != "All") {
      if (input$j_sjr_quartile == "Unranked") {
        df <- df %>% filter(is.na(sjr_quartile))
      } else {
        df <- df %>% filter(sjr_quartile == input$j_sjr_quartile)
      }
    }
    df
  })

  j_domain_pal <- reactive({
    domains <- sort(unique(j_filtered()$domain))
    setNames(viridis(length(domains), option = "plasma"), domains)
  })

  j_counts <- reactive({
    
    top_journals <- j_filtered() %>%
      group_by(journal_name_clean) %>%
      summarise(count = n_distinct(doi_clean), .groups = "drop") %>%
      arrange(desc(count)) %>%
      slice_head(n = input$j_top_n) %>%
      pull(journal_name_clean)
    
    j_filtered() %>%
      filter(journal_name_clean %in% top_journals) %>%
      group_by(journal_name_clean) %>%
      summarise(
        count = n_distinct(doi_clean),
        domain = paste(sort(unique(na.omit(domain))), collapse = "; "),
        .groups = "drop"
      ) %>%
      arrange(desc(count)) %>%
      rename(
        `Journal Name` = journal_name_clean,
        Domains = domain,
        Count = count
      )
  })
  # j_counts <- reactive({
  #   top_journals <- j_filtered() %>%
  #     group_by(journal_name_clean) %>%
  #     summarise(n_unique_doi = n_distinct(doi_clean), .groups = "drop") %>%
  #     arrange(desc(n_unique_doi)) %>%
  #     slice_head(n = input$j_top_n) %>%
  #     pull(journal_name_clean)
  # 
  #   j_filtered() %>%
  #     filter(journal_name_clean %in% top_journals) %>%
  #     group_by(journal_name_clean, domain) %>%
  #     summarise(n_unique_doi = n_distinct(doi_clean), .groups = "drop")
  # })
  
  j_plot_counts <- reactive({
    
    top_journals <- j_filtered() %>%
      group_by(journal_name_clean) %>%
      summarise(count = n_distinct(doi_clean), .groups = "drop") %>%
      arrange(desc(count)) %>%
      slice_head(n = input$j_top_n) %>%
      pull(journal_name_clean)
    
    j_filtered() %>%
      filter(journal_name_clean %in% top_journals) %>%
      group_by(journal_name_clean, domain) %>%
      summarise(
        count = n_distinct(doi_clean),
        .groups = "drop"
      )
  })
  
  
  output$j_journal_plot <- renderPlot({
    
    dat <- j_plot_counts()
    pal <- j_domain_pal()
    
    journal_order <- dat %>%
      group_by(journal_name_clean) %>%
      summarise(total = sum(count), .groups = "drop") %>%
      arrange(total) %>%
      pull(journal_name_clean)
    
    dat %>%
      mutate(
        journal_name_clean =
          factor(journal_name_clean, levels = journal_order)
      ) %>%
      ggplot(
        aes(
          x = journal_name_clean,
          y = count,
          fill = domain
        )
      ) +
      geom_col() +
      scale_fill_manual(values = pal, name = "Domain") +
      coord_flip() +
      labs(
        title = "Top Journals by Unique DOI",
        x = NULL,
        y = "Count"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  # output$j_journal_plot <- renderPlot({
  #   
  #   dat <- j_counts()
  #   
  #   dat %>%
  #     mutate(
  #       `Journal Name` = factor(
  #         `Journal Name`,
  #         levels = rev(`Journal Name`)
  #       )
  #     ) %>%
  #     ggplot(aes(x = `Journal Name`, y = Count)) +
  #     geom_col(fill = "#4C78A8") +
  #     coord_flip() +
  #     labs(
  #       title = "Top Journals by Unique DOI",
  #       x = NULL,
  #       y = "Count"
  #     ) +
  #     theme_minimal(base_size = 12)
  #   
  # })
  # 
  # output$j_journal_plot <- renderPlot({
  #   dat <- j_counts()
  #   pal <- j_domain_pal()
  # 
  #   journal_order <- dat %>%
  #     group_by(journal_name_clean) %>%
  #     summarise(total = sum(n_unique_doi), .groups = "drop") %>%
  #     arrange(total) %>%
  #     pull(journal_name_clean)
  # 
  #   dat %>%
  #     mutate(journal_name_clean = factor(journal_name_clean, levels = journal_order)) %>%
  #     ggplot(aes(x = journal_name_clean, y = n_unique_doi, fill = domain)) +
  #     geom_col() +
  #     scale_fill_manual(values = pal, name = "Domain") +
  #     coord_flip() +
  #     labs(title = "Top Journals by Unique DOI", x = NULL, y = "Unique DOI Count") +
  #     theme_minimal(base_size = 12) +
  #     theme(legend.position = "bottom")
  # })
  
  output$j_journal_table <- renderDT({
    datatable(
      j_counts(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # output$j_journal_table <- renderDT({
  #   datatable(j_counts(), rownames = FALSE,
  #             options = list(pageLength = 10, scrollX = TRUE))
  # })

  output$j_year_plot <- renderPlot({
    dat <- j_filtered() %>%
      distinct(doi_clean, publication_year, domain) %>%
      count(publication_year, domain)
    pal <- j_domain_pal()

    year_totals <- dat %>%
      group_by(publication_year) %>%
      summarise(year_total = sum(n), .groups = "drop")

    dat %>%
      ggplot(aes(x = publication_year, y = n, fill = domain)) +
      geom_col() +
      geom_text(data        = year_totals,
                mapping     = aes(x = publication_year, y = year_total, label = year_total),
                inherit.aes = FALSE, vjust = -0.4, size = 3.5) +
      scale_x_continuous(breaks = function(lims) seq(ceiling(lims[1]), floor(lims[2]), by = 1)) +
      scale_fill_manual(values = pal, name = "Domain") +
      labs(title = "Publications by Year", x = "Year", y = "Unique DOIs") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$j_export_csv <- downloadHandler(
    filename = function() paste0("journals_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(j_filtered(), file, row.names = FALSE)
  )

  # ---------------------------------------------------------------------------
  # Tab 3: Discipline Hierarchy  (static — rendered once)
  # ---------------------------------------------------------------------------

  output$j_sunburst_plot <- renderPlotly({
    plot_ly(
      sb_data,
      ids          = ~ids,
      labels       = ~labels,
      parents      = ~parents,
      values       = ~values,
      type         = "sunburst",
      branchvalues = "total",
      hoverinfo    = "label+value+percent parent"
    ) %>%
      layout(margin = list(t = 10, b = 10, l = 10, r = 10))
  })

  # ---------------------------------------------------------------------------
  # Tab 4: Journal Trends
  # ---------------------------------------------------------------------------

  # Update selectize picker when quartile filter changes
  observe({
    qf <- input$jt_quartile_filter

    filt <- if (qf == "All") {
      journal_totals
    } else if (qf == "Unranked") {
      journal_totals %>% filter(quartile_label == "Unranked")
    } else {
      journal_totals %>% filter(sjr_quartile == qf)
    }

    new_choices <- setNames(
      filt$journal_name,
      paste0(filt$journal_name,
             " (", filt$total_papers, " papers",
             if_else(!is.na(filt$sjr_quartile),
                     paste0(" | ", filt$sjr_quartile), ""),
             if_else(!is.na(filt$h_index),
                     paste0(" | h=", filt$h_index), ""),
             ")")
    )

    # Preserve currently-selected journals even if outside new filter
    current_sel <- isolate(input$jt_journals)
    valid_sel   <- intersect(current_sel, filt$journal_name)

    updateSelectizeInput(session, "jt_journals",
                         choices  = new_choices,
                         selected = valid_sel,
                         server   = TRUE)
  })

  # Paper counts for selected journals, year range applied, gaps filled
  jt_filtered <- reactive({
    req(input$jt_journals)

    journal_year_counts %>%
      filter(journal_name %in% input$jt_journals,
             publication_year >= input$jt_year_range[1],
             publication_year <= input$jt_year_range[2]) %>%
      complete(
        journal_name     = input$jt_journals,
        publication_year = seq(input$jt_year_range[1], input$jt_year_range[2]),
        fill             = list(n_papers = 0)
      ) %>%
      left_join(
        scimago_lookup %>%
          mutate(quartile_label = if_else(!is.na(sjr_quartile),
                                          as.character(sjr_quartile), "Unranked")),
        by = "journal_name"
      )
  })

  # Compute display values (count or % share)
  jt_display <- reactive({
    dat <- jt_filtered()

    if (input$jt_y_mode == "percent") {
      grand <- journal_year_counts %>%
        filter(publication_year >= input$jt_year_range[1],
               publication_year <= input$jt_year_range[2]) %>%
        group_by(publication_year) %>%
        summarise(grand_total = sum(n_papers), .groups = "drop")

      dat <- dat %>%
        left_join(grand, by = "publication_year") %>%
        mutate(value   = if_else(grand_total > 0, 100 * n_papers / grand_total, 0),
               y_label = "% of all FAIR-citing papers that year")
    } else {
      dat <- dat %>%
        mutate(value = n_papers, y_label = "FAIR-citing papers (unique DOIs)")
    }
    dat
  })

  # Colour palette (per journal or per quartile)
  jt_pal <- reactive({
    color_by <- input$jt_color_by
    if (color_by == "quartile") {
      QUARTILE_PAL
    } else {
      journals <- sort(unique(jt_display()$journal_name))
      n        <- length(journals)
      cols     <- if (n <= 10) hue_pal()(n) else viridis(n, option = "turbo")
      setNames(cols, journals)
    }
  })

  output$jt_trend_plot <- renderPlotly({
    dat      <- jt_display()
    req(nrow(dat) > 0)
    pal      <- jt_pal()
    y_label  <- unique(dat$y_label)
    color_by <- input$jt_color_by
    journals <- sort(unique(dat$journal_name))

    if (input$jt_chart_type == "line") {
      p <- plot_ly()
      for (j in journals) {
        d   <- dat %>% filter(journal_name == j)
        col <- if (color_by == "quartile") {
          pal[coalesce(unique(d$quartile_label), "Unranked")]
        } else {
          pal[j]
        }
        if (is.na(col) || length(col) == 0) col <- "#999999"

        p <- p %>%
          add_trace(
            data      = d,
            x         = ~publication_year,
            y         = ~value,
            name      = j,
            type      = "scatter",
            mode      = if (isTRUE(input$jt_show_points)) "lines+markers" else "lines",
            line      = list(color = col, width = 2.5),
            marker    = list(color = col, size = 7),
            hoverinfo = "text",
            text      = ~paste0(
              "<b>", journal_name, "</b><br>",
              "Year: ", publication_year, "<br>",
              "Papers: ", n_papers,
              if_else(!is.na(sjr_quartile), paste0("<br>Quartile: ", sjr_quartile), ""),
              if_else(!is.na(h_index),      paste0("<br>H-index: ",  h_index),      ""),
              if (input$jt_y_mode == "percent") paste0("<br>Share: ", round(value, 2), "%") else ""
            )
          )
      }
      p %>% layout(
        xaxis     = list(title = "Publication Year", tickmode = "linear", dtick = 1),
        yaxis     = list(title = y_label),
        legend    = list(title = list(text = "Journal")),
        hovermode = "x unified"
      )
    } else {
      plot_ly(dat,
              x         = ~factor(publication_year),
              y         = ~value,
              color     = if (color_by == "quartile") ~quartile_label else ~journal_name,
              colors    = pal,
              type      = "bar",
              hoverinfo = "text",
              text      = ~paste0(
                "<b>", journal_name, "</b><br>",
                "Year: ", publication_year, "<br>",
                "Papers: ", n_papers,
                if_else(!is.na(sjr_quartile), paste0("<br>Quartile: ", sjr_quartile), ""),
                if_else(!is.na(h_index),      paste0("<br>H-index: ",  h_index),      "")
              )) %>%
        layout(
          barmode = "stack",
          xaxis   = list(title = "Publication Year"),
          yaxis   = list(title = y_label),
          legend  = list(title = list(
            text = if (color_by == "quartile") "SJR Quartile" else "Journal"
          ))
        )
    }
  })

  output$jt_profile_table <- renderDT({
    req(input$jt_journals)

    journal_totals %>%
      filter(journal_name %in% input$jt_journals) %>%
      arrange(desc(total_papers)) %>%
      transmute(
        Journal            = journal_name,
        `FAIR Papers`      = total_papers,
        `SJR Quartile`     = quartile_label,
        `H-Index`          = h_index,
        `SJR Rank`         = sjr_rank,
        `Cites/Doc (2yr)`  = round(cites_per_doc, 2),
        Categories         = categories,
        Areas              = areas
      ) %>%
      datatable(rownames = FALSE,
                options  = list(pageLength = 20, scrollX = TRUE)) %>%
      formatStyle(
        "SJR Quartile",
        backgroundColor = styleEqual(
          c("Q1",      "Q2",      "Q3",      "Q4",      "Unranked"),
          c("#c8e6c9", "#f9fbe7", "#fff3e0", "#ffebee", "#f5f5f5")
        )
      )
  })

  output$jt_scatter_plot <- renderPlotly({
    req(input$jt_journals)

    scatter_dat <- journal_totals %>%
      filter(journal_name %in% input$jt_journals) %>%
      mutate(
        quartile_label = coalesce(quartile_label, "Unranked"),
        h_index_num    = as.numeric(h_index)
      )

    plot_ly(scatter_dat,
            x         = ~h_index_num,
            y         = ~total_papers,
            size      = ~total_papers,
            color     = ~quartile_label,
            colors    = QUARTILE_PAL,
            type      = "scatter",
            mode      = "markers",
            marker    = list(opacity = 0.75, sizemode = "diameter",
                             line    = list(width = 1, color = "white")),
            hoverinfo = "text",
            text      = ~paste0(
              "<b>", journal_name, "</b><br>",
              "H-index: ",      coalesce(as.character(h_index), "N/A"), "<br>",
              "SJR Quartile: ", quartile_label, "<br>",
              "SJR Rank: ",     coalesce(as.character(sjr_rank), "N/A"), "<br>",
              "FAIR Papers: ",  total_papers, "<br>",
              "Cites/Doc: ",    coalesce(as.character(round(cites_per_doc, 1)), "N/A")
            )) %>%
      layout(
        xaxis  = list(title = "H-Index (ScimagoJR 2025)"),
        yaxis  = list(title = "Total FAIR-Citing Papers"),
        legend = list(title = list(text = "SJR Quartile"))
      )
  })

  output$jt_data_table <- renderDT({
    jt_display() %>%
      transmute(
        Journal           = journal_name,
        Year              = publication_year,
        `FAIR Papers`     = n_papers,
        `Display Value`   = round(value, 2),
        `SJR Quartile`    = coalesce(as.character(sjr_quartile), "Unranked"),
        `H-Index`         = h_index,
        `SJR Rank`        = sjr_rank
      ) %>%
      arrange(Journal, Year) %>%
      datatable(filter   = "top", rownames = FALSE,
                options  = list(pageLength = 15, scrollX = TRUE)) %>%
      formatStyle(
        "SJR Quartile",
        backgroundColor = styleEqual(
          c("Q1",      "Q2",      "Q3",      "Q4",      "Unranked"),
          c("#c8e6c9", "#f9fbe7", "#fff3e0", "#ffebee", "#f5f5f5")
        )
      )
  })

  output$jt_export_csv <- downloadHandler(
    filename = function() paste0("journal_trends_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(
        jt_display() %>%
          transmute(journal          = journal_name,
                    year             = publication_year,
                    fair_paper_count = n_papers,
                    display_value    = round(value, 2),
                    sjr_quartile     = coalesce(as.character(sjr_quartile), "Unranked"),
                    h_index,
                    sjr_rank,
                    cites_per_doc    = round(cites_per_doc, 2)),
        file, row.names = FALSE
      )
    }
  )

  # ---------------------------------------------------------------------------
  # Tab 5: Publication Types
  # ---------------------------------------------------------------------------

  t_filtered <- reactive({
    dat <- df_pub_types %>%
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
    filename = function() paste0("pub_types_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(t_filtered(), file, row.names = FALSE)
  )

}

# =============================================================================
shinyApp(ui, server)

library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(DT)
library(textmineR)
library(stopwords)

# -----------------------------------------------------------------------------
# Data prep
# NOTE: lookup, domain_stopwords, and alex_doi_new must already be in your
#       environment (sourced from open_alex_topic_words_clean.R).
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
  filter(
    !is.na(journal_name_clean),
    !is.na(doi_clean),
    publication_date >= as.Date("2016-03-15"),
    publication_date <= as.Date("2026-03-15"),
    doi_clean != "10.1038/sdata.2016.18"
  ) %>%
  mutate(publication_year = as.integer(publication_year)) %>%
  # Join collapsed_term from the lookup built in open_alex_topic_words_clean.R
  left_join(lookup, by = c("topic" = "term_canonical")) %>%
  mutate(
    collapsed_term = coalesce(collapsed_term, topic)
  ) %>%
  group_by(publication_year, type, collapsed_term) %>%
  distinct(journal_name_clean, doi_clean, publication_year)

# -----------------------------------------------------------------------------
# TermDocFreq on collapsed_term (not raw topic)
# -----------------------------------------------------------------------------

dtm_topic <- CreateDtm(
  doc_vec      = alex_doi_unique$collapsed_term,
  doc_names    = alex_doi_unique$doi_clean,
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

tf_mat_topic <- TermDocFreq(dtm_topic)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- fluidPage(

  titlePanel("Journal & Topic Explorer"),

  sidebarLayout(
    sidebarPanel(

      sliderInput(
        "top_n",
        "Top N Journals",
        min   = 5,
        max   = 50,
        value = 20,
        step  = 5
      ),

      sliderInput(
        "year_range",
        "Publication Year",
        min   = min(alex_doi_unique$publication_year, na.rm = TRUE),
        max   = max(alex_doi_unique$publication_year, na.rm = TRUE),
        value = range(alex_doi_unique$publication_year, na.rm = TRUE),
        sep   = ""
      ),

      selectInput(
        "topic_choice",
        "Topic (collapsed)",
        choices  = c("All", sort(unique(alex_doi_unique$collapsed_term))),
        selected = "All"
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Top Journals",
          plotOutput("journal_plot"),
          DTOutput("journal_table")
        ),
        tabPanel(
          "Publications by Year",
          plotOutput("year_plot")
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output) {

  filtered_data <- reactive({
    df <- alex_doi_unique %>%
      filter(
        publication_year >= input$year_range[1],
        publication_year <= input$year_range[2]
      )

    if (input$topic_choice != "All") {
      df <- df %>% filter(collapsed_term == input$topic_choice)
    }

    df
  })

  journal_counts <- reactive({
    filtered_data() %>%
      group_by(journal_name_clean) %>%
      summarise(
        n_unique_doi = n_distinct(doi_clean),
        .groups = "drop"
      ) %>%
      arrange(desc(n_unique_doi)) %>%
      slice_head(n = input$top_n)
  })

  output$journal_plot <- renderPlot({
    journal_counts() %>%
      ggplot(aes(
        x = reorder(journal_name_clean, n_unique_doi),
        y = n_unique_doi
      )) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Top Journals by Unique DOI",
        x     = "Journal",
        y     = "Unique DOI Count"
      )
  })

  output$journal_table <- renderDT({
    datatable(
      journal_counts(),
      options = list(pageLength = 10)
    )
  })

  output$year_plot <- renderPlot({
    filtered_data() %>%
      distinct(doi_clean, publication_year) %>%
      count(publication_year) %>%
      ggplot(aes(x = publication_year, y = n)) +
      geom_col() +
      labs(
        title = "Publications by Year",
        x     = "Year",
        y     = "Unique DOIs"
      )
  })
}

shinyApp(ui, server)

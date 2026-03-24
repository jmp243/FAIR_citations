library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(viridis)

# -------------------------------
# Standardize dictionary columns
# -------------------------------
code_col <- intersect(c("code", "iso2", "iso2c", "country_code"), names(country_dict))
name_col <- intersect(c("name", "country_name", "country", "label"), names(country_dict))

if (length(code_col) == 0) stop("country_dict must have a code column.")
if (length(name_col) == 0) {
  country_dict_std <- country_dict %>%
    rename(code = !!code_col[1]) %>%
    mutate(name = code)
} else {
  country_dict_std <- country_dict %>%
    rename(code = !!code_col[1], name = !!name_col[1])
}

# -------------------------------
# Identify country columns
# -------------------------------
country_codes <- names(alex_doi_new_wide)[
  str_detect(names(alex_doi_new_wide), "^[A-Z]{2}$")
]

# -------------------------------
# Long form + attach country names
# -------------------------------
df_long <- alex_doi_new_wide %>%
  filter(publication_date >= as.Date("2016-03-15"),
         publication_date <= as.Date("2026-03-15")) %>%
  filter(doi_clean!= "10.1038/sdata.2016.18") %>% 
  mutate(publication_year = as.integer(publication_year)) %>%
  filter(!is.na(publication_year), publication_year > 2015) %>%
  select(publication_year, doi_clean, all_of(country_codes)) %>%
  pivot_longer(
    cols = all_of(country_codes),
    names_to = "country_code",
    values_to = "value"
  ) %>%
  filter(!is.na(value), value > 0) %>%
  left_join(country_dict_std, by = c("country_code" = "code")) %>%
  mutate(country_name = dplyr::coalesce(name, country_code)) %>%
  select(publication_year, doi_clean, country_code, country_name, value)

# -------------------------------
# Summarize by year × country
# -------------------------------
summary_full <- df_long %>%
  group_by(publication_year, country_code, country_name) %>%
  summarise(count = n_distinct(doi_clean), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(year_total = sum(count)) %>%
  ungroup()

all_countries <- sort(unique(summary_full$country_name))

# -------------------------------
# UI
# -------------------------------
ui <- fluidPage(
  titlePanel("Interactive Top-N Countries by Year"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(
        "year_range", "Publication year range:",
        min = min(summary_full$publication_year),
        max = max(summary_full$publication_year),
        value = c(min(summary_full$publication_year),
                  max(summary_full$publication_year)),
        step = 1, sep = ""
      ),
      
      sliderInput("top_n", "Top N countries:",
                  min = 3, max = 40, value = 15),
      
      tags$hr(),
      
      selectizeInput(
        "country_select",
        "Select countries:",
        choices = all_countries,
        multiple = TRUE,
        options = list(
          placeholder = "Search countries",
          plugins = list("remove_button")
        )
      ),
      
      fluidRow(
        column(6, actionButton("select_all", "Select all")),
        column(6, actionButton("clear_all", "Clear all"))
      ),
      
      radioButtons(
        "country_mode",
        "Country selection mode:",
        choices = c(
          "Top N only" = "top",
          "Selected only" = "selected",
          "Top N + Selected" = "both"
        ),
        selected = "top"
      ),
      
      checkboxInput("include_other", "Include 'Other' group", value = TRUE),
      
      radioButtons(
        "stack_mode", "Y-axis:",
        choices = c("Counts" = "count",
                    "Percent (within year)" = "percent"),
        selected = "count",
        inline = TRUE
      ),
      
      tags$hr(),
      
      downloadButton("export_csv", "Export to CSV")
    ),
    
    mainPanel(
      plotlyOutput("stacked", height = "650px")
    )
  )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {
  
  # ---- Select all / clear all ----
  observeEvent(input$select_all, {
    updateSelectizeInput(session, "country_select",
                         selected = all_countries)
  })
  
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "country_select",
                         selected = character(0))
  })
  
  summary_filtered <- reactive({
    summary_full %>%
      filter(publication_year >= input$year_range[1],
             publication_year <= input$year_range[2])
  })
  
  topN_summary <- reactive({
    
    dat <- summary_filtered()
    
    top_tbl <- dat %>%
      group_by(country_code, country_name) %>%
      summarise(total_count = sum(count), .groups = "drop") %>%
      arrange(desc(total_count))
    
    top_codes <- head(top_tbl$country_code, input$top_n)
    
    selected_codes <- dat %>%
      filter(country_name %in% input$country_select) %>%
      pull(country_code) %>%
      unique()
    
    keep_codes <- switch(
      input$country_mode,
      "top"      = top_codes,
      "selected" = selected_codes,
      "both"     = union(top_codes, selected_codes)
    )
    
    dat2 <- dat %>%
      mutate(country_group = if_else(
        country_code %in% keep_codes,
        country_name,
        "Other"
      ))
    
    if (!isTRUE(input$include_other)) {
      dat2 <- dat2 %>% filter(country_group != "Other")
    }
    
    dat_grouped <- dat2 %>%
      group_by(publication_year, country_group) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      group_by(publication_year) %>%
      mutate(year_total = sum(count)) %>%
      ungroup()
    
    if (input$stack_mode == "percent") {
      dat_grouped <- dat_grouped %>%
        mutate(
          value = if_else(year_total > 0, 100 * count / year_total, 0),
          y_title = "Percent of yearly total"
        )
    } else {
      dat_grouped <- dat_grouped %>%
        mutate(
          value = count,
          y_title = "Count"
        )
    }
    
    dat_grouped
  })
  
  # ---- CSV Export ----
  output$export_csv <- downloadHandler(
    filename = function() {
      paste0(
        "country_summary_",
        input$country_mode, "_",
        input$stack_mode, "_",
        Sys.Date(),
        ".csv"
      )
    },
    content = function(file) {
      write.csv(
        topN_summary(),
        file,
        row.names = FALSE
      )
    }
  )
  
  output$stacked <- renderPlotly({
    
    dat <- topN_summary()
    groups <- unique(dat$country_group)
    
    pal <- viridis(length(groups), option = "plasma")
    color_map <- setNames(pal, groups)
    
    plot_ly(
      dat,
      x = ~factor(publication_year),
      y = ~value,
      type = "bar",
      color = ~country_group,
      colors = color_map,
      hoverinfo = "text",
      text = ~paste0(
        "Year: ", publication_year, "<br>",
        "Country: ", country_group, "<br>",
        "Value: ", round(value, 2), "<br>",
        "Year total: ", year_total
      )
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Publication Year"),
        yaxis = list(title = unique(dat$y_title)),
        legend = list(title = list(text = "Country"))
      )
  })
}

shinyApp(ui, server)

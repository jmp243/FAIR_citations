library(shiny)
library(dplyr)
library(plotly)
library(viridis)

# -------------------------------
# Prepare data
# -------------------------------
df_full <- alex_doi_new_wide %>%
  filter(publication_date >= as.Date("2016-03-15"),
         publication_date <= as.Date("2026-03-15")) %>%
  mutate(publication_year = as.integer(publication_year)) %>%
  filter(doi_clean!= "10.1038/sdata.2016.18") %>% 
  # filter(!is.na(publication_year), publication_year > 2015) %>%
  group_by(publication_year, type) %>%
  summarise(doi_count = n_distinct(doi_clean), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(year_total = sum(doi_count)) %>%
  ungroup()

all_types <- sort(unique(df_full$type))

# -------------------------------
# UI
# -------------------------------
ui <- fluidPage(
  titlePanel("Unique DOIs by Publication Year and Type"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(
        "year_range",
        "Publication year range:",
        min = min(df_full$publication_year),
        max = max(df_full$publication_year),
        value = c(min(df_full$publication_year),
                  max(df_full$publication_year)),
        step = 1,
        sep = ""
      ),
      
      selectizeInput(
        "type_select",
        "Select article types:",
        choices = all_types,
        selected = all_types,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Select article types"
        )
      ),
      
      checkboxInput("include_other", "Include 'Other' group", value = FALSE),
      
      radioButtons(
        "y_mode",
        "Y-axis:",
        choices = c(
          "Counts" = "count",
          "Percent (within year)" = "percent"
        ),
        selected = "count",
        inline = TRUE
      ),
      
      tags$hr(),
      downloadButton("export_csv", "Export to CSV")
    ),
    
    mainPanel(
      plotlyOutput("stacked_plot", height = "650px")
    )
  )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    dat <- df_full %>%
      filter(
        publication_year >= input$year_range[1],
        publication_year <= input$year_range[2]
      )
    
    dat <- dat %>%
      mutate(
        type_group = if_else(
          type %in% input$type_select,
          type,
          "Other"
        )
      )
    
    if (!isTRUE(input$include_other)) {
      dat <- dat %>% filter(type_group != "Other")
    }
    
    dat_grouped <- dat %>%
      group_by(publication_year, type_group) %>%
      summarise(doi_count = sum(doi_count), .groups = "drop") %>%
      group_by(publication_year) %>%
      mutate(year_total = sum(doi_count)) %>%
      ungroup()
    
    if (input$y_mode == "percent") {
      dat_grouped <- dat_grouped %>%
        mutate(
          value = if_else(year_total > 0, 100 * doi_count / year_total, 0),
          y_title = "Percent of yearly total"
        )
    } else {
      dat_grouped <- dat_grouped %>%
        mutate(
          value = doi_count,
          y_title = "Number of unique DOIs"
        )
    }
    
    dat_grouped
  })
  
  output$stacked_plot <- renderPlotly({
    
    dat <- filtered_data()
    types <- unique(dat$type_group)
    
    pal <- viridis(length(types), option = "plasma")
    names(pal) <- types
    
    plot_ly(
      dat,
      x = ~factor(publication_year),
      y = ~value,
      type = "bar",
      color = ~type_group,
      colors = pal,
      text = ~paste0(
        "Year: ", publication_year, "<br>",
        "Type: ", type_group, "<br>",
        "DOIs: ", doi_count, "<br>",
        "Year total: ", year_total
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "Publication Year"),
        yaxis = list(title = unique(dat$y_title)),
        legend = list(title = list(text = "Article Type"))
      )
  })
  
  output$export_csv <- downloadHandler(
    filename = function() {
      paste0(
        "doi_by_year_type_",
        input$y_mode, "_",
        Sys.Date(),
        ".csv"
      )
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)


library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(viridis)

# -------------------------------
# Standardize dictionary columns
# -------------------------------
# Detect columns for code and name
code_col <- intersect(c("code", "iso2", "iso2c", "country_code"), names(country_dict))
name_col <- intersect(c("name", "country_name", "country", "label"), names(country_dict))

if (length(code_col) == 0) stop("country_dict must have a code column (e.g., 'code', 'iso2', 'iso2c', 'country_code').")
if (length(name_col) == 0) {
  # If there's no name column, create one equal to code
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
country_codes <- names(alex_doi_new_wide)[str_detect(names(alex_doi_new_wide), "^[A-Z]{2}$")]
# If you must use positions:
# country_cols <- names(alex_doi_new_wide)[32:219]

# -------------------------------
# Long form + attach country names
# -------------------------------
df_long <- alex_doi_new_wide %>%
  mutate(publication_year = as.integer(publication_year)) %>%
  filter(!is.na(publication_year), publication_year > 2015) %>%
  select(publication_year, doi_clean, all_of(country_cols)) %>%
  pivot_longer(
    cols = all_of(country_codes),
    names_to = "country_code",
    values_to = "value"
  ) %>%
  filter(!is.na(value), value > 0) %>%
  left_join(country_dict_std, by = c("country_code" = "code")) %>%
  # coalesce to be safe even if name is missing
  mutate(country_name = dplyr::coalesce(name, country_code)) %>%
  select(publication_year, doi_clean, country_code, country_name, value)

# -------------------------------
# Summarize by year × country
# -------------------------------
# If wide values are 0/1 indicators per DOI:
summary_full <- df_long %>%
  group_by(publication_year, country_code, country_name) %>%
  summarise(count = n_distinct(doi_clean), .groups = "drop") %>%
  group_by(publication_year) %>%
  mutate(year_total = sum(count)) %>%
  ungroup()

# If wide values are counts, use:
# summary_full <- df_long %>%
#   group_by(publication_year, country_code, country_name) %>%
#   summarise(count = sum(value, na.rm = TRUE), .groups = "drop") %>%
#   group_by(publication_year) %>%
#   mutate(year_total = sum(count)) %>%
#   ungroup()

# -------------------------------
# Shiny UI
# -------------------------------
ui <- fluidPage(
  titlePanel("Interactive Top-N Countries by Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year_range", "Publication year range:",
        min = min(summary_full$publication_year, na.rm = TRUE),
        max = max(summary_full$publication_year, na.rm = TRUE),
        value = c(min(summary_full$publication_year, na.rm = TRUE),
                  max(summary_full$publication_year, na.rm = TRUE)),
        step = 1, sep = ""
      ),
      sliderInput("top_n", "Top N countries:", min = 3, max = 40, value = 15, step = 1),
      checkboxInput("include_other", "Include 'Other' group", value = TRUE),
      radioButtons(
        "stack_mode", "Y-axis:",
        choices = c("Counts" = "count", "Percent (within year)" = "percent"),
        selected = "count", inline = TRUE
      )
    ),
    mainPanel(
      plotlyOutput("stacked", height = "650px")
    )
  )
)

# -------------------------------
# Shiny server
# -------------------------------
server <- function(input, output, session) {
  
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
    
    dat2 <- dat %>%
      mutate(country_group = if_else(country_code %in% top_codes, country_name, "Other"))
    
    if (!isTRUE(input$include_other)) {
      dat2 <- dat2 %>% filter(country_group != "Other")
    }
    
    dat_grouped <- dat2 %>%
      group_by(publication_year, country_group) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      group_by(publication_year) %>%
      mutate(year_total = sum(count)) %>%
      ungroup()
    
    if (identical(input$stack_mode, "percent")) {
      dat_grouped <- dat_grouped %>%
        mutate(value = if_else(year_total > 0, 100 * count / year_total, 0),
               y_title = "Percent of yearly total",
               hover_count = paste0(count, " (", round(value, 1), "%)"))
    } else {
      dat_grouped <- dat_grouped %>%
        mutate(value = count,
               y_title = "Count",
               hover_count = as.character(count))
    }
    
    country_order <- dat_grouped %>%
      group_by(country_group) %>%
      summarise(total = sum(value), .groups = "drop") %>%
      arrange(desc(total)) %>%
      pull(country_group)
    
    dat_grouped %>%
      mutate(country_group = factor(country_group, levels = c(setdiff(country_order, "Other"), "Other")))
  })
  
  output$stacked <- renderPlotly({
    dat <- topN_summary()
    groups <- levels(dat$country_group)
    pal <- viridis(length(groups), option = "plasma")
    color_map <- setNames(pal, groups)
    
    plot_ly(
      dat,
      x = ~factor(publication_year),
      y = ~value,
      type = "bar",
      color = ~country_group,
      colors = color_map,
      text = ~paste0(
        "Year: ", publication_year, "<br>",
        "Country: ", country_group, "<br>",
        ifelse(unique(dat$y_title)[1] == "Percent of yearly total", "Percent: ", "Count: "),
        paste0(hover_count), "<br>",
        "Year total: ", year_total
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "stack",
        title = paste0("Top ", input$top_n, " Countries by Year",
                       ifelse(isTRUE(input$include_other), " (+ Other)", "")),
        xaxis = list(title = "Publication Year"),
        yaxis = list(title = unique(dat$y_title)[1]),
        legend = list(title = list(text = "Country"))
      )
  })
}

shinyApp(ui, server)
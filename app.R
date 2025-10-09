# -----------------------------
# Posit CS Dashboard - app.R
# -----------------------------

# Load packages used by the app
library(shiny)
library(bslib)
library(thematic)
library(tidyverse)   # dplyr, ggplot2, tidyr, etc.
library(readr)
library(lubridate)
library(scales)

# Stop using the demo data
# source("setup.R")

# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()

# -----------------------------
# Load your CSV
# -----------------------------
accounts <- readr::read_csv("data/accounts.csv", show_col_types = FALSE) |>
  mutate(
    renewal_date = as.Date(renewal_date),
    quarter = if_else(!is.na(renewal_date),
                      paste0(lubridate::year(renewal_date), "-Q", lubridate::quarter(renewal_date)),
                      NA_character_),
    risk = factor(as.character(risk),
                  levels = c("None","Low","Medium","High","Critical"),
                  ordered = TRUE)
  )

# -----------------------------
# UI
# -----------------------------
ui <- page_sidebar(

  # Set CSS theme
  theme = bs_theme(
    bootswatch = "darkly",
    base_font    = font_google("Open Sans"),
    heading_font = font_google("Open Sans"),
    bg = "#222222",
    fg = "#86C7ED",
    success = "#86C7ED"
  ),

  # Title
  title = "Posit Customer Success, FSI Martineja Coats BoB",

  # Sidebar (labels already updated)
  sidebar = sidebar(
    title = "Select a segment of data to view",
    class = "bg-secondary",

    # NOTE: choices are filled dynamically in server()
    selectInput("industry",   "Account Executive", choices = NULL, selected = "", multiple = TRUE),
    selectInput("propensity", "Risk Level",
                choices = c("None","Low","Medium","High","Critical"),
                selected = "", multiple = TRUE),
    selectInput("contract",   "Renewal Quarter", choices = NULL, selected = "", multiple = TRUE),

    "This Customer Success Dashboard Provides High-Level Account Overviews for CSM BoB",
    tags$img(src = "logo.png", width = "100%", height = "auto")
  ),

  # Body layout (keeps your original IDs)
  layout_columns(
    card(card_header("Renewal ARR by Quarter"),
         plotOutput("line")),
    card(card_header("ARR Risk by Quarter"),
         plotOutput("bar")),

    value_box(title = "Total # Accounts",
              value = textOutput("recommended_eval"),
              theme_color = "secondary"),
    value_box(title = "Total ARR $",
              value = textOutput("number_of_customers"),
              theme_color = "secondary"),
    value_box(title = "At Risk ARR $",
              value = textOutput("High Risk and Above $"),
              theme_color = "secondary"),

    card(card_header("Detailed Account View"),
         tableOutput("table")),

    col_widths = c(8, 4, 4, 4, 4, 12),
    row_heights = c(4, 1.5, 3)
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {

  # Populate filter choices from data
  observe({
    updateSelectInput(session, "industry",
      choices = sort(unique(accounts$ae)), selected = character(0))
    updateSelectInput(session, "contract",
      choices = sort(na.omit(unique(accounts$quarter))), selected = character(0))
  })

  # Filtered dataset (drives all outputs)
  filtered <- reactive({
    df <- accounts
    if (length(input$industry))   df <- df |> filter(ae %in% input$industry)
    if (length(input$propensity)) df <- df |> filter(as.character(risk) %in% input$propensity)
    if (length(input$contract))   df <- df |> filter(quarter %in% input$contract)
    df
  })

  # KPIs (re-using your existing output IDs)
  # Total # Accounts
  output$recommended_eval <- renderText(
    scales::comma(dplyr::n_distinct(filtered()$account))
  )

  # Total ARR $
  output$number_of_customers <- renderText(
    scales::dollar(sum(filtered()$arr, na.rm = TRUE))
  )

  # At Risk ARR $ (Medium and above)
  output$`High Risk and Above $` <- renderText(
    scales::dollar(sum(filtered()$arr[filtered()$risk %in% c("Medium","High","Critical")], na.rm = TRUE))
  )

  # Line: Renewal ARR by Quarter (Total vs At-Risk)
  line_df <- reactive({
    filtered() |>
      group_by(quarter) |>
      summarise(
        `Total ARR`   = sum(arr, na.rm = TRUE),
        `At-Risk ARR` = sum(arr[risk %in% c("Medium","High","Critical")], na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(c(`Total ARR`, `At-Risk ARR`),
                          names_to = "metric", values_to = "value") |>
      arrange(quarter)
  })

  output$line <- renderPlot({
    ggplot(line_df(), aes(x = quarter, y = value, color = metric, group = metric)) +
      geom_line() + geom_point() +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = NULL, y = "ARR $", color = NULL) +
      theme_minimal(base_size = 12)
  })

  # Bar: ARR Risk by Quarter (stacked buckets)
  bar_df <- reactive({
    filtered() |>
      mutate(bucket = case_when(
        risk == "None" ~ "None",
        risk %in% c("Low","Medium") ~ "Low/Medium",
        TRUE ~ "High/Critical"
      )) |>
      group_by(quarter, bucket) |>
      summarise(arr = sum(arr, na.rm = TRUE), .groups = "drop")
  })

  output$bar <- renderPlot({
    ggplot(bar_df(), aes(x = quarter, y = arr, fill = bucket)) +
      geom_col() +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = NULL, y = "ARR $", fill = "Risk") +
      theme_minimal(base_size = 12)
  })

  # Detailed Account View (shows all rows/cols from filtered data)
  output$table <- renderTable({
    filtered() |> arrange(desc(arr))
  }, digits = 0)
}

# -----------------------------
# Run app
# -----------------------------
shinyApp(ui = ui, server = server)


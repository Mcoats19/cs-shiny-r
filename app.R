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
library(DT)
library(htmltools)

# ---- Chart palettes to match the example ----
risk_levels <- c("None","Low","Medium","High","Critical")
risk_pal    <- viridisLite::viridis(length(risk_levels), direction = -1)
names(risk_pal) <- risk_levels

# Line colors: Total ARR (teal) + At-Risk ARR (gold)
line_pal <- c("Total ARR" = "#2CB1BC", "At-Risk ARR" = "#F2A900")

# Pretty display labels for column headers (Title Case + keep acronyms)
pretty_labels <- function(nms) {
  lab <- gsub("_", " ", nms)
  lab <- tools::toTitleCase(lab)
  for (a in c("AE","ARR","ID","URL","API","KPI","ROI")) {
    lab <- gsub(paste0("\\b", tools::toTitleCase(a), "\\b"), a, lab)
  }
  lab
}

# Stop using the demo data
# source("setup.R")



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
     DTOutput("table")),

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

# --- Line: Renewal ARR by Quarter (dark panel + fixed colors) ---
output$line <- renderPlot({
  ggplot(line_df(), aes(x = quarter, y = value, color = metric, group = metric)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = line_pal) +                  # fixed teal/gold
    scale_y_continuous(labels = scales::dollar,
                       breaks = scales::breaks_pretty(5)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(x = NULL, y = "ARR $", color = NULL, title = NULL) +
    theme(                                                  # no theme_minimal()
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(face = "bold")
    )
})

# --- Bar: ARR Risk by Quarter (dark panel + viridis risk palette) ---
output$bar <- renderPlot({
  df <- filtered() %>%
    filter(!is.na(quarter)) %>%
    mutate(
      bucket = factor(as.character(risk), levels = risk_levels, ordered = TRUE)
    ) %>%
    group_by(quarter, bucket) %>%
    summarise(arr = sum(arr, na.rm = TRUE), .groups = "drop") %>%
    arrange(quarter, bucket) %>%
    mutate(quarter = factor(quarter, levels = sort(unique(quarter))))

  validate(need(nrow(df) > 0, "No data for current filters"))

  ggplot(df, aes(x = quarter, y = arr, fill = bucket)) +
    geom_col() +                                             # stacked (keep)
    scale_fill_manual(values = risk_pal,
                      limits = risk_levels, drop = FALSE) +  # fixed Risk colors
    scale_y_continuous(labels = scales::dollar,
                       breaks = scales::breaks_pretty(5)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(x = NULL, y = "ARR $", fill = "Risk", title = NULL) +
    theme(                                                  # no theme_minimal()
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.ticks.x = element_blank()
    )
})



# Detailed Account View — clickable accounts that open a details modal
output$table <- renderDT({
  df <- filtered() |>
    arrange(desc(arr))

# Make the Account column clickable; we keep a clean label but add a data-id
df <- df |>
  mutate(
    account_link = sprintf(
      '<a href="#" class="acct-link" data-acct="%s">%s</a>',
      htmlEscape(account), htmlEscape(account)
    )
  ) |>
  # put the clickable account first, then the rest (drop original 'account' to avoid dup)
  relocate(account_link, .before = everything()) |>
  select(-account) |>
  rename(account = account_link)

# NEW: pretty display labels for the table headers
col_labs <- pretty_labels(names(df))

DT::datatable(
  df,
  colnames = col_labs,
  escape = FALSE,                # allow HTML for the link
  rownames = FALSE,
  options = list(pageLength = 10, scrollX = TRUE),
  callback = JS(
    "table.on('click', 'a.acct-link', function(e){",
    "  e.preventDefault();",
    "  var acct = this.getAttribute('data-acct');",
    "  Shiny.setInputValue('account_clicked', acct, {priority: 'event'});",
    "});"
  )
)

}, server = FALSE)

# When a user clicks an account, show a modal with all fields/values for that row
observeEvent(input$account_clicked, {
  req(input$account_clicked)
  acct <- input$account_clicked

  # Use the pre-filtered data and grab the clicked row
  row <- filtered() |> dplyr::filter(account == acct) |> dplyr::slice(1)
  req(nrow(row) == 1)

  nms    <- names(row)
  labels <- pretty_labels(nms)   # <-- Title Case, keeps AE/ARR, removes underscores

  # Build key/value rows with friendly labels
  kv <- lapply(seq_along(nms), function(i) {
    nm <- nms[i]
    lab <- labels[i]
    val <- row[[nm]][1]

    # light formatting
    if (inherits(val, "Date")) {
      val <- format(val, "%Y-%m-%d")
    } else if (is.logical(val)) {
      val <- if (isTRUE(val)) "TRUE" else if (isFALSE(val)) "FALSE" else ""
    } else {
      val <- ifelse(is.na(val), "", as.character(val))
    }

    htmltools::tags$tr(
      htmltools::tags$th(style = "white-space: nowrap; padding-right:12px;", lab),
      htmltools::tags$td(style = "word-break: break-word;", val)
    )
  })

  shiny::showModal(shiny::modalDialog(
    title = paste("Account details —", acct),
    size = "l",
    easyClose = TRUE,
    footer = shiny::modalButton("Close"),
    htmltools::tags$div(
      style = "max-height:60vh; overflow:auto;",
      htmltools::tags$table(class = "table table-sm table-striped", kv)
    )
  ))
})
}


# -----------------------------
# Run app
# -----------------------------
shinyApp(ui = ui, server = server)

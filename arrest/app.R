# --------------------------- LOADING AND INTRO --------------------------------

# Loading libraries
library(shiny)
library(arrow)
library(janitor)
library(tidyverse)
library(DT)
library(digest)
library(openssl)
library(bslib)
library(readxl)

# # Defining data
# arrest_data <- read_excel("2025-ICLI-00019_2024-ICFO-39357_ERO Admin Arrests_LESA-STU_FINAL_unprotected.xlsx", skip = 5) %>%
#   clean_names() %>%
#   mutate(
#     apprehension_date = as.Date(apprehension_date, format = "yyyy-mm-dd"),
#     departed_date = as.Date(departed_date),
#     apprehension_quarter = paste0(year(apprehension_date), "Q", quarter(apprehension_date)),
#     final_order_date = as.Date(final_order_date),
#     departed_arrest = as.numeric(difftime(departed_date, apprehension_date, units = "days")),
#     departed_arrest_cat = case_when(
#       is.na(departed_arrest) ~ "Unknown",
#       departed_arrest < 3 ~ "Short (<3 days)",
#       departed_arrest < 14 ~ "Medium (3–13 days)",
#       departed_arrest < 90 ~ "Long (2 weeks–3 months)",
#       departed_arrest >= 90 ~ "Extended (3+ months)"
#     ),
#     apprehension_state = str_to_title(apprehension_state),
#     departure_country = str_to_title(departure_country),
#     final_order_yes_no = str_to_title(final_order_yes_no),
#     apprehension_site_landmark = str_to_title(apprehension_site_landmark),
#     citizenship_country = str_to_title(citizenship_country),
#     third_country = case_when(
#       is.na(citizenship_country) | is.na(departure_country) ~ "NA",
#       citizenship_country == departure_country ~ "No",
#       citizenship_country != departure_country ~ "Yes",
#       TRUE ~ NA
#     )
#   ) %>%
#   mutate(across(where(is.character), trimws)) %>%
#   mutate(across(where(is.character), ~ ifelse(is.na(.) | . == "" | . == "NA", "None Reported", .)))
# 
# write_csv(arrest_data, "arrest_file.csv")

arrest_data <- read_parquet("arrest.parquet")

# Defining variables to use
selectable_vars <- c(
  "apprehension_quarter",
  "apprehension_state",
  "apprehension_aor",
  "final_program",
  "apprehension_method",
  "apprehension_criminality",
  "case_status",
  "case_category",
  "departed_date",
  "departure_country",
  "final_order_yes_no",
  "birth_year",
  "citizenship_country",
  "gender",
  "apprehension_site_landmark",
  "departed_arrest_cat",
  "third_country"
)

# Friendly labels for the above variables
friendly_labels <- c(
  apprehension_quarter = "Apprehension Quarter",
  apprehension_state = "Apprehension State",
  apprehension_aor = "Area of Responsibility",
  final_program = "Final Program",
  apprehension_method = "Apprehension Method",
  apprehension_criminality = "Apprehension Criminality",
  case_status = "Case Status",
  case_category = "Case Category",
  departed_date = "Departed Date",
  departure_country = "Departure Country",
  final_order_yes_no = "Final Order (Yes/No)",
  birth_year = "Birth Year",
  citizenship_country = "Citizenship Country",
  gender = "Gender",
  apprehension_site_landmark = "Apprehension Site/Landmark",
  departed_arrest_cat = "Time between Apprehension and Departure (categorical)",
  third_country = "Third Country Deportation"
)

# Defining counts
count_type_options <- c(
  "Total Arrests" = "total",
  "Unique People" = "people"
)

# ----------------------------- CREATING UI ------------------------------------

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    base_font = font_google("Roboto"),
    heading_font = font_google("Roboto Condensed")
  ),
  
  ## ------------------------------ CSS ------------------------------------------
  
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto&family=Roboto+Condensed:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body {
        background-color: #eef1f5;
        font-family: 'Roboto', sans-serif;
        color: #2c3e50;
        margin: 0;
        padding: 0;
      }

      h1, h2, h3, h4, h5, h6, .title-panel {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 700;
        color: #130F54;
      }

      .title-panel {
        text-align: center;
        margin: 40px 0 20px 0;
      }

      .filter-subpanel {
        background: #eef1f5;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 8px;
        border: 1px solid #ddd;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.05);
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        height: 100%;
      }

      .filter-panel {
        background: white;
        padding: 25px;
        border-radius: 10px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.07);
        margin-bottom: 30px;
      }

      .filter-row {
        display: flex;
        gap: 15px;
      }

      .selection-info {
        margin: 15px 0 25px 0;
        font-size: 16px;
        color: #2c3e50;
        font-weight: 600;
        padding-left: 5px;
      }

      .selection-info span {
        color: #2980b9;
        font-weight: 700;
      }

      .shiny-input-container {
        margin-bottom: 20px;
      }

      .form-control, .selectize-input, .radio label, .dataTables_wrapper {
        font-size: 15px;
      }

      .download-btn {
        background-color: #0055AA;
        color: white;
        border: none;
        padding: 10px 20px;
        font-weight: 600;
        font-size: 15px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }

      .radio label {
        margin-bottom: 6px;
      }

      .shiny-bound-output {
        display: inline-block;
      }

      .filter-panel > .row {
        display: flex;
        flex-wrap: wrap;
      }

      .filter-subpanel {
        flex: 1 1 0;
        min-width: 0;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        margin-bottom: 20px;
        border-radius: 8px;
        border: 1px solid #ddd;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.05);
        background: #eef1f5;
        padding: 20px;
      }

      body {
        font-size: 16px;
      }

      .form-control, .selectize-input, .radio label, .dataTables_wrapper {
        font-size: 16px;
      }

      h1 {
        font-size: 40px !important;
      }

      h3 {
        font-size: 25px !important;
      }

      .help-block {
        font-size: 16px;
      }
      
      table.dataTable.my-bordered-table {
        border: 1px solid #d9edf7 !important;
        border-collapse: collapse;
      }
      table.dataTable.my-bordered-table th,
      table.dataTable.my-bordered-table td {
        border: 1px solid #d9edf7 !important;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.05);
      }
  
    "))
  ),
  
  ## ----------------------------- TITLE -----------------------------------------
  
  tags$head(
  ),
  
  # Your custom styled titlePanel
  titlePanel(
    div(
      class = "title-panel",
      tags$h1(
        style = "font-size: 36px; margin-bottom: 5px;",
        tags$i(class = "fas fa-database", style = "color:#31708f; margin-right: 10px;"),
        "ICE Arrests"
      )
    )
  ),
  
  ## -------------------------- SELECT PANEL -------------------------------------
  
  div(class = "filter-panel",
      helpText("Choose variables and filtering options to explore the arrest data."),
      fluidRow(
        column(4,
               div(class = "filter-subpanel",
                   div(
                     h5("Variable Selection"),
                     helpText("Select the main variables you want to explore. These define how the data will be grouped in the summary table.")
                   ),
                   selectInput(
                     "row_var", 
                     "Variable 1 (Rows):", 
                     choices = setNames(names(friendly_labels)[order(friendly_labels)], 
                                        friendly_labels[order(friendly_labels)]), 
                     selected = "apprehension_state"
                   ),
                   selectInput(
                     "col_var", 
                     "Variable 2 (Columns):", 
                     choices = c("None" = "None", 
                                 setNames(names(friendly_labels)[order(friendly_labels)], 
                                          friendly_labels[order(friendly_labels)])), 
                     selected = "None"
                   )
               )
        ),
        column(4,
               div(class = "filter-subpanel",
                   div(
                     h5("Counting"),
                     helpText("Choose how values should be counted in the table. You can display total counts, percentages, or other metrics.")
                   ),
                   radioButtons("count_method", "Count by:", 
                                choices = count_type_options, selected = "total"),
                   checkboxInput("show_percent", "Show Percentages", value = FALSE)
               )
        ),
        column(4,
               div(class = "filter-subpanel",
                   div(
                     h5("Filtering"),
                     helpText("Apply optional filters to narrow the dataset based on date or case status.")
                   ),
                   checkboxInput("filter_by_date", "Filter by Apprehension Date?", value = FALSE),
                   conditionalPanel(
                     condition = "input.filter_by_date == true",
                     dateRangeInput("bookin_date_range", "Apprehension Date Range:",
                                    start = "2023-01-01",
                                    end = max(arrest_data$apprehension_date, na.rm = TRUE),
                                    format = "yyyy-mm-dd"
                     )
                   ),
                   checkboxInput("non_active", "Inactive Cases", value = FALSE)
               )
        )
      )
  ),
  
  ## ------------------------------ RESULTS --------------------------------------
  
  fluidRow(
    column(8,
           h3("Tabulated Result"),
           div(class = "selection-info",
               "Selected Row Variable: ", span(textOutput("row_var_label", inline = TRUE)), br(),
               "Selected Column Variable: ", span(textOutput("col_var_label", inline = TRUE)), br(),
               "Counting Method: ", span(textOutput("count_method_label", inline = TRUE))
           )
    ),
    column(4, align = "right",
           br(),
           downloadButton("download_table", "Download Summary as CSV", class = "download-btn")
    )
  ),
  
  div(
    style = "width: 100%;",
    DTOutput("summary_table")
  ),
  
  ## ------------------------------- FOOTER --------------------------------------
  
  
)





# -------------------------------- SERVER --------------------------------------

server <- function(input, output, session) {
  
  # Prevent same variable in row and column
  observeEvent(input$row_var, {
    choices_col <- c("None" = "None",
                     setNames(
                       names(friendly_labels)[order(friendly_labels)][
                         names(friendly_labels)[order(friendly_labels)] != input$row_var
                       ],
                       friendly_labels[order(friendly_labels)][
                         names(friendly_labels)[order(friendly_labels)] != input$row_var
                       ]
                     )
    )
    
    updateSelectInput(session, "col_var", choices = choices_col,
                      selected = if (input$col_var == input$row_var) "None" else input$col_var)
  })
  
  # Filtering logic
  apply_filters <- function(data) {
    if (input$filter_by_date && !is.null(input$bookin_date_range)) {
      data <- data %>% filter(
        apprehension_date >= input$bookin_date_range[1],
        apprehension_date <= input$bookin_date_range[2]
      )
    }
    if (input$non_active) {
      data <- data %>% filter(case_status != "ACTIVE")
    }
    data
  }
  
  # Define distinct ID column for 'people'
  get_distinct_col <- function() {
    if (input$count_method == "people") {
      "unique_identifier"
    } else {
      NULL
    }
  }
  
  # Generate summary table
  generate_summary <- function(data) {
    row_var <- input$row_var
    col_var <- input$col_var
    distinct_col <- get_distinct_col()
    
    if (is.null(distinct_col)) {
      if (col_var == "None") {
        result <- data %>%
          count(.data[[row_var]], name = "Count") %>%
          arrange(-Count)
        
        if (input$show_percent) {
          result <- result %>%
            mutate(Percent = round(100 * Count / sum(Count), 2))
        }
      } else {
        result <- data %>%
          tabyl(.data[[row_var]], .data[[col_var]]) %>%
          arrange(desc(rowSums(across(-1))))
        
        if (input$show_percent) {
          result[-1] <- lapply(result[-1], function(col) {
            round(100 * col / sum(col, na.rm = TRUE), 2)
          })
        }
      }
    } else {
      result <- data %>%
        group_by(across(all_of(c(row_var, if (col_var != "None") col_var)))) %>%
        summarise(Count = n_distinct(.data[[distinct_col]]), .groups = "drop")
      
      if (col_var != "None") {
        result <- result %>%
          mutate(!!col_var := replace_na(.data[[col_var]], "Missing")) %>%
          pivot_wider(names_from = all_of(col_var), values_from = Count, values_fill = 0)
      }
      
      if (col_var == "None" && input$show_percent) {
        result <- result %>%
          mutate(Percent = round(100 * Count / sum(Count), 2))
      }
      
      if (col_var != "None" && input$show_percent) {
        result[-1] <- lapply(result[-1], function(col) {
          round(100 * col / sum(col, na.rm = TRUE), 2)
        })
      }
    }
    
    result
  }
  
  # Render summary table
  output$summary_table <- renderDT({
    req(input$row_var)
    
    data <- apply_filters(arrest_data)
    summary_table <- generate_summary(data)
    
    count_columns <- names(summary_table)[-1]
    breaks <- quantile(unlist(summary_table[count_columns]), probs = seq(0.05, 0.95, 0.05), na.rm = TRUE)
    n_colors <- length(breaks) + 1
    
    colors <- rgb(
      red = seq(255, 0, length.out = n_colors),
      green = seq(255, 85, length.out = n_colors),
      blue = seq(255, 170, length.out = n_colors),
      maxColorValue = 255
    )
    
    # Friendly names
    names(summary_table) <- ifelse(
      names(summary_table) %in% names(friendly_labels),
      friendly_labels[names(summary_table)],
      names(summary_table)
    )
    
    datatable(
      summary_table,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = "_all", className = "dt-left", width = "250px")
        ),
        headerCallback = JS("
          function(thead) {
            $(thead).find('th').css({
              'font-weight': 'bold',
              'font-family': 'Roboto Condensed',
              'font-size': '20px',
              'white-space': 'nowrap'
            });
          }
        ")
      ),
      class = "stripe hover compact nowrap my-bordered-table"
    ) %>%
      formatStyle(columns = count_columns, backgroundColor = styleInterval(breaks, colors))
  })
  
  # CSV Download
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("detention_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$row_var)
      data <- apply_filters(arrest_data)
      summary_table <- isolate(generate_summary(data))
      write.csv(summary_table, file, row.names = FALSE)
    }
  )
  
  # Dynamic labels
  output$row_var_label <- renderText({ friendly_labels[[input$row_var]] })
  output$col_var_label <- renderText({
    if (input$col_var == "None") "None" else friendly_labels[[input$col_var]]
  })
  output$count_method_label <- renderText({
    names(count_type_options)[count_type_options == input$count_method]
  })
}


# Running app
shinyApp(ui, server)


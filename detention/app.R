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

detention_data <- read_csv("detention.csv")

# Defining variables to use
selectable_vars <- c(
  "detention_facility",
  "detention_release_reason",
  "stay_release_reason",
  "religion",
  "gender",
  "marital_status",
  "ethnicity",
  "entry_status",
  "felon",
  "case_status",
  "case_category",
  "final_order_yes_no",
  "case_threat_level",
  "book_in_criminality",
  "final_charge",
  "departure_country",
  "detention_length_cat",
  "state",
  "detention_quarter"
)

# Creating friendly variable names
friendly_labels <- c(
  detention_facility = "Detention Facility",
  detention_release_reason = "Detention Release Reason",
  stay_release_reason = "Stay Release Reason",
  religion = "Religion",
  gender = "Gender",
  marital_status = "Marital Status",
  ethnicity = "Ethnicity",
  entry_status = "Entry Status",
  felon = "Felon Status",
  case_status = "Case Status",
  case_category = "Case Category",
  final_order_yes_no = "Final Order (Yes/No)",
  case_threat_level = "Case Threat Level",
  book_in_criminality = "Book-in Criminality",
  final_charge = "Final Charge",
  departure_country = "Departure Country",
  detention_length_cat = "Detention Length (Categorical)",
  state = "Detention Facility State",
  detention_quarter = "Book-in Quarter"
)

# Defining counts
count_type_options <- c(
  "Total Detentions" = "total",
  "Unique People" = "people",
  "Unique Stays" = "stays",
  "Median Detention Length" = "median"
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
        "ICE Detentions"
      ),
      
    )
  ),
  
  
  ## -------------------------- DESCRIPTION --------------------------------------

  
  ## -------------------------- SELECT PANEL -------------------------------------
  
  div(class = "filter-panel",
      helpText("Choose variables and filtering options to explore the detention data."),
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
                     selected = "state"
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
                   checkboxInput("filter_by_date", "Filter by Book-In Date?", value = FALSE),
                   conditionalPanel(
                     condition = "input.filter_by_date == true",
                     dateRangeInput("bookin_date_range", "Book-In Date Range:",
                                    start = "2023-09-01",
                                    end = max(detention_data$book_in_date_time, na.rm = TRUE),
                                    min = min(detention_data$book_in_date_time, na.rm = TRUE),
                                    max = max(detention_data$book_in_date_time, na.rm = TRUE),
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
  
  
  ## --------------------------- NO SELECT SAME ----------------------------------
  
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
  
  
  
  ## ------------------------------ HELPERS --------------------------------------
  
  
  # Applying filters
  apply_filters <- function(data) {
    if (input$filter_by_date && !is.null(input$bookin_date_range)) {
      data <- data %>% filter(
        book_in_date_time >= input$bookin_date_range[1],
        book_in_date_time <= input$bookin_date_range[2]
      )
    }
    if (input$non_active) {
      data <- data %>% filter(case_status != "ACTIVE")
    }
    data
  }
  
  # Getting distinct columns
  get_distinct_col <- function() {
    switch(input$count_method,
           "people" = "unique_identifier",
           "stays" = "stay_id",
           NULL)
  }
  
  # Generating the summary
  generate_summary <- function(data) {
    
    # Defining the variables for the output
    row_var <- input$row_var
    col_var <- input$col_var
    distinct_col <- get_distinct_col()
    
    # Defining median logic
    if (input$count_method == "median") {
      result <- data %>%
        group_by(across(all_of(c(row_var, if (col_var != "None") col_var)))) %>%
        summarise(
          Median = round(median(detention_length_days, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        arrange(-Median)
      
      if (col_var != "None") {
        result <- result %>%
          mutate(!!col_var := replace_na(.data[[col_var]], "Missing")) %>%
          pivot_wider(names_from = all_of(col_var), values_from = Median, values_fill = NA)
        
      } else {
        names(result)[2] <- "Median Detention Length (Days)"
      }
      return(result)
    }
    
    # For total detentions 
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
    }
    
    # For unique people, stays, ect. 
    else {
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
  
  ## ------------------------- SUMMARY TABLE RENDERING ---------------------------
  
  output$summary_table <- renderDT({
    req(input$row_var)
    
    # Filtering and summarizing
    data <- apply_filters(detention_data)
    summary_table <- generate_summary(data)
    
    
    # Defining colors
    count_columns <- names(summary_table)[-1]
    breaks <- quantile(unlist(summary_table[count_columns]), probs = seq(0.05, 0.95, 0.05), na.rm = TRUE)
    n_colors <- length(breaks) + 1
    
    colors <- rgb(
      red = seq(255, 0, length.out = n_colors),
      green = seq(255, 85, length.out = n_colors),
      blue = seq(255, 170, length.out = n_colors),
      maxColorValue = 255
    )
    
    # Defining names to make friendly
    names(summary_table) <- ifelse(
      names(summary_table) %in% names(friendly_labels),
      friendly_labels[names(summary_table)],
      names(summary_table)
    )
    
    # Defining the data table
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
        headerCallback = JS(
          "function(thead) {
         $(thead).find('th').css({
           'font-weight': 'bold',
           'font-family': 'Roboto Condensed',
           'font-size': '20px',
           'white-space': 'nowrap'
         });
       }"
        )
      ),
      class = "stripe hover compact nowrap my-bordered-table"
    ) %>%
      formatStyle(columns = count_columns, backgroundColor = styleInterval(breaks, colors))
    
  })
  
  ## ----------------------------- DOWNLOADER -----------------------------------
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("detention_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$row_var)
      data <- apply_filters(detention_data)
      summary_table <- isolate(generate_summary(data))
      write.csv(summary_table, file, row.names = FALSE)
    }
  )
  
  ## ------------------------------ LABELS --------------------------------------
  
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
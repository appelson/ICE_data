# Loading Libraries
library(shiny)


# Defining UI
ui <- fillPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto&family=Roboto+Condensed:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
    
    tags$style(HTML("
    function resizeIframe(tabId) {
      var iframeId = '';
      if (tabId === 'Arrests') {
        iframeId = 'iframeArrests';
      } else if (tabId === 'Detentions') {
        iframeId = 'iframeDetentions';
      }
      var iframe = document.getElementById(iframeId);
      if (!iframe) return;

      var tabs = document.getElementById('tabs');
      var windowHeight = window.innerHeight;
      var tabsRect = tabs.getBoundingClientRect();
      var tabsTop = tabsRect.top;
      var newHeight = windowHeight - tabsTop - 10;
      iframe.style.height = newHeight + 'px';
    }

    window.addEventListener('resize', function() {
      var activeTab = document.querySelector('.tab-pane.active').getAttribute('data-value');
      resizeIframe(activeTab);
    });

    window.addEventListener('load', function() {
      var activeTab = document.querySelector('.tab-pane.active').getAttribute('data-value');
      resizeIframe(activeTab);
    });

    Shiny.addCustomMessageHandler('tabChange', function(message) {
      resizeIframe(message.tab);
    });
    
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
        font-size: 20px;
        border-radius: 5px;
        font-family: 'Roboto Condensed', sans-serif;
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
      
      .nav-tabs {
  margin-bottom: 20px;
  background-color: #e0e5ec;
  border-radius: 8px 8px 0 0;
  padding-left: 30px;
  font-family: 'Roboto Condensed', sans-serif;
}

/* Individual tab */
.nav-tabs > li > a {
  color: #2c3e50;
  font-weight: 600;
  font-size: 20px;
  padding: 12px 20px;
  border: none;
  border-radius: 8px 8px 0 0;
  margin-right: 2px;
  background-color: #e0e5ec;
  transition: background-color 0.3s ease, color 0.3s ease;
}

/* Active tab */
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover {
  background-color: #e0e5ec;
  border: none;
  border-bottom: 2px solid white;
  color: #0055AA;
  font-weight: 700;
  box-shadow: inset 0 -3px 0 #0055AA;
}

html, body, .shiny-fill-page {
    height: auto !important;
    min-height: 100vh;
    overflow-y: auto !important;
  }
  
    "))
  ),
  
  
  # Creating tab panels
  tabsetPanel(
    id = "tabs",
    tabPanel("Home",
             div(
               class = "title-panel",
               tags$i(class = "fas fa-database", style = "color:#31708f; margin-right: 10px;"),
               h1("Immigration Data Explorer"),
               tags$h4(
                 style = "font-weight: 400; color: #555; font-size: 25px;",
                 "September 2023 – July 2025"
               )
             ),
             br(), br(),
             div(
               style = "border-left: 5px solid #31708f; border-radius: 4px; max-width: 900px; margin: 0 auto; background: white; padding: 30px; border-radius: 10px; box-shadow: 0 5px 20px rgba(0,0,0,0.2);",
               h3("Project Overview"),
               p("This interactive dashboard is built to enhance transparency around immigration enforcement activities in the United States. This tool allows the user to create and export tables from data released by the ",
                 a("Deportation Data Project", href = "https://deportationdata.org/", target = "_blank"),
                 " and covers arrests and detention records from September 2023 through July 2025. For Deportation Data Project's methodology and variable definitions, view the ",
                 a("ICE dataset codebook", href = "https://deportationdata.org/docs/ice.html", target = "_blank"), "."
               ),
               p(tags$i("Note: This dataset may have reporting lags or incomplete records. Interpret data with care. Additionally note that this project was created independently from the Deportation Data Project.")),
               tags$hr(style = "margin: 30px 0; margin: 0 auto; "),
               h3("Explore"),
               tags$ul(
                 tags$li(tags$b("Arrests:"), " Visualize immigration arrests by geography, charges, and demographics."),
                 tags$li(tags$b("Detentions:"), " Explore custody trends, facility-level stats, and outcomes.")
               ),
               tags$div(
                 style = "margin-top: 30px; text-align: center;",
                 actionButton(
                   inputId = "go_to_arrests",
                   label = "Arrests",
                   class = "download-btn",
                   onclick = "document.querySelector('[data-value=Arrests]').click();"
                 ),
                 HTML("&nbsp;&nbsp;"),
                 actionButton(
                   inputId = "go_to_detentions",
                   label = "Detentions",
                   class = "download-btn",
                   onclick = "document.querySelector('[data-value=Detentions]').click();"
                 )
               )
             ),
             
             br(),
             br(),
             br(),
             br(),
             br(),
             
    ),
    
    tabPanel("Arrests",
             div(style = "height: 100%;",
                 tags$iframe(
                   id = "iframeArrests",
                   src = "https://appelson.shinyapps.io/arrest/",
                   style = "border:none; width:100%; height:100%;",
                   scrolling = "auto"
                 )
             )
    ),
    
    tabPanel("Detentions",
             div(style = "height: 100%;",
                 tags$iframe(
                   id = "iframeDetentions",
                   src = "https://appelson.shinyapps.io/detention/",
                   style = "border:none; width:100%; height:100%;",
                   scrolling = "auto"
                 )
             )
    ),
    tags$footer(
      style = "color: black; text-align: center; padding: 15px; margin-top: 30px;",
      HTML('
      <div>
        Immigration Data Explorer.
        &nbsp;|&nbsp;
        <a href="https://github.com/appelson/ice_data_explorer" target="_blank">
          GitHub Repository
        </a>
      </div>
    ')
    )
  ),
  tags$script(HTML("
  
    function resizeIframe(tabId) {
      var iframeId = '';
      if (tabId === 'Arrests') {
        iframeId = 'iframeArrests';
      } else if (tabId === 'Detentions') {
        iframeId = 'iframeDetentions';
      }
      var iframe = document.getElementById(iframeId);
      if (!iframe) return;

      var tabs = document.getElementById('tabs');
      var windowHeight = window.innerHeight;
      var tabsRect = tabs.getBoundingClientRect();
      var tabsTop = tabsRect.top;
      var newHeight = windowHeight - tabsTop - 10;
      iframe.style.height = newHeight + 'px';
    }

    window.addEventListener('resize', function() {
      var activeTab = document.querySelector('.tab-pane.active').getAttribute('data-value');
      resizeIframe(activeTab);
    });

    window.addEventListener('load', function() {
      var activeTab = document.querySelector('.tab-pane.active').getAttribute('data-value');
      resizeIframe(activeTab);
    });

    Shiny.addCustomMessageHandler('tabChange', function(message) {
      resizeIframe(message.tab);
    });
  "))
)

# Server
server <- function(input, output, session) {
  observeEvent(input$tabs, {
    session$sendCustomMessage('tabChange', list(tab = input$tabs))
  })
}

shinyApp(ui, server)

library(shiny)
library(shinydashboard)

shinyUI <- dashboardPage(
  dashboardHeader(title = "BirdSurveyCrunch",
                  tags$li(
                    class = "dropdown",  # Ensures it appears on the header bar
                    tags$div(
                      style = "display: flex; align-items: center; justify-content: flex-end; gap: 10px; padding-right: 10px;",
                      tags$img(src = "BCI.png", height = "30px", style = "margin-top: 10px;"),
                      tags$img(src = "eBird.png", height = "30px", style = "margin-top: 10px;")
                    )
                  )
  ),

  dashboardSidebar(
    tags$head( 
      tags$style(HTML("
      /* Custom styles for helpText */
      .shiny-input-container .help-block {
        color: white; /* Makes the help text white */
      }
      .shiny-input-container .help-block a {
        color: blue; /* Makes links in help text blue */
        text-decoration: underline; /* Adds an underline for links */
      }
      .shiny-input-container .help-block a:hover {
        color: #0000EE; /* Changes link color on hover */
      }
    "))
    ),  
    helpText(h4("Upload your data in exactly the same format (zip) that you got it from eBird. You may also upload the unzipped '.csv' file")),
    fileInput('zipfile', h3('Upload',accept = c('.zip','.csv'))),


    helpText(h4("Select the forest division for data analysis.")),
    selectInput('forestDivision', 'Forest Division', choices = divisions, selected = 'Periyar Tiger Reserve'),
    
    # value is always yyyy-mm-dd, even if the display format is different
    # Start date for the survey
    helpText(h4("Select the start and end dates of the survey")),
    dateInput("startdate", "Start Date of Survey", value = "2025-01-01", format = "dd/mm/yy"),
    
    # value is always yyyy-mm-dd, even if the display format is different
    # End date for the survey
    dateInput("enddate", "End Date of Survey", value = "2025-12-31", format = "dd/mm/yy"), 

    helpText(h4("Check this box if you would like to generate indicator species summary per range")),
    checkboxInput("indicaterspeciesperrange", label = "Generete per Range", value = FALSE),
    helpText(h4("Uncheck this box if you would like to remove lists falling outside the designated area")),
    checkboxInput("pickalllists", label = "Pick All Lists", value = TRUE)
    
  ),
  
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @media (max-width: 768px) { 
          .sidebar-collapse .main-sidebar { 
            transform: translateX(0) !important; 
          }
          .main-sidebar {
            transition: none !important;  /* Disable transition for the sidebar on mobile */
          }
        },
        
        /* Header colours - I have no idea why this code is needed twice, but it is needed */
        .skin-blue .main-header .navbar {
          background-color: #93B791;
        }
        .skin-blue .main-header .navbar {
          background-color: #93B791;
        }
        
        .skin-blue .main-header .logo {
          background-color: #93B791;
        }
        
        /* Sidebar colours */
        .skin-blue .main-sidebar {
          background-color: #93B791;
        }
        .skin-blue .sidebar-menu > li > a {
          color: white;
        }
        .skin-blue .sidebar-menu > li > a:hover {
          background-color: #1a252f;
          color: white;
        }
      ")),
      tags$script(HTML("
        $(document).on('shiny:connected', function() {
          if ($(window).width() <= 768) {
            $('body').addClass('sidebar-open'); // Force sidebar to open on mobile
            // Open the sidebar if it is closed
            if ($('.sidebar-toggle').length > 0) {
              $('.sidebar-toggle').click(); // Simulate click on the hamburger menu to open the sidebar
            }
          }
        });
      "))
    ),
    tabBox(
      id = "main_tabs", width = 12, height = "700px", 
      
#    tabItems(
      tabPanel("Summary", tableOutput('summary')),
      tabPanel("Checklist", dataTableOutput('checklist')),
      tabPanel("Density", dataTableOutput('density')),
      tabPanel("Common Species",
               uiOutput("tables_common_species") ),      
      tabPanel("Threatened Species", tableOutput('iucnspecies')),
      tabPanel("Endemic Species", tableOutput('endemicspecies')),
      tabPanel("Diversity Analysis", plotOutput('shannon')),
      tabPanel("Cluster Analysis", plotOutput('braycrutis')),
      tabPanel("Guild Analysis", plotOutput('guildAnalysis')),
      tabPanel("Indicator Species Species",
               uiOutput("tables_indicator_species") ),      
      tabPanel("Maps",
               fluidRow( 
               column( 4,
                  helpText(h4("Configure Maps")),
                  selectInput( 'gridsize', 'Grid Size (km)', choices = c(2, 3, 4, 5), selected = 4, width = "130px"),
                  selectInput( 'cutoff', 'Minimum Lists/Grid', choices = c(2, 3, 4, 5, 6, 7, 8, 9, 10), selected = 3, width = "130px"),
                  checkboxInput("smooth", label = "Smooth Maps", value = TRUE),
                  checkboxInput("empty",  label = "Show Unsampled Grids", value = FALSE),
                  br(),
                  selectInput( 'noofspecies', 'No of species for Maps', choices = c(5, 10, 15, 20, 25, 30, 40, 50, 75, 100), selected = 10, width = "130px"),
                  downloadButton('downloadMaps', 'Download Maps')
               ),
               column( 5,
                  selectInput( 'speciesname', 'Show map of Birds', choices = species$English.India, width = "400px" )
               ),
               plotOutput("surveymaps") 
               )
               ),
       tabPanel("About", 
               p(strong("Bird Count India Tools: BirdSurveyCrunch - The Bird Survey Report Generator")),
               p("Bird surveys are conducted in various forest areas with the help of bird-watchers."), 
               p("Bird-watchers use eBird enter their observations as multiple lists."), 
               p("This app helps in analysing the eBird data and create summaries for a bird survey report:"),
               p("Added google drive support for maps and species list and hence additional forest divisions can be added: 14 Feb 2023"),
               p("Fixed issues with empty endemic and threatened lists: 08 March 2023"),
               p("Moved to Bird Count India and optimized start time of the tool: 08 February 2025")
       )
    ),
    # Download Button placed at the bottom
    div(class = "download-section",
        h3("Download Survey Report"),
    downloadButton('downloadData', 'Download')
)
  )
)

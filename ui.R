library(shiny)

shinyUI <- fluidPage(

  titlePanel("Bird Survey Report"),
  
  sidebarPanel(
    
    helpText(h4("Just upload your data in exactly the same format (zip) that you got it from eBird; we'll unzip it for you. You can upload the unzipped '.csv' file as well")),
    fileInput('zipfile', h3('Upload',accept = c('.zip','.csv'))),


    helpText(h4("Select the forest division for which you would like to analyse data")),
    selectInput('forestDivision', 'Forest Division', choices = divisions, selected = 'Thrissur'),
    
    # value is always yyyy-mm-dd, even if the display format is different
    # Start date for the survey
    helpText(h4("Select the start and end dates of the survey")),
    dateInput("startdate", "Start Date of Survey", value = "2017-01-15", format = "dd/mm/yy"),
    
    # value is always yyyy-mm-dd, even if the display format is different
    # End date for the survey
    dateInput("enddate", "End Date of Survey", value = "2017-03-15", format = "dd/mm/yy")
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", tableOutput('summary')),
      tabPanel("Checklist", dataTableOutput('checklist')),
      tabPanel("Common Species",
               uiOutput("tables") ),      
      tabPanel("Threatened Species", tableOutput('iucnspecies')),
      tabPanel("Endemic Species", tableOutput('endemicspecies')),
      tabPanel("About", 
               br(), h1("About Bird Survey Report Generator"), 
               br(), p("Bird surveys are conducted in various forest areas with the help of bird-watchers."), 
               br(), p("Bird-watchers use eBird enter their observations as multiple lists."), 
               br(), p("This app helps in analysing the eBird data and create summaries for a bird survey report:")
      )
    ),
    downloadButton('downloadData', 'Download Report')
  )
)

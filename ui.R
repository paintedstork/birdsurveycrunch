library(shiny)

shinyUI <- fluidPage(

  titlePanel("Bird Survey Report"),
  
  sidebarPanel(
    
    fileInput("zipfile", "Choose the zip file downloaded from eBird",
              accept = c(
                "application/zip",
                ".zip")),

    selectInput('forestDivsion', 'Forest Division', choices = divisions, selected = 'Thrissur'),
    
    # value is always yyyy-mm-dd, even if the display format is different
    # Start date for the survey
    dateInput("startdate", "Start Date of Survey", value = "2017-01-15", format = "dd/mm/yy"),

    # value is always yyyy-mm-dd, even if the display format is different
    # End date for the survey
    dateInput("enddate", "End Date of Survey", value = "2017-03-15", format = "dd/mm/yy")
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Checklist", dataTableOutput('checklist')),
      tabPanel("About", 
               br(), h1("About Bird Survey Report Generator"), 
               br(), p("Bird surveys are conducted in various forest areas with the help of bird-watchers."), 
               br(), p("Bird-watchers use eBird enter their observations as multiple lists."), 
               br(), p("This app helps in analysing the eBird data and create summaries for a bird survey report:")
      )
    ),
    downloadButton('downloadData', 'Download')
  )
)





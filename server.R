library(shiny)
source("processEbd.R")
source("genSpeciesList.R")
source("writeWordDoc.R")

shinyServer <- function(input, output) {

  output$checklist <-   renderDataTable ( {
    
      if(is.null(input$zipfile)) return(NULL)
      
      processEBirdFiles(input$zipfile$datapath, species, forestmap, input$startdate, input$enddate) %>% 
      generateSpeciesList ()
      }, 
      options = list( lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
      pageLength = 100))

  output$downloadData <- downloadHandler(
    filename = function() { paste('Report_Vazhachal',
                                  Sys.Date(),'_',
                                  '.docx', sep='') },
    content = function(file) {
        processEBirdFiles(input$zipfile$datapath, species, forestmap, input$startdate, input$enddate) %>% 
        generateSpeciesList ()  %>% 
        createWordDocument (file, input$forestDivsion)
      }  
  )

}

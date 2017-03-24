library(shiny)
library(xtable)

source("processEbd.R")
source("genSpeciesList.R")
source("genSummary.R")
source("writeWordDoc.R")
source("genBirdDensity.R")
source("genCommonSpecies.R")
source("genThreatenedSpecies.R")
source("genEndemicSpecies.R")

shinyServer <- function(input, output) {

  # An output with renderTable. There are more formatting options which are not tried
  output$summary <-   renderTable ( {
    
    if(is.null(input$zipfile)) return(NULL)
    
    print (input$zipfile$datapath)
    processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
      generateSummary ()
  })
  
# Trying out render Table table - looks neat and formatted
  output$checklist <-   renderDataTable ( {
    
      if(is.null(input$zipfile)) return(NULL)
      
      processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
      generateSpeciesList ()
      }, 
      options = list( lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
      pageLength = 100))


  output$iucnspecies <-   renderTable ( {
    
    if(is.null(input$zipfile)) return(NULL)
    
    processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
      generateBirdDensity() %>% 
      generateThreatenedDensity()
  })

  output$endemicspecies <-   renderTable ( {
    
    if(is.null(input$zipfile)) return(NULL)
    
    processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
      generateBirdDensity() %>% 
      generateEndemicDensity ()
  })
  
  
# Writing to a word document
  output$downloadData <- downloadHandler(
    
    filename = function() { paste('Report_', input$forestDivision,
                                  Sys.Date(),'_',
                                  '.docx', sep='') },
    

    content = function(file) {
        doc = docx() %>%
        createWordDocument(paste ('Birds of', input$forestDivision))  %>%
        createTableinDoc (processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
                            generateSummary (), paste('Summary of Birds of', input$forestDivision)) %>%
        createTableinDoc (processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision,input$startdate, input$enddate) %>% 
                                                              generateSpeciesList (), paste('Checklist of Birds of', input$forestDivision))         
        
        chartdatasplit <- processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
          generateBirdDensity() %>% 
          generateCommonSpecies()
        
        for (x in names(chartdatasplit)){ ##go through all individually stored variable data frames in chartdatasplit list
          tabledata <- chartdatasplit[[x]]
          createTableinDoc(doc, tabledata, paste ("Most Common Birds of",x,"Range"))
        }

        doc %>%
        createTableinDoc (processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision,input$startdate, input$enddate) %>% 
                            generateBirdDensity() %>% 
                            generateThreatenedDensity(), paste('Threatened Birds of', input$forestDivision))    %>%  
        createTableinDoc (processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision,input$startdate, input$enddate) %>% 
                            generateBirdDensity() %>% 
                            generateEndemicDensity(), paste('Endemic Birds of', input$forestDivision))    %>%  
        writeDoc (file)
    }
  )


# Multiple tables in one tab using HTML. Useful but formatting not yet perfect
  
  output$tables <- renderUI({

# HTML formatter for the list of data frames        
    tableize <- function(chartdatasplit){  
      tables <- list()
      for (x in names(chartdatasplit)){ ##go through all individually stored variable data frames in chartdatasplit list
        tabledata <- chartdatasplit[[x]]  ###function that returns a dataframe to use in table
        tables[[as.character(x)]] <- 
          print(xtable(tabledata, caption=paste("Range:",x)),
                type="html", include.rownames = FALSE,
                html.table.attributes='class="data table table-bordered table-condensed"',
                caption.placement="top", auto=TRUE)
      }
      return(lapply(tables,paste))    
    }
    
    
    out <- processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
      generateBirdDensity() %>% 
      generateCommonSpecies() %>%
      tableize %>%
      unlist
    return(div(HTML(out),class ="shiny-html-output"))
  })
} 

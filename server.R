library(shiny)
library(xtable)
library(plotly)
library(ggplot2)

source("processEbd.R")
source("genSpeciesList.R")
source("genSummary.R")
source("writeWordDoc.R")
source("genBirdDensity.R")
source("genCommonSpecies.R")
source("genThreatenedSpecies.R")
source("genGroupSummaries.R")
source("genDiversity.R")
source("genGuildAnalysis.R")

shinyServer <- function(input, output) {

  # An output with renderTable. There are more formatting options which are not tried
  output$summary <-   renderTable ( {
    
    if(is.null(input$zipfile)) return(NULL)
    
    print (input$zipfile$datapath)
    ebd <- processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate)
    cbind (generateSummary(ebd), generateOverallSummary(ebd, paste(input$forestDivision,' Division')))                            
  })
  
# Trying out render Table table - looks neat and formatted
  output$checklist <-   renderDataTable ( {
    
      if(is.null(input$zipfile)) return(NULL)
      
      processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
      generateSpeciesList ()
      }, 
      options = list( lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
      pageLength = 100))

  output$density <-   renderDataTable ( {
    
    if(is.null(input$zipfile)) return(NULL)
    
    processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
      generateOverallBirdDensity () %>%
      subset(select = c("SlNo", "English Name","Scientific Name", "IUCN", "WG", "Density"))
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
      generateGroupSummaries ("WG")
  })

  output$shannon <-   renderPlot ( {
    
    if(is.null(input$zipfile)) return(NULL)

    inTable <-  processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
                genCommunityData() %>% 
                genShannonDiversity ()
    
    barplot( t(inTable), width = 1, xlim= c(1,4 * nrow(inTable)), ylim = c(0,ceiling(max(inTable))), space = 2, border = par("fg"), main="Shannon Diversity", 
             xlab="Ranges", col=c("darkblue","red")) %>%
             text(t(inTable), labels = t(inTable), pos = 3)
  }, width = 1000, height = 800)

  output$braycrutis <-   renderPlot ( {
    
    if(is.null(input$zipfile)) return(NULL)
    
    processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
    genCommunityData() %>% 
    genClusterAnalaysis() %>%
    plot(horiz = T,  main="Cluster Analysis", xlim=c(1.0, 0.0), xlab="Dissimilarity", ylab = "Ranges") 
  }, width = 1000, height = 800)

  output$guildAnalysis <-   renderPlot ( {
    
    if(is.null(input$zipfile)) return(NULL)
    
      processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>%
      genGuildAnalysis () %>%
      ggplot(aes(x = Range, y = Percentage, fill=Guild)) +
        geom_bar(stat='identity', size = 10, show.legend = TRUE)
  }, width = 1000, height = 800)
  
  
# Writing to a word document
  output$downloadData <- downloadHandler(
    
    filename = function() { paste('Report_', input$forestDivision,
                                  Sys.Date(),'_',
                                  '.docx', sep='') },
    

    content = function(file) {
        ebd <- processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate)
        doc <- docx() %>%
        createWordDocument(paste ('Birds of', input$forestDivision))  %>%
        createTableinDoc (  cbind (generateSummary(ebd), generateOverallSummary(ebd, paste(input$forestDivision,' Division'))),                            
                            paste('Summary of Birds of', input$forestDivision)) %>%
        createTableinDoc (  ebd %>%
                            generateSpeciesList (), paste('Checklist of Birds of', input$forestDivision)) %>%
        createTableinDoc (  ebd %>%
                            generateOverallBirdDensity () %>%
                            subset(select = c("SlNo", "English Name","Scientific Name", "IUCN", "WG", "Density")),  
                            paste('Density of Birds of', input$forestDivision))
                            
        chartdatasplit <-   ebd %>% 
          generateBirdDensity() %>% 
          generateCommonSpecies()
        
        for (x in names(chartdatasplit)){ ##go through all individually stored variable data frames in chartdatasplit list
          tabledata <- chartdatasplit[[x]]
          createTableinDoc(doc, tabledata, paste ("Most Common Birds of",x,"Range"))
        }

        
        doc <- doc %>%
        createTableinDoc (  ebd %>% 
                            generateBirdDensity() %>% 
                            generateThreatenedDensity(), paste('Threatened Birds of', input$forestDivision))    %>%  
        createTableinDoc (  ebd %>%
                            generateBirdDensity() %>% 
                            generateGroupSummaries("WG"), paste('Endemic Birds of', input$forestDivision))    %>%  
        createBarPlotinDoc (ebd %>%
                            genCommunityData() %>% 
                            genShannonDiversity(), paste('Bird Diversity Index for', input$forestDivision))    %>%  
        createPlotinDoc (   ebd %>% 
                            genCommunityData() %>% 
                            genClusterAnalaysis(), paste('Cluster Analysis for', input$forestDivision))     %>% 
        createStackedBarPlotinDoc (ebd %>% 
                            genGuildAnalysis(), paste('Guild Analysis for', input$forestDivision))     
          
        chartdatasplit <-   ebd %>% 
        generateIndicatorSpecies(input$indicaterspeciesperrange) 
        
        for (x in names(chartdatasplit)) { ##go through all individually stored variable data frames in chartdatasplit list
          tabledata <- chartdatasplit[[x]]
          createTableinDoc(doc, tabledata, paste ("Encounter Rates of ",x))
        }
        
        writeDoc (doc, file)
    }
  )


# Multiple tables in one tab using HTML. Useful but formatting not yet perfect
  
  output$tables_common_species <- renderUI({

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

  output$tables_indicator_species <- renderUI({
    
    # HTML formatter for the list of data frames        
    tableize <- function(chartdatasplit){  
      tables <- list()
      for (x in names(chartdatasplit)){ ##go through all individually stored variable data frames in chartdatasplit list
        tabledata <- chartdatasplit[[x]]  ###function that returns a dataframe to use in table
        tables[[as.character(x)]] <- 
          print(xtable(tabledata, caption=paste("Group: ",x)),
                type="html", include.rownames = FALSE,
                html.table.attributes='class="data table table-bordered table-condensed"',
                caption.placement="top", auto=TRUE)
      }
      return(lapply(tables,paste))    
    }

    out <- processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate) %>% 
      generateIndicatorSpecies(input$indicaterspeciesperrange) %>%
      tableize %>%
      unlist
    
    return(div(HTML(out),class ="shiny-html-output"))
  })
} 

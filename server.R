library(shiny)
library(xtable)

library(ggplot2)

source("global.R")
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
source("surveymapfunctions.R")
source("genMaps.R")

options(shiny.maxRequestSize=30*1024^2) 

shinyServer <- function(session, input, output) {

  observeEvent(input$forestDivision, {
    selected <- shapelist %>% filter (DivisionName == input$forestDivision)
    forestmap <<- getShape (selected$Polygon, selected$Folder)
    print(nrow(forestmap))
  })
  
  ebd_proc <- reactive ({
    req(input$zipfile)
    req(forestmap)
    print(nrow(forestmap))
    processEBirdFiles(input$zipfile, species, forestmap, input$forestDivision, input$startdate, input$enddate, input$pickalllists)
  })
  
  birdlist <- reactive({
      ebd_proc()  %>%
      generateOverallBirdDensity () %>% 
      setorder (-Density) %>%
      inner_join (species, by = c('Scientific Name' = 'Scientific.Name')) 
      })
  
  observe(
    {
    updateSelectInput(session, 
                      input = "speciesname",  
                      choices = birdlist()$English.India)
    }
    )
  
  # An output with renderTable. There are more formatting options which are not tried
  output$summary <-   renderTable ( {
    print (input$zipfile$datapath)
    ebd <- ebd_proc()
    cbind (generateSummary(ebd), generateOverallSummary(ebd, paste(input$forestDivision,' Division')))                            
  })
  
# Trying out render Table table - looks neat and formatted
  output$checklist <-   renderDataTable ( {
      ebd_proc() %>% 
      generateSpeciesList ()
      }, 
      options = list( lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
      pageLength = 100))

  output$density <-   renderDataTable ( {
      ebd_proc() %>% 
      generateOverallBirdDensity () %>%
      subset(select = c("SlNo", "English Name","Scientific Name", "IUCN", "WG", "Density"))
  }, 
  options = list( lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
                  pageLength = 100))
  
  
  output$iucnspecies <-   renderTable ( {
      ebd_proc() %>% 
      generateBirdDensity() %>% 
      generateThreatenedDensity()
  })

  output$endemicspecies <-   renderTable ( {
      ebd_proc() %>% 
      generateBirdDensity() %>% 
      generateGroupSummaries ("WG")
  })

  output$shannon <-   renderPlot ( {
    inTable <-  ebd_proc() %>% 
                genCommunityData() %>% 
                genShannonDiversity ()
    
    barplot( t(inTable), width = 1, xlim= c(1,4 * nrow(inTable)), ylim = c(0,ceiling(max(inTable))), space = 2, border = par("fg"), main="Shannon Diversity", 
             xlab="Ranges", col=c("darkblue","red")) %>%
             text(t(inTable), labels = t(inTable), pos = 3)
  }, width = 1000, height = 800)

  output$braycrutis <-   renderPlot ( {
    ebd_proc() %>% 
    genCommunityData() %>% 
    genClusterAnalaysis() %>%
    plot(horiz = T,  main="Cluster Analysis", xlim=c(1.0, 0.0), xlab="Dissimilarity", ylab = "Ranges") 
  }, width = 1000, height = 800)

  output$guildAnalysis <-   renderPlot ( {
      ebd_proc() %>% 
      genGuildAnalysis () %>%
      ggplot(aes(x = Range, y = Percentage, fill=Guild)) +
        geom_bar(stat='identity', size = 10, show.legend = TRUE)
  }, width = 1000, height = 800)
  
output$surveymaps <-   renderPlot ( {
  req(input$speciesname)
  surveymaps(species = as.character(species[species$English.India == input$speciesname,]$Scientific.Name),
             data = ebd_proc(), 
             tempmap = forestmap, 
             filter = input$forestDivision, 
             dataformat = "PROCESSED DATA", 
             gridsize = as.numeric(input$gridsize), 
             smooth = input$smooth, 
             h = 0.1, 
             cutoff = as.numeric(input$cutoff), 
             showempty = input$empty)  
}, width = 500, height = 400)

# Writing to a word document
  output$downloadData <- downloadHandler(

    filename = function() { paste('Report_', input$forestDivision,
                                  Sys.Date(),'_',
                                  '.docx', sep='') },
    
    content = function(file) {
        ebd <- ebd_proc()
        doc <- read_docx() %>%
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
                            generateThreatenedDensity(), paste('Threatened Birds of', input$forestDivision))    
        
          
        doc <- doc %>%
        createTableinDoc (  ebd %>%
                        generateBirdDensity() %>% 
                        generateGroupSummaries("WG"), paste('Endemic Birds of', input$forestDivision))       

#        doc <- doc %>% createBarPlotinDoc (ebd %>%
#                            genCommunityData() %>% 
#                            genShannonDiversity(), paste('Bird Diversity Index for', input$forestDivision))       
        doc <- doc %>%
        createStackedBarPlotinDoc (ebd %>% 
                                     genGuildAnalysis(), paste('Guild Analysis for', input$forestDivision))     
        
        if(length(unique(ebd$RANGE)) > 1)
        {   # If there is only a single range, then these does not make sense 
#            doc <- doc %>%   
#            createDendPlotinDoc ( ebd %>% 
#                            genCommunityData() %>% 
#                            genClusterAnalaysis(), paste('Cluster Analysis for', input$forestDivision))    
        }
         
        chartdatasplit <-   ebd %>% 
        generateIndicatorSpecies(input$indicaterspeciesperrange) 

        for (x in names(chartdatasplit)) { ##go through all individually stored variable data frames in chartdatasplit list
          tabledata <- chartdatasplit[[x]]
          createTableinDoc(doc, tabledata, paste ("Encounter Rates of ",x))
        }
        doc <- doc %>% body_end_section_landscape() 
        print(doc, target = file )
        print(file)
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
    
    
    out <-  ebd_proc() %>%
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

    out <- ebd_proc() %>%
      generateIndicatorSpecies(input$indicaterspeciesperrange) %>%
      tableize %>%
      unlist
    
    return(div(HTML(out),class ="shiny-html-output"))
  })
  
  # Writing to a zip file with all maps 
  output$downloadMaps <- downloadHandler(
    filename = function() { paste('Maps_', input$forestDivision,
                                  Sys.Date(),'_',
                                  '.zip', sep='') },
    content = function(file) {
        ebd <-  ebd_proc()
#        saveRDS(ebd, "temp.RDS")
#        ebd <- readRDS("temp.RDS")
        freqdata <- generateOverallBirdDensity(ebd) 
        dir <- "maps" 
        dir.create(dir)
        genMaps(ebd, freqdata, forestmap,
                input$forestDivision,
                as.numeric(input$gridsize),
                input$smooth,
                as.numeric(input$cutoff),
                input$empty,
                noofspecies = as.numeric(input$noofspecies), 
                folder = dir, 
                extension = '.jpg')   
        print(dir)
        print(zip(file, files = dir))
        print(file)
        unlink(dir, TRUE)
    }
  )    
} 

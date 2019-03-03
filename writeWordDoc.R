library(officer)
library(flextable)
library(magrittr)

###########################################################################
#        Create Word Document out of data                                 #
# Param 1: ebdChecklist: Checklist that need to be printed                #
# Param 2: Filename of the word document                                  #
# Param 3: Name of the Forest Division                                    #
###########################################################################

createWordDocument <- function (doc, inTitle)  {
  
  # Add a title
  doc <- body_add_par(doc, inTitle, style = "heading 1")
  
  # Add paragraph
  doc <- body_add_par(doc, "This Word document is generated from eBird checklists using Bird Survey Report App.", style = "Normal")
  
  return (doc)
}

createTableinDoc <- function (doc, inTable, inTitle) {
  
  print(nrow(inTable))
  doc <- body_add_par(doc, inTitle, style = "heading 2")
 
  inTable_flex <- regulartable (inTable,
                             col_keys = names(inTable)) %>% 
                  autofit() %>%
                  theme_zebra(odd_body = "#DDDDDD", 
                              even_body = "#FFFFFF") %>%
                  bold(bold = TRUE, part = "header") %>%
                  color (color = "white", part = "header") %>%
                  bg (bg = "#003366", part = "header") %>%
                  fontsize (size = 9) 
# %>%
# ???                 align (align = "left", part = "all") 

  if("Scientific Name" %in% colnames(inTable))
  {  
    italic (inTable_flex, j = "Scientific Name", italic = TRUE, part = "body") 
  }
  # Add FlexTable to the document
  doc <- body_add_flextable(doc, inTable_flex)
  doc <- body_add_break(doc) # go to the next page
  
  return (doc)
}

createBarPlotinDoc <- function (doc, inTable, inTitle) {
  
  doc <- body_add_par(doc, inTitle, style = "heading 2")
  
#  doc = addPlot( doc, 
#                fun = function() { barplot( t(inTable), width = 1, xlim= c(1,4 * nrow(inTable)), ylim = c(0,ceiling(max(inTable))), space = 2, border = par("fg"), main=inTitle, 
#                                            xlab="Ranges", col=c("darkblue","red")) %>%
#                                   text(t(inTable), labels = t(inTable), pos = 3)},
#               vector.graphic = TRUE, width = 4, height = 5,
#               par.properties = parProperties(text.align = "center")
#)
  p <- barplot( t(inTable), 
                width = 1, 
                xlim= c(1,4 * nrow(inTable)), ylim = c(0,ceiling(max(inTable))), 
                space = 2, 
                border = par("fg"), 
                main=inTitle, 
                xlab="Ranges", 
                col=c("darkblue","red")) %>%
                text(t(inTable), 
                labels = t(inTable), 
                pos = 3)

    doc <- body_add_gg (doc, 
                      value = p, 
                      width = 4, height =5, 
                      style = "centered")
  
  doc <- body_add_break(doc) # go to the next page
  
return (doc)
}

createStackedBarPlotinDoc <- function (doc, inTable, inTitle) {
  
  doc <- body_add_par(doc, inTitle, style = "heading 2")
  
  p <- ggplot(inTable, aes(x = Range, y = Percentage,fill = Guild)) + 
       geom_bar(stat='identity', size = 10, show.legend = TRUE) +
       theme(axis.text.x = element_text(angle = -90))
  
  doc <- body_add_gg (doc, 
                      value = p, 
                      width = 4, height =5, 
                      style = "centered")
#  doc = addPlot( doc, 
#                 fun = print, x= p,
#                 vector.graphic = TRUE, width = 4, height = 5,
#                 par.properties = parProperties(text.align = "center")
#  )
  doc <- body_add_break(doc) # go to the next page
  
  return (doc)
}

createPlotinDoc <- function (doc, inTable, inTitle) {
  
  doc <- body_add_par(doc, inTitle, style = "heading 2")
  
  p <- plot(inTable, 
            horiz = T,  
            main="Cluster Analysis", 
            xlim=c(1.0, 0.0), 
            xlab="Dissimilarity", 
            ylab = "Ranges")
  
  doc <- body_add_gg (doc, 
                      value = p, 
                      width = 4, height =5, 
                      style = "centered")
  
  doc <- body_add_break(doc) # go to the next page
  
  return (doc)
}

# Test Code 

testHarness_createWordDocument <- function ()
{
  unzip('..\\data\\ebird_1510638067328.zip')
  ebd     <- read.csv('MyEBirdData.csv', header = TRUE, sep = ",") 

  ebd_species   <- ebd[!duplicated(ebd$Taxonomic.Order),]

    # Strip unwanted columns from eBird records
  ebd_species <- subset(ebd_species, select = c("Common.Name", "Scientific.Name"))
  
  colnames(ebd_species)[2] <- "Scientific Name"
  
  read_docx() %>%
  createWordDocument("Birds of Vazhachal Forest Division")   %>%
  createTableinDoc (ebd_species, 'Checklist of Birds of Vazhachal Forest Division') %>%
  print(target = "ReportVazhachal.docx" )

}

#testHarness_createWordDocument()

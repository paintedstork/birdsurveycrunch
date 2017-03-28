library(ReporteRs)
library(magrittr)

###########################################################################
#        Create Word Document out of data                                 #
# Param 1: ebdChecklist: Checklist that need to be printed                #
# Param 2: Filename of the word document                                  #
# Param 3: Name of the Forest Division                                    #
###########################################################################

createWordDocument <- function (doc, inTitle)  {
  
  # Add a title
  doc <- addTitle(doc, inTitle , level=1)
  
  # Add paragraph
  doc <- addParagraph(doc, "This Word document is generated from eBird checklists using Bird Survey Report App.")
  
  return (doc)
}

createTableinDoc <- function (doc, inTable, inTitle) {
  
  print(nrow(inTable))
  doc <- addTitle(doc, inTitle, level=2)
  
  # Convert to flex table and format it
  inTable_flex <- FlexTable (inTable, 
                                 header.cell.props = cellProperties( background.color =  "#003366" ),
                                 header.text.props = textBold( color = "white" ))
  setZebraStyle( inTable_flex, odd = "#DDDDDD", even = "#FFFFFF" ) 
  inTable_flex[] <- textProperties( font.size=9)

  if("Scientific Name" %in% colnames(inTable))
  {  
    inTable_flex[, 'Scientific Name'] <- textProperties(  font.size=9, font.style="italic")
  }
  
  # Add FlexTable to the document
  doc <- addFlexTable(doc, inTable_flex)
  doc <- addPageBreak(doc) # go to the next page
  
  return (doc)
}

createBarPlotinDoc <- function (doc, inTable, inTitle) {
  
  doc <- addTitle(doc, inTitle, level=2)

  doc = addPlot( doc, 
                fun = function() { barplot( t(inTable), width = 1, xlim= c(1,4 * nrow(inTable)), ylim = c(0,ceiling(max(inTable))), space = 2, border = par("fg"), main=inTitle, 
                                            xlab="Ranges", col=c("darkblue","red")) %>%
                                   text(t(inTable), labels = t(inTable), pos = 3)},
               vector.graphic = TRUE, width = 4, height = 5,
               par.properties = parProperties(text.align = "center")
)
doc <- addPageBreak(doc) # go to the next page

return (doc)
}

createStackedBarPlotinDoc <- function (doc, inTable, inTitle) {
  
  doc <- addTitle(doc, inTitle, level=2)

  p <- ggplot(inTable, aes(x = Range, y = Percentage,fill = Guild)) + 
       geom_bar(stat='identity', size = 10, show.legend = TRUE)
  
  doc = addPlot( doc, 
                 fun = print, x= p,
                 vector.graphic = TRUE, width = 4, height = 5,
                 par.properties = parProperties(text.align = "center")
  )
  doc <- addPageBreak(doc) # go to the next page
  
  return (doc)
}

createPlotinDoc <- function (doc, inTable, inTitle) {
  
  doc <- addTitle(doc, inTitle, level=2)
  
  doc = addPlot( doc, 
                 fun = function() { plot(inTable, horiz = T,  main="Cluster Analysis", xlim=c(1.0, 0.0), xlab="Dissimilarity", ylab = "Ranges") },
                 vector.graphic = TRUE, width = 4, height = 5,
                 par.properties = parProperties(text.align = "center") 
  )
  doc <- addPageBreak(doc) # go to the next page
  
  return (doc)
}

# Test Code 

testHarness_createWordDocument <- function ()
{
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEbirdData.csv', header = TRUE, sep = ",") 

  ebd_species   <- ebd[!duplicated(ebd$Taxonomic.Order),]

    # Strip unwanted columns from eBird records
  ebd_species <- subset(ebd_species, select = c("Common.Name", "Scientific.Name"))
  
  colnames(ebd_species)[2] <- "Scientific Name"
  
  docx() %>%
  createWordDocument('Birds of Vazhachal Forest Division')   %>%
  createTableinDoc (ebd_species, 'Checklist of Birds of Vazhachal Forest Division') %>%
  writeDoc ('ReportVazhachal.docx')
}

#testHarness_createWordDocument()

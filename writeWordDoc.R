library(ReporteRs)
library(magrittr)

###########################################################################
#        Create Word Document out of data                                 #
# Param 1: ebdChecklist: Checklist that need to be printed                #
# Param 2: Filename of the word document                                  #
# Param 3: Name of the Forest Division                                    #
###########################################################################

createWordDocument <- function (ebdChecklist, docFileName, forestName) {
  
  # Create a Word document
  doc <- docx()
  
  # Add a title
  doc <- addTitle(doc, paste ('Checklist of Birds of ',forestName,' Division', sep='') , level=1)
  
  # Add paragraph
  doc <- addParagraph(doc, "This Word document is generated from eBird checklists using Bird Survey Report App.")
  
  doc <- addTitle(doc, "Consolidated Checklist", level=2)
  
  # Convert to flex table and format it
  ebd_species_flex <- FlexTable (ebdChecklist, 
                                 header.cell.props = cellProperties( background.color =  "#003366" ),
                                 header.text.props = textBold( color = "white" ))
  setZebraStyle( ebd_species_flex, odd = "#DDDDDD", even = "#FFFFFF" ) 
  ebd_species_flex[] <- textProperties( font.size=9)
  ebd_species_flex[, 'Scientific Name'] <- textProperties(  font.size=9, font.style="italic")
  
  # Add FlexTable to the document
  doc <- addFlexTable(doc, ebd_species_flex)
  doc <- addPageBreak(doc) # go to the next page
  
  # Write the word document to a file
  writeDoc(doc, file=docFileName)
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
  
  createWordDocument(ebd_species, 'ReportVazhachal.docx', 'Vazhachal')  
}

#testHarness_createWordDocument()

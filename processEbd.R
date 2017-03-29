library (plyr)
library (dplyr)
library (rgdal)
library (sp)
library (tools)

MapColumns <- function (ebd)
{
  colnames(ebd)[colnames(ebd) == 'TAXONOMIC.ORDER'] <- "Taxonomic.Order"  
  colnames(ebd)[colnames(ebd) == 'COMMON.NAME'] <- 'Common.Name'  
  colnames(ebd)[colnames(ebd) == 'SCIENTIFIC.NAME'] <- 'Scientific.Name'  
  colnames(ebd)[colnames(ebd) == 'OBSERVATION.COUNT'] <- 'Count'  
  colnames(ebd)[colnames(ebd) == 'STATE_PROVINCE'] <- 'State.Province'  
  colnames(ebd)[colnames(ebd) == 'COUNTY'] <- 'County'  
  colnames(ebd)[colnames(ebd) == 'LOCALITY'] <- 'Location'  
  colnames(ebd)[colnames(ebd) == 'LATITUDE'] <- 'Latitude'  
  colnames(ebd)[colnames(ebd) == 'LONGITUDE'] <- 'Longitude'  
  colnames(ebd)[colnames(ebd) == 'OBSERVATION.DATE'] <- 'Date'  
  colnames(ebd)[colnames(ebd) == 'TIME.OBSERVATIONS.STARTED'] <- 'Time'  
  colnames(ebd)[colnames(ebd) == 'UNIQUE_SAMPLING_ID'] <- 'Submission.ID'  
  colnames(ebd)[colnames(ebd) == 'PROTOCOL.TYPE'] <- 'Protocol'  
  colnames(ebd)[colnames(ebd) == 'DURATION.MINUTES'] <- 'Duration..Min.'  
  colnames(ebd)[colnames(ebd) == 'EFFORT.DISTANCE.KM'] <- 'Distance.Traveled..km.'  
  colnames(ebd)[colnames(ebd) == 'EFFORT.AREA.HA'] <- 'Area.Covered..ha.'  
  colnames(ebd)[colnames(ebd) == 'NUMBER.OBSERVERS'] <- 'Number.of.Observers'  
  colnames(ebd)[colnames(ebd) == 'ALL.SPECIES.REPORTED'] <- 'All.Obs.Reported'  
  colnames(ebd)[colnames(ebd) == 'TRIP.COMMENTS'] <- 'Checklist.Comments'  
  colnames(ebd)[colnames(ebd) == 'SPECIES.COMMENTS'] <- 'Species.Comments'  

  return (ebd)
}
readEBirdFiles <- function (inputEbdFile, startDate, endDate) {
  
  print(inputEbdFile$name)

  if( tools::file_ext(inputEbdFile)[1] == 'csv') ebd <- read.csv(inputEbdFile$datapath)
  
  if( tools::file_ext(inputEbdFile)[1] == 'zip') {
    if ( any ('MyEBirdData.csv'== unzip(inputEbdFile$datapath, list=TRUE)$Name))
    {
      ebd <- read.csv(unz(inputEbdFile$datapath,'MyEBirdData.csv'))
      
      #Filter on date selected. Format is different between the two downloads
      if(as.Date(startDate) < as.Date(endDate))
      {
        ebd <- ebd [which(as.Date(ebd$Date,"%m-%d-%Y") >= as.Date(startDate)), ]
        ebd <- ebd [which(as.Date(ebd$Date,"%m-%d-%Y") <= as.Date(endDate)), ]
      }
    }
    else
    {
      
      ebd <- read.delim(unz(inputEbdFile$datapath,gsub('zip','txt',inputEbdFile$name)), na.strings = c("NA", "", "null"), as.is=TRUE, quote="")

      #Add unique list identifier for removing duplicates
      ebd <- within (ebd, UNIQUE_SAMPLING_ID <-  ifelse(is.na(GROUP.IDENTIFIER),SAMPLING.EVENT.IDENTIFIER,GROUP.IDENTIFIER))
      #Remove entries from shared lists
      ebd <- ebd[!duplicated(ebd[c("UNIQUE_SAMPLING_ID","COMMON.NAME")]),]
      
      #Map Column names to same as personal eBird data columns
      ebd <- MapColumns (ebd)  
      
      #Filter on date selected. Format is different between the two downloads
      if(as.Date(startDate) < as.Date(endDate))
      {
        ebd <- ebd [which(as.Date(ebd$Date,"%Y-%m-%d") >= as.Date(startDate)), ]
        ebd <- ebd [which(as.Date(ebd$Date,"%Y-%m-%d") <= as.Date(endDate)), ]
      }
    }
  }
  

  # Strip unwanted columns from eBird records
  ebd <- subset(ebd, select = c("Taxonomic.Order", 
                                                "Common.Name", 
                                                "Scientific.Name",
                                                "Count",
                                                "Latitude",
                                                "Longitude",
                                                "Submission.ID",
                                                "Duration..Min.",
                                                "All.Obs.Reported" ))
  
  
  return (ebd)
}

processEBirdFiles <- function (inputEbdFile, species, forestmap, forestDivision, startDate, endDate) {
  #Unzip and read eBird records
  print (inputEbdFile$datapath)
  print (file_ext(inputEbdFile)[1])

  ebd <- readEBirdFiles (inputEbdFile, startDate, endDate)
  print(nrow(ebd))  

  if (nrow(ebd) == 0)  { return (NULL) }

  # Obtain details of birds by joining with species file
  ebd <- join (ebd, species, by = 'Scientific.Name')
  
  sp::coordinates(ebd) <- ~Longitude+Latitude
  # Map the CRS
  sp::proj4string(ebd) <- sp::proj4string(forestmap)
  

  ebd_with_range_and_division <- NULL
  
  for (rangeindex in 1:nrow(forestmap@data))
  {
    # Filter lists according to set filter polygons 
    range_selected  <- forestmap[rangeindex, ]
    
    rgl_ebd   <- ebd
    rgl_ebd$RANGE  <- 0;
    rgl_ebd$DIVISION  <- 0;
    rgl_ebd   <- rgl_ebd[range_selected, ]
    
    if(nrow(rgl_ebd) > 0)
    {
      # For all filtered lists, assign the filter_index
      rgl_ebd$RANGE     <- as.character(forestmap$Range[rangeindex]);
      rgl_ebd$DIVISION  <- as.character(forestmap$Division[rangeindex]);
      
      if(!is.null(ebd_with_range_and_division))
      {
        ebd_with_range_and_division <- rbind (ebd_with_range_and_division, rgl_ebd)
      }
      else
      {
        ebd_with_range_and_division <- rgl_ebd
      }
    }
    
    rangeindex  <- rangeindex + 1
  }

  # Filter for records from the forest division selected
  ebd_with_range_and_division <- ebd_with_range_and_division [which (ebd_with_range_and_division$DIVISION == forestDivision), ]
  
  if (nrow(ebd_with_range_and_division) == 0)  { return (NULL) }
  
  
  print(paste ('No species ',nrow(ebd_with_range_and_division)))
  
  return (as.data.frame(ebd_with_range_and_division)) 
}

# Test Code 

testHarness_processEBirdFiles <- function () {

unzip('..\\data\\ebd_IN-KL-TS_relFeb-2017.zip')
ebd <- read.delim(paste('ebd_IN-KL-TS_relFeb-2017','.txt',sep=''), na.strings = c("NA", "", "null"), as.is=TRUE, quote="")
  
species <- read.csv('Species.csv', header = TRUE, sep = ",") 

unzip('keralaforest.zip')
forestmap <- rgdal::readOGR('keralaforest.shp', 'keralaforest')

output <- processEBirdFiles('..\\data\\ebird_1489816770850.zip', species, forestmap, 'Vazhachal', '2017-01-15', '2017-03-01')
write.csv(output, 'testout.csv')

output <- processEBirdFiles('MyEBirdData.csv', species, forestmap, 'Vazhachal', '2017-01-15', '2017-03-01')
write.csv(output, 'testout.csv')

print (nrow(output))
}


#testHarness_processEBirdFiles()

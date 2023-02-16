library (dplyr)
library (rgdal)
library (sp)
library (tools)
library (sqldf)

':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

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

  if( tools::file_ext(inputEbdFile$name) == 'csv') 
  {
      ebd <- read.csv(inputEbdFile$datapath)
      print(nrow(ebd))
      
      #Filter on date selected. Format is different between the two downloads
      if(as.Date(startDate) < as.Date(endDate))
      {
        ebd <- ebd [which(as.Date(ebd$Date,"%Y-%m-%d") >= as.Date(startDate)), ]
        ebd <- ebd [which(as.Date(ebd$Date,"%Y-%m-%d") <= as.Date(endDate)), ]
        print(nrow(ebd))
      }
      print(nrow(ebd))
  }

  if( tools::file_ext(inputEbdFile$name) == 'zip') {
    if ( any ('MyEBirdData.csv'== unzip(inputEbdFile$datapath, list=TRUE)$Name))
    {
      print("Unzipping file")
      ebd <- read.csv(unz(inputEbdFile$datapath,'MyEBirdData.csv'))
      print(nrow(ebd))
      
      #Filter on date selected. Format is different between the two downloads
      if(as.Date(startDate) < as.Date(endDate))
      {
        ebd <- ebd [which(as.Date(ebd$Date,"%Y-%m-%d") >= as.Date(startDate)), ]
        ebd <- ebd [which(as.Date(ebd$Date,"%Y-%m-%d") <= as.Date(endDate)), ]
        print(nrow(ebd))
      }
      print(nrow(ebd))
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
  print(paste("Final number of observations ", nrow(ebd)))
  

  # Strip unwanted columns from eBird records
  ebd <- subset(ebd, select = c("Taxonomic.Order", 
                                                "Common.Name", 
                                                "Scientific.Name",
                                                "Count",
                                                "Latitude",
                                                "Longitude",
                                                "Submission.ID",
                                                "Date",
                                                "Duration..Min.",
                                                "All.Obs.Reported" ))
  
  ebd_lists <- ebd[!duplicated(ebd[c("Submission.ID")]), ]
  
  return (list (ebd, ebd_lists))
}

processEBirdFiles <- function (inputEbdFile, species, forestmap, forestDivision, startDate, endDate, pickalllists) {
  #Unzip and read eBird records
  print (inputEbdFile$datapath)
  print (file_ext(inputEbdFile)[1])

  c (ebd, ebd_lists) := readEBirdFiles (inputEbdFile, startDate, endDate)
  print(nrow(ebd))
  print(nrow(ebd_lists))

  if (nrow(ebd) == 0)  { return (NULL) }

  missing_species <- sqldf("SELECT [Scientific.Name] FROM ebd except SELECT [Scientific.Name] FROM species")
  
  if(nrow(missing_species) > 0)
  {
    print(missing_species)
  }
  
  # Obtain details of birds by joining with species file
  ebd <- inner_join (ebd, species, by = 'Scientific.Name')

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
      rgl_ebd_lists <- as.data.frame (rgl_ebd[!duplicated(as.data.frame (rgl_ebd[c("Submission.ID")])), ])
      print (paste ("Lists", nrow(rgl_ebd_lists) , as.character(forestmap$Range[rangeindex])))
      
      rgl_ebd$RANGE     <- as.character(forestmap$Range[rangeindex])
      rgl_ebd$DIVISION  <- as.character(forestmap$Division[rangeindex])
      
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

  ebd_with_range_and_division <- as.data.frame (ebd_with_range_and_division [which (ebd_with_range_and_division$DIVISION == forestDivision), ])
  ebd <- as.data.frame (ebd)

  print("Processing")  
  if(pickalllists)
  {# Remaining lists should be processed with this condition 
     ebd_selected <- ebd_with_range_and_division
     ebd_selected$DIVISION <- NULL
     ebd_selected$RANGE <- NULL
     
     ebd_selected <- sqldf ("SELECT * FROM ebd EXCEPT SELECT * FROM ebd_selected")
     
     print(nrow(ebd_selected))
     # If there are any lists from Outskirts, then add them
     if(nrow (ebd_selected))
     {
        ebd_selected$RANGE <- "Outskirts"
        ebd_selected$DIVISION <- forestDivision
     
        ebd_with_range_and_division <- rbind (ebd_with_range_and_division, ebd_selected)
        ebd_with_range_and_division <- ebd_with_range_and_division [which (ebd_with_range_and_division$DIVISION == forestDivision), ]
     }
  }
  else 
  {
    ebd_selected_lists <- ebd_with_range_and_division[!duplicated(ebd_with_range_and_division[c("Submission.ID")]), ]
    ebd_selected_lists <- ebd_selected_lists[,c("Submission.ID", "Common.Name")]
    print(nrow(ebd_selected_lists))
    
    ebd_lists <- ebd_lists [,c("Submission.ID", "Common.Name")]
    print(nrow(ebd_lists))
    
    ebd_excluded_lists <- sqldf ("SELECT * FROM ebd_lists EXCEPT SELECT * FROM ebd_selected_lists")
    print(ebd_excluded_lists[,"Submission.ID"])  
#    write.csv(ebd_excluded_lists, "excluded.csv")
  }

  if (nrow(ebd_with_range_and_division) == 0)  { return (NULL) }
  
  
  print(paste ('No records ',nrow(ebd_with_range_and_division)))
 
  print(tail(ebd_with_range_and_division,1))
  write.csv(ebd_with_range_and_division, "output.csv")  
  return (ebd_with_range_and_division) 
}

# Test Code 

testHarness_processEBirdFiles <- function () {

inputEbdFile <- NULL
inputEbdFile$datapath <- '..\\data\\ebd_IN-KL-TS_relFeb-2017.zip'
inputEbdFile$name <- 'ebd_IN-KL-TS_relFeb-2017.zip'


species <- read.csv('Species.csv', header = TRUE, sep = ",") 

unzip('keralaforest.zip')
forestmap <- rgdal::readOGR('keralaforest.shp', 'keralaforest')
forestDivision <- 'Vazhachal'
startDate <- '2015-01-15'
endDate <- '2017-03-01'

output <- processEBirdFiles(inputEbdFile, species, forestmap, forestDivision, startDate, endDate)

output <- processEBirdFiles('MyEBirdData.csv', species, forestmap, 'Vazhachal', '2017-01-15', '2017-03-01')
#write.csv(output, 'testout.csv')

print (nrow(output))
}


#testHarness_processEBirdFiles()

library (plyr)
library (dplyr)
library (rgdal)
library (sp)
library (tools)

processEBirdFiles <- function (inputEbdFile, species, forestmap, forestDivision, startDate, endDate) {
  #Unzip and read eBird records
  print (inputEbdFile)
  print (file_ext(inputEbdFile)[1])
  if(tools::file_ext(inputEbdFile)[1] == 'zip') ebd <- read.csv(unz(inputEbdFile$datapath,'MyEBirdData.csv'))
  if(tools::file_ext(inputEbdFile)[1] == 'csv') ebd <- read.csv(inputEbdFile$datapath)
  
  if (nrow(ebd) == 0)  { return (NULL) }
  
  # Remove records outside the date. 
  if(as.Date(startDate) < as.Date(endDate))
  {
    ebd <- ebd [which(as.Date(ebd$Date,"%m-%d-%Y") >= as.Date(startDate)), ]
    ebd <- ebd [which(as.Date(ebd$Date,"%m-%d-%Y") <= as.Date(endDate)), ]
  }
  
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

library (dplyr)
library(reshape2)
source("genBirdDensity.R")

generateGroupSummaries <- function(ebd_density, col_filter, numeral_col=11) {
  
  ebd_density  <- ebd_density[ebd_density[,col_filter] != "",]
  
  if (nrow(ebd_density) == 0)  { return (NULL) }

  # First numeral_col columns are metadata and not values. 
  ebd_density <- cbind (ebd_density["English Name"], ebd_density [numeral_col:ncol(ebd_density)])
  
  colnames(ebd_density)[1] <- c("Species")
  return (ebd_density)
}

generateIndicatorSpecies <- function (ebd, range_level_summary=FALSE) {
  
  print(range_level_summary)

  if (range_level_summary) {
    ebd_density <- generateBirdDensity(ebd)
  }
  else {
    ebd_density <- generateOverallBirdDensity (ebd) [1:ncol(generateOverallBirdDensity(ebd))-1] # Remove SlNo last column
  }
    
  ebd_Indicator_Species <- NULL 
  ebd_Indicator_Species$Indicator <- NULL 
  
  l_ebd_Indicator_Species <- generateGroupSummaries(ebd_density, "Raptors")
  l_ebd_Indicator_Species$Indicator <- "Raptors"
  if(nrow(l_ebd_Indicator_Species) > 0)
    ebd_Indicator_Species <- rbind (ebd_Indicator_Species, l_ebd_Indicator_Species)
  
  l_ebd_Indicator_Species <- generateGroupSummaries(ebd_density, "Primary Hole Nesters")
  l_ebd_Indicator_Species$Indicator <- "Primary Hole Nesters"
  if(nrow(l_ebd_Indicator_Species) > 0)
    ebd_Indicator_Species <- rbind (ebd_Indicator_Species, l_ebd_Indicator_Species)
  
  l_ebd_Indicator_Species <- generateGroupSummaries(ebd_density, "Woodland Understorey Birds")
  l_ebd_Indicator_Species$Indicator <- "Woodland Understorey Birds"
  if(nrow(l_ebd_Indicator_Species) > 0)
    ebd_Indicator_Species <- rbind (ebd_Indicator_Species, l_ebd_Indicator_Species)
  
  l_ebd_Indicator_Species <- generateGroupSummaries(ebd_density, "Parasitic Cuckoos")
  l_ebd_Indicator_Species$Indicator <- "Parasitic Cuckoos"
  ebd_Indicator_Species <- rbind (ebd_Indicator_Species, l_ebd_Indicator_Species)
  
  # Split dataframes per range
  chartdatasplit <- split(ebd_Indicator_Species[1:ncol(ebd_Indicator_Species)-1], ebd_Indicator_Species$Indicator, drop=FALSE)
  
  return (chartdatasplit)
}

# Test Code 
testHarness_generateEndemicDensity <- function () {
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEBirdData.csv', header = TRUE, sep = ",") 
  species <- read.csv('Species.csv', header = TRUE, sep = ",") 
  
  # Obtain details of birds by joining with species file
  ebd <- left_join (ebd, species, by = 'Scientific.Name')
  ebd$RANGE <- 'Vazhachal'
  ebd$RANGE [100:500] <- 'Sholayar'
  
  output <- generateIndicatorSpecies (ebd, TRUE)
  write.csv(output$Raptors, 'testout.csv')
  output <- generateIndicatorSpecies (ebd, FALSE)
  write.csv(output$Raptors, 'testout.csv')
  print (nrow(output))
}

#testHarness_generateEndemicDensity()




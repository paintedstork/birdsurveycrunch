library (dplyr)
library(reshape2)
source ("genBirdDensity.R")

generateCommonSpecies <- function(ebd_density, numeral_col=11) {
  
  if (nrow(ebd_density) == 0)  { return (NULL) }
  
  
  # Pick English name and the range values  
  # First numeral_col columns are metadata and not values.
  ebd_density <- subset(ebd_density, select = c ("English Name", colnames (ebd_density)[numeral_col:ncol(ebd_density)]))

  ebd_density_over_ranges <- NULL
  ebd_range_list <- NULL
  # Iterate over the ranges and create individual data frames for top ten common birds
  for (x in 2:ncol(ebd_density))
  {
    ebd_range_density       <- cbind (as.data.frame (ebd_density[,1]), as.data.frame (ebd_density [,x]))
    ebd_range_density       <- head (ebd_range_density [ order (-ebd_range_density [2]),], 10)
    ebd_range_density$Range <- colnames(ebd_density)[x]
    ebd_density_over_ranges <- rbind (ebd_density_over_ranges, ebd_range_density)
  }
  
  colnames (ebd_density_over_ranges) <- c ("Species", "Density", "Range")

  # Split dataframes per range
  chartdatasplit <- split(ebd_density_over_ranges[1:2], ebd_density_over_ranges$Range, drop=FALSE)
  return (chartdatasplit)
}

# Test Code 
testHarness_generateCommonSpecies <- function () {
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEBirdData.csv', header = TRUE, sep = ",") 
  species <- read.csv('Species.csv', header = TRUE, sep = ",") 
  
  # Obtain details of birds by joining with species file
  ebd <- left_join (ebd, species, by = 'Scientific.Name')
  ebd$RANGE <- 'Vazhachal'
  ebd$RANGE [100:500] <- 'Sholayar'
  
  output <- generateBirdDensity(ebd) %>% generateCommonSpecies(11)
  write.csv(output$Sholayar, 'testout.csv')
  write.csv(output$Vazhachal, 'testout.csv')
}

#testHarness_generateCommonSpecies()


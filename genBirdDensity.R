library (plyr)
library (dplyr)
library(reshape2)

#################################################################
#           Generate Bird Density from ebd Data                 #
#                                                               #
# Param 1: ebd data                                             #
#################################################################

  
generateBirdDensity <- function(ebd) {

print(nrow(ebd))
if (nrow(ebd) == 0)  { return (NULL) }
  
# Get the complete lists and count the complete lists per range
ebd_lists           <- ebd[!duplicated(ebd[c("Submission.ID")]), ] 
ebd_complete_lists  <- ebd_lists[ebd_lists$All.Obs.Reported == 1,]
ebd_complete_lists_per_range<- dcast(ebd_complete_lists, RANGE ~ ., value.var = "Submission.ID", fun.aggregate = length )
colnames (ebd_complete_lists_per_range) <- c ("Range", "No of Complete Lists")

ebd_all <- ebd  

#Remove data from incomplete lists
ebd_all  <- ebd_all[ebd_lists$All.Obs.Reported == 1,]

#Remove spuhs
ebd_all   <- ebd_all[ebd_all$Category != 'spuh',]

#Remove slashes
ebd_all   <- ebd_all[ebd_all$Category != 'slash',]

#Remove subspecies/issf
ebd_all   <- ebd_all[!duplicated(ebd_all[c("Genus.Name","Species.Name", "Submission.ID")]),]

#Calculate the encounters per species per range
ebd_density_per_range <- dcast(ebd_all, Taxonomic.Order + English.India + Scientific.Name + IUCN + WG ~ RANGE, value.var = "Submission.ID", fun.aggregate = length )

# Dividing the encounters by number of lists for each range.
# Pick the generic columns out, only divide the columns with encounter and then cbind them back. 
# Rounding to two decimal places
# Note, the data frame is transposed for division and transposed back
ebd_density_per_range <- cbind (ebd_density_per_range [1:5], 
                  t( round (100 * t(ebd_density_per_range[6:ncol(ebd_density_per_range)]) / 
                      t(ebd_complete_lists_per_range[2])[1:ncol(t(ebd_complete_lists_per_range))], 2)))

return (ebd_density_per_range)
}


# Test Code 
testHarness_generateBirdDensity <- function () {
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEbirdData.csv', header = TRUE, sep = ",") 
  species <- read.csv('Species.csv', header = TRUE, sep = ",") 
  
  # Obtain details of birds by joining with species file
  ebd <- join (ebd, species, by = 'Scientific.Name')
  ebd$RANGE <- 'Vazhachal'
  ebd$RANGE [100:200] <- 'Sholayar'
  ebd$RANGE [200:500] <- 'Charpa'
  ebd$RANGE [500:800] <- 'Kollathirumedu'
  
  output <- generateBirdDensity(ebd) 
  write.csv(output, 'testout.csv')
  print (nrow(output))
}

#testHarness_generateBirdDensity()


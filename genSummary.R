library (plyr)
library (dplyr)
library(reshape2)
library(data.table)

#################################################################
#           Generate Species List from ebd Data                 #
#                                                               #
# Param 1: ebd data                                             #
#################################################################


generateSummary <- function(ebd) {
  
  if (nrow(ebd) == 0)  { return (NULL) }
  
  # Get the lists and Count the number of lists per range
  ebd_lists           <- ebd[!duplicated(ebd[c("Submission.ID")]), ]
  ebd_lists_per_range <- dcast(ebd_lists, RANGE ~ ., value.var = "Submission.ID", fun.aggregate = length )
  colnames (ebd_lists_per_range) <- c ("Range", "No of Lists")
  
  # Get the complete lists and count the complete lists per range
  ebd_complete_lists          <- ebd_lists[ebd_lists$All.Obs.Reported == 1,]
  ebd_complete_lists_per_range<- dcast(ebd_complete_lists, RANGE ~ ., value.var = "Submission.ID", fun.aggregate = length )
  colnames (ebd_complete_lists_per_range) <- c ("Range", "No of Complete Lists")
  
  # Get the effort per range
  ebd_effort_per_range        <- dcast(ebd_complete_lists, RANGE ~ ., value.var = "Duration..Min.", fun.aggregate = sum )
  colnames (ebd_effort_per_range) <- c ("Range", "Total Effort (Minutes)")
  
  #Create species list by removing duplicate species entries
  ebd_species   <- ebd[!duplicated(ebd[c("Taxonomic.Order","RANGE")]), ] 
  
  #Remove spuhs
  ebd_species   <- ebd_species[ebd_species$Category != 'spuh',]
  
  #Remove slashes
  ebd_species   <- ebd_species[ebd_species$Category != 'slash',]

  #Remove subspecies/issf
  ebd_species   <- ebd_species[!duplicated(ebd_species[c("Genus.Name","Species.Name", "RANGE")]),]
  
  # Count the species per range
  ebd_species_per_range   <- dcast(ebd_species, RANGE ~ ., value.var = "Scientific.Name", fun.aggregate = length )
  colnames (ebd_species_per_range) <- c ("Range", "No of Species")
  
  # Get the IUCN species and count the IUCN species per range
  ebd_iucn_species          <- ebd_species[ebd_species$IUCN != '',]
  ebd_iucn_species_per_range<- dcast(ebd_iucn_species, RANGE ~ ., value.var = "IUCN", fun.aggregate = length )
  colnames (ebd_iucn_species_per_range) <- c ("Range", "No of Threatened Species")
  
  
  # Get the WG endemic species and count the WG endemic species per range
  ebd_wg_species          <- ebd_species[ebd_species$WG == 'X',]
  ebd_wg_species_per_range<- dcast(ebd_wg_species, RANGE ~ ., value.var = "WG", fun.aggregate = length )
  colnames (ebd_wg_species_per_range) <- c ("Range", "No of Endemic Species")
  
  rangesummary <-   as.data.frame (ebd_lists_per_range$Range)
  colnames(rangesummary) <- c("Range")
  
  rangesummary <-   join (rangesummary, as.data.frame(ebd_lists_per_range), type ='left', by ='Range') 
  rangesummary <-   join (rangesummary, as.data.frame(ebd_complete_lists_per_range), type ='left', by ='Range')
  rangesummary <-   join (rangesummary, as.data.frame(ebd_effort_per_range), type ='left', by ='Range')
  rangesummary <-   join (rangesummary, as.data.frame(ebd_species_per_range), type ='left', by ='Range')
  rangesummary <-   join (rangesummary, as.data.frame(ebd_iucn_species_per_range), type ='left', by ='Range')
  rangesummary <-   join (rangesummary, as.data.frame(ebd_wg_species_per_range), type ='left', by ='Range')

  rangesummary [is.na(rangesummary)] <- 0
  rangesummary <- as.data.frame(t(rangesummary))

  # Next three lines convert the first row as column names  
  names(rangesummary) <- as.matrix(rangesummary[1, ])
  rangesummary <- rangesummary[-1, ]
  rangesummary[] <- lapply(rangesummary, function(x) type.convert(as.character(x)))
  
  # Next two lines moves the row names as first column with a name for the column
  setDT(rangesummary, keep.rownames = TRUE)[]
  colnames(rangesummary)[1] <- "Region===>"
  
  print (rangesummary)
  return (rangesummary)
}

generateOverallSummary <- function(ebd, division) {
  
  if (nrow(ebd) == 0)  { return (NULL) }
  
  # Get the lists and Count the number of lists per range
  ebd_lists           <- ebd[!duplicated(ebd[c("Submission.ID")]), ]
  
  # Get the complete lists and count the complete lists per range
  ebd_complete_lists          <- ebd_lists[ebd_lists$All.Obs.Reported == 1,]

  #Create species list by removing duplicate species entries
  ebd_species   <- ebd[!duplicated(ebd[c("Taxonomic.Order","RANGE")]), ] 
  
  #Remove spuhs
  ebd_species   <- ebd_species[ebd_species$Category != 'spuh',]
  
  #Remove slashes
  ebd_species   <- ebd_species[ebd_species$Category != 'slash',]
  
  #Remove subspecies/issf
  ebd_species   <- ebd_species[!duplicated(ebd_species[c("Genus.Name","Species.Name")]),]
  
  
  divisionsummary <- data.frame ( c(nrow(ebd_lists),
                                    nrow (ebd_complete_lists),
                                    sum (ebd_complete_lists[,"Duration..Min."]),
                                    nrow(ebd_species),
                                    nrow(ebd_species[ebd_species$IUCN != '',]),
                                    nrow(ebd_species[ebd_species$WG == 'X',])))
  
  colnames(divisionsummary)  <- c(division)
  
  return (divisionsummary)
}

# Test Code 
testHarness_generateSummary <- function () {
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEbirdData.csv', header = TRUE, sep = ",") 
  species <- read.csv('Species.csv', header = TRUE, sep = ",") 
  
  # Obtain details of birds by joining with species file
  ebd <- join (ebd, species, by = 'Scientific.Name')
  ebd$RANGE <- 'Vazhachal'
  ebd$RANGE [100:500] <- 'Sholayar'
  
  output <- cbind (generateSummary(ebd), generateOverallSummary(ebd, paste('Vazhachal ','Division')))
  write.csv(output, 'testout.csv')
  print (nrow(output))
}

#testHarness_generateSummary()


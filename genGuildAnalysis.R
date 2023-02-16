library(vegan)
library (ggplot2)

#################################################################
#           Generate Community Data set from ebd Data           #
#           Used for Vegan package                              #
# Param 1: ebd data                                             #
#################################################################

genGuildAnalysis <- function (ebd)
{
  print(nrow(ebd))
  
  # Calculate guild encounters
  guild <- dcast (ebd, Feeding.Guild ~ RANGE, value.var = "Submission.ID", fun.aggregate = length)

  # Calculate guild values in percentages
  if (ncol(guild) > 2 )
  {
    guild <- cbind (guild [1], t (round (100 * t (guild [2:ncol(guild)]) / colSums (guild [,-1]),2)))
  }
  else
  { # Strangely colSums does not work on guild[,-1]
    guild <- cbind (guild [1], t (round (100 * t (guild [2:ncol(guild)]) / colSums (guild [2]),2)))
  }

  # Show only dominant guilds - all the rest (<5% of birds) can be shown as others.   
  guild[2:ncol(guild)] [guild[2:ncol(guild)] < 5] <- 0
  
  # Convert guild column to character
  guild$Feeding.Guild <- as.character(guild$Feeding.Guild)

  # Bind a row with Others equal to 100 - x values
  guild <- rbind(guild, c(as.character(" Others"), round (100-colSums(guild[2:ncol(guild)]), 2)))

  # Convert all guild values to numeric
  guild[,c(2:ncol(guild))] <- lapply(c(2:ncol(guild)), function(x) as.numeric(as.character(guild[,x])))  
  
  # Find which rows to keep and remove rows with all 0
  rows_to_keep <- rowSums(guild[2:ncol(guild)]) > 0 

  # Filter out all zero rows
  guild <- guild[t(rows_to_keep),]
  
  # Convert it to long table for ggplot
  guild <- melt (guild) 
  
  # name columns properly
  colnames (guild) <- c("Guild", "Range", "Percentage")
  return (guild)
}

# Test Code 
testHarness_genGuildAnalysis <- function () {
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEBirdData.csv', header = TRUE, sep = ",") 
  species <- read.csv('Species.csv', header = TRUE, sep = ",") 
  
  # Obtain details of birds by joining with species file
  ebd <- left_join (ebd, species, by = 'Scientific.Name')
  ebd$RANGE <- 'Vazhachal'
  ebd$RANGE [100:500] <- 'Sholayar'
  ebd$RANGE [500:1000] <- 'Charpa'
  
  output <- genGuildAnalysis(ebd) 
  ggplot(output, aes(x = Range, y = Value,fill=Guild)) +
    geom_bar(stat='identity', size = 10, show.legend = TRUE)
}

#testHarness_genGuildAnalysis()

library(vegan)

#################################################################
#           Generate Community Data set from ebd Data           #
#           Used for Vegan package                              #
# Param 1: ebd data                                             #
#################################################################

genCommunityData <- function(ebd) {

  ebd_all <- ebd  
  
  #Remove data from incomplete lists
  ebd_all  <- ebd_all[ebd_all$All.Obs.Reported == 1,]
  
  #Remove spuhs
  ebd_all   <- ebd_all[ebd_all$Category != 'spuh',]
  
  #Remove slashes
  ebd_all   <- ebd_all[ebd_all$Category != 'slash',]
  
  ebd_diversity <- reshape2::dcast(ebd_all, Submission.ID + RANGE  ~ English.India, value.var = "Submission.ID", fun.aggregate = length ) 
  ebd_diversity <- cbind (ebd_diversity[1:2], ebd_diversity[4:ncol(ebd_diversity)]) 
  
  return (ebd_diversity)
}

genShannonDiversity  <- function (ebd_diversity)
{
  print(nrow(ebd_diversity))
  agg <- aggregate(. ~ RANGE, data=ebd_diversity[2:ncol(ebd_diversity)], sum)
  
  diversity <- as.data.frame(round (diversity (agg[2:ncol(agg)]), 2))
  rownames(diversity) <- t(agg[1])
  
  colnames (diversity) <- c ("Diversity")
 
#  print(diversity)  
  return (diversity)
}

genClusterAnalaysis  <- function (ebd_diversity)
{
  print(nrow(ebd_diversity))
  agg <- aggregate(. ~ RANGE, data=ebd_diversity[2:ncol(ebd_diversity)], sum)

  vare.dist <- vegdist(agg[2:ncol(agg)])
  
  clust.res<-hclust(vare.dist,method="average")
  clust.res$labels <- t(agg[1])
  
  return (as.dendrogram(clust.res))
}


# Test Code 
testHarness_genShannonDiversity <- function () {
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEBirdData.csv', header = TRUE, sep = ",") 
  species <- read.csv('Species.csv', header = TRUE, sep = ",") 
  
  # Obtain details of birds by joining with species file
  ebd <- left_join (ebd, species, by = 'Scientific.Name')
  ebd$RANGE <- 'Vazhachal'
  ebd$RANGE [100:200] <- 'Sholayar'
  ebd$RANGE [200:500] <- 'Charpa'
  ebd$RANGE [500:800] <- 'Kollathirumedu'
  
  output <- genCommunityData(ebd) %>% 
            genShannonDiversity ()

  mp <- barplot( t(output), width = 1, xlim= c(1,4 * nrow(output)), ylim = c(0,ceiling(max(output))), space = 2, border = par("fg"), main="Shannon Diversity", 
           xlab="Ranges", col=c("darkblue","red"))  
  text(mp, t(output), labels = t(output), pos = 3)
  
}

#testHarness_genShannonDiversity()


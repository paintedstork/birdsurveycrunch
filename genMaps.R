#################################################################
#           Generate Maps for a species list                    #
# Param 1: ebd data                                             #
# Param 2: frequency Data                                       #
# Param 3: Base map shape file                                  #
# Param 4: no of species for which maps are generated           #                          #
# Param 5: format function                                      #
# Param 6: format extension                                     #
#################################################################

genMaps <- function(ebd = NULL,
                     frequencyData = NULL,
                     forestmap = NULL,
                     division = NULL,
                     gridsize = 3,
                     smooth = T,
                     cutoff = 3,
                     empty = F, 
                     noofspecies = 10,
                     folder = "maps",
                     extension = '.jpg')
{
   print(names(frequencyData))
   print(nrow(frequencyData))
   # Sort descending by density and pick the required common species   
   commonspecies <- frequencyData %>% setorder (-Density) %>% head ( min (noofspecies, nrow(frequencyData)))  
   print(paste("Generating maps for ", nrow(commonspecies),"species"))
   for (species in commonspecies$'Scientific Name')
   {
      print(paste("Generating maps for species",species))
      print(nrow(ebd))
      out = surveymaps (species, data = ebd, tempmap = forestmap, filter = division,
                       dataformat = "PROCESSED DATA", 
                       gridsize = gridsize, 
                       smooth = smooth,
                       h = 0.1, 
                       cutoff = cutoff, 
                       showempty = empty)
      ggsave (paste(species, extension, sep=''), 
              plot = out,
              path = folder,
              width =  5,
              height = 3,
              units = 'in',
              dpi = 72)
      out <- NULL
    }
}
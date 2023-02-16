# Load the googledrive and utils packages
library(googledrive)
library(utils)
library(gargle)
library(readxl)
library(tidyverse)

drive_auth(path = "birdsurveycrunch-967d049a0daa.json")
options(googledrive_quiet = TRUE)

# Get species from Google spreadsheet
getSpecies <- function()
{
  files <- drive_ls(path="taxonomy")
  species_file <- drive_download(files$id, overwrite = TRUE)
  print(species_file)
  sp <- read_excel("Species.xlsx")
  return ( replace(sp, is.na(sp), ""))
}

getShapes <- function()
{
  folders <- drive_ls(path="shapefiles", type = 'folder')
  
  
  forestmap <- NA
  # Every shape file is in a folder
  for (i in 1: nrow(folders))
  {
    files <- drive_ls(folders$id[i], folders$name[i])
    
    # Download all files in the folder
    for (j in 1: nrow(files))
    {
      # Download the zip file from Google Drive
      drive_download(file = files$id[j], path = paste0("shapes\\", files$name[j]), overwrite = TRUE)
    }
    
    # Open the shape file
    rmap <- readOGR(paste0("shapes\\", folders$name[i],".shp"), folders$name[i])
    print("rmap")
    print(paste("rmap", data.frame (rmap$Division) %>% nrow()))
    
    # Binding multiple spatial polygons
    if(is.na(forestmap))
    {
      forestmap <- rmap
    }
    else
    {
      print(paste("Binding",data.frame (forestmap$Division) %>% nrow()))
      forestmap <- rbind(forestmap, rmap)
    }
  }
  return (forestmap) 
}



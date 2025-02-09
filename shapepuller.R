# Load the googledrive and utils packages
library(googledrive)
library(utils)
library(gargle)
library(readxl)
library(tidyverse)
library(sf)

drive_auth(path = "birdsurveycrunch-967d049a0daa.json")
options(googledrive_quiet = TRUE)

# Get species from Google spreadsheet
getSpecies <- function()
{
  files <- drive_ls(path="taxonomy")
  species_file <- drive_download(files$id, overwrite = TRUE)
  print(species_file)
  sp <- read_excel("Species.xlsx")
  # Replace spaces and hyphens with dots in column names
  names(sp) <- gsub("[ -]", ".", names(sp))
  
  return ( replace(sp, is.na(sp), ""))
}

# Get species from Google spreadsheet
getShapeList <- function()
{
  files <- drive_ls(path="shapefiles") %>% 
                  as.data.frame() %>% 
                      filter(name == 'Shapefilelist')
  shapelistfile <- drive_download(files$id, overwrite = TRUE)
  print(shapelistfile)
  shapefiles <- read_excel("Shapefilelist.xlsx")

  return (shapefiles)
}

getShape <- function(polygon, folder)
{
  rmap <- NULL
  print("Calling getShape")

  if(!file.exists(paste0("shapes\\", folder, ".shp")))
  {
    print(paste("Downloading",folder,polygon))
    folders <- drive_ls(path="shapefiles", type = 'folder')
    
    if (grepl(folder, folders) %>% any())
    { # Folder present
      fid   <- folders %>% as.data.frame() %>% filter (name == folder) %>% dplyr::select(id)
      files <- drive_ls(fid$id, folder)
      
      # Download all files in the folder
      for (j in 1: nrow(files))
      {
        # Download the zip file from Google Drive
        drive_download(file = files$id[j], path = paste0("shapes\\", files$name[j]), overwrite = TRUE)
      }
    }
  }
  
  if(file.exists(paste0("shapes\\", folder, ".shp")))
  {
    rmap <- st_read(paste0("shapes\\", folder, ".shp")) %>% st_make_valid()
    print(paste("rmap", data.frame (rmap$Division) %>% nrow()))
  }
  return (rmap)    
}

getShapes <- function()
{
  print("Calling getShapes")
  folders <- drive_ls(path="shapefiles", type = 'folder')
#  folders <- data.frame()
#  rmap <- readOGR(".\\MM Hills.shp","MM Hills")
#  print("MM Hills map reading")
  
  forestmap <- NA
#  forestmap <- rgdal::readOGR('Division&Range7.shp', 'Division&Range7')
  
  # Exit if there are no shapes
  if(nrow(folders) > 0 )
  {
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
#      rmap <- readOGR(paste0("shapes\\", folders$name[i],".shp"), folders$name[i])
      rmap <- st_read(paste0("shapes\\", folders$name[i], ".shp"))
      rmap <- st_make_valid(rmap)
      print("rmap")
      print(paste("rmap", data.frame (rmap$Division) %>% nrow()))
      
      # Binding multiple spatial polygons
      if (!inherits(forestmap, "sf")) {
        # If forestmap is not an sf object (i.e., not initialized), initialize it with rmap
        forestmap <- rmap
      } else if (nrow(forestmap) == 0) {
        # If forestmap is an empty sf object, initialize it with rmap
        forestmap <- rmap
      } else {      
        print(paste("Binding",data.frame (forestmap$Division) %>% nrow()))
        forestmap <- rbind(forestmap, rmap)
      }
    }
  }

  return (forestmap) 
}



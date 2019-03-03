library(tidyverse)
library(ggfortify)
library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(mltools)
library(scales)
library(maptools)
library(mapproj)

## transform data makes different types of data uniform

transformdata = function(data, dataformat = "PUBLIC DATA")
{
  
  ## choosing important variables
  
  if (dataformat == "PUBLIC DATA")
  {
    imp = c("SCIENTIFIC.NAME","OBSERVATION.COUNT",
            "LOCALITY.ID","LOCALITY.TYPE","COUNTY.CODE",
            "LATITUDE","LONGITUDE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID", 
            "FIRST.NAME", "LAST.NAME","SAMPLING.EVENT.IDENTIFIER",
            "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
            "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","group.id","day")
    
    
    data = data %>%
      mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
      mutate(day = as.numeric(as.factor(OBSERVATION.DATE))) %>%
      dplyr::select(imp) %>%
      ungroup
  }
  
  if (dataformat == "OWN DATA")
  {
    data = data[,c(1,3,9,10,11,15)]
    names(data) = c("group.id","SCIENTIFIC.NAME","LATITUDE",
                    "LONGITUDE","OBSERVATION.DATE","ALL.SPECIES.REPORTED")
  }

  if (dataformat == "PROCESSED DATA")
  {
    data = data[,c(3,5,6,7,8,10)]
    names(data) = c("SCIENTIFIC.NAME",
                    "LATITUDE",
                    "LONGITUDE", 
                    "group.id",
                    "OBSERVATION.DATE",
                    "ALL.SPECIES.REPORTED")
  }
  
  return(data)
}
## data is self-explanatory
## dataformat = PUBLIC DATA, OWN DATA, PROCESSED DATA
## tempmap is where the shapefile dataframe is provided
## 'filter' can take the names of multiple divisions, and will compute subsequently on only those
## (cont'd) polygons; currently defaulted to "Periyar Tiger Reserve"
## 'species' can take only a single value
## 'gridsize' is the desired resolution
## 'smooth' = T for smoothing
## 'h' is the primary smoothing parameter
## 'cutoff' is the minimum no. of lists to consider a grid filled
## 'showempty' = F will not shopw empty grids in grey

surveymaps = function(species, 
                      data,
                      tempmap,
                      filter,
                      dataformat = "PROCESSED DATA",
                      gridsize = 3, 
                      smooth = F, 
                      h = 0.1, 
                      cutoff = 2, 
                      showempty = T)
{
  print( paste (species,
                nrow(data), 
                filter,
                nrow(tempmap),
                dataformat,
                gridsize,
                smooth,
                h,
                cutoff,
                showempty
                ))
  
# If data has not been passed, read the data from the file supplied.  
  if(is.null(data))
  {
    return (0);
  }
  
  data = transformdata(data, dataformat)

  data = data %>%
    filter(ALL.SPECIES.REPORTED == 1)
  
  ## change the next 2 statements as required
  tmap = tempmap[tempmap@data$Division %in% filter,]
  tmapf = fortify(tmap, region = c("Range"))

  
  map = ggplot() +
    geom_polygon(data = tmapf, aes(x=long, y=lat, group=group), colour = 'black', fill = "white")+  
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw()+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank())+
    coord_map()

  bb = bbox(tmap) # creates a box with extents from map
  ## add 10%
  x = abs(bb[1] - bb[3])*0.1
  y = abs(bb[2] - bb[4])*0.1
  bb[1] = bb[1] - x
  bb[3] = bb[3] + x
  bb[2] = bb[2] - y
  bb[4] = bb[4] + y
  
  cs = c(gridsize*1000/111111,gridsize*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  gridmap = sp_grd_poly
  mask = gridmap - tmap

  ## add grid attributes to data
  
  temp = data %>% group_by(group.id) %>% slice(1)

  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmap)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "grid"

  ## calculate frequencies, find empty grids, set foundation for smoothing
  
  data1 = data %>%
    filter(SCIENTIFIC.NAME == species)
  
  if(nrow(data1) == 0) return (NULL)
                               
  temp = data %>% 
    group_by(grid) %>%
    mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
    filter(SCIENTIFIC.NAME == species, lists >= cutoff) %>%
    group_by(grid) %>%
    summarize(freq = n_distinct(group.id)/max(lists))
  temp$grid = as.factor(temp$grid)
  
  min = min(temp$freq)
  max = max(temp$freq)
  
  filled = data %>%
    group_by(grid) %>%
    mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
    filter(lists >= cutoff) %>%
    distinct(grid,lists)
  
  fortified = fortify(gridmap, region = c("id"))
  forttot = fortify(tmap)
  mnlo = min(forttot$long)
  mnla = min(forttot$lat)
  mxlo = max(forttot$long)
  mxla = max(forttot$lat)
  
  fort1 = fortified %>%
    group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
  
  zlists = setdiff(unique(fort1$id),unique(filled$grid))
  empty = data.frame(unique(zlists),0)
  names(empty) = names(filled)
  
  fortified$id = as.factor(fortified$id)
  fort1$id = as.factor(fort1$id)
  
  plotdf = na.omit(left_join(fortified,temp, by = c('id' = "grid"))) # SPDF to plot
  
  if(length(zlists > 0))
  {
    emptydf = na.omit(left_join(fort1,empty, by = c('id' = "grid"))) # SPDF to plot
    switch = T
  }else{
    switch = F
  }
  
  data1 = data1 %>%
    group_by(grid) %>% slice(1) %>% ungroup()
  data1$grid = as.factor(data1$grid)
  
  mmplotdf = plotdf %>%
    group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), 
                               maxlat = max(lat), freq = max(freq)) %>% ungroup()
  
  data1 = left_join(data1,mmplotdf,by = c("grid" = 'id'))
  roundUp = function(x) 10^ceiling(log10(x))
  data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
  data1 = data1 %>% filter(freq > 0)
  
  
  data1 = data1[rep(row.names(data1), data1$freq),]
  
  
  data1 = data1 %>%
    group_by(grid) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                      max(maxlong)),runif(n(),
                                                                          max(minlat),
                                                                          max(maxlat)))[,1],
                              LATITUDE = cbind(runif(n(),max(minlong),
                                                     max(maxlong)),runif(n(),
                                                                         max(minlat),
                                                                         max(maxlat)))[,2])
  
  plotdf$freq1 = mltools::bin_data(plotdf$freq, bins=4, binType = "quantile")
  
  sm = plotdf %>%
    group_by(freq1) %>% summarize(min = round(min(freq),2),max = round(max(freq),2))
  
  l = length(sm$freq1)
  vals = c("#99CCFF","#6699CC","#336699","#003399")
  
  if(smooth){
    stats = map +
      stat_density2d(data = data1, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon") +
      scale_fill_gradient2(low = muted("blue"),
                           high = "white", space = "Lab", na.value = "grey50", trans = 'reverse')
    minstat = min(ggplot_build(stats)$data[[2]]$level)
    maxstat = max(ggplot_build(stats)$data[[2]]$level)
    
    for (i in c(4,3,2,1))
    {
      breaks = seq(minstat,maxstat,length.out=i)
      labels = round(seq(min,max,length.out=i),2)
      if (length(unique(labels)) == i)
        break
    }
  }
  
  
  cols = "grey30"
  cl = paste(" <",cutoff,"lists")
  names(cols) = cl
  
  
  out = map +
    {if(smooth)stat_density2d(data = data1, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
    {if(!isTRUE(smooth))geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq1))} +
    {if(switch & showempty)geom_polygon(data = emptydf, aes(x = long, y = lat, group = group, col = cl), fill = "grey30")} +
    #geom_point(data = data, aes(x = LONGITUDE, y = LATITUDE)) +
    geom_path(data = fortify(gridmap), aes(x = long, y = lat, group = group), col = "black") +
    geom_polygon(data = mask, aes(x = long, y = lat, group = group), col = 'white', fill = 'white') +
    geom_path(data = tmap, aes(x = long, y = lat, group = group), col = 'black', size = 1) +
    {if(l <= 2 & !isTRUE(smooth))scale_fill_manual(values = vals[1:l],
                                                   breaks = sm$freq1, labels = sm$freq1)} +
    {if(l == 3 & !isTRUE(smooth))scale_fill_manual(values = vals[1:l],
                                                   breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2],
                                                                                                             " - ",sm$max[2]), paste(">",sm$min[3])))} +
    {if(l > 3 & !isTRUE(smooth))scale_fill_manual(values = vals,breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),
                                                                                              paste(sm$min[2]," - ",sm$max[2]), paste(sm$min[3]," - ",sm$max[3]),
                                                                                              paste(">",sm$min[4])))} +
    {if(smooth)scale_fill_gradient2(low = muted("blue"),
                                    high = "white", space = "Lab", na.value = "grey50", trans = 'reverse',
                                    breaks = breaks, labels = labels)} +
    {if(switch)scale_colour_manual(values = cols)} +
    #theme(legend.justification=c(1,1), legend.position=c(0.99,0.99)) +
    theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
    guides(fill = guide_legend(title = "", reverse = TRUE, override.aes = list(size=10)))
  
  return(out)
}

# Test harness
# reading maps
test_harness_map <- function()
{
    forestmap = readOGR("Division&Range7.shp","Division&Range7")
# To test data streaming
   temp <- readRDS("temp.RDS")
   print(nrow(temp))
# To test raw data
#    temp = read.csv("output.csv", stringsAsFactors=F)
    surveymaps("Ocyceros griseus", data = temp, tempmap = forestmap, filter = c("Periyar Tiger Reserve"), dataformat = "PROCESSED DATA", gridsize = 4, smooth = T, h = 0.1, cutoff = 3, showempty = F)
}

#test_harness_map()

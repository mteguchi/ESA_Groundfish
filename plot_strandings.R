#plot_strandings

rm(list=ls())
# ifelse(Sys.info()[1] == 'Linux',
#        source('~/Documents/R/tools/TomosFunctions.R'),
#        source('~/R/tools/TomosFunctions.R'))
library(tidyverse)

water.color <- "lightblue"
land.color <- "darkgray"

library(maps)          # creating geographical maps
# library(mapdata)       # go with maps - contains topo and geologic data
# library(mapproj)       # for creating projected maps
library(sp)            # classes and methods for spatial data
# library(maptools)      # tools for reading and handling spatial objects
# library(gpclib)        # can't install because needs to be compiled... 5/18/2016
# library(sfsmisc)       # utilities from Seminar fuer Statistik ETH Zurich
# library(raster)        # tools to deal with raster images
# library(rgeos)         # interface to geometry engine
library(rgdal)         # bindings for the geospatial data abstraction library
# library(scales)        # making transparency
# 
#library(ggmap)
# library(ggplot2)
# library(ggsn)         # for adding scale bars
# library(directlabels) # for adding contour line labels
library(broom)        # instead of fortify in ggplot2 (recommended)
library(tidyverse)

save.fig <- FALSE
# if there is no internet, use the stored file - this may not work
# as Google apparently allow the use of downloaded images for only
# 30 days. 
#internet <- T

# # create the base map - Google stopped sharing these maps...
# if (internet){
#   West.coast <- get_map(location = c(lon = -120.0,
#                                      lat = 40.0),
#                         zoom = 5,
#                         maptype = "terrain",
#                         source = 'google')
#   saveRDS(West.coast, file = 'RData/DC_stranding_westcoast.RData')  
# } else {
#   West.coast <- readRDS(file = 'RData/DC_stranding_westcoast.RData')  
#   print('read from RData')
# }
# 
# map.west.coast <- ggmap(West.coast)


# convert the lat/lon into northing/easting
# An arbitrary center point was created here.
# approx.center <- data.frame(X=-125.0, Y=40.0)
# sp::coordinates(approx.center) <- c("X", "Y")
# sp::proj4string(approx.center) <- sp::CRS("+proj=longlat +datum=WGS84")
# center.UTM <- sp::spTransform(approx.center,
#                               sp::CRS("+proj=utm +zone=10 ellps=WGS84"))

# file for the stranding data:
infile <- 'data/Dermochelys_Strandings_2020-10-29.csv'

E.end <- -112
W.end <- -128
N.end <- 51
S.end <- 30

# center.latlon = data.frame(lat = approx.center$Y, 
#                            lon = approx.center$X)
# coast.line <- getCoastLine(paste0(dirSelector()$Rdir,
#                                   'Oceans and Maps/coast/coast_Epac.txt'),
#                            lon.limits = c(-128, E.end),
#                            lat.limits = c(30, N.end))

# coast line data are from here: http://openstreetmapdata.com/data/coastlines or
# https://shoreline.noaa.gov/data/datasheets/medres.html and
# http://datapages.com/gis-map-publishing-program/gis-open-files/global-framework/global-heat-flow-database/shapefiles-list
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2016-eng.cfm

# https://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html f is the full resolution - too big
# h is high resolution - should be good enough. 
all.coast <- sp::spTransform(rgdal::readOGR(dsn = paste0("C:/Users/tomo.eguchi/Documents/R/Oceans and Maps/gshhg-shp-2.3.7/GSHHS_shp/h"),
                                            layer = "GSHHS_h_L1",
                                            verbose = FALSE),
                             sp::CRS("+proj=longlat +datum=WGS84"))

W.coast <- raster::crop(all.coast, raster::extent(c(W.end, E.end, S.end, N.end)))

W.coast.df <- broom::tidy(W.coast) 
#   filter(long < E.end & long > W.end & lat < N.end & lat > S.end)

# coast.line.df <- do.call(rbind, W.coast.df)
# colnames(coast.line.df) <- c('X', 'Y', 'idx')
# coast.line.Sp <- latlon2sp(coast.line.df, center.latlon)

# the following shape file was obttained from here: #https://gis.stackexchange.com/questions/153514/us-mexican-border-data
# note that the USGS link is broken but direct link "here" is available. 
# http://txpub.usgs.gov/BEHI/Data_download/Boundaries_Layers/International_Boundary_shp.zip
US_MX_border <- sp::spTransform(rgdal::readOGR(dsn = paste0("c:/Users/tomo.eguchi/Documents/R/Oceans and Maps/International_Boundary/shp"),
                                               layer = "International_Boundary_Final",
                                               verbose = FALSE),
                                sp::CRS("+proj=longlat +datum=WGS84"))

US_MX_border <- raster::crop(US_MX_border, raster::extent(c(W.end, E.end, 30, 35)))
US_MX_border.df <- broom::tidy(US_MX_border)

# US-Canada border was obtained from here: 
# https://hifld-geoplatform.opendata.arcgis.com/datasets/canada-and-us-border
US_Canada_border <- sp::spTransform(rgdal::readOGR(dsn = paste0("C:/Users/tomo.eguchi/Documents/R/Oceans and Maps/Canada_and_US_Border"),
                                                  layer = "Canada_and_US_Border",
                                                  verbose = FALSE),
                                   sp::CRS("+proj=longlat +datum=WGS84"))

US_Canada_border <- raster::crop(US_Canada_border, raster::extent(c(W.end, E.end, 47, 51)))
US_Canada_border.df <- broom::tidy(US_Canada_border) 

# state borders from here: https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
state_border <- sp::spTransform(rgdal::readOGR(dsn = paste0("C:/Users/tomo.eguchi/Documents/R/Oceans and Maps/cb_2017_us_state_500k"),
                                                   layer = "cb_2017_us_state_500k",
                                                   verbose = FALSE),
                                    sp::CRS("+proj=longlat +datum=WGS84"))

state_border <- raster::crop(state_border, raster::extent(c(W.end, E.end, 32, 50)))
state_border.df <- broom::tidy(state_border)

# critical habitat polygons can be found here:
# http://www.nmfs.noaa.gov/pr/species/criticalhabitat.htm
Dc_CH <- spTransform(readOGR(dsn = "C:/Users/tomo.eguchi/Documents/R/Oceans and Maps/Final_LeatherbackCH",
                             layer = "Final_LeatherbackCH",
                             verbose = F),
                     CRS("+proj=longlat +datum=WGS84"))

Dc_CH.df <- tidy(Dc_CH)
border.color <- "gray20"
p1 <- ggplot() + 
  geom_polygon(data = data.frame(y = c(N.end, S.end, S.end, N.end, N.end),
                                 x = c(W.end, W.end, E.end, E.end, W.end)),
               aes(x = x, y = y),
               fill = water.color,
               alpha = 0.8) + 
  geom_polygon(fill = land.color,
               data = W.coast.df,
               aes(x=long, y=lat, group = id),
               alpha = 0.9) +  
  geom_path(data = US_MX_border.df,
            aes(x = long, y = lat, group = group),
            color = border.color,
            size = 0.5) + 
  geom_path(data = US_Canada_border.df,
            aes(x = long, y = lat, group = group),
            color = border.color,
            size = 0.5) + #coord_map()
  geom_path(data = state_border.df,
            aes(x = long, y = lat, group = group),
            color = border.color,
            size = 0.5) + #coord_map()
  geom_path(data = data.frame(x = c(E.end, E.end), 
                              y = c(S.end, N.end)),
            aes(x = x, y = y),
            color = land.color,
            size = 1.2) + 
  geom_polygon(data = Dc_CH.df,
               aes(x = long, y = lat, group = group),
               fill = 'azure', colour = 'firebrick1',
               alpha = 0.4) +
  coord_map() +
  xlim(c(-128, E.end))+
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  xlab(expression(paste("Longitude (", degree, "W)", sep=""))) 
    
p1


dat1 <- read.table(infile, sep = ",", header = TRUE)
dat.north <- filter(dat1, Latitude > 50)
dat1 %>% filter(Latitude <= 50) %>%
  mutate(Year = factor(Year_Initially_Observed)) -> dat1
# dat1$yr.fac <- as.factor(dat1$Year_Initially_Observed)
# 
# dat1.dead <- dat1[-grep("TRUE", dat1$Alive_Released),]
# #dat1.dead.CA <- dat1.dead[dat1.dead$STATE == 'CA',]
# dat1.human <- dat1.dead[dat1.dead$Human_Interaction != 'NO',]
# 
# dat1.human.locs <- na.omit(dat1.human[, c('yr.fac', 'Latitude', 'Longitude')])
# colnames(dat1.human.locs) <- c('Year', 'Latitude', 'Longitude')

p1a <- p1 + 
  geom_point(data = dat1,
             aes(x = Longitude, 
                 y = Latitude,
                 color = Year)) 
  
p1a
if (save.fig){
  ggsave(filename = paste0('figures/Dc_strandings_map_', 
                           Sys.Date(), '.png'),
         device = "png",
         width = 8,
         height = 7,
         plot = p1a,
         dpi = 600)
  
}


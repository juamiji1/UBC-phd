#--------------------------------------------------------------------------------------------------
# PROJECT: Guerrillas and Development 
# AUTHOR: JMJR
#
# TOPIC: Preparin GIS data
#--------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------
## PACKAGES AND LIBRARIES:
#
#---------------------------------------------------------------------------------------
#install.packages('bit64')
#install.packages('raster')
#install.packages('exactextractr')
#install.packages('rmapshaper')
#install.packages('geojsonio')

library(data.table)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggrepel)
library(sf)
library(spdep)
library(sp)
library(ggpubr)
library(dplyr)
library(tidyr)
library(scales) 
library(tidyverse)
library(lubridate)
library(gtools)
library(foreign)
library(ggmap)
library(maps)
library(gganimate)
library(gifski)
library(transformr)
library(tmap)
library(raster)
library(exactextractr)
library(matrixStats)
library(rgeos)
library(rmapshaper)
library(geojsonio)
library(plyr)
library(spatialEco)

#Directory: 
current_path ='C:/Users/juami/Dropbox/My-Research/Guerillas_Development/2-Data/Salvador/'
setwd(current_path)


#---------------------------------------------------------------------------------------
## PREPARING SHAPEFILES:
#
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# Preparing Administrative boundaries:
#---------------------------------------------------------------------------------------
#Importing El salvador shapefile
slvShp <- st_read(dsn = "gis/slv_adm_2020_shp", layer = "slv_borders_census2007")
slv_crs <- st_crs(slvShp)

#Importing El salvador shapefile of segments 
#slvShp_segm <- st_read(dsn='censo2007/shapefiles', layer = "DIGESTYC_Segmentos2007")
slvShp_segm <- st_read(dsn='gis/maps_interim', layer = "segm07_nowater")
st_crs(slvShp)==st_crs(slvShp_segm)
slvShp_segm <-st_transform(slvShp_segm, crs=slv_crs)
st_crs(slvShp)==st_crs(slvShp_segm)

#Transforming sf object to sp object 
slvShp_sp <- as(slvShp, Class='Spatial')

#---------------------------------------------------------------------------------------
# Preparing Guerrilla boundaries:
#---------------------------------------------------------------------------------------
#Importing FMLN control zones
controlShp <- st_read(dsn = "gis/guerrilla_map", layer = "zona_control_onu_91")
st_crs(controlShp) <- slv_crs

controlShp82 <- st_read(dsn = "gis/maps_interim", layer = "zona_control_82")
st_crs(controlShp82) <- slv_crs

controlShp_cut <- st_read(dsn = "gis/maps_interim", layer = "zona_control_onu_91_cut")
st_crs(controlShp_cut) <- slv_crs

controlShp_noslv <- st_read(dsn = "gis/maps_interim", layer = "zona_control_onu_91_noslv")
st_crs(controlShp_noslv) <- slv_crs

#Importing FMLN disputed zones
disputaShp <- st_read(dsn = "gis/guerrilla_map", layer = "zona_fmln_onu_91")
st_crs(disputaShp) <- slv_crs

#Converting polygons to polylines
#control_line <- st_cast(controlShp,"MULTILINESTRING")
control_line <- st_read(dsn = "gis/maps_interim", layer = "control91_line")
control_line_cut <- st_read(dsn = "gis/maps_interim", layer = "control91_line_cut")
control_line_noslv <- st_read(dsn = "gis/maps_interim", layer = "control91_line_noslv")

disputa_line <- st_cast(disputaShp,"MULTILINESTRING")


#---------------------------------------------------------------------------------------
## INCLUDING THE LINE BREAK FE:
#
#---------------------------------------------------------------------------------------
slvShp_segm_info<-st_make_valid(slvShp_segm)

#Sampling points int the borders for the RDD
set.seed(1234)

control_line_sample <- st_sample(control_line, 50, type="regular")
pnt_controlBrk_50 <- st_cast(control_line_sample, "POINT")

control_line_sample <- st_sample(control_line, 100, type="regular")
pnt_controlBrk_100 <- st_cast(control_line_sample, "POINT")

control_line_sample <- st_sample(control_line, 200, type="regular")
pnt_controlBrk_200 <- st_cast(control_line_sample, "POINT")

#Calculating the distance of each census segment to Control border breaks
distBrk<-st_distance(slvShp_segm_info, pnt_controlBrk_50, by_element = FALSE)

#Converting from units object to numeric array
distMatrix<-distBrk %>% as.data.frame() %>%
  data.matrix()

#Calculating the min for each row
distMin<-rowMins(distMatrix)

#Extracting the column indexes as the breaks FE 
brkIndex<-which((distMatrix==distMin)==1,arr.ind=TRUE)

#Dropping duplicates and sorting by row 
brkIndexUnique<-brkIndex[!duplicated(brkIndex[, "row"]), ]  
brkIndexUnique<-brkIndexUnique[order(brkIndexUnique[, "row"]),]

#Adding information to shapefile
slvShp_segm_info$cntrldist_brk50<-distMin
slvShp_segm_info$cntrlbrkfe50<-brkIndexUnique[, 'col']

#Calculating the distance of each census segment to disputed border breaks
distBrk<-st_distance(slvShp_segm_info, pnt_controlBrk_100, by_element = FALSE)

#Converting from units object to numeric array
distMatrix<-distBrk %>% as.data.frame() %>%
  data.matrix()

#Calculating the min for each row
distMin<-rowMins(distMatrix)

#Extracting the column indexes as the breaks FE 
brkIndex<-which((distMatrix==distMin)==1,arr.ind=TRUE)

#Dropping duplicates and sorting by row 
brkIndexUnique<-brkIndex[!duplicated(brkIndex[, "row"]), ]  
brkIndexUnique<-brkIndexUnique[order(brkIndexUnique[, "row"]),]

#Adding information to shapefile
slvShp_segm_info$cntrldist_brk100<-distMin
slvShp_segm_info$cntrlbrkfe100<-brkIndexUnique[, 'col']

#Calculating the distance of each census segment to disputed border breaks
distBrk<-st_distance(slvShp_segm_info, pnt_controlBrk_200, by_element = FALSE)

#Converting from units object to numeric array
distMatrix<-distBrk %>% as.data.frame() %>%
  data.matrix()

#Calculating the min for each row
distMin<-rowMins(distMatrix)

#Extracting the column indexes as the breaks FE 
brkIndex<-which((distMatrix==distMin)==1,arr.ind=TRUE)

#Dropping duplicates and sorting by row 
brkIndexUnique<-brkIndex[!duplicated(brkIndex[, "row"]), ]  
brkIndexUnique<-brkIndexUnique[order(brkIndexUnique[, "row"]),]

#Adding information to shapefile
slvShp_segm_info$cntrldist_brk200<-distMin
slvShp_segm_info$cntrlbrkfe200<-brkIndexUnique[, 'col']





# Converting from sf to sp object
slvShp_segm_info_sp <- as(slvShp_segm_info, Class='Spatial')

#Exporting the all data shapefile
#writeOGR(obj=slvShp_segm_info_sp_v2, dsn="C:/Users/jmjimenez/Dropbox/My-Research/Guerillas_Development/2-Data/Salvador/gis/nl_segm_lvl_vars", layer="slvShp_segm_info_sp_onu_91", driver="ESRI Shapefile",  overwrite_layer=TRUE)
writeOGR(obj=slvShp_segm_info_sp, dsn="C:/Users/juami/Dropbox/My-Research/Guerillas_Development/2-Data/Salvador/gis/maps_interim", layer="slvShp_segm_brks", driver="ESRI Shapefile",  overwrite_layer=TRUE)



pnt_controlBrk_50_sp <- as(pnt_controlBrk_50, Class='Spatial')
pnt_controlBrk_50_sp <-as(pnt_controlBrk_50_sp,"SpatialPointsDataFrame")

writeOGR(obj=pnt_controlBrk_50_sp, dsn="C:/Users/juami/Dropbox/My-Research/Guerillas_Development/2-Data/Salvador/gis/maps_interim", layer="pnt_controlBrk_50", driver="ESRI Shapefile",  overwrite_layer=TRUE)



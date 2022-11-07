## Malisse Lummus- PhD Candidate, University of Kansas
## Script to delineate water tower Units for High Mountain Asia
## Inspired and derived from code provided by Immerzeel et al 2020
## Assigns ice volume, area and snow persistence to GMBA polygons and subsets based on thresholds
## Updated for revision to delineate only downstream basin parts connected to WTU
## Result is saved as shapefile

# Import libraries
rm(list=ls(all=TRUE))
library(sf)
library(raster)
library(maptools)
library(rgdal)
library(rgeos)
library(fasterize)
library(tmaptools)
library(stars)
library(spex)
library(tidyverse)
library(lwgeom)
library(terra)
library(tidyr)

##SETTINGS
base = "F:\\HMA_impact_index\\water_towers\\data\\"
outdir <- "F:\\HMA_impact_index\\water_towers\\data\\index\\units\\"
hydrobasins = read_sf("F:\\HMA_impact_index\\Q_file\\fixed_hkh_sub_basin.shp")
continents <- c("asia", "neareast")
resolution <- 0.05
##SETTINGS END

# DID ALL OF THE FOLLOWING IN QGIS:
#   GMBA Mountain inventory_v1.2-World.shp was fixed using the "Fix geometries" tool.
#   p05_degree_glacier_volume_km3 and p05_degree_glacier_area_km2 rasters from Farinotti 
#   and the Snow_persistence_avg_annual raster from MODIS were loaded and clipped to the study area.
#   The zonal statistics of each GMBA zone were calculated, using the sum for the volume and area and then
#   the mean of the snow using Zonal Statistics tool. The GMBA, glacier volume
#   glacier area, and snow persistence stats were combined into one shapefile (named: gmba_all). A subset of the data is selected 
#   using the "select by function" tool, with function "(vol_km3 > 0.1 OR snow_p >0.1) AND area_km2 >0.1"
#   as done above (named: gmba_ss). The new shapefile has an attribute table with "Name", "Country", 
#   "vol_km3", "Area_km2", and "snow_p" as table column names. Any names that differ can be changed using
#   the vector table toolbox > rename field

# intersect hydrobasins and filtered GMBA mountain ranges, then dissolve boundaries to get WTUs
gmbafn3 = "F:\\HMA_impact_index\\Q_file\\gmba_ss_HMA.shp" 
mntns <- read_sf(gmbafn3)
WTU1 <- st_intersection(hydrobasins,mntns) #find the intersection btwn the hydro basins and mountain basins
WTU2 <- st_make_valid(WTU1) # fix the geometry
WTU3 <- WTU2 %>% group_by(WTU2$L2_Name) %>% summarize_all(mean) # dissolve the mntn basin boundaries, keeping the hydro basin boundaries

write_sf(WTU3,paste(outdir,"wtu_temp.shp",sep="")) # make this a shapefile and save to the out directory

#read WTU shp
WTU <- st_make_valid(read_sf(paste(outdir,"wtu_temp.shp",sep=""))) # read in the newly created shapefile and fix the geometry

#extract downstream areas of WTUs
#loop over continents using continent-based FAO hydrobasins shapefiles with subbasins
for (con in continents)
{
  #get subbasins for continent
  subbasins <- st_make_valid(read_sf(paste(base,"Basins\\FAO\\hydrobasins_",con,"\\hydrobasins_",con,".shp",sep="")))
  
  #subset of all subbasins that overlap with WTUs, and store their IDs
  subbasins_overlap_wtu <- st_intersection(WTU,subbasins)
  df_subbas <- unique(subbasins_overlap_wtu$SUB_BAS)
  
  #get IDs of downstream subbasins, downstream of subset
  downstream <- unique(subbasins_overlap_wtu$TO_BAS)
  
  #subset the subbasins with the downstream IDs, and store their IDs
  subbasins_x <- subset(subbasins,subbasins$SUB_BAS %in% downstream)
  df_subbas <- c(df_subbas,unique(subbasins_x$SUB_BAS))
  
  #repeat the above until no more downstream subbasins remain
  i <- 1
  repeat {
    downstream <- unique(subbasins_x$TO_BAS)
    subbasins_y <- subset(subbasins,subbasins$SUB_BAS %in% downstream)
    df_subbas <- c(df_subbas,unique(subbasins_x$SUB_BAS))
    subbasins_x <- subbasins_y
    print(paste(con,i,sep=" "))
    i <- i+1
    
    if(nrow(subbasins_x)<=1)
    {
      break
    }
    ##in neareast there is a sequence of 6 subbasins draining into each other resulting in endless loop. made exception below to exit the loop earlier
    if(con == "neareast" & nrow(subbasins_x)<=7)
    {
      break
    }
  }
  #extract polygons with WTU basins per continent
  basins_final <- unique(df_subbas)
  wtu_connected <- subset(subbasins,subbasins$SUB_BAS %in% basins_final)
  WTU_ds <- wtu_connected %>% group_by(wtu_connected$MAJ_BAS) %>% summarize_all(mean)
  
  #write as shapefile
  write_sf(WTU_ds,paste(outdir,"WTU_basins_",con,"_temp.shp",sep=""))
}

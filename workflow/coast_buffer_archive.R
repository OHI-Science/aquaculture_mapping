## This script will buffer the coastline of each place which has known locations and save each as a file. When it is done it will compile them all together and rasterize.
## We will use this raster to validate our model. 

#### load libraries and data ####
#------------------------------------------------------------------------------#
library(sf)
library(spatialEco)
library(tidyverse)
library(raster)
library(rgeos)
library(rmapshaper)
library(mapview)
library(fasterize)

# load in crs, coastlines, eez, masks, country shapes, ports, depth, and rasters
source("./_spatial/template_raster.R")
gadm_diss <- st_read("/home/shares/food-systems/Global_datasets/World_map/World_ISO_dissolve.shp")
gadm_full <- st_read("/home/shares/food-systems/Global_datasets/World_map/gadm36.shp")
land_mask <- raster("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/land_mask.tif")
eez_mask <- raster("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/eez_mask.tif")
eez_sf <- st_read("/home/shares/food-systems/Global_datasets/EEZ/eez_v10.shp")
coastline <- readRDS("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/outline.rds")


inverted_land <- land_mask
inverted_land[is.na(inverted_land[])] <- .5
inverted_land[inverted_land[]==1] <- NA
inverted_land[inverted_land[]==.5] <- 1

blank_raster <- food_raster
blank_raster[] <- 1

the_crs <- crs(fake_raster, asText = TRUE)



data <- read_csv("marine/output/all_marine_farms.csv") %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = crs(food_raster), 
           agr = "constant") 


data <- read_csv("all_marine_aquaculture_farms_sources_31km.csv") %>% 
  st_as_sf(., 
           coords = c("X", "Y"),
           crs = crs(food_raster), 
           agr = "constant") %>%
  filter(data_type_2 == "A")


need_validation_allocation <- data %>%
  st_drop_geometry() %>%
  group_by(iso3c, species_group) %>%
  summarise(num_farms = n()) %>%
  ungroup()



for (i in 1:length(unique(need_validation_allocation$iso3c))) {
  
  
  # i = 1
  ## 1.a get country shape file
  this_iso3 <- unique(need_validation_allocation$iso3c)[i]
  
  this_farm_info <- need_validation_allocation %>% 
    filter(iso3c == this_iso3) 
  
  set.seed(001)
  
  this_type_vector <- this_farm_info %>% 
    group_by(species_group) %>% 
    tidyr::expand(types = seq(1:num_farms)) %>% 
    pull(species_group) %>% 
    sample()
  
  
  this_country <- gadm_full %>% 
    filter(GID_0 == this_iso3) %>% 
    group_by(GID_0) %>% 
    st_union() %>% # union all touching polygons
    sf::st_as_sf() %>%
    ungroup()
  
  this_eez <- eez_sf %>% 
    filter(ISO_Ter1 == this_iso3) %>% 
    group_by(ISO_Ter1)%>% 
    st_union() %>% 
    sf::st_as_sf() %>% 
    ungroup() 
  
  
  # plot(this_country)
  # class(this_country)
  
  ## 1.e filter for ports and coast line of this country
  this_coast <- this_country %>%
    st_union() %>% # union all touching polygons
    sf::st_as_sf() %>%
    st_cast("MULTILINESTRING") # Cast to a multilinestring
  
  this_simplified_coast <- this_coast %>% 
    ms_simplify() 
  
## buffer coast 2nm offshore
  this_coast_meters <- st_transform(this_simplified_coast, crs = "+proj=moll") %>% ## transform to a crs that can buffer in meters
    st_buffer(dist = 200) %>% # buffer in meters km
    st_transform(crs = crs(the_crs))
  mapview(this_coast_meters)
  mapview(this_coast_clip)
  
  
  # plot(this_country)
  # class(this_country)
  
  ## 1.c both country and eez
  # this_area <- this_country %>% 
  #   bind_rows(this_eez)
  # 
  # 
  # ## This ifelse statement is just for USA and RUS, because if we don't restrain the bbox the bbox will span the entire globe, making the distance to ports calculation take like 5 hours... now it only takes 20 mins for USA!
  # if(this_iso3 == "USA"){
  #   
  #   this_bbox <- st_bbox(c(xmin = -180, xmax = -65.69972, ymax = 66, ymin = 15.56312), crs = st_crs(this_area)) ## cut out arctic since it gets cut out later anyways.
  #   
  #   this_eez <- st_crop(this_eez, this_bbox)
  #   
  # }else if(this_iso3 == "RUS"){
  #   this_bbox <- st_bbox(c(xmin = 28, xmax = 180, ymax = 66, ymin = 39.72669), crs = st_crs(this_area)) ## make it easier since the arctic will be classed out later anyways
  #   
  #   this_eez <- st_crop(this_eez, this_bbox)
  #   
  # }else{
  #   
  #   ## 1.d get bounding box and limit to ymax = 66
  #   this_bbox_coords <- st_bbox(this_area)
  #   
  #   this_bbox_xmin = as.numeric(this_bbox_coords$xmin)
  #   this_bbox_ymin = as.numeric(this_bbox_coords$ymin)
  #   this_bbox_xmax = as.numeric(this_bbox_coords$xmax)
  #   
  #   this_bbox <- st_bbox(c(xmin = this_bbox_xmin, xmax = this_bbox_xmax, ymax = 66, ymin =this_bbox_ymin), crs = st_crs(this_area))
  #   
  #   this_eez <- st_crop(this_eez, this_bbox)
  # }
  # 
  ## 1.e filter for ports and coast line of this country
  # this_coast <- this_country %>%
  #   st_union() %>% # union all touching polygons
  #   sf::st_as_sf() %>%
  #   st_cast("MULTILINESTRING") # Cast to a multilinestring
  # 
  # this_simplified_coast <- this_coast %>% 
  #   ms_simplify() 
  # 
  
  # this_coast_clip <- st_crop(coastline, this_simplified_coast) %>%
  #   st_crop(this_bbox) %>%
  #   st_cast("MULTIPOINT") %>%
  #   st_cast("POINT") %>%
  #   as_Spatial() 
  # 
  # mapview(this_coast_clip)
  
  # 
  # coast_rast <- rasterize(this_coast_clip, food_raster)
  # 
  # 
  # # plot(coast_rast, col = "black")
  # # cellStats(coast_rast, "sum")
  # #  
  # #  s <- raster::select(coast_rast)
  # #  plot(s)
  # 
  # coast_rast[coast_rast==0] <- NA
  # coast_rast[coast_rast >0] <- 1
  # 
  


 coast_rast <- fasterize(this_coast_meters, food_raster)
  

  #UNCOMMENT WHEN SAVING!
  writeRaster(coast_rast, paste0("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/validation_coast_buffer/temp_200m", i, ".tif"), overwrite = TRUE)
  
#   test <- raster(file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/validation_coast_buffer/temp_coast_A_2.tif"))
# plot(test)
# 
# s <- raster::select(test)
# plot(s)
  
  
## Now buffer 1 mile inland

  if(unique(need_validation_allocation$iso3c)[i] == "HKG"){

    ## buffer coast 1 mile inland
    this_inland_meters <- st_transform(this_simplified_coast, crs = "+proj=moll") %>% ## transform to a crs that can buffer in meters
      st_buffer(dist = 1609.34) %>% # buffer in meters km
      st_transform(crs = crs(the_crs))

    #mapview(this_inland_meters)

    inland_rast <- fasterize(this_inland_meters, food_raster)

    # plot(coast_rast, col = "black")
    # cellStats(coast_rast, "sum")

  }else{
  ## 1.e filter for ports and coast line of this country
  this_eez_coast <- this_eez %>%
    st_union() %>% # union all touching polygons
    sf::st_as_sf() %>%c
    st_cast("MULTILINESTRING") # Cast to a multilinestring

  this_simplified_eez_coast <- this_eez_coast %>%
    ms_simplify()

  this_inland_meters <- st_transform(this_simplified_coast, crs = "+proj=moll") %>% ## transform to a crs that can buffer in meters
    st_buffer(dist = 1609.34) %>% # buffer in meters km
    st_transform(crs = crs(the_crs))

  ## rasterize this
  inland_rast <- fasterize(this_inland_meters, food_raster)

  }

  # cellStats(inland_rast, "sum") # 22
  ## multiply by the land mask

  ## clip out ocean
  inland_rast_fixed <- inland_rast*land_mask
  # cellStats(inland_rast, "sum")
  # cellStats(inland_rast_fixed, "sum") # 10
  # plot(inland_rast_fixed, col = "black")

  # s <- raster::select(inland_rast_fixed)
  # plot(s)

  ## Mosaic together
  coast_and_inland <- mosaic(coast_rast_fixed, inland_rast_fixed, fun  = "sum")

  coast_and_inland[coast_and_inland == 0] <- NA
  # plot(coast_and_inland, col = "black")
  # cellStats(coast_and_inland, "sum") # 25

  #plot(coast_and_inland)

  # s <- raster::select(coast_and_inland)
  # plot(s)
  ## write
  writeRaster(coast_and_inland, paste0("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/validation_coast_buffer/temp_coast_inland_fix_A_", i, ".tif"), overwrite = TRUE)

}

# [1] "ARE" "AUS" "BGR" "BLZ" "CAN" "CHL" "CYP" "DNK" "ESP" "FIN" "FRA" "FRO" "GBR" "GRC" "HKG" "IRL" "ITA" "JEY" "MEX"
# [20] "MLT" "NOR" "PYF" "SAU" "SWE" "USA"

all_coasts <- list.files("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/validation_coast_buffer", pattern = "temp_coast_inland_fix", full.names = TRUE)

# all_coasts_stack <- raster::stack(all_coasts)
# plot(all_coasts_stack[[18]], col = "red")


allrasters <- lapply(all_coasts, raster)

allrasters$fun <- sum

plot(allrasters[[12]])

s <- raster::select(allrasters[[12]])
plot(s)

## mosaic works 

mos <- do.call(mosaic, allrasters)
plot(mos)

unique(mos)

mos[mos == 2] <- NA
mos[mos == 0] <- NA
mos[mos == 3] <- NA

plot(mos, col = "black")

s <- raster::select(mos)
plot(s, col = "black")

writeRaster(mos, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/coast_buff_2nm_inland_1mi.tif"), overwrite = TRUE)


new_crs <- "+proj=cea +lat_ts=45 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
new_raster <- raster(nrows=3000, ncols=4736, xmn=-14207430, xmx=14208570, ymn=-9000182, ymx=8999818, crs = new_crs)

mos_cea <- projectRaster(mos, new_raster, crs = new_crs)

unique(mos_cea)
plot(mos_cea)

s <- raster::select(mos_cea)
plot(s)

mos_cea[mos_cea == 0] <- NA
mos_cea[!is.na(mos_cea)] <- 1

plot(area(mos_cea))
plot(area(mos))

plot(mos_cea, col = "black")
plot(mos, col = "black")

cellStats(mos, "sum") # 42484

cellStats(mos_cea, "sum") # 149651

writeRaster(mos_cea, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/coast_buff_2nm_inland_1mi_cea.tif"), overwrite = TRUE)


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
  
  # this_farm_info <- need_validation_allocation %>% 
  #   filter(iso3c == this_iso3) 
  
  set.seed(001)
  
  this_eez <- eez_sf %>% 
    filter(ISO_Ter1 == this_iso3) %>% 
    group_by(ISO_Ter1)%>% 
    st_union() %>% 
    sf::st_as_sf() %>% 
    ungroup() 
  
  
  if(unique(need_validation_allocation$iso3c)[i] == "HKG"){
   
     this_country <- gadm_full %>% 
      filter(GID_0 == this_iso3) %>% 
      group_by(GID_0) %>% 
      st_union() %>% # union all touching polygons
      sf::st_as_sf() %>%
      ungroup()
    
    
    ## 1.e filter for ports and coast line of this country
    this_coast <- this_country %>%
      st_union() %>% # union all touching polygons
      sf::st_as_sf() %>%
      st_cast("MULTILINESTRING") # Cast to a multilinestring
    
    this_simplified_coast <- this_coast %>% 
      ms_simplify() 
    
    
    ## buffer coast 1 mile inland
    this_eez <- st_transform(this_simplified_coast, crs = "+proj=moll") %>% ## transform to a crs that can buffer in meters
      st_buffer(dist = 4000) %>% # buffer in meters km
      st_transform(crs = crs(the_crs))
    
    mapview(this_eez)
    
    eez_rast <- fasterize(this_eez, food_raster)
    
    # plot(eez_rast, col = "black")
    # 
    # s <- raster::select(eez_rast)
    # 
    # plot(s)
    # cellStats(eez_rast, "sum")
    
  }else{
  
    this_eez_buff <- st_transform(this_eez, crs  = "+proj=moll") %>%
      st_buffer(dist = 5000) %>%
      st_transform(crs = crs(the_crs))
   
  eez_rast <- fasterize(this_eez_buff, food_raster)
  
  }
  
  # plot(eez_rast)
  # 
  # s <- raster::select(eez_rast)
  # plot(s)
  
  writeRaster(eez_rast, paste0("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/validation_coast_buffer/temp_eez", i, ".tif"), overwrite = TRUE)
  
  print(cat(this_iso3, "done"))
  
}

# [1] "ARE" "AUS" "BGR" "BLZ" "CAN" "CHL" "CYP" "DNK" "ESP" "FIN" "FRA" "FRO" "GBR" "GRC" "HKG" "IRL" "ITA" "JEY" "MEX"
# [20] "MLT" "NOR" "PYF" "SAU" "SWE" "USA"

all_coasts <- list.files("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/validation_coast_buffer", pattern = "temp_eez", full.names = TRUE)

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


mos[mos == 0] <- NA
mos[mos >0] <- 1

plot(mos, col = "black")

s <- raster::select(mos)
plot(s)

writeRaster(mos, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/eez_validation_mask.tif"), overwrite = TRUE)


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

writeRaster(mos_cea, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/eez_validation_mask_cea_36km.tif"), overwrite = TRUE)



## need this for 25km2 cells 
res(mos_cea) <- c(5000, 5000)

mos_cea

plot(mos_cea)

mos_cea_25 <- projectRaster(mos, mos_cea, crs = crs(mos_cea))

plot(mos_cea_25)
plot(area(mos_cea_25))

mos_cea_25[mos_cea_25 == 0] <- NA
mos_cea_25[!is.na(mos_cea_25)] <- 1

plot(mos_cea_25)

writeRaster(mos_cea_25, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/eez_validation_mask_cea_25km.tif"), overwrite = TRUE)


## now reproject to 0.25 degree cell sizes 

mos_quart <- mos
res(mos_quart) <- c(0.25,0.25)
plot(area(mos_quart))

mos_quart_reproj <- projectRaster(mos, mos_quart, crs = crs(mos_quart))

plot(mos_quart_reproj)
plot(area(mos_quart_reproj))

writeRaster(mos_quart_reproj, file.path("/home/shares/food-systems/Food_footprint/_raw_data/Aquaculture_locations/global_maps/eez_validation_mask_quarter_degree.tif"), overwrite = TRUE)

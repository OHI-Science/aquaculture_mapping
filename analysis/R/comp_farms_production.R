

comp_farms_production <- function(species_type){
# browser()
# species_type <- "finfish"
  
if(species_type == "finfish"){
  farm_files <- list.files(here("marine", sprintf('marine_fish_general/%s_farms/data/known_farm_sites_2_4_5', species_type)), full = TRUE)
  
  all_known_farms = plyr::ldply(farm_files, read_csv) %>%
    mutate(type = "marine_fish_general")
  
  number_files <- list.files(here("marine", sprintf("marine_fish_general/%s_farms/data/known_number_farms_3", species_type)), full=TRUE)
  
  all_number_3 = plyr::ldply(number_files, read_csv)
  
  subset_farms_files <- list.files(here("marine", sprintf("marine_fish_general/%s_farms/data/subset_known_add_points_4", species_type)), full=TRUE)
  
  subset_add_4 = plyr::ldply(subset_farms_files, read_csv)
  
  # Read in relevant production data
  
  species_tonnes <- read_csv(here::here("marine", sprintf("marine_fish_general/%s_farms/data/fao_mariculture_%s.csv", species_type, species_type))) %>%
    mutate(type = "marine_fish_general") %>%
    filter(year == 2017)
  
  
}else{
  
farm_files <- list.files(here("marine", sprintf('%s/%s_farms/data/known_farm_sites_2_4_5', species_type, species_type)), full = TRUE)

all_known_farms = plyr::ldply(farm_files, read_csv) %>%
  mutate(type = species_type)

number_files <- list.files(here("marine", sprintf("%s/%s_farms/data/known_number_farms_3", species_type, species_type)), full=TRUE)

all_number_3 = plyr::ldply(number_files, read_csv)

subset_farms_files <- list.files(here("marine", sprintf("%s/%s_farms/data/subset_known_add_points_4", species_type, species_type)), full=TRUE)

subset_add_4 = plyr::ldply(subset_farms_files, read_csv)

# Read in relevant production data

species_tonnes <- read_csv(here::here("marine", sprintf("%s/%s_farms/data/fao_mariculture_%s.csv", species_type, species_type, species_type))) %>%
  mutate(type = species_type) %>%
  filter(year == 2017)


}



# Calculate a global tonnes per farm for each species 

## Filter for data_quality = 4 or 5 and group by country to get number of farms
comprehensive_farms <- all_known_farms %>%
  filter(data_quality %in% c(4,5)) %>% 
  group_by(iso3c, type, data_quality) %>%
  summarise(n_farms = n()) %>% ## now we can see the places that has additional farms to add
  ungroup()

## Join this with the additional farms we need to add to some places, so we can calculate a comprehensive tonnes/farm for bivalve 

data_quality_3 <- all_number_3 %>%
  rename("n_farms" = "num_additional_farms")

data_quality_4 <- subset_add_4 %>%
  rename("n_farms" = "num_additional_farms")

comprehensive_farms_total <- comprehensive_farms %>%
  full_join(data_quality_3) %>%
  full_join(data_quality_4) %>%
  group_by(iso3c, type) %>%
  summarise(n_farms = sum(n_farms))


tonnes_per_farm <- comprehensive_farms_total %>%
  left_join(species_tonnes) %>%
  mutate(tonnes_per_farm = ifelse(type == "crustaceans", 336.5783, fao_tonnes_production/n_farms))

return(tonnes_per_farm)

}


plot_farms_prod <- function(df){
  
  # plot(df$n_farms, df$fao_tonnes_production)
  # abline(0,1, col="red")
  
  
  ggplot(data = df, aes(x = n_farms, y = fao_tonnes_production)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
  
}

lm_farms_prod <- function(df){
  
  summary(lm(data = df,  n_farms ~ fao_tonnes_production))
}

library(tidyverse)
library(dplyr)
library(rgdal)


# Script to clean data for Shiny WebApp Dogs of NYC

# Set as same directory as ShinyApp
setwd("~/code/bootcamp/shinyDogs/")

# Raw CSV from NYC Datasets
dogs_raw = read.csv("NYC_Dog_Licensing_Dataset.csv", stringsAsFactors = F)

#Raw shape file from 
sh_raw = readOGR("shape_raw", stringsAsFactors = F)

#Convert raw shape file to longitude and latitude
sh_obj = spTransform(sh_raw, CRS("+proj=longlat +datum=WGS84"))

# Count total dogs by zipcode
dogs_count = dogs_raw %>%
  group_by(., ZipCode) %>%
  summarise(., num_dogs = n()) %>%
  # change name to match shape file
  mutate(., ZIPCODE = ZipCode) %>%
  filter(., ZipCode %in% sh_obj$ZIPCODE)

# Count dog breeds by zipcode
pop_breed = dogs_raw %>%
  filter(., BreedName != "Unknown") %>%
  group_by(., BreedName, ZipCode) %>%
  summarise(., num_breeds = n()) %>%
  group_by(., ZipCode) %>%
  arrange(., desc(num_breeds)) %>%
  filter(., row_number() == 1) %>%
  filter(., ZipCode %in% sh_obj$ZIPCODE) %>%
  mutate(., pop_breed =  BreedName) %>%
  mutate(., ZIPCODE = ZipCode) %>%
  filter(., ZipCode %in% sh_obj$ZIPCODE)

pop_name = dogs_raw %>%
  filter(., AnimalName != "UNKNOWN" & AnimalName != "NAME NOT PROVIDED" & !is.na(AnimalName)) %>%
  group_by(., AnimalName, ZipCode) %>%
  summarise(., num_names = n()) %>%
  group_by(., ZipCode) %>%
  arrange(., desc(num_names)) %>%
  filter(., row_number() == 1) %>%
  filter(., ZipCode %in% sh_obj$ZIPCODE) %>%
  mutate(., pop_name =  AnimalName) %>%
  mutate(., ZIPCODE = ZipCode) %>%
  filter(., ZipCode %in% sh_obj$ZIPCODE)
  

# Add a collumn for each breed with the count
breed_counts = dogs_raw %>%
  filter(., BreedName != "Unknown" & BreedName != "") %>%
  group_by(., BreedName, ZipCode) %>%
  summarise(., num_breeds = n()) %>%
  spread(., key = BreedName, value = num_breeds) %>%
  mutate(., ZIPCODE = ZipCode) %>%
  filter(., ZipCode %in% sh_obj$ZIPCODE)

# Add boroughs from dog_raw
borougs = dogs_raw %>%
  group_by(., ZipCode, Borough) %>%
  #filter(., Borough %in% c("Brooklyn", "Bronx", "Manhattan", "Queens", "Statten Island")) %>%
  filter(., ZipCode %in% sh_obj@data$ZIPCODE) %>%
  #filter(., n_dogs_borough == n()) %>%
  summarise(., n_dogs_borough = n()) %>%
  group_by(., ZipCode) %>%
  filter(., n_dogs_borough == max(n_dogs_borough))


# Join dogs count and dogs per cepita to modidied shape object
sh_join_dogs = sh_obj@data %>%
  # change ZIPCODE to int for proper join
  mutate(., ZIPCODE = as.integer(ZIPCODE)) %>%
  left_join(x = ., y = dogs_count) %>%
  left_join(x = ., y = pop_breed) %>%
  left_join(x = ., y = pop_name) %>%
  left_join(x = ., y = pop_name) %>%
  left_join(x = ., y = borougs) %>%
  # Add Percapita unless population is 0
  mutate(., dogs_per_capita = num_dogs%%(ifelse(POPULATION == 0, NA, POPULATION))) %>%
  left_join(x = ., y = breed_counts)

simulation = function(){
  #shape@data$ZIPCODE = sub('[.]', 'N', make.names(shape@data$ZIPCODE, unique=TRUE))
  filter = (!is.na(shape@data$num_dgs)) & shape@data$POPULAT != 0 & shape@data$ZIPCODE != 11370 & shape@data$ZIPCODE != 10463
  num_sim = 1e4
  means_df = data.frame(ZIPCODE = shape@data$ZIPCODE[filter], means = rep(0, length(shape@data$ZIPCODE[filter])))
  for (i in 1:num_sim){
    spl = sample(means_df$ZIPCODE, sum(shape@data$num_dgs[filter]), replace = T, prob = shape@data$POPULAT[filter])
    spl_df = data.frame(ZIPCODE = spl) %>%
      group_by(., ZIPCODE) %>%
      summarise(., count = n()) %>%
      right_join(., means_df)
    means_df$means = means_df$means + spl_df$count
  }
  means_df$means = means_df$means / num_sim
  
  # Add simulated mean to shape object
  shape_test = sh_join_dogs %>%
    left_join(x= ., y = unique(means_df), by = "ZIPCODE")

  sh_join_dogs$means = shape_test$means
  }

# Write shape object to a csv
write.csv(sh_join_dogs, "dogs_joined2.csv")

# Add filds from temporary table to shape object
sh_obj@data$num_dogs = sh_join_dogs$num_dogs
sh_obj@data$dogs_per_capita = sh_join_dogs$dogs_per_capita
sh_obj@data$ZIPCODE = sh_join_dogs$ZIPCODE
sh_obj@data$POPULATION = sh_join_dogs$POPULATION


file_name = "nyc_dogs_zipcode"
shape_dir = "./shape_files/"
dir.create(shape_dir)
path = paste0(shape_dir, file_name)

writeOGR(sh1, path, driver = "ESRI Shapefile")

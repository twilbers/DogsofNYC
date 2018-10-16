library(shiny)
library(dplyr)
library(rgdal)
library(DT)
library(tm)
library(wordcloud)
library(memoise)
#library(wordcloud2)
library(plotly)
library(leaflet)
library(googleVis)
library(ggvis)


shape = readOGR("./shape/shape2.shp", stringsAsFactors = F)

#dogs_join = read.csv("dogs_joined2.csv")
dogs_join = read.csv("dogs_joined.csv", stringsAsFactors = F)

colnames(shape@data)[14] = "DogsPerCapita"

shape@data$pop_breed = as.character(dogs_join$pop_breed)
shape@data$num_breeds = dogs_join$num_breeds
shape@data$pop_name = dogs_join$pop_name
shape@data$Borough = dogs_join$Borough
shape@data = cbind(shape@data, dogs_join[25:length(dogs_join)])


dogs = read.csv("NYC_Dog_Licensing_Dataset.csv", stringsAsFactors = F)

# Some cleaning
 dogs =  dogs %>% 
   mutate(., Borough = ifelse(Borough == "New York" | Borough == "New york" | Borough == "NEW YORK" | Borough == "NY" | Borough == "New York ", "Manhattan", Borough))


top_12_breeds = dogs %>%
  filter(., BreedName != "Unknown") %>%
  group_by(., BreedName) %>%
  summarise(., count = n()) %>%
  arrange(., desc(count)) %>%
  #select(., BreedName) %>%
  head(., 12)

breedFreq = function(dogs, breed){
  breed = gsub("\\."," ", breed)
  dogs %>%
    filter(., AnimalName != "UNKNOWN" & AnimalName != "NAME NOT PROVIDED" & !is.na(AnimalName)) %>%
    filter(., BreedName == breed) %>%
    group_by(., AnimalName) %>%
    summarise(., count = n()) %>%
    arrange(., desc(count)) %>%
    return(.)
}

define_pal = function(xvar, colors = "YlOrRd"){
  pal = colorNumeric(palette = colors,
                     domain = range(xvar, na.rm=T), na.color = "")
  return(pal)
}

top_names = function(breed, zip_){
  breed = gsub("\\."," ", breed)
  name_df = dogs %>%
    filter(., AnimalName != "UNKNOWN" & AnimalName != "NAME NOT PROVIDED" & !is.na(AnimalName)) %>%
    filter(., ZipCode == zip_) %>%
    filter(., BreedName == breed) %>%
    group_by(AnimalName)%>%
    summarise(., freq = n()) %>%
    arrange(., desc(freq))
  out = paste(name_df$AnimalName[1])
  return(out)
}

boro_table = dogs %>%
  #mutate(., Borough = ifelse(Borough == "New York" | Borough == "New york" | Borough == "NEW YORK" | Borough == "NY" | Borough == "New York ", "Manhattan", Borough)) %>%
  filter(., Borough %in% c("Brooklyn", "Bronx", "Manhattan", "Queens", "Statten Island")) %>%
  filter(., ZipCode %in% shape@data$ZIPCODE) %>%
  group_by(., Borough) %>%
  summarise(., num_dogs = n()) %>%
  mutate(., density = num_dogs/sum(num_dogs))

zip_table = dogs %>%
  #mutate(., Borough = ifelse(Borough == "New York" | Borough == "New york" | Borough == "NEW YORK" | Borough == "NY" | Borough == "New York ", "Manhattan", Borough)) %>%
  filter(., Borough %in% c("Brooklyn", "Bronx", "Manhattan", "Queens", "Statten Island")) %>%
  filter(., ZipCode %in% shape@data$ZIPCODE) %>%
  group_by(., Borough, ZipCode) %>%
  summarise(., num_z = n()) %>%
  group_by(., Borough) %>%
  summarise(., Total_Zipcodes = n())



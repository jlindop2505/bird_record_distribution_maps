# Install packages
install.packages('ggmap')
install.packages("leaflet")
install.packages("terra", type = "binary") #Need to specify binary to download this package in windows
install.packages("rsconnect")
install.packages("readxl")

library(leaflet)
library(maptiles)
library(readxl)
library(ggplot2)
library(rsconnect)
library(htmlwidgets)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(showtext)
library(ggmap)


# Read the Excel file and sheet
df <- read_excel("Data_raw_short.xlsx", sheet = "Data 2020-2026")

#Create a map of the record distributions

#Convert dataset Easting and Northing columns into coordinates for plotting
records_sf <- st_transform(
  st_as_sf(filter(df, !is.na(Easting), !is.na(Northing)),
    coords = c("Easting", "Northing"),
    crs = 27700), #27700 is code for UK national grid map 
    crs = 4326)   #Converts to standard latitude and longitude for map function

record_distribution_map <- addCircleMarkers(
  addTiles(leaflet(records_sf)),
  radius = 4,
  fillColor = "#009ACD",
  fillOpacity = 0.6,
  stroke = FALSE)

#Display map
record_distribution_map 

#Create HTML of map for Github
saveWidget(record_distribution_map, file = "record_distribution_map.html", selfcontained = TRUE)
#Download map URL for sharing
#rsconnect::rpubsUpload("Record Distribution Map", "record_distribution_map.html")
rsconnect::rpubsUpload(
  title = "Record Distribution Map",
  contentFile = "record_distribution_map.html",
  originalDoc = "record_distribution_map.html")


#Habitat analysis

#Load the LCM raster (downloaded from UKCEH website)
lcm <- rast("ukregion-southeastengland.tif")

#Convert points to the same format as the LCM (British National Grid)
bird_points <- st_transform(records_sf, crs = 27700)

#Extract habitat code at each point
bird_points$habitat_code <- terra::extract(lcm, bird_points)[, 2]

#Add habitat labels with the given lookup table
habitat_lookup <- c(
  "1" = "Broadleaved woodland",
  "2" = "Coniferous woodland",
  "3" = "Arable",
  "4" = "Improved grassland",
  "5" = "Neutral grassland",
  "6" = "Calcareous grassland",
  "7" = "Acid grassland",
  "8" = "Fen, marsh, swamp",
  "9" = "Heather",
  "10" = "Heather grassland",
  "11" = "Bog",
  "12" = "Inland rock",
  "13" = "Saltwater",
  "14" = "Freshwater",
  "15" = "Supralittoral rock",
  "16" = "Supralittoral sediment",
  "17" = "Littoral rock",
  "18" = "Littoral sediment",
  "19" = "Saltmarsh",
  "20" = "Urban",
  "21" = "Suburban")

#Ordered so that they appear in the given order for each analysis
habitat_order <- c("Broadleaved woodland", "Coniferous woodland", "Arable", "Improved grassland",
  "Neutral grassland", "Calcareous grassland", "Acid grassland", "Fen, marsh, swamp",
  "Heather", "Heather grassland", "Bog", "Inland rock", "Saltwater", "Freshwater",
  "Supralittoral rock", "Supralittoral sediment", "Littoral rock", "Littoral sediment",
  "Saltmarsh", "Urban", "Suburban")

bird_points$habitat <- habitat_lookup[as.character(bird_points$habitat_code)]

#Create bar plot of habitat record counts
ggplot(mutate(
    count(bird_points, habitat),
    habitat = factor(habitat, levels = habitat_order)),
  aes(x = habitat, y = n)) +
  geom_col(fill = "deepskyblue3") +
  coord_flip() +
  labs(title = "Bird Records by Habitat", x = "Habitat", y = "Number of Records") +
  theme_minimal()




#Bird species data analysis
#Code only given here for Bittern, replace with species name for other species

#Bittern record distribution analysis

#Read the Excel file and sheet for Bittern
df <- read_excel("Focal birds.xlsx", sheet = "Bittern")

#Convert Easting and Northing into coordinates for plotting
bittern_sf <- st_transform(
  st_as_sf(filter(df, !is.na(Easting), !is.na(Northing)),
    coords = c("Easting", "Northing"),
    crs = 27700), #27700 is code for UK national grid map 
    crs = 4326)   #Converts to standard latitude and longitude for map function

#Create a colour gradient based on Year
#Dark blue refers to the oldest records, and light blue to the most recent
pal <- colorNumeric(
  palette = c("blue4", "blue", "deepskyblue"),  
  domain = c(1965, 2026))

bittern_distribution_map <- addCircleMarkers(
  addTiles(leaflet(bittern_sf)),
  radius = 4,
  color = ~pal(RecYear),
  fillColor = ~pal(RecYear),
  fillOpacity = 0.8,
  stroke = FALSE)

#Display map
bittern_distribution_map

#Create HTML of map
saveWidget(bittern_distribution_map, file = "bittern_distribution_map.html", selfcontained = TRUE)
#Download map URL for sharing
#rsconnect::rpubsUpload("Bittern Distribution Map", "bittern_distribution_map.html")
rsconnect::rpubsUpload(
  title = "Bittern Distribution Map",
  contentFile = "bittern_distribution_map.html",
  originalDoc = "bittern_distribution_map.html")


#Bittern habitat analysis

#Load the LCM raster (downloaded from UKCEH website)
lcm <- rast("ukregion-southeastengland.tif")

#Convert points to the same format as the LCM (British National Grid)
bittern_points <- st_transform(records_sf, crs = 27700)

#Extract habitat code at each point
bittern_points$habitat_code <- terra::extract(lcm, bittern_points)[, 2]

#No habitat lookup table and order required as the code has already run, however remember this if running the species analysis alone

bittern_points$habitat <- habitat_lookup[as.character(bittern_points$habitat_code)]

#Create bar plot of habitat record counts
ggplot(mutate(
  count(bittern_points, habitat),
  habitat = factor(habitat, levels = habitat_order)),
  aes(x = habitat, y = n)) +
  geom_col(fill = "deepskyblue3") +
  coord_flip() +
  labs(title = "Bittern Records by Habitat", x = "Habitat", y = "Number of Records") +
  theme_minimal()


#Line graph of Bittern records
#Count records per year
bittern_counts <- count(filter(df, !is.na(RecYear)), RecYear)

#Plot
ggplot(bittern_counts, aes(x = RecYear, y = n)) +
  geom_line(color = "deepskyblue3") +
  labs(
    title = "Bittern Records per Year",
    x = "Year",
    y = "Number of Records") +
  scale_x_continuous(limits = c(NA, 2024)) +
  theme_minimal()

#Greatest value
slice_max(bittern_counts, n)

#Values in 1980 and 2024
filter(bittern_counts, RecYear == 1980 | RecYear == 2024)
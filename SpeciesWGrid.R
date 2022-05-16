#Gabriel's Script#
remove(list = ls())

install.packages("purr")
library(ggplot2)
library(rgdal)
library(raster)
library(broom)
library(maps)
library(sf)
library(tidyverse)
library(fs)
library(dplyr)
library(purrr)


#import geographical database layers
GEO_V5 <- read.csv("~/Dropbox/FOME_DATA/GEOGRAPHICAL_LAYER/GEO_SPATIAL_META_MOL720GRID_V5.csv", header=TRUE)

View(GEO_V5)

#Mapping the world
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
ggplot() + geom_sf(data = worldmap) + theme_bw()

#Mapping Canada
canada <- worldmap[worldmap$name == 'Canada',]
ggplot() + geom_sf(data = canada) + theme_bw()

#mapping shapefile
library(rgdal)
setwd("~/Documents/MSC_Thesis")
shp <- readOGR("~/Documents/MSC_Thesis/ScotianShelf_BayOfFundy_Bioregion/MaritimesPlanningArea.shp")
GEO <- readOGR("~/Dropbox/FOME_DATA/720 x 228 grid GIS INFO/")
ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_polygon(data = GEO, aes(x = long, y = lat), colour = "red", fill = NA)



st_intersects()


#trying to overlay the coordinates of the scotia shelf region with gabriels map
library(sf)
library(raster)
mar_boundary<-st_read("~/Documents/MSC_Thesis/ScotianShelf_BayOfFundy_Bioregion/MaritimesPlanningArea.shp")
st_crs(mar_boundary) <- 4326
sf::st_crs(mar_boundary) <- mar_boundary
map_boundary <- st_transform(mar_boundary, crs = st_crs(GEO_V5))

if (!require("rspatial")) remotes::install_github('rspatial/rspatial')
library(rspatial)
counties <- sp_data('counties.rds')

#TEST
install.packages("PBSmapping")
library(PBSmapping)

#prepare towns
GEO <- points(GEO_V5$longitude, GEO_V5$latitude)
# read in shapefiles 
boundaries <- importShapefile("~/Documents/MSC_Thesis/ScotianShelf_BayOfFundy_Bioregion/MaritimesPlanningArea.shp")

# note that importShapefile reads the .prj file if it exists, but it
# does not adopt the proj4 format used by the above approaches
proj.abbr <- attr(boundaries, "projection") # abbreviated projection info
proj.full <- attr(boundaries, "prj") # full projection info
print(proj.abbr)
# [1] "LL"

# generate map using PBSmapping plotting functions
plotPolys(boundaries, projection=proj.abbr, border="gray",
          xlab="Longitude", ylab="Latitude")
addPoints(GEO, pch=20, cex=0.8)

#Access data using spatial variables
library(magrittr)
library(spData)
library(rnaturalearth)
data("world")
world[140,] %>% st_geometry() %>% ggplot() + geom_sf() + theme_minimal()



#FROM GABRIEL
x<-which(GEO_V5$latitude>ymin & GEO_V5$latitude<=ymax & GEO_V5$longitude>xmin & 
           GEO_V5$longitude<=xmax)










##LOOKING AT EACH SPECIES FILE FOR PRESENCE/ABSENCE WITHIN EACH GRID CELL
#import species, traits, and ecology
load("/Users/amyirvine/Documents/MSC_Thesis/MASTER_ECOLOGICAL_BIOLOGICAL_FISH_11JN2021.RData")

#Practice loading Gadus morhua 2113
Gadus_morhua_SDM <- read.csv("/Users/amyirvine/Dropbox/FOME_DATA/FISH_TST/FINAL_SDM_RANGE_V4/FINAL_SDM_RANGE_V4_SPID_2113.csv", header=TRUE, sep= ";")

#import multiple csv with loop
file_paths <- fs::dir_ls("~/Dropbox/FOME_DATA/FISH_TST/FINAL_SDM_RANGE_V4")
file_paths

species_data_list <-list()

# Read files into list
for (i in seq_along(file_paths)) {
  species_data_list[[i]] <- read.csv(
    file = file_paths[[i]], sep=";"
  )
}

# Sum up presence/absence across species in each grid cell

# Find unique grid cells
grid_cell_numbers = vector(length = 0)

# Loop through species
for (ii in 1:length(species_data_list)) ##Put 10 instead of length to make this quicker
{
  # Extract species from the list
  temp_species <- species_data_list[[ii]]
  
  # Loop through grid cells rows
  for (jj in 1:dim(temp_species)[1])
  {
    # Extract grid cell number
    grid_cell = temp_species$INDEX[jj]
    # See if the grid cell number is in the list
    if (grid_cell %in% grid_cell_numbers)
    {
      ;
    } else
    {
      grid_cell_numbers = c(grid_cell_numbers, grid_cell)
      
    }
  }
  
}

# Sort the list
grid_cell_numbers = sort(grid_cell_numbers)

# Create a data frame to hold the richness in each grid cell
cell_richness = vector(length = length(grid_cell_numbers))
grid_cell_richness = data.frame(GridCellNumber = grid_cell_numbers, SpeciesRichness = cell_richness)



# Loop through grid cells
for (kk in 1: length(grid_cell_numbers))
{
  # Get grid cell number
  grid_cell = grid_cell_numbers[kk]
  
  # Set the species richness in that grid cell to zero
  species_richness = 0
  
  # Loop through species
  for (ii in 1:length(species_data_list))
  {
    # Extract species from the list
    temp_species <- species_data_list[[ii]]
    
    # See if the grid cell exists in the list for this species
    if (grid_cell %in% temp_species$INDEX)
    {
      # Find which row is that grid cell
      grid_row = which(temp_species$INDEX == grid_cell)
      
      # Extract the value in the grid
      if (!is.na(temp_species$BINARY[grid_row]))
      {
        species_richness = species_richness + temp_species$BINARY[grid_row]
      }    
    }
  }
  
  # Write the value of species richness to the data frame
  grid_cell_richness$SpeciesRichness[kk] = species_richness
  
}
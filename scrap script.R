library(sf)
library(ggplot2)
library(dplyr)
library(stars)


background <- st_read("Street_Name_Routes.shp")

tree_data <- read.csv("CorrectedData_ENVS4826.csv")
tree_data$x <- as.numeric(tree_data$x)
str(tree_data$x)

tree_data_coord <- st_read("CorrectedData_ENVS4826.csv", 
                           options= c("X_POSSIBLE_NAMES=x","Y_POSSIBLE_NAMES=y"), 
                           crs = 4326)
trees.sf <- st_as_sf(tree_data_coord, coords = c("x", "y"))

st_crs(trees.sf) <- st_crs(4326) # assign crs
trees.sf <- st_transform(trees.sf, crs = 32721) # transform

?st_sf

bg.sf <- st_sf(id = 'L1', st_sfc(st_linestring(as.matrix(background), dim = "XY")))
st_crs(background) <- st_crs(4326) # assign crs
bg.sf <- st_transform(background, crs = 32721) # transform

distances <- st_distance(x = trees.sf, y = bg.sf)
print(distances)
View(distances)

distances.filtered<- distances(!is.na(distances))

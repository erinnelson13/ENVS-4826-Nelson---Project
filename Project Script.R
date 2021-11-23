library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)

background <- st_read("Street_Name_Routes.shp")

tree_data <- read.csv("CorrectedData_ENVS4826.csv")
tree_data$x <- as.numeric(tree_data$x)
str(tree_data$x)

tree_database_coord <- st_read("Tree_database.csv",
                               options = c("X_POSSIBLE_NAMEs=x", 
                                           "Y_POSSIBLE_NAMES=Y"),
                               crs = 4326) 

trees_wf <- filter(tree_database_coord, location == "waterfront")

max(trees_wf$x)
min(trees_wf$x)
max(trees_wf$y)
min(trees_wf$y)

trees_wf_bbox <- st_bbox(c(xmin = -63.5750000, ymin = 44.642000, xmax = -63.566000, ymax = 44.6510000),
                         crs = st_crs(background))
background_cropped <- st_crop(background, trees_wf_bbox)

ggplot() +
  geom_sf(data = background_cropped) +
  geom_sf(data = trees_wf, mapping = aes(colour = crown_condition)) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Waterfront Trees Crown Condition") + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(1, "in"), pad_y = unit(1, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

trees_wf$trunk_damage[262] = "Y"
ggplot() +
  geom_sf(data = background_cropped) +
  geom_sf(data = trees_wf, mapping = aes(colour = trunk_damage)) +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("Waterfront Trees Trunk Damage") + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(1, "in"), pad_y = unit(1, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

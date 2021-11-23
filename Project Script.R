library(sf)
library(ggplot2)
library(dplyr)

background <- st_read("Street_Name_Routes.shp")

tree_data <- read.csv("CorrectedData_ENVS4826.csv")
tree_data$x <- as.numeric(tree_data$x)
str(tree_data$x)

max(trees_wf$x)
min(trees_wf$x)
max(trees_wf$y)
min(trees_wf$y)

trees_wf_bbox <- st_bbox(c(xmin = -63.5750000, ymin = 44.642000, xmax = -63.566000, ymax = 44.6510000),
                         crs = st_crs(background))
background_cropped <- st_crop(background, trees_wf_bbox)


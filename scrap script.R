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

#st_crs(trees.sf) <- st_crs(4326) # assign crs
#trees.sf <- st_transform(trees.sf, crs = 32721) # transform

#?st_sf

trees_wf_bbox <- st_bbox(c(xmin = -63.5750000, ymin = 44.642000, xmax = -63.566000, ymax = 44.6510000),
                         crs = st_crs(background))
background_cropped <- st_crop(background, trees_wf_bbox)

#bg.sf <- st_sf(id = 'L1', st_sfc(st_linestring(as.matrix(background_cropped), dim = "XY")))
#st_crs(background_cropped) <- st_crs(4326) # assign crs
#bg.sf <- st_transform(background_cropped, crs = 32721) # transform

#distances <- st_distance(x = trees.sf, y = bg.sf)
#print(distances)
#View(distances)

trees_wf_bbox <- st_bbox(c(xmin = -63.5750000, ymin = 44.642000, xmax = -63.566000, ymax = 44.6510000),
                         crs = st_crs(background))
background_cropped <- st_crop(background, trees_wf_bbox)

#distances.filtered<- distances(!is.na(distances))





  #load vector data
  library(sf)
streets <- st_read("Street_Name_Routes.shp")
trees <- st_read("Tree_Database.csv")

#this calculates the minimum distance from your roads to one tree
min(st_distance(background_cropped, trees.sf[1, ]))
View(trees_wf_bbox)
# but you need to repeat that over all the trees using sapply
sapply(1:nrow(trees.sf), function(x) min(st_distance(background_cropped, trees.sf[x, ])))

greg<-st_read("gregtrees.shp")
tree_projected <- st_transform(trees.sf,st_crs(greg))
st_crs(tree_projected)

streets_transform <- st_transform(background_cropped, st_crs(greg))
st_crs(streets_transform)

#this calculates the minimum distance from your roads to one tree
min(st_distance(streets_transform, tree_projected[1, ]))
View(trees_wf_bbox)
# but you need to repeat that over all the trees using sapply
treedist1 <- sapply(1:nrow(tree_projected), function(x) min(st_distance(streets_transform, tree_projected[x, ])))
wf_trees <- cbind(tree_projected, treedist1)
View(wf_trees)

write.csv(wf_trees)

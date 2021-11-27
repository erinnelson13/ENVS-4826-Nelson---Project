#ENVS 4826 Project
#Mairi-Jo Musgrave A00407737 & Erin Nelson A00419820
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(generalhoslem)

#Setting up for maps
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

#Maps
ggplot() +
  geom_sf(data = background_cropped) +
  geom_sf(data = trees_wf, mapping = aes(colour = crown_condition)) +
  labs(x = "Longitude", y = "Latitude", colour = "Crown Condition") + 
  ggtitle("Waterfront Trees Crown Condition") + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(1, "in"), pad_y = unit(1, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot() +
  geom_sf(data = background_cropped) +
  geom_sf(data = trees_wf_filtered, mapping = aes(colour = trunk_damage)) +
  labs(x = "Longitude", y = "Latitude", colour = "Trunk Damage") + 
  ggtitle("Waterfront Trees Trunk Damage") + 
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(1, "in"), pad_y = unit(1, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Setting up for graphs and analysis
greg<-st_read("gregtrees.shp")
tree_projected <- st_transform(trees_wf,st_crs(greg))
st_crs(tree_projected)

streets_transform <- st_transform(background_cropped, st_crs(greg))
st_crs(streets_transform)



treedist1 <- sapply(1:nrow(tree_projected), function(x) min(st_distance(streets_transform, tree_projected[x, ])))
wf_trees <- cbind(tree_projected, treedist1)

trees_wf_filtered <- filter(wf_trees, location == "waterfront")
View(trees_wf_filtered)

trees_wf_filtered$trunk_damage[262] = "Y"


trees_wf_filtered$numerical_cc <- recode(trees_wf_filtered$crown_condition, G = 1, F = 1, P = 0)
trees_wf_filtered$numerical_td <- recode(trees_wf_filtered$trunk_damage, N = 1, Y = 0)
View(trees_wf_filtered)

trees_wf_filtered$numerical_cc <- as.numeric(trees_wf_filtered$numerical_cc)
str(trees_wf_filtered$numerical_cc)
trees_wf_filtered$numerical_td <-as.numeric(trees_wf_filtered$numerical_td)


#Graphs
ggplot(data = trees_wf_filtered, 
       aes(x = treedist1, y = numerical_cc, colour = crown_condition)) +
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, color = "black",
              method.args = list(family = "binomial")) +
  labs(x = "Distance from road (m)", y = "Crown condition", colour = "Crown Condition") +
  ggtitle ("Crown Condition") +
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  scale_colour_manual(values = c("#FFCC00", "#00EE00", "#FF0000"), 
                      labels = c("Fair", "Good", "Poor"))
ggplot(data = trees_wf_filtered, 
       aes(x = treedist1, y = numerical_td, colour = trunk_damage)) +
  geom_point() +
  geom_smooth(method = "glm", se = FALSE, color = "black",
              method.args = list(family = "binomial")) +
  labs(x = "Distance from road (m)", y = "Trunk damage", colour = "Trunk Damage") +
  ggtitle ("Trunk Damage") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_colour_manual(values = c("#00EE00", "#FF0000"), 
                      labels = c("1 = No", "0 = Yes"))

#Analysis model
cc_model <- glm(numerical_cc ~ treedist1, data = trees_wf_filtered, family = "binomial")
summary(cc_model)
  

td_model <- glm(numerical_td ~ treedist1, data = trees_wf_filtered, family = "binomial")
summary(td_model)

#Hosmer-Lemeshow
logitgof(trees_wf_filtered$numerical_cc, fitted(cc_model), g = 10)

logitgof(trees_wf_filtered$numerical_td, fitted(td_model), g = 10)


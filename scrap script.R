install.packages("viridis")
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




#load vector data
streets <- st_read("Street_Name_Routes.shp")
trees <- st_read("Tree_Database.csv")

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

trees_wf_filtered <- filter(wf_trees, location == "waterfront")
View(trees_wf_filtered)

trees_wf_filtered$trunk_damage[262] = "Y"

#set as number and not factors 
trees_wf_filtered$numerical_cc <- recode(trees_wf_filtered$crown_condition, G = 1, F = 1, P = 0)
trees_wf_filtered$numerical_td <- recode(trees_wf_filtered$trunk_damage, N = 1, Y = 0)
View(trees_wf_filtered)

trees_wf_filtered$numerical_cc <- as.numeric(trees_wf_filtered$numerical_cc)
str(trees_wf_filtered$numerical_cc)
trees_wf_filtered$numerical_td <-as.numeric(trees_wf_filtered$numerical_td)
str(trees_wf_filtered$numerical_td)

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

cc_model <- glm(numerical_cc ~ treedist1, data = trees_wf_filtered, family = "binomial")
summary(cc_model)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  3.093092   0.383562   8.064 7.38e-16 ***
#  treedist1   -0.006980   0.006989  -0.999    0.318  

td_model <- glm(numerical_td ~ treedist1, data = trees_wf_filtered, family = "binomial")
summary(td_model)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  1.3947463  0.2076393   6.717 1.85e-11 ***
#  treedist1   -0.0009688  0.0042058  -0.230    0.818   

library(generalhoslem)

logitgof(trees_wf_filtered$numerical_cc, fitted(cc_model), g = 10)
#data:  trees_wf_filtered$numerical_cc, fitted(cc_model)
#X-squared = 4.0291, df = 8, p-value = 0.8545

logitgof(trees_wf_filtered$numerical_td, fitted(td_model), g = 10)
#data:  trees_wf_filtered$numerical_td, fitted(td_model)
#X-squared = 19.223, df = 8, p-value = 0.01371


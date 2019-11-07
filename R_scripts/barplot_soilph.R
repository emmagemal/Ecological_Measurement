# Effect of land management on soil pH
# Attempting to produce a bar chart 
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh
# Created 22/10/2019, Updated: 28/10/2019


## Library ----
library(plyr)
library(tidyverse)
library(ggpubr)  
library(plotrix)


## Loading data ----
soilph <- read.csv("Data/soil-ph-data.csv")
str(soilph)


## Checking normality ----
ggqqplot(soilph$ph)
# pretty close to the line, looks good 


## Data manipulation ----
soilph <- soilph %>% 
              mutate(site = revalue(site, c("site1" = "Young.Recovering",
                                            "site2" = "Young.Plantation",
                                            "site3" = "Old.Recovering",
                                            "site4" = "Old.Plantation",
                                            "site5" = "Grassland"))) 
detach(package:plyr)
phmean_se <- soilph %>% 
              group_by(site) %>% 
              summarize(mean_ph = mean(ph), se = std.error(ph))


## Black & White plot ----
(bw <- ggplot(data = phmean_se, aes(x = site, y = mean_ph, fill = site)) +
                geom_col(show.legend = FALSE, width = 0.6, color = "black") +
                geom_errorbar(aes(ymin = mean_ph - se, ymax = mean_ph + se), width = 0.08) +
                scale_fill_manual(values = c("#f7f7f7", 
                                             "#cccccc",
                                             "#969696",
                                             "#636363",
                                             "#252525")) +
                labs(y = "pH") +
                scale_x_discrete(labels = c("Young.Recovering" = "Young\nRecovering", 
                                            "Young.Plantation" = "Young\nPlantation",
                                            "Old.Recovering" = "Old\nRecovering",
                                            "Old.Plantation" = "Old\nPlantation",   # makes it more clear
                                            "Grassland" = "Grassland")) +   
                theme_light() +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_text(size = 10, angle = 30, hjust = 1),
                      axis.title.y = element_text(size = 13, vjust = 3),
                      axis.text.y = element_text(size = 12)) +
                coord_cartesian(ylim = c(3.5,5.6)))


## Colored plot ----
(color <- ggplot(data = phmean_se, aes(x = site, y = mean_ph, fill = site)) +
                geom_col(show.legend = FALSE, width = 0.6, color = "black") +
                geom_errorbar(aes(ymin = mean_ph - se, ymax = mean_ph + se), width = 0.08) +
                scale_fill_manual(values = c("#a6611a", 
                                             "#dfc27d",
                                             "#80cdc1",
                                             "#018571",
                                             "#21332A")) +
                labs(y = "pH") +
                scale_x_discrete(labels = c("Young.Recovering" = "Young\nRecovering", 
                                            "Young.Plantation" = "Young\nPlantation",
                                            "Old.Recovering" = "Old\nRecovering",
                                            "Old.Plantation" = "Old\nPlantation",   
                                            "Grassland" = "Grassland")) +
                theme_grey() +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_text(size = 10, angle = 30, hjust = 1),
                      axis.title.y = element_text(size = 13, vjust = 3),
                      axis.text.y = element_text(size = 12)) +
                coord_cartesian(ylim = c(3.5,5.6)))


# Saving & Finalizing Plots ----
ggsave(filename = "color_plot.png",
       path = "Images/Graphs",
       width = 3.7,
       height = 4, units = c("in"))

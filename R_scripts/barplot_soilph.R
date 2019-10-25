# Effect of land management on soil pH
# Attempting to produce a bar chart 
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh
# Created 22/10/2019


setwd("~/Documents/Uni Work/Year 3/Ecological Measurement")

## Library ----
library(plyr)
library(tidyverse)
library(ggpubr)  
library(plotrix)


## Loading data ----
soilph <- read.csv("soil-ph-data.csv")
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


## All sites barplot ----
(all_sites <- ggplot(data = phmean_se, aes(x = site, y = mean_ph, fill = site)) +
                geom_col(show.legend = FALSE, width = 0.7) +
                geom_errorbar(aes(ymin = mean_ph - se, ymax = mean_ph + se), width = 0.1) +
                scale_fill_manual(values = c("#a6611a", 
                                             "#dfc27d",
                                             "#80cdc1",
                                             "#018571",
                                             "#21332A")) +
                labs(y = "pH") +
                theme_grey() +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_text(size = 10),
                      axis.title.y = element_text(size = 15)) +
                coord_cartesian(ylim = c(4,5.6)))

# ggsave("allsites_scaled2.png")


## All sites (reordered) ----
sites_ordered <- phmean_se %>% 
                    mutate(site = factor(site, levels = c("Young.Recovering",
                                                          "Old.Recovering",
                                                          "Young.Plantation",
                                                          "Old.Plantation",
                                                          "Grassland"))) %>% 
                    arrange(site)

# plot of all sites (ordered by recovering and plantation)
(plot_all_ordered <- ggplot(data = sites_ordered, aes(x = site, y = mean_ph, fill = site)) +
                        geom_col(show.legend = FALSE, width = 0.7) +
                        geom_errorbar(aes(ymin = mean_ph - se, ymax = mean_ph + se), width = 0.1) +
                        scale_fill_manual(values = c("#a6611a", 
                                                     "#dfc27d",
                                                     "#80cdc1",
                                                     "#018571",
                                                     "#21332A")) +
                        labs(y = "pH") +
                        theme_grey() +
                        theme(axis.title.x = element_blank(),
                              axis.text.x = element_text(size = 10),
                              axis.title.y = element_text(size = 15)) +
                        coord_cartesian(ylim = c(4,5.6)))

# ggsave("allsites_ordered_scaled2.png")


## Managed sites ----
managed_sites <- subset(sites_ordered, site != "Grassland")

(plot_managed <- ggplot(data = managed_sites, aes(x = site, y = mean_ph, fill = site)) +
        geom_col(show.legend = FALSE, width = 0.7) +
        geom_errorbar(aes(ymin = mean_ph - se, ymax = mean_ph + se), width = 0.1) +
        scale_fill_manual(values = c("#a6611a", 
                                     "#dfc27d",
                                     "#80cdc1",
                                     "#018571")) +
        labs(y = "pH") +
        theme_grey() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(size = 10),
              axis.title.y = element_text(size = 15)) +
        coord_cartesian(ylim = c(3,5.3)))

# ggsave("managed_sites.png")

# secondary plots produced with ylim set to 4 and 5.x


## Black & White plots ----
# all sites
(all_bw <- ggplot(data = phmean_se, aes(x = site, y = mean_ph, fill = site)) +
     geom_col(show.legend = FALSE, width = 0.7, color = "black") +
     geom_errorbar(aes(ymin = mean_ph - se, ymax = mean_ph + se), width = 0.1) +
     scale_fill_manual(values = c("#f7f7f7", 
                                  "#cccccc",
                                  "#969696",
                                  "#636363",
                                  "#252525")) +
     labs(y = "pH") +
     theme_light() +    # has to go before the other theme preferences  
     theme(axis.title.x = element_blank(),
           axis.text.x = element_text(size = 10),
           axis.title.y = element_text(size = 15)) +
     coord_cartesian(ylim = c(4,5.6)))

# ggsave("all_bw.png")

# all sites rearranged  
(all_ord_bw <- ggplot(data = sites_ordered, aes(x = site, y = mean_ph, fill = site)) +
        geom_col(show.legend = FALSE, width = 0.7, color = "black") +
        geom_errorbar(aes(ymin = mean_ph - se, ymax = mean_ph + se), width = 0.1) +
        scale_fill_manual(values = c("#f7f7f7", 
                                     "#cccccc",
                                     "#969696",
                                     "#636363",
                                     "#252525")) +
        labs(y = "pH") +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(size = 10),
              axis.title.y = element_text(size = 15)) +
        coord_cartesian(ylim = c(4,5.6)))

# ggsave("all_order_bw.png")

# managed sites 
(managed_bw <- ggplot(data = managed_sites, aes(x = site, y = mean_ph, fill = site)) +
        geom_col(show.legend = FALSE, width = 0.7, color = "black") +
        geom_errorbar(aes(ymin = mean_ph - se, ymax = mean_ph + se), width = 0.1) +
        scale_fill_manual(values = c("#f7f7f7", 
                                     "#cccccc",
                                     "#969696",
                                     "#636363")) +
        labs(y = "pH") +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_text(size = 10),
              axis.title.y = element_text(size = 15)) +
        coord_cartesian(ylim = c(4,5.2)))

# ggsave("managed_bw.png")

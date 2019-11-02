# ANOVA for soil pH with land management practices
# Emma Gemal, s1758915@sms.ed.ac.uk
# Ecological Measurement - Year 3 
# University of Edinburgh
# Created 30/9/2019, Updated 29/10/2019

# Library ----
library(plyr)
library(dplyr)
library(multcompView)


## ANOVA all sites ----
# (including grassland)
soilph <- read.csv("Data/soil-ph-data.csv")
str(soilph)    #checking to make sure site is a factor

aov.all <- summary(aov(ph ~ site, data = soilph))
aov.all
# p-value = 1.56e-13 (statistically significant)
# F value: 31.06

plot(aov(ph ~ site, data = soilph))  # checking model convergence 


## ANOVA forest sites ----
# for all forest sites (excluding grassland)
no.grass <- soilph %>% 
                filter(site != "site5") %>%  
                droplevels(exclude = if(anyNA(levels(5))) NULL else NA)
  
str(no.grass)  # making sure it only has 4 levels 

# running ANOVA for sites 1-4
aov.nograss <- summary(aov(ph ~ site, data = no.grass))
aov.nograss
# p-value = 7.93e-0.5 (statistically significant)
# F-value = 9.166

plot(aov(ph ~ site, data = soilph))


## ANOVA management type ----
# for ph vs management type (recovering vs plantation)
managementph <- no.grass %>% 
                    mutate(site = revalue(site, c("site1" = "recovering",
                                                  "site2" = "plantation",
                                                  "site3" = "recovering",
                                                  "site4" = "plantation"))) %>% 
                    dplyr::rename(management = site)

str(managementph)   # double checking number of levels and variable names

# running ANOVA for recovering vs plantation 
aov.management <- summary(aov(ph ~ management, data = managementph))
aov.management
# p-value = 0.538 (not statistically significant)
# F-value = 0.384 (not significant)


## Tukey test ----
data4tukey <- aov(ph ~ site, data = soilph)

# Tukey test to study each pair of treatment
tukey <- TukeyHSD(x=data4tukey)

tukey 
# significant difference between 1&3, 1&4, 2&3, 2&4 and all with 5
# not significant for 1&2 and 3&4
# 1 = young recovering, 2 = young plantation, 3 = old recovering, 4 = old plantation, 5 = grassland

# Tuckey test representation
plot(tukey , las=1 , col="brown")   # not 100% needed 

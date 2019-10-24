# ANOVA for soil pH with land management practices
# Emma Gemal, s1758915@sms.ed.ac.uk
# Ecological Measurement - Year 3 
# University of Edinburgh
# Created 30/9/2019, Updated 11/10/2019

# Library ----
library(plyr)
library(dplyr)

setwd("/Users/emmagemal/Documents/Uni Work/Year 3/Ecological Measurement")

# Workflow:
# 1. do an ANOVA analysis for all 5 sites using aov()
# 2. create an object with only sites 1-4 
# 3. do an ANOVA analysis for sites 1-4 using aov()
# 4. substitute site names for "recovering" and "plantation"
    # sites 1 and 3 = recovering, sites 2 and 4 = plantation
# 5. do an ANOVA analysis for the recovering vs plantation sites using aov()


## all sites ----
# ANOVA for all sites (including grassland)
soilph <- read.csv("~/Documents/Uni Work/Year 3/Ecological Measurement/soil-ph-data.csv")
str(soilph)    #checking to make sure site is a factor

aov.all <- summary(aov(ph ~ site, data = soilph))
aov.all
# p-value = 1.56e-13 (statistically significant)

plot(aov(ph ~ site, data = soilph))  # checking model convergence 


## forest sites ----
# ANOVA for all forest sites (excluding grassland)
no.grass <- soilph %>% 
                filter(site != "site5") %>%  
                droplevels(exclude = if(anyNA(levels(5))) NULL else NA)
  
str(no.grass)  # making sure it only has 4 levels 

# running ANOVA for sites 1-4
aov.nograss <- summary(aov(ph ~ site, data = no.grass))
aov.nograss
# p-value = 7.93e-0.5 (statistically significant)

plot(aov(ph ~ site, data = soilph))


## management type ----
# ANOVA for ph vs management type (recovering vs plantation)
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

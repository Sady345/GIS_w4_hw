library(sf)
library(here)
library(tidyverse)
library(dplyr)
library(readr)
install.packages("countrycode")
library(countrycode)
library(janitor)
library(tmap)
library(tmaptools)

#TASK: combine global gender inequality data and global spatial data; compare difference in inequality between 2010 and 2019.

#reading in dataset of all indices
allcomps <- read_csv(here("HDR23-24_Composite_indices_complete_time_series.csv"))
summary(allcomps)
#filtering for only global inequality data
gii <- allcomps %>%
  select(c(iso3, country, starts_with("gii")))
summary(gii)
#creating new column for difference in 2010 and 2019 gii
gii_2 <- gii %>%
  mutate(., gii_diff = gii_2010 - gii_2019)
gii_2$gii_diff

#reading in global spatial data
world <- st_read(here("World_Countries_(Generalized)_9029012925078512962.geojson"))
world <- clean_names(world)
summary(world)
head(world$iso)
head(gii_2$iso3)

#world uses iso2c while gii_2 uses iso3c. i'm changing the iso column in world to iso3c.
world <- world %>%
  mutate(., iso = countrycode(iso, origin = "iso2c", destination = "iso3c"))
head(world$iso)

#joining gii_2 to world data
world_gii <- left_join(world, gii_2, by = c("iso" = "iso3")) %>%
  distinct(., .keep_all = TRUE)
summary(world_gii)

# Mapping the GII difference
tm_shape(world_gii) +
  tm_polygons("gii_diff", title = "GII Difference (2010-2019)", 
              palette = "-RdYlBu", 
              style = "quantile", 
              textNA = "No data") +
  tm_layout(main.title = "Global Gender Inequality Index Difference (2010-2019)",
            legend.outside = TRUE)

#Whole spectrum of colour pallete
tm_shape(world_gii) +
  tm_polygons("gii_diff", title = "GII Difference (2010-2019)", 
              palette = "-RdYlBu", 
              style = "quantile", 
              textNA = "No data", 
              midpoint = NA) +  # Setting midpoint to NA
  tm_layout(main.title = "Global Gender Inequality Index Difference (2010-2019)",
            legend.outside = TRUE)
#Boilerplate--------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(sf)
library(cbsodataR)
library(spData)
library(ggplot2)

setwd("C:/Users/Laurens/Documents/Semiproductivity/R Practice/Ducttape_Gravity_Model")

#Import-----------------------------------

#stations
StationLocations_import = read_sf("StationLocaties.json")

StationLocations = StationLocations_import %>%
  filter(beheerder == "ProRail"
         & evenement == "N"
         & stationsgrootte != "Onbekend"
         & !is.na(objectid))

#precalculated 500grid

cbs500 = read_sf("grid_with_adjacencies.gpkg")

#passenger numbers

pax_2023_import = read_csv("passengers_2023.csv")

pax_2023 = pax_2023_import %>%
  filter(NS != "no") %>%
  mutate(Reizigers_2023 = as.numeric(Reizigers_2023))

#Joins----------------------------------------

combined = StationLocations %>%
  left_join(pax_2023, by = join_by(afkorting == afkorting)) %>%
  filter(is.na(Reizigers_2023) == FALSE)

combined = st_join(combined, cbs500["SumQueen1"])

#plotting-------------------



ggplot(combined, aes(x = SumQueen1,
                     y = Reizigers_2023)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  labs(x = "Population within 750 meters",
       y = "Passengers",
       title = "NS stations in 2023",
       subtitle = "Log scale")


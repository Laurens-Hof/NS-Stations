#Boilerplate--------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(sf)
library(cbsodataR)
library(spData)

setwd("C:/Users/Laurens/Documents/Semiproductivity/R Practice/Ducttape_Gravity_Model")

#Import-----------------------------------

#stations
StationLocations_import = read_sf("StationLocaties.json")

StationLocations = StationLocations_import %>%
  filter(beheerder == "ProRail"
         & evenement == "N"
         & stationsgrootte != "Onbekend"
         & !is.na(objectid))
  
#pop distribution
cbs500_import = read_sf("cbs_vk500_2023_v1.gpkg")

cbs500 = cbs500_import %>%
  mutate(Pops = case_when(
            aantal_inwoners < -99000 ~ 0,
            is.na(aantal_inwoners) ~ 0,
            .default = aantal_inwoners),
         Urbanity = case_when(
            stedelijkheid < -99000 ~ 0,
            .default = stedelijkheid
         )) %>%
  select(crs28992res500m,
         Pops,
         Urbanity,
         geom)


#calculate pos in cell plus surrounding cells--------------------------

#Create numeric coordinates to easily calculate orthogonal distance
cbs500$EastCode = as.numeric(substr(cbs500$crs28992res500m, 2, 5))
cbs500$NorthCode = as.numeric(substr(cbs500$crs28992res500m, 7, 10))

cbs500$SumQueen1 = 0

for(i in 1:nrow(cbs500)){
  cbs500$SumQueen1[i] = 
    sum(cbs500[abs(cbs500$EastCode - cbs500$EastCode[i]) <= 5 &
               abs(cbs500$NorthCode - cbs500$NorthCode[i]) <= 5, ]$Pops)
  print(paste(round(i/nrow(cbs500) * 100, 1), "%", sep = ""))
}

write_sf(cbs500, "grid_with_adjacencies.gpkg")

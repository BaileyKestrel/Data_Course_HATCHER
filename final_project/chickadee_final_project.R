library(tidyverse)
library(ggplot2)
library(skimr)
library(janitor)

# load in MAPS banding capture data set
chickadee_raw <- read_csv("MAPS_BANDING_capture_data.csv")

# select columns of interest and rename columns for better understanding
chickadee_thin <- chickadee_raw %>% 
  select(LOC, STA, STATION, C, BAND, SPEC, AGE, HA, WRP, SEX, HS, SK, CP, BP, F, 
         BM, FM, FW, JP, WEIGHT, STATUS, DATE, DISP, BRSTAT) %>% 
  rename(location = LOC) %>% 
  rename(station_num = STA) %>% 
  rename(station_code = STATION) %>% 
  rename(capture_code = C) %>% 
  rename(band_num = BAND) %>% 
  rename(species = SPEC) %>% 
  rename(age = AGE) %>% 
  rename(aged_how = HA) %>% 
  rename(sex = SEX) %>% 
  rename(sexed_how = HS) %>% 
  rename(skull_pneum = SK) %>% 
  rename(cloacal_prot = CP) %>% 
  rename(broodpatch = BP) %>% 
  rename(fat = F) %>% 
  rename(body_molt = BM) %>% 
  rename(flight_molt = FM) %>% 
  rename(flight_wear = FW) %>% 
  rename(juvenile_plum = JP) %>% 
  rename(wing_mm = WNG) %>% 
  rename(weight_g = WEIGHT) %>% 
  rename(status = STATUS) %>% 
  rename(date = DATE) %>% 
  rename(disposition = DISP) %>% 
  rename(breed_status = BRSTAT)
  
# remove rows with NA for BAND
chickadee_thin <- chickadee_thin %>% 
  filter(!is.na(band_num))

# create data set with just the recaptured birds
recap_chickadee <- chickadee_thin %>% 
  filter(capture_code == "R")












num(unique(recap_chickadee$band_num))

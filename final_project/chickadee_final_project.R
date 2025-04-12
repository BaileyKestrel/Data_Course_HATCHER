library(tidyverse)
library(ggplot2)
library(skimr)
library(janitor)

# load in MAPS data sets
capture_data <- read_csv("MAPS_BANDING_capture_data.csv")
location_data <- read_csv("MAPS_STATION_location_and_operations.csv")

# merge capture_data and location_data
joined_data <- left_join(capture_data, location_data, 
                         by = c("LOC", "STA", "STATION"))

# select columns of interest and rename columns for better understanding
filtered_data <- joined_data %>% 
  select(STASLIST, LATITUDE, LONGITUDE, ELEV, HABITAT, LOC, STA, STATION, BCR, 
         DATE, C, BAND, SPEC, AGE, HA, WRP, SEX, HS, SK, CP, BP, F, BM, FM, FW, 
         JP, WEIGHT, WNG, STATUS, DISP, BRSTAT) %>% 
  rename(staslist = STASLIST,
         latitude = LATITUDE, 
         longitude = LONGITUDE, 
         elevation = ELEV, 
         habitat = HABITAT, 
         location = LOC, 
         station_num = STA, 
         station_code = STATION, 
         conserv_reg = BCR, 
         date = DATE, 
         capture_code = C, 
         band_num = BAND, 
         species = SPEC, 
         age = AGE, 
         aged_how = HA, 
         age_molt = WRP, 
         sex = SEX, 
         sexed_how = HS, 
         skull_pneum = SK, 
         cloacal_p = CP, 
         broodpatch = BP, 
         fat = F, 
         body_molt = BM, 
         flight_molt = FM, 
         fliht_wear = FW, 
         juvenal_p = JP, 
         weight_g = WEIGHT,
         wing_mm = WNG,
         status = STATUS, 
         disposition = DISP, 
         breed_stat = BRSTAT)
  
  
 
  
# remove rows with NA for BAND
filtered_data <- filtered_data %>% 
  filter(!is.na(band_num))

# create data set with just the recaptured birds
recap_data <- chickadee_thin %>% 
  filter(capture_code == "R")



















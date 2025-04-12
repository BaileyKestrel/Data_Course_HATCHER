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
  select(STASLIST, STATE, LATITUDE, LONGITUDE, ELEV, HABITAT, LOC, STA, STATION, 
         BCR, DATE, C, BAND, SPEC, AGE, HA, SEX, HS, SK, CP, BP, F, BM, FM, FW, 
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
  


# define function that converts Degrees Minutes Seconds (DMS) to Decimal Degrees (DD)
# function header defines function named dms_to_dd that takes argument: dms_str
dms_to_dd <- function(dms_str) { 
  # apply function to each element (x) of dms_str
  sapply(dms_str, function(x) {
    # Split string into parts using spaces as separators
    parts <- strsplit(x, " ")[[1]]
    
    # skips converting if incorrect data form and inputs NA
    if (length(parts) != 3) return(NA)
    
    # Converts each part to numeric values
    deg <- as.numeric(parts[1])
    min <- as.numeric(parts[2])
    sec <- as.numeric(parts[3])
    
    # Handle sign for degrees
    sign <- ifelse(deg < 0, -1, 1) # if deg is negative, store the sign as -1, otherwise 1
    deg <- abs(deg) # ensure calculation is not altered by sign until end
    
    # Convert to decimal degrees
    dd <- sign * (deg + min / 60 + sec / 3600)
    # return result so it can be used outside this formula
    return(dd)
  })
}


filtered_data$latitude <- dms_to_dd(filtered_data$latitude)
filtered_data$longitude <- dms_to_dd(filtered_data$longitude)

























library(maps)
us <- map_data("state")

ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray70") +
  geom_point(data = filtered_data, 
             aes(x = longitude, y = latitude, color = species),
             alpha = 0.6) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Chickadee MAPS Locations on U.S. Map")




ggplot(filtered_data, aes(x = longitude, y = latitude, color = species)) +
  geom_point(alpha = 0.5) +
  coord_fixed() +
  facet_wrap(~sex) +
  theme_minimal() +
  labs(title = "Chickadee Species Ranges Over Time")


skim(filtered_data)

# remove rows with NA for band_num
filtered_data <- filtered_data %>% 
  filter(!is.na(band_num))

# create data set with just the recaptured birds
recap_data <- chickadee_thin %>% 
  filter(capture_code == "R")



















library(tidyverse)
library(ggplot2)
library(janitor)
library(maps)
library(ggthemes)
library(gganimate)
library(concaveman)

# load in MAPS data sets
capture_data <- read_csv("MAPS_BANDING_capture_data.csv")
location_data <- read_csv("MAPS_STATION_location_and_operations.csv")

# merge capture_data and location_data
joined_data <- left_join(capture_data, location_data, 
                         by = c("LOC", "STA", "STATION"))

# select columns of interest and rename columns for better understanding
dat <- joined_data %>% 
  select(LATITUDE, LONGITUDE, LOC, STA, STATION, DATE, C, BAND, SPEC, AGE, SEX, 
         F, STATUS) %>% 
  rename(latitude = LATITUDE, longitude = LONGITUDE, location = LOC, station_num 
         = STA, station_code = STATION, date = DATE, capture_code = C, band_num 
         = BAND, species = SPEC, age = AGE, sex = SEX, fat = F, status = STATUS)

####Chickadee Distribution####
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
    return(dd) # return result so it can be used outside this formula
  })
}

# apply the conversion formula dms_to_dd to latitude and longitude columns
dat$latitude <- dms_to_dd(dat$latitude)
dat$longitude <- dms_to_dd(dat$longitude)

# change date format and create new columns for year, month, day
dat <- dat %>%
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date),
         day = day(date))

# remove NA from necessary columns
dat_latlong <- dat %>%
  filter(!is.na(longitude), !is.na(latitude))




# Create a base map of US and Canada for use in other plots
map_world <- map_data("world")
north_america <- map_world %>% 
  filter(region %in% c("USA", "Canada"))



# create color palette that is high contrast and color blind friendly
color_palette <- c("BCCH" = "#F0E442",
                   "BOCH" = "#56B4E9",
                   "CACH" = "#009E73",
                   "CBCC" = "hotpink",
                   "CBCH" = "#0072B2",
                   "MOCH" = "#D55E09") 

# remove NA from necessary columns
dat_latlong <- dat %>%
  filter(!is.na(longitude), !is.na(latitude))

# create scatter plot of all chickadee capture data
ggplot() +
  # create base map 
  geom_polygon(data = north_america, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray70") +
  # jitter points to avoid overlapping points (MAPS stations are stationary)
  geom_jitter(data = dat_latlong,
              aes(x = longitude, y = latitude, fill = species),
              shape = 21, size = 3.5,
              alpha = 0.2, stroke = 0.3,
              color = "black", width = 0.2, height = 0.2) +
  coord_cartesian(xlim = c(-175, -50), ylim = c(25, 75)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  guides(fill = guide_legend(override.aes = list(alpha = 1, size = 5))) +
  scale_fill_manual(values = color_palette)








# faceted hexbin map of all chickadee captures
ggplot() +
  geom_polygon(data = north_america, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "gray70") +
  geom_hex(data = dat_latlong,
           aes(x = longitude, y = latitude, fill = ..count..),
           bins = 70, alpha = 0.7) +
  labs(x = "Longitude", y = "Latitude", title = "Title") +
  scale_fill_viridis_c(option = "plasma") +
  coord_cartesian(xlim = c(-175, -50), ylim = c(25, 75)) +
  facet_wrap(~species) +
  theme_minimal()



# animated hexbin map of all chickadee captures over time faceted by species 
dat_year <- dat %>% filter(!is.na(year)) %>%
  mutate(year = as.integer(year))

anim_plot <- ggplot() +
  geom_polygon(data = north_america, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray70") +
  geom_hex(data = dat_year,
           aes(x = longitude, y = latitude, fill = ..count..),
           bins = 70, alpha = 0.7) +
  scale_fill_viridis_c(option = "plasma") +
  coord_cartesian(xlim = c(-180, -50), ylim = c(25, 75)) +
  facet_wrap(~species) +
  theme_minimal(base_size = 14) +
  labs(title = 'Year: {current_frame}', 
       subtitle = 'Species density in North America',
       fill = "Capture Count") +
  transition_manual(year)

# Animate
animate(anim_plot, nframes = 100, fps = 10, width = 1000, height = 800, 
        renderer = gifski_renderer())












# create convex hull plot
# high contrast and color blind friendly palette
high_contrast <- c(
  "#EE7733", "#0077BB", "#33BBEE", "#009988",
  "#CC3311", "#EE3377"
)
# Compute convex hulls
hulls <- dat_latlong %>%
  group_by(species) %>%
  filter(n() >= 3) %>%  # chull requires at least 3 points
  slice(chull(longitude, latitude))

# Plot
ggplot() +
  geom_polygon(data = north_america, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray50") +
  geom_polygon(data = hulls, aes(x = longitude, y = latitude, fill = species, group = species),
               alpha = 0.7, color = "black") +
  coord_cartesian(xlim = c(-175, -50), ylim = c(25, 75)) +
  theme_minimal() +
  scale_fill_manual(values = high_contrast) +
  labs(title = "Estimated Chickadee Ranges by Convex Hull",
       x = "Longitude", y = "Latitude",
       fill = "Species")







# create a concave hull (alpha shape)
alpha_hulls <- dat_latlong %>%
  group_by(species) %>%
  filter(n() >= 3) %>%
  group_split() %>%
  map_dfr(~ {
    # Extract longitude and latitude as a matrix
    hull <- concaveman(as.matrix(.x[, c("longitude", "latitude")]))
    
    # Convert the hull result to a data frame
    hull_df <- data.frame(longitude = hull[, 1], latitude = hull[, 2], species = unique(.x$species))
    
    return(hull_df)
  })

# Plot
ggplot() +
  geom_polygon(data = north_america, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray50") +
  geom_polygon(data = alpha_hulls, aes(x = longitude, y = latitude, fill = species, group = species),
               alpha = 0.7, color = "black") +
  coord_cartesian(xlim = c(-175, -50), ylim = c(25, 75)) +
  theme_minimal() +
  scale_fill_manual(values = high_contrast) +
  labs(title = "Estimated Chickadee Ranges by Concave Hull (Alpha Shape)",
       x = "Longitude", y = "Latitude",
       fill = "Species", color = "Species")





#### Chickadee Count/Proportion Comparison ####

species_year_props <- dat %>%
  group_by(year, species) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(prop = n / sum(n))

ggplot(species_year_props, aes(x = year, y = prop, fill = species)) +
  geom_area() +
  labs(
    y = "Proportion",
    title = "Proportional Abundance of Chickadee Species Over Time",
    x = "Year", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = color_palette)


species_year_counts <- dat %>%
  group_by(year, species) %>%
  summarise(count = n(), .groups = "drop")

ggplot(species_year_counts, aes(x = year, y = count, fill = species)) +
  geom_area() +
  labs(
    title = "Chickadee Species Counts Over Time",
    x = "Year",
    y = "Count",
    fill = "Species"
  ) +
  theme_minimal() +
  scale_fill_manual(values = color_palette)



#### Chickadee Recapture ####

# identify band codes that were recaptured at least once
recaptured_ids <- dat %>%
  filter(capture_code == "R") %>%
  distinct(band_num)

# create new variable if a bird was ever recaptured 
dat <- dat %>%
  mutate(ever_recaptured = ifelse(band_num %in% recaptured_ids$band_num, "Recaptured", "Not Recaptured"))

# find the first capture event
first_capture_of_recaptures <- dat %>%
  semi_join(recaptured_ids, by = "band_num") %>%  # Only birds that were recaptured
  group_by(band_num) %>%
  slice_min(order_by = year, n = 1, with_ties = FALSE) %>%  # Get first capture per bird
  ungroup()

# count by species and year
recaptured_unique_counts <- first_capture_of_recaptures %>%
  group_by(year, species) %>%
  summarise(unique_recaptures = n(), .groups = "drop")
# plot
ggplot(recaptured_unique_counts, aes(x = year, y = unique_recaptures, fill = species)) +
  geom_area() +
  labs(
    title = "Unique Recaptured Chickadees by Species Over Time",
    x = "Year",
    y = "Count of Recaptured Individuals",
    fill = "Species"
  ) +
  theme_minimal() +
  scale_fill_manual(values = color_palette)


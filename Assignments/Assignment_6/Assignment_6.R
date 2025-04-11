library(tidyverse)
library(ggplot2)
library(janitor)
library(skimr)
library(gganimate)

# load csv file into new object and view
dat <- read.csv("../../Data/BioLog_Plate_Data.csv")
View(dat)

#### 1. clean data into tidy (long) form ####

# make the names clean for easy manipulation
dat <- clean_names(dat)

clean_dat <- dat %>% 
  pivot_longer(cols = starts_with("hr_"),
               names_to = "time",
               values_to = "abs") %>% 
  mutate(time = as.numeric(str_remove(time, 'hr_')))



#### 2. create a new column specifying whether a sample is from soil or water ####

# identify the unique names of the sample_id
clean_dat$sample_id %>% unique()

# create a new column called type that is either soil or water
clean_dat_2 <- clean_dat %>% 
  mutate(type = case_when(
    sample_id %in% c("Clear_Creek", "Waste_Water") ~ "water",
    sample_id %in% c("Soil_1", "Soil_2") ~ "soil",
    TRUE ~ 'Wrong'
  ))


#### 3. generate plot matching given (just plotting dilution == 0.1) ####

# filter to only include dilution of 0.1
dat_plot <- clean_dat_2 %>% 
  filter(dilution == 0.1)

# make plot
dat_plot %>% 
  ggplot(aes(x = time, y = abs, color = type)) +
  geom_smooth(se = F) +
  facet_wrap(~substrate) +
  labs(
    title = "Just dilution 0.1",
    x = "Time",
    y = "Absorbance",
    color = "Type") +
  theme_minimal()



#### generate a matching animated plot (absorbance values are mean of all 3 replicates for each group) ####
## The plot is just showing values for the substrate “Itaconic Acid” ##

# identify unique substrate names for itaconic acid
clean_dat_2$substrate %>% unique()

# filter data to only include substrate of Itaconic Acid
itaconic_dat <- clean_dat_2 %>% 
  filter(substrate == "Itaconic Acid")

# create object that stores mean abs values (itaconic_dat with new column mean_abs)
mean_abs <- itaconic_dat %>% 
  group_by(sample_id, dilution, time) %>% 
  summarise(mean_abs = mean(abs))

# create animated plot
mean_abs %>% 
  ggplot(aes( x = time, y = mean_abs, color = sample_id)) +
  geom_line() +
  facet_wrap(~dilution) +
  labs(
    title = "This plot is just showing values for the substrate 'Itaconic Acid'",
    x = "Time",
    y = "Mean_absorbance",
    color = "Sample ID") +
  theme_minimal() +
  transition_reveal(time)







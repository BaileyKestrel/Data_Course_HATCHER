library(tidyverse)
library(skimr)
library(janitor)
library(ggplot2)
library(gganimate)

dat <- read.csv('Data/BioLog_Plate_Data.csv')
View(dat)

## clean names
names(dat)
dat %>% clean_names()

## create a new col = time, pivot longer
dat_clean <- dat %>% 
  pivot_longer(cols = starts_with('Hr_'),
               names_to = 'time',
               values_to = 'abs') %>% 
  mutate(time = as.numeric(str_remove(time, 'Hr_')))
## create a new col = type (soil or water)
dat_clean$Sample.ID %>% unique()

dat_clean_v2 <- dat_clean %>% 
  mutate(type = case_when(
    Sample.ID %in% c("Clear_Creek", "Waste_Water") ~ 'Water',
    Sample.ID %in% c("Soil_1", "Soil_2") ~ 'Soil',
    TRUE ~ 'Wrong' #Set the rest as 'Wrong' that are neither water or soil
  ))

# filter to only include dilution of 0.1
dat_plot <- dat_clean_v2 %>% 
  filter(Dilution == 0.1)

dim(dat_plot)

dat_plot$Dilution %>% unique()



dat_plot %>% 
  ggplot(aes(x = time, y = abs, color = type))+
  geom_smooth(se = F)+
  facet_wrap(~Substrate)+
  labs(title = 'Just dilution 0.1',
       x = 'Time',
       y = 'Absorbance',
       color = 'Type')+
  theme_minimal()




## Generates an animated plot that matches this one (absorbance values are mean of all 3 replicates for each group):
## This plot is just showing values for the substrate “Itaconic Acid”

itaconic_dat <- dat_clean_v2 %>% 
  filter(Substrate == 'Itaconic Acid')

mean_abs <- itaconic_dat %>% 
  group_by(Sample.ID, Dilution, time) %>% 
  summarise(mean_abs = mean(abs))

library(gganimate)
mean_abs %>% 
  ggplot(aes(x = time, y = mean_abs, color = Sample.ID))+
  geom_line()+
  facet_wrap(~Dilution)+
  labs(x = 'Time', y = 'Mean_absorbance',
       color = 'Sample ID')+
  theme_minimal()+
  transition_reveal(time)



# read Height.xlsx file (from class teams) and make it tidy
library(readxl)
dat <- read_xlsx('Data/height.xlsx')

install.packages("measurements")
library(measurements)

dat_2 <- dat %>% 
  pivot_longer(everything(),
               names_to = 'sex',
               values_to = 'height') %>% 
  # separate the height column between the feet and inches
  separate(height, into = c('feet', 'inches'), convert = T) %>% 
  mutate(inches_all = (feet*12) + inches) %>% 
  mutate(cm = conv_unit(inches_all, from = 'inch', to = 'cm'))

dat_2 %>% ggplot(aes(x = cm, fill = sex))+
  geom_density(alpha = 0.5)

#statistical test. compare avg height of male vs avg. height of female
t.test(cm ~ sex, data = dat_2)
cor.test()
?glm()

mod <- glm(data = dat_2,
    formula = cm ~ sex)
summary(mod)

#### Feb 4 ####

# install package and view data
install.packages("palmerpenguins")
library(palmerpenguins)
View(penguins)
?penguins

# load in tidyverse
library(tidyverse)
# pipe: %>% : shift + command + M

# 1.1. find the fat penguins (body_mass > 5000)
fat_penguins <- penguins %>% 
  filter(body_mass_g > 5000)

View(fat_penguins)
  
# 1.2. count how many are male and how many are female
# option 1:
fat_male <- fat_penguins %>% 
  filter(sex == "male")
View(fat_male)

fat_female <- fat_penguins %>% 
  filter(sex == "female")
View(fat_female)
# option 2:
penguins %>% 
  filter(body_mass_g > 5000) %>% 
  group_by(sex) %>% 
  summarise(count = n())

# 1.3. return the max body mass for male and female
penguins %>% 
  filter(body_mass_g > 5000) %>% 
  group_by(sex) %>% 
  summarise(count = n(),
            fattest = max(body_mass_g),
            skinny_among_fatties = min(body_mass_g))

# max body mass full
max(penguins$body_mass_g, na.rm = T)

# 2.1. add new column to penguins to dataset that says whether they're fat
# option 1:
dat_peng <- penguins # don't want to mess up original data
dat_peng$fat_or_not <- dat_peng$body_mass_g > 5000
View(dat_peng)

# option 2:
penguins %>% 
  filter(body_mass_g > 5000) %>% 
  mutate(fat_or_not = body_mass_g ) %>% 
  View()

# option 3:
penguins %>% 
  mutate(fat_or_not = case_when(body_mass_g > 5000 ~ 'fat',
                                body_mass_g <= 5000 & body_mass_g > 3000 ~ 'medium',
                                body_mass_g <= 3000 ~ 'skinny',
                                TRUE ~ 'NA')) %>% 
  View()

##if condition is TRUE ~ do this
# mutate means to add a column




# make a graph of the data
dat <- penguins %>% 
  mutate(fat_or_not = case_when(body_mass_g > 5000 ~ 'fat',
                                body_mass_g <= 5000 & body_mass_g > 3000 ~ 'medium',
                                body_mass_g <= 3000 ~ 'skinny'))
# basic and simple plot
plot(dat_peng$bill_length_mm, dat_peng$body_mass_g)                                

# install ggplot2 (makes pretty graphs)
library(ggplot2)
?ggplot2

# without using tidyverse:ggplot(data = dat_peng)

dat_peng %>% 
  ggplot(aes(x = bill_length_mm,
             y = body_mass_g)) +
  geom_point() +
  geom_smooth() 
                                
# aes = aesthetic
# + means to add another layer
# geom_ how do I want to present the data? geom_point is scatter plot

dat_peng %>% 
  ggplot(aes(x = bill_length_mm,
             y = body_mass_g,
             color = sex,
             shape = fat_or_not)) +
  geom_point() 

  
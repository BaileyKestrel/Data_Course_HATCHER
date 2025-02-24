## Make a plot with 'flipper_length' on the x-axis
## and 'body mass' on the y-axis.

library(tidyverse)
library(ggplot2)
library(palmerpenguins)
names(penguins)

penguins %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_col()

penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = flipper_length_mm, 
             y = body_mass_g)) +
  geom_area(outline.type = 'upper')


penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = flipper_length_mm, 
             y = body_mass_g,
             color = species,
             shape = sex)) +
  geom_point()

penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = flipper_length_mm, 
             y = body_mass_g,
             color = species,
             shape = sex)) +
  geom_path() +
  geom_point() +
  stat_elipse()


penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = body_mass_g,
             color = species)) +
  geom_density()

penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = body_mass_g,
             fill = species)) +
  geom_histogram(alpha = 0.4) #use histogram for continuous data; bar for category data


my_plot <- penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = body_mass_g,
             y = flipper_length_mm,
             color = species)) +
  geom_point()

my_2nd_plot <- my_plot + stat_ellipse()

my_2nd_plot


#save the plot
ggsave("filename.jpg", plot = my_2nd_plot,
       width = 6, height = 8, dpi = 300) #default scale is inch
# 6*300 = 1800; 8*300 = 2400; 1800x2400





## load DatasaurusDozen.tsv (tab separated value)
dat <- read_tsv("Data/DatasaurusDozen.tsv") #tidyverse
view(dat)
head(dat)
str(dat)

read.delim() #not under tidyverse, tab-deliminated
dat_2 <- read.delim("Data/DatasaurusDozen.tsv")
str(dat_2)

summary(dat_2$y)

dat_2 %>% 
  group_by(dataset) %>% 
  summarise(mean = mean(x),
            sd = sd(x),
            max = max(x), 
            min = min(x))

dat_2 %>% 
  ggplot(aes(x = x,
             fill = dataset)) +
  geom_density()

dat_2 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_point() +
  facet_wrap(~ dataset) # how the data distributes


install.packages('GGally')
library(GGally) #shows any possibility of two variable combinations
ggpairs(penguins)

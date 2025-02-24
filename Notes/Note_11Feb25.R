rm(list = ls()) # remove items in your environment

library(tidyverse)
library(palmerpenguins)

penguins %>% 
  ggplot(aes(x = species)) +
  geom_bar(stat = 'count') # default

penguins %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_bar(stat = 'identity') # plots the count

penguins %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_col() # default = stacked

penguins %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_col(position = 'dodge') # plots out fattest penguin

penguins %>% 
  group_by(species) %>% 
  summarise(avg_mass = mean(body_mass_g, na.rm = T)) #need to remove the NA values

penguins %>% 
  group_by(species) %>% 
  summarise(max_mass = max(body_mass_g, na.rm = T))

penguins %>% 
  ggplot(aes(x = species, fill = island)) +
  geom_bar(stat = 'count', position = 'stack') # default
         
penguins %>% 
  ggplot(aes(x = species, fill = island)) +
  geom_bar(stat = 'count', position = 'dodge') 


# default for geom_bar is count... can only have x or y
penguins %>% 
  group_by(species) %>% 
  summarise(avg_mass = mean(body_mass_g, na.rm = T),
            sd = sd(body_mass_g, na.rm = T)) %>% 
  ggplot(aes(x = species, y = avg_mass)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = avg_mass - sd,
                    ymax = avg_mass + sd),
                width = 0.2)



## making an interesting graph for penguins data
## DO NOT using geom_point()

# make a density plot
penguins %>% 
  ggplot(aes(x = body_mass_g, fill = species)) +
  geom_density(alpha = .5)

penguins %>% 
  filter(!is.na(body_mass_g)) %>% # removes the NA lines to remove the warning message
  ggplot(aes(x = body_mass_g, fill = island)) + 
  geom_density(alpha = 0.5)

# make a hex plot
penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = body_mass_g, y = species)) +
  geom_hex()
 
penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = body_mass_g, y = species)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter()




penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = factor(year), y = body_mass_g)) +
  geom_boxplot() +
  geom_jitter()

str(penguins)


install.packages('qrcode')
library(qrcode)
url <- 'https://gzahn.github.io/data-course/#top'
qr <- qrcode::qr_code(url)
plot(qr)

##https://cran.r-project.org
##https://www.bioconductor.org

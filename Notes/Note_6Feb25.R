library(palmerpenguins)
View(penguins)
?penguins

penguins %>% 
  names()

bad_dat <- penguins %>%
  mutate(yearr = year + 20)

View(bad_dat[, -(ncol(bad_dat)-1)])  
bad_dat[, -ncol()]
ncol(bad_dat)
nrow(bad_dat)

bad_dat %>% 
  select(-island) %>% 
  View()

bad_dat %>% 
  select(-c(island, sex, yearr)) %>% 
  View()


#! logical operator for false
is.na # returns TRUE if the object is NA 

x <- c(1, 2, 3, NA, 5)
is.na(x)
!is.na(x)
y <- x[!is.na(x)] # removes the NA from the dataset




ggplot(penguins)
## make a plot to show fat penguins and their species
library(ggplot2)
View(penguins)

# look at the data to see what qualifies as "fat"
penguins %>% 
  summary()

#scatterplot
penguins %>% 
  filter(body_mass_g > 5000) %>% 
  ggplot(aes(x = body_mass_g,
           y = bill_length_mm,
           color = species)) +
  geom_point()
  
#bar graph
penguins %>% 
  filter(body_mass_g > 3000) %>%
  group_by(species, sex) %>% 
  summarise(mean_body_mass_g = mean(body_mass_g),
            sd_body_mass_g = sd(body_mass_g))
  ggplot(aes(x = mean_body_mass_g))+
  geom_bar(stat = 'identity')



data("penguins")

penguins %>% 
  ggplot(aes(x = bill_length_mm,
           y = body_mass_g,
           color = species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)



penguins %>% 
  ggplot(aes(x = bill_length_mm,
             y = body_mass_g,
             color = species)) +
  geom_point() +
  scale_color_viridis_d()



penguins %>% 
  ggplot(aes(x = bill_length_mm,
             y = body_mass_g,
             color = species)) +
  geom_point() +
  scale_color_manual(values = c('Gentoo' = 'pink', 'Adelie' = 'lightblue', 'Chinstrap' = 'black')) + #color is assigned alphabetical to species unless specified
  theme_dark() + #changes background color darker
  theme(axis.text = element_text(angle = 190, face = 'italic')) # changes the font of the axis
  
  
  

# making a bar graph:
penguins %>% 
  ggplot(aes(x = flipper_length_mm)) +
  geom_bar()

penguins %>% 
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g,
             fill = species)) + # shows stacked species and individuals
  geom_col() #column requires x and y 
  
penguins %>% 
  ggplot(aes(x = flipper_length_mm,
             y = body_mass_g,
             fill = species)) + 
  geom_col(position = 'dodge') #shows them side by side instead of stacked 

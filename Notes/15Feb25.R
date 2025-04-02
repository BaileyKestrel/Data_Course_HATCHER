library(ggmap)
install.packages("leaflet")
library(leaflet)

geocode('Lisbon')


library(ggplot2)
library(tidyverse)


dat_ex <- data.frame(
  ID = c(1, 2, 3),
  Weight = c(78, 88, 100),
  Height = c(167, 180, 155)
)

dat_ex %>% 
  pivot_longer(cols = c(Height, Weight),
               names_to = 'Measurement',
               values_to = 'Value') 
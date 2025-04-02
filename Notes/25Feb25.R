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
               values_to = 'Value') %>% 
  View()


dat_ex %>% 
  pivot_longer(cols = everything(),
               names_to = 'Measure',
               values_to = 'Value') %>% 
  View()


dat_ex %>% 
  pivot_longer(cols = -ID,
               names_to = 'Measurement',
               values_to = 'Value') %>% 
  View()

dat_long <- dat_ex %>% 
  pivot_longer(cols = -ID,
               names_to = 'Measure',
               values_to = 'Value')

View(dat_long)




# read this data and plot rent for each state
# make it good format for plotting
# hint: pivot_longer, pivot_wider
# x-axis = state, y-axis = rent, bar chart
df <- read_csv('Data/wide_income_rent.csv')

df %>%
  pivot_longer(cols = -variable, #pivot_longer everything except for variable
               names_to = "state",
               values_to = "value") %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  ggplot(aes(x = rent, y = income)) +
  geom_point() +
  geom_text(aes(label = state))
  


# fix built in table2
table2

table2 %>% 
  pivot_wider(names_from = 'type',
              values_from = 'count')


# fix table3
table3
table3 %>% 
  separate(rate, c('col1', 'col2'))

#fix table4a and table4b
table4a
table4b


  


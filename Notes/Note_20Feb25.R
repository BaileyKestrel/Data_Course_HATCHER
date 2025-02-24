library(gapminder)
library(tidyverse)
library(ggimage)
library(gganimate)
library(patchwork)

## data input
df <- gapminder

#stores plot in p3, need to view p3 to see the plot
p3 <- df %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent)) +
  geom_point()

p3 #view the plot
  
df$year %>%  range
df$year %>% unique()
  

dat <- gapminder
## make graph
p1 <- dat %>% 
  ggplot(aes(x = year,
             y = lifeExp,
             color = continent)) +
  geom_point(aes(size = pop)) +
  facet_wrap(~ continent)

#make the graph first 
p1 + transition_time(time = year)
#then make the animation
p1 + transition_components(time = year)

p3 + transition_time(time = year) +
  labs(title = 'Year:{frame_time}')


ggsave()

anim_save('gganimate_gif')


df$country %>% unique() #look at the countries that are in the data
my_country <- c("China", "Malaysia", "Singapore", "Japan", "Nepal", "Iceland", "Uganda", "Cote d'Ivoire", "Rwanda")

df %>% 
  mutate(my_coutries = case_when(country %in% my_country ~ country))

df2 <- df %>% 
  mutate(my_countries = case_when(country %in% my_country ~ country))

#makes a super ugly plot with every country labeled
p4 <- df %>% 
  mutate(my_countries = case_when(country %in% my_country ~ country)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  geom_text(aes(label = country))

p4 + transition_time(time = year) +
  labs(title = 'Year:{frame_time}')

p5 <- df %>% 
  mutate(my_countries = case_when(country %in% my_country ~ country)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  geom_text(aes(label = my_countries))

#animates the previous plot
p5 + transition_time(time = year) +
  labs(title = 'Year:{frame_time}')


anim_p5 <- p5 + transition_time(time = year) +
  labs(title = 'Year:{frame_time}')

anim_save('anim.gif', animation = anim_p5)


install.packages("ggmap")
library(ggmap)

#define location
nyc_map <- get_map(location = location, zoom = 12)

# plot the map 
ggmap(nyc_map)

#with your own data
#ran out of time... look on my own time








##
df <- read_csv('Data/wide_income_rent.csv')
View(df)

# read this data and plot rent for each state
# hint: x-axis = state, y-axis = rent, bar chart

# we need to clean the data first

?pivot_longer
?pivot_wider

ggplot(df_t, aes(x = state, y = rent)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dat_ex <- data.frame(
  ID = c(1, 2, 3),
  Weight = c(78, 88, 100),
  Height = c(167, 180, 155)
)

dat_ex %>% 
  pivot_longer(cols = c(Weight, Height),
               names_to = 'Measure',
               values_to = 'Value') %>% 
  View()


?geom_smooth() # loess method is the default

##try to make longer data frame back to original using 'pivot wider'
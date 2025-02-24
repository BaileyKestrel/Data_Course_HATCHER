library(tidyverse)
library(ggplot2)
library(palmerpenguins)
names(penguins)

# reverse engineer BIOL3100_ggplot_reverse_engenineeing.png in class files
# my attempt
penguins %>% 
  filter(!is.na(sex)) %>%
  filter(!is.na(bill_depth_mm)) %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = bill_depth_mm, y = body_mass_g, color = sex)) +
  geom_point(size = 3, alpha = 0.7) +
  facet_wrap(~species) +
  labs(x = "Bill depth (mm)", y = "Body mass (g)", color = "Sex") +
  scale_color_manual(values = c("female" = "darkorchid4", "male" = "limegreen")) +
  theme(panel.background =  element_blank(),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
        panel.grid.minor = element_line(color = "gray90", linewidth = 0.3),
        panel.border = element_rect(color = "gray30", fill = NA),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text())
# class example:
penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = bill_depth_mm,
             y = body_mass_g,
             color = sex)) +
  geom_point() +
  facet_wrap(~species) +
  labs(x = 'Bill depth (mm)',
       y = 'Body mass (g)',
       color = 'Sex') +
  theme_bw() + #sometimes the order of theme_bw() matters, may overwrite something
  theme(axis.title = element_text(face = 'bold', size = 14),
        strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 14)) +
  scale_color_viridis_d(end = 0.8)



# histogram plot
penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = body_mass_g,
             fill = species,
             color = species)) +
  geom_histogram(alpha = 0.5)

#histogram change line/border color
penguins %>% 
  filter(!is.na(flipper_length_mm)) %>% 
  filter(!is.na(body_mass_g)) %>% 
  ggplot(aes(x = body_mass_g,
             fill = species)) +
  geom_histogram(alpha = 0.3, color = 'grey42')




# install packages for use today
install.packages("gapminder")
install.packages("ggimage")
install.packages("gganimate")
install.packages("patchwork")
# load packages for use today
library(gapminder)
library(ggimage)
library(gganimate)
library(patchwork)

# take a look at gapminder data, make a graph, and save graph to local computer
View(gapminder)
dat <- gapminder

#shows what the uniqe years are (get better idea of data)

gapminder$year %>%  unique()
p1 <- dat %>% 
  ggplot(aes(x = year,
         y = lifeExp,
         color = continent)) +
  geom_point(aes(size = pop)) +
  facet_wrap(~ continent) 
  
p1
str(p1)
ggsave('my_graph.png', plot = p1)

# the following is due to patchwork
p2 <- p1 + theme_bw()
p3 <- p2 + theme_dark()

p1 + p2 # side by side
p1 / p3 # up and down
(p1 + p2) / p3 + plot_annotation('Main title') +
  plot_layout(guides = 'collect')

c1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + ggtitle('Plot 1')
c2 <- ggplot(mtcars, aes(disp, mpg)) + geom_point() + ggtitle('Plot 2')
c3 <- ggplot(mtcars, aes(cyl, mpg)) + geom_point() + ggtitle('Plot 3')


(c1 + c2) / c3 +
  plot_annotation(
    title = 'Main title',
    tag_levels = 'A'
  )

#gganimate
p3 + transition_time(time = year)

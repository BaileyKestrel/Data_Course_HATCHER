library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(easystats)
library(janitor)


## does body weight vary significantly between penguin species
dat_peng <- penguins
names(dat_peng)

# this only shows two species (incorrectly sets Adelie as intercept in summary(mod))
mod <- glm(data = dat_peng, 
    formula = body_mass_g ~ species)
summary(mod)

unique(dat_peng$species)

# reset intercept now body weight of Gentoo penguin
dat_peng$species <- relevel(dat_peng$species, ref = 'Gentoo')

mod <- glm(data = dat_peng,
           formula = body_mass_g ~ species)

summary(mod)


#set species as factor and give them levels
dat_peng$species <- factor(dat_peng$species, levels = c('Gentoo', 'Chinstrap', 'Adelie'))

mod <- glm(data = dat_peng,
           formula = body_mass_g ~ species)

summary(mod)


# true or false... boolean
names(dat_peng)
View(dat_peng)

dat_peng %>% 
  mutate(gentoo = case_when(species == 'Gentoo' ~ TRUE,
                            TRUE ~ FALSE)) %>% View()

dat_p <- dat_peng %>% 
  mutate(gentoo = case_when(species == 'Gentoo' ~ TRUE,
                            TRUE ~ FALSE))

glm(data = dat_p,
    formula = gentoo ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
    family = 'binomial')

# (Intercept) -1.477e+02 means baseline before everything...



mod <- glm(data = dat_p,
    formula = gentoo ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
    family = 'binomial')

predict(mod, dat_p)
#converts prediction into a probability (type = 'response')
predict(mod, dat_p, type = 'response')

#store prediction probability into pred column
dat_p$pred <- predict(mod, dat_p, type = 'response')
View(dat_p)

# plot to see if model accurate
dat_p %>% 
  ggplot(aes(x = body_mass_g, y = pred, color = species))+
  geom_point()

pred <- dat_p %>% 
  mutate(outcome = case_when(pred < 0.01 ~ 'Not gentoo',
                             pred > 0.75 ~ 'Gentoo')) %>% 
  select(species, outcome) %>% 
  mutate(accurate = case_when(species == 'Gentoo' & outcome == 'Gentoo' ~ TRUE,
         species != 'Gentoo' & outcome == 'Not gentoo' ~ TRUE,
         TRUE ~ FALSE))

# how accurate our model is  0.994186 (99.42% accurate)
pred %>% 
  pluck('accurate') %>% 
  sum()/nrow(pred)







## Data/GradSchool_Admission.csv
## build a logical regression model and predict the admission of grad school
dat <- read_csv("Data/GradSchool_Admissions.csv")
names(dat)

# need to convert to logical (true false) using as.logical.. use binomial to fit this
mod <- glm(data = dat,
           formula = as.logical(admit) ~ gre + gpa + rank,
           family = 'binomial')

# : interaction
# + additive
# * both??


summary(mod)


mod3 <- glm(data = dat,
            formula = as.logical(admit) ~ (gre + gpa) * rank,
            family = 'binomial')

#main effect: gre, gpa, rank
#interaction: gre:rank, gpa:rank
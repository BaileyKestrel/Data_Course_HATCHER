library(readxl)
library(tidyverse)
library(ggplot2)

dat <- read_xlsx('Data/messy_bp.xlsx', skip = 3)


dat$Race %>% unique()

bp <- dat %>% 
  select(-starts_with('HR'))

bp <- bp %>% 
  pivot_longer(starts_with('BP'),
               names_to = 'visit',
               values_to = 'bp') %>% 
  mutate(visit = case_when(visit == 'BP...8' ~1,
                           visit == 'BP...10' ~2,
                           visit == 'BP...12' ~3)) %>% 
  separate(bp, into = c('systolic', 'diatolic'))



hr <- dat %>% 
  select(-starts_with('BP'))

hr <- hr %>% 
  pivot_longer(starts_with('HR'),
               names_to = 'visit',
               values_to = 'hr') %>% 
  mutate(visit = case_when(visit == 'HR...9' ~1,
                           visit == 'HR...11' ~2,
                           visit == 'HR...13' ~3))

dat_join <- full_join(hr, bp)


library(janitor)

clean_names()
make_clean_names()

make_clean_names('# of bacteria')
make_clean_names('% of growth')
make_clean_names(c('# of bacteria', '% of growth'))

dat_join %>% clean_names()









for (i in 2:nrow(dat)) {
  if(dat$pat_id[i] == dat$pat_id[i - 1]) {
    dat$pat_id[i] <- dat$pat_id[i] + 1
  }
}


duplicated()
id <- c(1, 2, 3, 4, 4, 5)
duplicated(id)

dat %>% 
  mutate(id_fix = pat_id + cumsum(duplicated(pat_id))) %>% 
  View()

dat %>% 
  arrange('Year birth') %>% 
  View()

dat %>% 
  clean_names() %>% 
  arrange(year_birth) %>% 
  View()



dat_join$Race %>% unique()

dat_join %>% 
  mutate(Race_new = case_when(Race == 'Caucasian' ~ 'White',
                              Race == 'WHITE' ~ 'White',
                              TRUE ~ Race)) %>%  View()

dat_join %>% 
  mutate(Race_new_2 = case_when(Race == 'Caucasian' | Race == 'WHITE' ~ 'White',
                              TRUE ~ Race)) %>%  View()

dat_join %>% 
  mutate(Race_new_2 = case_when(Race == 'Asian' | Sex == 'Female' ~ 'Asian_female',
                                TRUE ~ Race)) %>%  View()





#option 1
df <- dat_join %>% 
  clean_names() %>% 
  mutate(race = case_when(race == 'Caucasian' ~ 'White',
                              race == 'WHITE' ~ 'White',
                              TRUE ~ race)) %>% 
  mutate(systolic = as.numeric(systolic),
         diatolic = as.numeric(diatolic)) %>% 
  mutate(birthday = paste(year_birth, month_of_birth, day_birth, sep = '-')) %>% 
  select(-year_birth, -month_of_birth, -day_birth)

#option 2
df$systolic <- as.numeric(df$systolic)

##make a graph to show blood pressure changes throughout visits
df2 <- dat_join %>% 
  clean_names() %>% 
  mutate(race = case_when(race == 'Caucasian' ~ 'White',
                          race == 'WHITE' ~ 'White',
                          TRUE ~ race)) %>% 
  mutate(systolic = as.numeric(systolic),
         diatolic = as.numeric(diatolic)) %>% 
  mutate(birthday = paste(year_birth, month_of_birth, day_birth, sep = '-')) %>% 
  select(-year_birth, -month_of_birth, -day_birth)


df3 <- df2 %>% 
  pivot_longer(cols = c('systolic', 'diatolic'),
               names_to = 'bp_type', values_to = 'bp')

df3 %>% ggplot(aes(x = visit, y = bp, color = bp_type))+
  geom_path()+
  facet_wrap(~bp_type)+
  facet_grid(hispanic ~ race)

















dat <- read.csv("Data/Bird_Measurements.csv")
View(dat)

install.packages('skimr')
library(skimr)

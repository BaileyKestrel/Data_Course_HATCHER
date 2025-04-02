library(tidyverse)
library(ggplot2)

# make table4a and table4b tidy (like table1)
table1

table4a
table4b

x <- table4a %>% 
  pivot_longer(cols = -country,
               names_to = 'year',
               values_to = 'cases')
y <- table4b %>% 
  pivot_longer(cols = -country,
               names_to = 'year',
               values_to = 'population')

table4_tidy <- full_join(x, y) #joins two tables together


#make table5 tidy
table5
table5_tidy <- table5 %>% 
  separate(rate, c('cases', 'population'), convert = T) %>% #convert = T converts to integer
  mutate(year = paste0(table5$century, table5$year)) %>% 
  select(-century)
table5_tidy  
  


#entering data to excel (or Google Sheets)
#path: ../Exercises/Data_Entry_Case_Study.txt

text <- read.delim('Exercises/Data_Entry_Case_Study.txt')

##
library(readxl)

dat <- read_xlsx('Data/messy_bp.xlsx', skip = 3)  

dat$Race %>% unique()

  


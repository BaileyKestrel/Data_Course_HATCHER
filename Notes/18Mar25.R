dat <- read.csv('Data/Bird_Measurements.csv')
View(dat)
dim(dat)

library(tidyverse)
library(janitor)
library(skimr)
skim(dat)


## clean bird measurement data
## keep: Family Species_number, Species_name, English_name
## clutch size, egg mass, mating system

keep = c("Family", "Species_number", "Species_name", 
         "English_name", "Clutch_size", "Egg_mass", "Mating_System")

names(dat)

male <- dat %>% 
  select(keep, starts_with('M_'), -ends_with('_N')) %>% 
  mutate(sex = 'male')

names(male) <- names(male) %>% str_remove('M_')


female <- dat %>% 
  select(keep, starts_with('F_'), -ends_with('_N')) %>% 
  mutate(sex = 'female')

names(female) <- names(female) %>% str_remove('F_')


unsexed <- dat %>% 
  select(keep, starts_with('Unsexed_'), -ends_with('_N')) %>% 
  mutate(sex = 'unsexed')

# need to rename twice b/c there are uppercase and lowercase
names(unsexed) <- names(unsexed) %>% str_remove('unsexed_')
names(unsexed) <- names(unsexed) %>% str_remove('Unsexed_')



join_dat <- full_join(male, female)
join_dat_2 <- full_join(join_dat, unsexed)

clean_dat <- 
  male %>% 
  full_join(female) %>% 
  full_join(unsexed)


# how to compare to see if identical
identical(names(male), names(female))
identical(letters[1:3], c('a', 'b', 'c'))







library(readxl)
dat <- read_csv('Data/Bird_Measurements.csv') 

file = 'Data/Bird_Measurements.csv'
dat <- read_csv(file)

path = '../../../Downloads/Worst Data Storage Ever.xlsx'
dat <- read_xlsx(path)


dat <- read_xlsx(path, sheet = 2)
dat <- read_xlsx(path, sheet = 2, range = 'A1:G10')





## function

everything()
mean()
sd()
read.csv(argument1, argument2, ...)

# define the function "weather"
weather <- function(){
  print('it is cold')
}
# call the function "weather"
weather()



add_numbers <- function(a, b){
  result <- a + b
  return(result)
}
add_numbers(2, 5)



clean_bird_data <- function(dat) %>% 
  #put in the R script from above so that I have a function that cleans the data...

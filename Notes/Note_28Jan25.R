## load 'mtcars' dataset
data()
mtcars

## 1. What type of object is this?
str(mtcars)
class(mtcars)
View(mtcars)
dim(mtcars) #14x11


## 2. Find cars with an mpg greater than 20 and 4 cyl,
## then save them to a new object
data(mtcars) # reset everything (restore mtcars)
my_mtcars <- mtcars
# option 1:
my_mtcars <- my_mtcars[my_mtcars$mpg > 20, ]
my_mtcars <- my_mtcars[my_mtcars$cyl == 4, ]
# option 2:
my_mtcars <- my_mtcars[my_mtcars$mpg > 20 & my_mtcars$cyl == 4, ] #[row, column] if a section is blank, we want everything from it
View(my_mtcars)


## 3. convert mpg to a character data type.
my_mtcars$mpg <- as.character(my_mtcars$mpg)
str(my_mtcars)


my_mtcars$new_col <- as.numeric(my_mtcars$mpg) # creates new column and stores it in data set my_mtcars
str(my_mtcars)
my_mtcars$new_col <- my_mtcars$gear * my_mtcars$cyl


## 4. convert the entire data frame to character data type. 
str(my_mtcars)
names(my_mtcars)

dat1 <- as.character(my_mtcars) # incorrect, replaces everything with character
class(dat1)

# option 1: write everything out
my_mtcars$mpg <- as.character(my_mtcars$mpg)
my_mtcars$cyl <- as.character(my_mtcars$cyl)  
my_mtcars$disp <- as.character(my_mtcars$disp)
my_mtcars$hp <- as.character(my_mtcars$hp)
my_mtcars$drat <- as.character(my_mtcars$drat)
my_mtcars$wt <- as.character(my_mtcars$wt)
my_mtcars$qsec <- as.character(my_mtcars$qsec)
my_mtcars$vs <- as.character(my_mtcars$vs)
my_mtcars$am <- as.character(my_mtcars$am)
my_mtcars$gear <- as.character(my_mtcars$gear)
my_mtcars$carb <- as.character(my_mtcars$carb)
my_mtcars$new_col <- as.character(my_mtcars$new_col)

# option 2: make a for loop
for (col in names(my_mtcars)) {
  my_mtcars[, col] <- as.character(my_mtcars[, col])
}
str(my_mtcars)
  
# option 3: use the apply function
?apply # apply(X, MARGIN, FUN, ..., simplify = TRUE)
apply(my_mtcars, 2, as.character) # 1 indicates rows, 2 indicates columns (2: apply to all columns)
str(my_mtcars)

new_dat <- apply(mtcars, 2, as.character)
class(new_dat)

# only use apply to change to character on rows 1 - 3:
new_input <- mtcars[1:3, ]
new_dat_new_input <- apply(new_input, 2, as.character)

# how to convert it back to data frame:
class(new_dat_new_input)
new_dat <- as.data.frame(new_dat_new_input)
class(new_dat)



# how to save file into csv
read.csv()
write.csv(new_dat, 'class_practice_28Jan25.csv', row.names = FALSE)



# how to install package in R
install.packages("tidyverse")

library(tidyverse)
# %>% # shift+Command+M

mtcars$mpg %>% 
  mean()











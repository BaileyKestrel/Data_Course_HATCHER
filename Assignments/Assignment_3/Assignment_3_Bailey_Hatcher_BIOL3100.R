# YOUR REMAINING HOMEWORK ASSIGNMENT (Fill in with code) ####

# I put multiple options for some of the questions to practice different methods


library(tidyverse) # load in the tidyverse library
library(dplyr) # load in the dplyr library


# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows
# seq(2,150,2) # here's the code to get a list of the even numbers between 2 and 150

data(iris) # load the iris dataset
dim(iris) # find dimensions of iris
View(iris)

## option 1:
# iris[seq(2, 150, 2), ] #filters dat by even rows only and all columns (columns blank = all)
even_dat <- iris[seq(2, 150, 2), ] # store the filtered iris dataset into even_dat

## option 2:
# even_dat_tidy_modulo <- iris %>% 
#   filter(row_number() %% 2 == 0) # keeps only the even rows using modulo bc filter expects logical

## option 3:
# even_dat_tidy_slice <- iris %>% 
#   slice(seq(2, nrow(iris), 2)) # subset rows using their positions

## both option 2 and option 3 grab the correct rows but re-number the rows starting from 1


# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

## option 1: convert each column to character
iris_chr <- iris
iris_chr$Sepal.Length <- as.character(iris_chr$Sepal.Length)
iris_chr$Sepal.Width <- as.character(iris_chr$Sepal.Width)
iris_chr$Petal.Length <- as.character(iris_chr$Petal.Length)
iris_chr$Petal.Width <- as.character(iris_chr$Petal.Width)
iris_chr$Species <- as.character(iris_chr$Species)
str(iris_chr)

## option 2: create a for loop that iterates over each column and converts them to character
iris_chr <- iris
for (col in colnames(iris_chr)) {
  iris_chr[[col]] <- as.character(iris_chr[[col]])
}
str(iris_chr)
# for (col in colnames(iris_chr)) # for loop that iterates over each column name in iris_chr
# colnames(iris_chr) # gets a vector of column names from iris_chr
# iris_chr[[col]] # allows us to select the name of the column that the loop is iterating over
# as.character(iris_chr[[col]]) # converts the column of current iteration to character

## option 3: use mutate_all 
iris_chr <- iris %>% 
  mutate_all(as.character) # mutate_all affects every variable (column)
str(iris_chr)


# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width

Sepal.Area <- as.numeric(iris$Sepal.Length * iris$Sepal.Width)
str(Sepal.Area)


# 4.  Add Sepal.Area to the iris data frame as a new column

## option 1:
iris$Sepal.Area <- as.numeric(iris$Sepal.Length * iris$Sepal.Width)

## option 2: using tidyverse and mutate
iris <- iris %>% 
  mutate(Sepal.Area = as.numeric(iris$Sepal.Length * iris$Sepal.Width))


# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
# (name it big_area_iris)

## option 1: using tidyverse
big_area_iris <- iris %>% 
  filter(iris$Sepal.Area > 20)

## option 2:
big_area_iris <- iris[iris$Sepal.Area > 20, ] # this method preserves the row number




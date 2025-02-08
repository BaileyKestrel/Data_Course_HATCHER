###########################
#                         #
#    Assignment Week 3    #
#                         # 
###########################

# Instructions ####
# Fill in this script with stuff that we do in class.
# It might be a good idea to include comments/notes as well so you remember things we talk about
# At the end of this script are some comments with blank space after them
# They are plain-text instructions for what you need to accomplish.
# Your task is to write the code that accomplished those tasks.

# Then, make sure to upload this to both Canvas and your GitHub repository




# Vector operations! ####

# Vectors are 1-dimensional series of values in some order
1:10 # ':' only works for integers
letters # built-in pre-made vector of a - z



vector1 <- c(1,2,3,4,5,6,7,8,9,10)
vector2 <- c(5,6,7,8,4,3,2,1,3,10)
vector3 <- letters # letters and LETTERS are built-in vectors

vector1 + 5
vector2 / 2
vector1*vector2

vector3 + 1 # can't add 1 to "a"


# Logical expressions (pay attention to these...they are used ALL THE TIME)
vector1 > 3
vector1 >= 3
vector1 < 5
vector1 <= 5
vector1 == 7
letters == "a"
letters != "c"
letters %in% c("a","b","c","z") # checks if the given characters are in the list letters
vector1 %in% 1:6 # checks if the values 1 - 6 are in vector1 and returns boolean value


# Data Frames ####
# R has quite a few built-in data sets
data("iris") # load it like this

# For built-in data, there's often a 'help file'
?iris

# "Iris" is a 'data frame.' 
# Data frames are 2-dimensional (think Excel spreadsheet)
# Rows and columns
# Each row or column is a vector


dat <- iris # can rename the object to be easier to type if you want
# doing this also prevents you from messing up the original data

# ways to get a peek at our data set
names(dat) # gives the column header names of the data frame
dim(dat) # gives the dimensions of the data frame (150 rows x 5 columns)
head(dat) # gives the first 6 rows of the data (little peek)


# You can access specific columns of a "data frame" by name using '$'
dat$Species # shows you column values for Species
dat$Sepal.Length # shows you values for column Sepal.Length

# You can also use square brackets to get specific 1-D or 2-D subsets of a data frame (rows and/or columns)
dat[1,1] # [Rows, Columns]
dat[1:3,5] # selecting the first three rows of column five

vector2[1] # selects first element in vector2
letters[1:7] # selects first seven letters
letters[c(1,3,5,7)] # selects specific letters


# Plotting ####

# Can make a quick plot....just give vectors for x and y axes
plot(x=dat$Petal.Length, y=dat$Sepal.Length) # scatter plot
plot(x=dat$Species, y=dat$Sepal.Length) # box plot


# Object "Classes" ####

#check the classes of these vectors
class(dat$Petal.Length) # numeric
class(dat$Species) # factor (represents categorical data)

# plot() function behaves differently depending on classes of objects given to it!
# this explains the different plot types above

# Check all classes (for each column in dat)
str(dat)

# "Classes" of vectors can be changed if needed (you'll need to, for sure, at some point!)

# Let's try
nums <- c(1,1,2,2,2,2,3,3,3,4,4,4,4,4,4,4,5,6,7,8,9)
class(nums) # make sure it's numeric

# convert to a factor
as.factor(nums) # show in console but does not change nums vector yet... need to store to object
nums_factor <- as.factor(nums) #assign it to a new object as a factor
class(nums_factor) # check it... successfully changed to factor 


# convert numeric to character
as.character(vector1) # show in console as character
as.character(vector1) + 5 # cannot add characters and numeric

# convert character to numeric
as.numeric(vector3) # this does not work: inputs NA instead of numeric value.
as.numeric(c('5', '7', '8', '11')) # this does work though... strings to numbers


#check it out
plot(nums) # scatter plot of numeric data
plot(nums_factor) # histogram of factor data (has categories 1 - 9)
# take note of how numeric vectors and factors behave differently in plot()




# Simple numeric functions
# R is a language built for data analysis and statistics so there is a lot of functionality built-in
vector1
max(vector1) # returns max value in vector
min(vector1) # returns min value in vector
median(vector1) # returns the median value in vector (splits in half and finds center)
mean(vector1) # returns the mean/average of the values in the vector
range(vector1) # returns min and max value in numeric vector
summary(vector1) # returns min, median, mean, max, 1st. and 3rd quartiles

# cumulative functions
vector1
cumsum(vector1) # returns the cumulative sum from left to right of the numeric data set
cumprod(vector1) # returns the cumulative product from left to right
cummin(vector1) # returns the cumulative minimum value from left to right
cummin(vector2) # shows the results better 
cummax(vector1) # returns the cumulative maximum value from left to right
cummin(vector2) # shows the results better

# even has built-in statistical distributions (we will see more of these later)
dbinom(50,100,.5) # probability of getting exactly 50 heads out of 100 coin flips








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


# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
      # to canvas
      # I should be able to run your R script and get all the right objects generated


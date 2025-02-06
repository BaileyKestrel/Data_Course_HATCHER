##/Users/baileyhatcher/Desktop/BIOL3100/Data_Course_HATCHER/Data_Course_HATCHER.Rproj


####Jan 16th####
## directory = folder
getwd()
?list.files()

list.files(path = 'Assignments/')
list.files(path = 'Assignments/', recursive = T)
# recursive = F (default): only search current directory
# recursive = T search current and all subdirectories

list.files(path = 'Data/', recursive = T)
list.files(path = 'Data/', recursive = F)

'this is how you type a string in R'

# returns the files that have the given pattern (end with .txt in this case)
list.files(path = 'Data/', pattern = '.txt')
# returns the files that have the given pattern (end with .txt in this case) in Data and subdirectories
list.files(path = 'Data/', pattern = '.txt', recursive = T)


## how many total files under 'Data' directory, including subdirectories
length(list.files(path = 'Data/', recursive = T)) #my attempt
list.files(path = 'Data/', recursive = T) #professor attempt
obj <- list.files(path = 'Data/', recursive = T) 
length(obj)

no_of_files = length(obj_1) # store length of obj_1 as another object


list.files(path = 'Assignments/') #print out result on screen
obj_1 <-  list.files(path = 'Assignments/') # save results in an object
length(obj_1)


echo = 'some' # define object called echo containing string

some = 1 #need to define the object first
print(some)

print('some')

echo 'some' > README.md # save into README.md

print() # () means a function
list.files() #this is a function


# echo in bash = print() in R = print in python2 = print() in python3
print('something')
print(obj_1)
print(length(obj))



list.files(path = 'Data/', recursive = T)

list.files(path = 'Data/', recursive = T)
?list.files

list.files(path = 'Data/', pattern = '.csv')
list.files(path = 'Data/', pattern = 'grade') #need to have exact match

## file name start with something
list.files(path = 'Data/', pattern = '^b', recursive = T) # ^ means beginning with b (case sensitive) had to go in further directories using recursive = T
list.files(path = 'Data/', pattern = '^B') # ^ means beginning with 

## to learn more, search: regex  (means regular expression)

# filenames begin with lower base b = ^b

# end with
list.files(path = 'Data/', pattern = 'b$', recursive = T) # $ means ending with
list.files(path = 'Data/', pattern = 'b?', recursive = T)





# readLines reads file line by line
readLines('Data/cleaned_bird_data.csv')
line <- readLines('Data/cleaned_bird_data.csv') # store lines of file into object called line
length(line) # how many lines file contains


readLines('Data/wide_income_rent.csv')
line <- readLines('Data/wide_income_rent.csv')
length(line)


# shows data in a different way; makes it look more like a chart (read.csv)
read.csv('Data/wide_income_rent.csv')

## read a file and savve into an object
df_rent_by_state = read.csv('Data/wide_income_rent.csv')
dim(df_rent_by_state) # 2 53 [number of rows, number of columns]

# how to tell R this is not a header
df_rent_by_state = read.csv('Data/wide_income_rent.csv', header = F) # tell R there is no header in my input data
dim(df_rent_by_state) # 3 53

df_rent_by_state = read.csv('Data/wide_income_rent.csv', row.names = 1)
dim(df_rent_by_state)
















####Jan 21st####
dat <- iris
dim(dat) #150x5

head(dat)
tail(dat)
dat[3, 4]

new_dat <- dat[1:3, 1:4]
View(dat)

#object

#vector: one dimension with same type of data (numeric, character, logical). e.g. cannot mix number and character together.
# c()

# numeric vector
vec_num <- c(1:3, 2:4)
vec_num
vec_num2 <-  c(1, 2, 3, 2, 3, 4)
vec_num[2] # 2nd element in the vec_num vector

vec_num2 + 1 #can do math with numeric vector
vec3 <-  vec_num2^2

as.character(vec_num2) # can convert to character
vec_4 <-  as.character(vec_num2)
vec3 + as.numeric(vec_4)
as.factor(vec_num2) #convert back using as.numeric()



# character
vec <- 'apple', 'banana', 'kiwi'
vec_chr <- c('apple', 'banana', 'kiwi')
vec_num[2]

# logical (TRUE/FALSE)
is.numeric(vec_num)
vec_mix <- c(1, 'apple', TRUE) #if you add more than one data type into a vector, it turns it all into a string

thre <- c(TRUE, TRUE, FALSE)
is.vector(thre)

# types of objects in R
# 1. vector (one dimension, same type)
# 2. matrix (2 dimension, same type of data)
# 3. array (multiple dimensions, same type)
# 4. data frame (2 dimension, different type of data, same length)
# 5. list (multiple dimensions, different type, different length)
# 6. function (store a function - shortcut key)

# vector
vec <- c(1, 2, 3, 4)
vec_2 <- c(2, 3, 4)
vec_3 <- c(5, 6, 7, 8)
ar <- as.array(c(vec, c(5, 6, 7, 8)))
list <- list(1, 2, 3, 4)
array(1:3, c(2,4))

dat <-  as.data.frame(vec, vec_2)
dat <-  as.data.frame(vec_2, vec_3)

list <- list(vec, vec_2)


mat <- matrix(1:6, nrow = 3)
mat[2, 1]

sum(iris$Sepal.Length)



readLines(file1, n = 1)
readLines(file2, n = 1)
readLines(file3, n = 1)
## Loop ##

# while loop (while the assumption is true)



# for loop

for(variable in vector) { ## i.e. go through each variable/element in the vector}

for (index in c(1,2,3,4,5,6,7,8,9,10)) {
  print(index)
}
print(index[1])
print(index[2])
print(.......
      
vec <- c(1,2,3,4,5,6,7,8,9,10)
for(index in vec) {
  new_score <- index + 1
  print(new_score)
}

vec <- c('apple', 'banana', 'kiwi')
for (fruit in vec){
  print(fruit)
}

for (fruit in vec) {
  out = paste('I like')
  print(out)
}

# write a for loop to print out 1,2,3,4,5 and multiply by pi 
for (i in c(1,2,3,4,5)){
  new_value <- i * pi
  print(new_value)
}
  
  

for (i in vec){
  for (j in vec_2){
    out=paste('I like', j)
    print(out)
    print(i)
  }
}





####Jan 23####
# 8. 
getwd()

list.files(pattern = '^b', recursive = T)

# 9.
readLines('Data/data-shell/creatures/basilisk.dat', n = 1)
readLines('Data/data-shell/data/pdb/benzaldehyde.pdb', n = 1)
readLines('Data/Messy_Take2/b_df.csv', n =1)

vec <- c(1, 2, 3)

bfile <- list.files('Data/', pattern = '^b', recursive = T)

## option 1
for(file in bfile){
  setwd('~/Desktop/BIOL3100/Data_Course_HATCHER/Data')
  first_line <- readLines(file, n = 1)
  print(first_line)
}
  
for(file in bfile){
  filepath <- paste0('Data/', file)
  #print(filepath)
  first_line <- readLines(filepath, n = 1)
  print(first_line)
  
}
bfile <- list.files()
  

## 8. (assignment 2) Find any files (recursively) in the Data/ directory that begin with the letter “b” (lowercase)
getwd()
list.files(pattern = '^b', recursive = T)



#review
arr <- array(1:18, dim = c(3,3,2)) # row, column, layer
print(arr)


vec <- c(1,2,3)
is.vector(vec)
str(vec)

#numeric
vec1 <- vec*vec
#logical
# character
chr <- as.character(vec)
str(chr)


chr[1] #gives us the 1st item in the ...
chr[3]


dat <- read.csv('Data/1620_scores.csv')
dim(dat) #89 25
dat[3, 4] #[row, col]
dat[1:3, 1:4]
dat[,3]




## build a data frame for 'mtcars' dataset'
mtcars
data(mtcars)

dat <- mtcars
dim(dat) # 32 11
str(dat)

# get cars with cyl greater than 4
dat$cyl > 4 # returns true or false as logical statement
cyl_greater_4 <- dat[dat$cyl > 4,] #grab every row with cyl greater than 4


## pull out mpg data and calculate average, min, and max mpg
#option 1... $ only takes one column
str(dat)
car_mpg <- dat$mpg # $ collects column by name
mean(car_mpg)
summary(dat$mpg)
max()
min()

# option 2... [] brackets can grab multiple columns
dat[, c("mpg", "cyl")]
dat[, c(1:2)]
dat[2:5, c("mpg", "cyl")] #can also pull out row 2 - 5 and columns mpg and cyl




## convert 'mpg' to character in mtcars data frame
str(dat)
as.character(dat$mpg)
chr_mpg <- as.character(dat$mpg)#need to save it in an object
str(chr_mpg)
# or
dat$mpg <- chr_mpg
str(dat)

dat$mpg_num <- as.numeric(chr_dat)
View(dat)
str(dat)
dat$mpg <- as.character(dat$mpg)
str(dat$mpg)


## convert entire data frame to character
str(dat)
chr_dat <- as.character(dat)
str(chr_dat)

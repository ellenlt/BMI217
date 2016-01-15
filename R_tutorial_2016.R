##	BMI 217
##
##  Diego Calderon (dcal@stanford.edu)
## adapted from Sandra Andorf (andorf@stanford.edu)
##
################################################################################


############################### R calculator ###################################

248 + 21 - 1 - 8 * 4 / 2

# this will not be evaluated because it is a comment
x <- 2; y <- 4
x + y

x # prints element
print(x) # also prints
ls() # shows assigned elements

rm(x) # removes variables
ls()

# to look up documentation on a function
?ls()
?`+` # use ` for special functions

############################### Vector #########################################

# vector is the most basic data structure
# typically has numeric, character, or logical data type.

# different way to make vectors
x <- 1:3 # :
y <- seq(1, 3, by = 1) # seq is a sequence
rep(1:2, 10) # rep repeats
# c(...) is a handy function for building vectors from other vectors
z <- c(x, y)
# but nested vectors flatten
c(c(x, x), x)

# look up class of a variable numeric
class(z)
is.numeric(z) # TRUE

# or the type
typeof(z)

# but can only be one type
w <- c('hi', 'hello')
c(z, w)

# many operations operate element-wise on vectors and return vectors
z + 2
# comparisons give us logic vector back
z > 2
class(z > 2)
is.logical(z > 2)

# referencing elements of a vector using []
z[1] # NOTE: R starts indexing at 1
# you can specify a range using the ':' operator
z[1:4]
# or index by logical vector
z[z>2]
# modify element by accessing and using assignment
z
z[1] <- 20
z[1] # now 20
z
z[1] <- NA # special keyword for missing element
is.na(z[1]) # TRUE
z

# access multiple elements
z[c(1,3)]
z[c(T,T,F,F,F,T)]

# provide symbolic names of elements in vector z
names(z) <- c('first', 'second', 'third',
              'fourth', 'fifth', 'sixth')
z['first'] # still NA
# NA annoying so let's remove
z[!is.na(z)]
# or change by setting missing value to 0
z[is.na(z)] <- 0

# check length of vector
length(z) # should be 6
# sort vector
sort(z) # puts 20 at end
z[order(z)] # alternative. order(z) gives you indices of where the 1st, 2nd, etc element should be


############################### Factor #########################################

# factors are categorical values

# variable 'gender' with 20 "male" entries and 30 "female" entries
gender <- c(rep("male",20), rep("female", 30))
class(gender)
gender <- factor(gender)
# stores gender as 20 1s and 30 2s and associates
# 1=female, 2=male internally (alphabetically)
typeof(gender) # Internal representation is binary sort of thing
gender
levels(gender)
summary(gender)
class(gender)

# NOTE: be careful with factors, they are tricky
# some data structures automatically turn character vectors into factors
# when it might be more intuitive to use a character vector

# convert factor to character vector
gender <- as.character(gender)
class(gender)
typeof(gender)

############################### Matrix #########################################

# like vector but indexed by two elements

m1 <- matrix(1, nrow=3, ncol=3) # create 3 by 3 zero matrix
m2 <- matrix(2, nrow=3, ncol=3) # create 3 by 3 zero matrix
m1
m2
m <- rbind(m1, m2)
m
m <- cbind(m, m)
m

# check dimensions of matrix, now 6 by 6
nrow(m)
ncol(m)
dim(m)

# get transpose of m
t(m)

# indexing elements of matrix by number
m1[1,] # select first row
m1[, 1] # select first column
m1[1, ] <- 3 # change all first row to ones
m1

# indexing by logical vector
m1[m1[,1] > 2, ]

# indexing elements of matrix by symbolic name
rownames(m1) <- c('first', 'second', 'third')
colnames(m1) <- c('first', 'second', 'third')
m1
m1['first', ]
m1['first', 'third']

############################### List ###########################################

# lists are like hashes, or dictionaries in python

# making new lists, note heterogenous data and length
# notice name = data pair.
x <- list(a = c(1,2,3), b = c('d', 'e', 'f'), # provide name and data
          c = "foo", d = list(e = 1:4, f = 'bar')) # even nested list possible

# check names
names(x)

# index by number, note the double bracket
x[1] # Returns a list of one dimension
x[[1]] # Returns the actual vector of values
x[['a']] # or by symbolic name
x$a # or name using $

############################### Data frames ####################################
######## adapted from ?merge example

# data frames are a specialized list with all elements having the same length
# even if they are heterogeneous data types

# create author data frame
authors <- data.frame(surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
                      nationality = c("US", "Australia", "US", "UK", "Australia"),
                      deceased = c("yes", rep("no", 4)), stringsAsFactors = F)
authors
authors[1:3, 1:2] # index by number
authors$surname # or by name with $

# look at top elements
head(authors)
head(authors, n=2)

# summarize data
summary(authors) # More useful for numeric df's

# We can sort by rows in a data frame
authors[order(authors$surname),]

# or select those from the US
authors[authors$nationality == 'US',]

# or count authors of different nationalities
table(authors$nationality)

# create books data frame
books <- data.frame(name = c("Tukey", "Venables", "Tierney",
                             "Ripley", "Ripley", "McNeil", "R Core"),
                    title = c("Exploratory Data Analysis",
                              "Modern Applied Statistics ...",
                              "LISP-STAT",
                              "Spatial Statistics", "Stochastic Simulation",
                              "Interactive Data Analysis",
                              "An Introduction to R"), stringsAsFactors = F)
books

# let's say you wanted to look up which books had authors that were deceased

# merge the two
author_and_books <- merge(authors, books, by.x = "surname", by.y = "name")
author_and_books

# then look it up
author_and_books[author_and_books$deceased == 'yes', ]

################################## Loops #######################################

# similar to other languages

mat <- matrix(c(1,3,2,6,5,4), ncol = 2)
mat

# zero vector to hold calculated mean data
means <- rep(0, nrow(mat))

# for loop
for (i in 1:nrow(mat)) {
  # mean is a function that calculates mean of vector
  means[i] <- mean(mat[i,])
}
means

# apply, "1" means that the function will be applied to the rows
# "2" -> will be applied to columns
apply(mat, MARGIN = 1, FUN = mean)

# "2" means that the function will be applied to the colums
apply(mat, 2, sum)

# or use your own function
apply(mat, 1, function(x){ min(x)*2 })

# there are other applies lapply is for lists, sapply (user-friendly).

# sometimes already implemented vectorized versions: rowMeans, colMeans
rowMeans(mat)
colMeans(mat)

# other basic mathematical functions that are vectorized
x <- c(3,5, -2, 4)
median(x)
sum(x)
min(x)
max(x)
sd(x) #Standard Deviation
var(x) #Variance
sqrt(x) #square root
abs(x) #absolute value
sqrt(abs(x))
x^2

############################### Control statements #############################

# should look similar to other popular languages
5 > 2
if (5 > 2) print("5 is greater than 2")
i <- 5
if (i == 5) print("i is equal to 5")
i <- 6
if (i != 5) print("i is not equal to 5")
if (i > 10) {
  print("i is larger than 10")
} else{
  print("i is not larger than 10")
}

# several conditions
if (5 < 10 & 6 < 10) print("Yes")
if(5 < 10 & 6 > 10) print("Yes")
if(5 < 10 | 6 > 10) print("Yes, at least one condition is TRUE") # | means OR
TRUE & TRUE
TRUE & FALSE
TRUE | FALSE

############################### Defining functions #############################

# writing functions help keep your code modular and easy to follow

# writing a standard error function
stderr <- function(x) {
  sqrt(var(x)/length(x)) # returned value since last line
}
stderr(1:20) # run function on vector 1:20


################################ DATA IO #######################################

# check what the current working directory is
getwd()
# set a working directory
setwd()

# write a data frame to a csv text file, or matrix
authors
write.table(authors, file = "authors.csv", sep = ",",
            col.names = T, row.names = F, quote = F)
# many more options: ?write.table

# read in data from a file
# from previous sep = ','
read.table(file = "authors.csv", sep = ",", header = TRUE)

# write arbitrary R object to file
example_object <- list(a = list(a = 1),
                       b = list(c = c(2,3)),
                       c = 'hi')
example_object
save(example_object, file = 'example_object.Rda')
rm(example_object)

# load object again, good for saving analysis
# no need to assign to a name. It is an R object.
load(file = 'example_object.Rda')
example_object

############################### ggplot2 ########################################

# install and load a package
install.packages('ggplot2')
library(ggplot2) # load ggplot2

# sample 1000 data points from a log-normal distribution
data <- data.frame(log_normal = rlnorm(1000))

# pass in data frame and aes (aesthetic mapping) + geom_layer
ggplot(data, aes(x = log_normal)) +
  geom_density() # creates a simple density plot

# look up other geoms in ggplot2 documentation

############################### ggplot2 + reshape ##############################

install.packages('reshape')
library(reshape)

# add more data sampled from chisq distribution
data$chisq <- rchisq(1000, df = 1)

head(data)
summary(data)

# melt data so that the name is the variable and the value is the sample
# essentially stretches the two columns into one long column
melted_data <- melt(data)

head(melted_data)
head(melted_data[melted_data$variable == 'chisq',])

# note that in aes fill is set to variable this is why we see different colors
ggplot(melted_data, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.4) # alpha is the opacity

data(iris)
head(iris)

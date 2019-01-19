setwd("~/Desktop/r-novice-inflammation")
getwd()

# intro to files
read.csv(file = "data/inflammation-01.csv", header = FALSE)
read.csv(file = "data/inflammation-01.csv", header = TRUE)
# Rows = patients 1-60
# Columns = 40 days of observation

# intro to variables, types, etc.
weight_kg <- 55   # note: the current convention is to use "_" not "." in variable names
weight_kg
weight_lb <- weight_kg * 2.2
weight_lb
class(weight_lb)
this_string <- "this is a string"
this_string
class(this_string)

# variables, data, types
dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
dat <- read.csv("data/inflammation-01.csv", FALSE) # If you use the correct ordering, you can 

View(dat)
head(dat)
head(dat, n=10) # Interesting! I didn't know that I could add an n to this argument. :D
class(dat)
dim(dat) # gives us the dimensions. Always rows, columns. So for this example, it's 60 rows, 40 columns
dat[1,1] # This tells us what's in row 1, column 1. Note: Indexing starts at 1 in R. 
dat[c(1,2,5),7:9] # This returns the data from rows 1, 2, and 5, in columns 7-9.
dat[1,]
dat[,c(1,2)]
1:8 # this just returns 1-8
dat[1:8,] 

# functions for summarizing data
patient_10 <- dat[10,] # sets patient_10 as the 
min(patient_10) # returns the minimum value for patient_10
patient_10
max(patient_10) # returns the maximum value for patient_10
mean(patient_10) # This doesnt work because mean() expects a vector, not a data frame. 
# And when we set patient_10, we just subset a data frame so the class is still a dataframe.
class(patient_10) 
mean(as.numeric(patient_10))

?apply # This lets me know that apply applies a function to portions of a given array or matrix
# apply(x,MARGIN,FUN) x=data(must be matrix), MARGIN = for my purposes, this specifies whether it's applied to rows or columns (1=rows,2=columns); fun = what function to apply
apply(dat, 1, mean) # So this applies mean across every row in the data frame

# exercise: use apply to summarize dat with functions min, max, and sd
row_mins <- apply(dat, 1, min); row_mins
row_maxs <- apply(dat, 1, max)
row_sds <- apply(dat, 1, sd)
row_means <- apply(dat,1,mean)
col_mins <- apply(dat, 2, min)
col_maxs <- apply(dat, 2, max)
col_sds <- apply(dat, 2, sd)
col_means_exercise <- apply(dat[1:10,],2,mean)
# class(col_sds) They're both numeric
# class(row_sds) They're both numeric

summary(dat)
summary(dat[4:6, 1]) # This gives a summary for the data from column 1, rows 4-6.

# plotting
plot(row_maxs)
plot(row_maxs, type = "l")

# exercise: make a plot of mean value across patients 1-10 for all columns

col_means_exercise <- apply(dat[1:10,],2,mean)
plot(col_means_exercise, type = "l")
plot(apply(dat[1:10,], 2, mean), type = "l",
    ylab = "mean", xlab = "days",
    main = "Mean Inflammation Across Days")

## Writing Functions
# To define a function, we do {} brackets. everything bw those brackets is the function. 
# What's before{} is the data the function will be applied to. 
fahrenheit_to_kelvin <- function(temp_F) {
    temp_K <- (temp_F - 32) * (5/9) + 273.15
} 
# ^^ We're writing a function and tagging it with that name.
# The function fahrenheit_to_kelvin is a function of the data "temp_f"
# A function in R creates its own environment. 
# fahrenheit_to_kelvin(51) <-- this doesn't work because we didn't create a 'return' in R. 
# That is, temp_K existed in the sub-environment, but not in the global environment (on the right-hand panel)
# Below, I address this by adding return(temp_K). 
# Notably, temp_K still doesn't exist in the global environment. I could assign it, if I wanted.
# I would do this using: temp_K <- fahrenheit_to_kelvin(51), for example.

fahrenheit_to_kelvin <- function(temp_F) {
  temp_K <- (temp_F - 32) * (5/9) + 273.15
  return(temp_K)
} 
fahrenheit_to_kelvin(51)

divide_by_input <- function(input) {
  input / 5
}
divide_by_input(5)
# If you don't do assign something (eg: temp_K <- math), then the function just returns 
# the last thing that was run in the equation.
# Generally speaking, we should assign it something and return it.


# The next example will similarly take in a temp in F and convert it to C.
fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * (5/9)
  return(temp_C)
}

fahrenheit_to_celsius(51)
fahrenheit_to_celsius
fahrenheit_to_celsius(100)
fahrenheit_to_celsius(75)

# An example of using a function inside a function.
fahrenheit_to_celsius_v2 <- function(temp_F) {
  temp_C <- fahrenheit_to_kelvin(temp_F) - 273.15
  return(temp_C)
}

fahrenheit_to_celsius_v2(51)

# check outputs match
all.equal(fahrenheit_to_celsius(51), 
          fahrenheit_to_celsius_v2(50))

# EXERCISE: Write a function that computes the min and max for 
#       a numeric vector input, and then returns the sum of the two
#       HINT: sum() computes the sum of two values)

min_max_function <- function(numeric_vector_input) {
  temporary_data <- sum(min(numeric_vector_input)+max(numeric_vector_input))
    return(temporary_data)
}

sample_stuff <- as.numeric(c(1:10))
min_max_function(sample_stuff)
class(sample_stuff)
sample_stuff_2 <- 1:10
sample_stuff_2 <- as.numeric(sample_stuff_2)
class(sample_stuff_2)


# Alternative Solution
min_max_function_2 <- function(numeric_vector_input) {
  min_vector <- min(numeric_vector_input)
  max_vector <- max(numeric_vector_input)
  sum_of_both <- sum(min_vector, max_vector)
  return(sum_of_both)
}

## Functions with multiple arguments and default values

# The character vector 'to' tells it whether to convert to K or C. Usually in R, people do if/else.
# Start with if to == celsius... then if it's not celsius ("else"), we assume it's kelvin that's wanted.
# Also: We can put in comments inside our function!

## Version 1
fahrenheit_conversion <- function(temp_F, 
                                  to) {
  # What to do if asking for celsius conversion
  if (to == "celsius") {
    temp_C <- fahrenheit_to_celsius(temp_F)
    temp_out <- temp_C
   # What to do if otherwise
  } else {
    temp_K <- fahrenheit_to_kelvin(temp_F)
    temp_out <- temp_K
  }
  return(temp_out)
}

## Version 2
fahrenheit_conversion_v2 <- function(temp_F, 
                                  to) {
  # What to do if asking for celsius conversion
  if (to == "celsius") {
    temp_C <- fahrenheit_to_celsius(temp_F)
    return(temp_C)
    # What to do if otherwise
  } else {
    temp_K <- fahrenheit_to_kelvin(temp_F)
    return(temp_K)
  }
}


## Version 3
fahrenheit_conversion_v3 <- function(temp_F, 
                                     to) {
  # What to do if asking for celsius conversion
  if (to == "celsius") {
    temp_out <- fahrenheit_to_celsius(temp_F)
    # What to do if otherwise
  } else {
    temp_out <- fahrenheit_to_kelvin(temp_F)
  }
  return(temp_out)
}

## Testing
converted_temperature <- fahrenheit_conversion(temp_F = 51, to = "kelvin"); converted_temperature
converted_temperature <- fahrenheit_conversion(temp_F = 51, to = "fahrenheit"); converted_temperature
# Uhoh! If I set to = "" to anything other than celsius, it will return the temp in kelvin. 
# That's not always what we want.
# To get around this, we can do something other than if/else. And instead to if/"else if"

## Updated conversion model
fahrenheit_conversion_v4 <- function(temp_F, 
                                     to = "celsius") {    # this sets the default to celsius
  # check class of temp_F
  stopifnot(class(temp_F) == "numeric")
  stopifnot(class(to) == "character")
    
  # what to do if asking for celsius conversion
  if (to == "celsius") {
    temp_out <- fahrenheit_to_celsius(temp_F)
    return(temp_out)
  
  # what to do if kelvin
  } else if (to == "kelvin") {
    temp_out <- fahrenheit_to_kelvin(temp_F)
    return(temp_out)
    
  # what to do if otherwise
  } else {
    message("unexpected input for argument 'to'")
  }
}

converted_temperature_51kelvin <- fahrenheit_conversion_v4(temp_F = 51, to = "kelvin"); converted_temperature_51kelvin
converted_temperature_51celsius <- fahrenheit_conversion_v4(temp_F = 51, to = "celsius"); converted_temperature_51celsius
converted_temperature_51fahrenheit <- fahrenheit_conversion_v4(temp_F = 51, to = "what?"); converted_temperature_51fahrenheit
############## Why is mine giving me the NULL response? His did not...


## EXERCISE: In the case that our new function
#           converted_temperature() has a sensible input to argument 'to', create a plot of the 
#           converted temperature as output
input_temperature <- 50:90

fahrenheit_conversion_v4 <- function(temp_F, 
                                     to = "celsius") {    # this sets the default to celsius
  # check class of temp_F
  stopifnot(class(temp_F) == "numeric")
  stopifnot(class(to) == "character")
  
  # what to do if asking for celsius conversion
  if (to == "celsius") {
    temp_out <- fahrenheit_to_celsius(temp_F)
    return(temp_out)
    
    # what to do if kelvin
  } else if (to == "kelvin") {
    temp_out <- fahrenheit_to_kelvin(temp_F)
    return(temp_out)
    
    # what to do if otherwise
  } else {
    message("unexpected input for argument 'to'")
  }
}


fahrenheit_conversion_v4(input_temperature)

x <- 50
plot_input_data <- repeat{
  print(fahrenheit_conversion_v4(x))
        x = x+1
        if (x == 91){
          break
        }
}


## Trying again
input_temperature <- as.numeric(input_temperature)
class(input_temperature)
plot_data <- fahrenheit_conversion_v4(input_temperature)
plot(plot_data)

## And again
fahrenheit_conversion_plot <- function(temp_F, 
                                     to = "celsius") {    # this sets the default to celsius
  # check class of temp_F
  stopifnot(class(temp_F) == "numeric")
  stopifnot(class(to) == "character")
  
  # what to do if asking for celsius conversion
  if (to == "celsius") {
    temp_out <- fahrenheit_to_celsius(temp_F)
    plot(temp_F, temp_out, xlab = "Temperature in F", ylab = "Converted Temperature")
    
    # what to do if kelvin
  } else if (to == "kelvin") {
    temp_out <- fahrenheit_to_kelvin(temp_F)
    plot(temp_F, temp_out, xlab = "Temperature in F", ylab = "Converted Temperature")
    
    # what to do if otherwise
  } else {
    message("unexpected input for argument 'to'")
  }
}

fahrenheit_conversion_plot(input_temperature)

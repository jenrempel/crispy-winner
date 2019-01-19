### Day Two of Training ###

# rm = remove
# ls = list everything in your global environment
rm(list = ls())
# combo = remove everything in your global environment

setwd("~/Desktop/r-novice-inflammation")
dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)

### New Function to get summary statistics from data ###
analyze <- function(file_name) {
  
  # read in data at file_name
  dat <- read.csv(file = file_name, header = FALSE)
  
  # get average, min, max along columns
  column_max <- apply(dat, 2, max)
  column_min <- apply(dat, 2, min)
  column_avg <- apply(dat, 2, mean)
  
  # output
  return(column_max, column_min, column_avg)
}

## Try running the function
analyze(file_name = "data/inflammation-01.csv")
# That didn't work! Ah, we need to bind the results. Let's make sure 
# they work individually first.

column_max <- apply(dat, 2, max)
column_min <- apply(dat, 2, min)
column_avg <- apply(dat, 2, mean)

# Try putting the three vectors into an array. 
# Try it outside the function first.
rbind(column_avg, column_max, column_min)
# cbind() <-- This would also work

## Okay, we'll change that in the function itself 

analyze <- function(file_name) {
  
  # read in data at file_name
  dat <- read.csv(file = file_name, header = FALSE)
  
  # get average, min, max along columns
  column_max <- apply(dat, 2, max)
  column_min <- apply(dat, 2, min)
  column_avg <- apply(dat, 2, mean)
  
  # output
  return(rbind(column_max, column_min, column_avg))
}
# R functions try by default to return whatever happens on the last line in 
# the function (unless we're assigning the results of something to smthg),
# so we could actually drop the word "return" and just have "rbind(...)" like:

analyze <- function(file_name) {
  
  # read in data at file_name
  dat <- read.csv(file = file_name, header = FALSE)
  
  # get average, min, max along columns
  column_max <- apply(dat, 2, max)
  column_min <- apply(dat, 2, min)
  column_avg <- apply(dat, 2, mean)
  
  # output
  rbind(column_max, column_min, column_avg)
}

analyze(file_name = "data/inflammation-01.csv")
# It works!

## Let's look at all of these files
? list.files
list.files() #Not correct
# First argument below takes me to the right folder in my wd.
inflammation_files <- list.files(path = "data", 
           pattern = "inflammation",
           full.names = TRUE) 

# This gives us the input to our analyze function
# We can create a for loop in R to iterate over these for names.

### For loops for iteration

# First I do print(file) to test if the variable I created in my
# for loop works correctly. 
for (file in inflammation_files) {
  print(file)
  # analyze(file_name = ...)
}

# Now that I've verified it, I know longer need that print line.
for (file in inflammation_files) {
  print(paste("Analyzing", file)) # Paste() contcatinates into one file
  # analyze(file_name = ...)
}
# Great, it's working at adding analyzing to the file name.

# Now try the actual script
for (file in inflammation_files) {
  print(paste("Analyzing", file)) 
  out <- analyze(file_name = file)
  print(out)
}
# It's printing everything in the console. If we look at it 
# using View(out), though, it's not working (for some reason...)

# Now try printing into a list called inflammation_results.
# We will also set it to go from 1 to something... with the i stuff.
# i is an arbitrary character that we're assigning it to
# This lets us run repeatedly until it's exhausted the loop.
inflammation_results <- vector("list", 
                               length(inflammation_files))
i <- 1
for (file in inflammation_files) {
  i <- i + 1
  print(paste("Analyzing", file)) 
  out <- analyze(file_name = file)
  print(out)
} 
  
# This is one example of iteration in R. 
# You don't need the i variable though. 
# There are other built-in functions in R that we can use.

inflammation_results <- vector("list", 
                               length(inflammation_files))

seq_along(inflammation_results)

for (file_index in seq_along(inflammation_files)) {
  file <- inflammation_files[file_index]
  print(paste("Analyzing", inflammation_files[file_index]))
  out <- analyze(file_name = file)
  print(out)
} 

## L Apply
?lapply
# It applies a function over a list of vector

# This took our for loop and rewrote it as an lapply call.
inflammation_results2 <- 
  lapply(X = seq_along(inflammation_results), 
         FUN = function(file_index) {
             file <- inflammation_files[file_index]
             out <- analyze(file_name = file)
             return(out)
           })

# Check if inf_results1 = inf_results2
all.equal(inflammation_results, 
          inflammation_results2)

### I have messed smthg up.

# lapply is much faster than for loops. They're much better 
# style for R. So don't use for loops! Do an lapply call! 
# It will be much faster!!



### Visualization in ggplot2 ###

library(ggplot2)
dat_for_plotting <- dat[,1:5]
dim(dat_for_plotting)
hist(dat_for_plotting[,2])
?ggplot

p_1 <- ggplot(dat_for_plotting, aes(x = V2)) + 
  geom_histogram()
p_1 

## EXERCISE: create a scatterplot of V2 against V3
#               x-axis = V2, y-axis = V3
p_2 <- ggplot(dat_for_plotting, aes(x = V2, y = V3)) + 
  geom_point() + 
  xlab("Day 3 inf data") + 
  ylab("Day 2 inflammation data") + 
  ggtitle("Scatterplot of Day 3 v. 4 of inflammation data") + 
  theme_minimal(); p_2
p_2

### Data subsetting with dplyr ###
# It's intended to make things easier in R. But it uses a 
# completely different syntax than what we've been using in R.
# Dply introduces a pipe, very similar to in Unix/bash. 
# But slightly different.

# Stylistically, they're written line by line. Each line ends
# with a pipe (%>%).

dat_subset <- filter(dat, V2 > 0)
dim(dat_subset)

dat_subset_piped <- dat %>%
  filter(V2 > 0) %>%
  select(c("V3", "V4"))

dim(dat_subset_piped)

# EXERCISE: Try to do what we just did with dplyr, but using normal R.
#           ie - Create an object at_subset_piped2 that is exactly the
#           same as dat_subset_piped but does not use the pipe
#           to create a sequence of function calls.

# solution 1
dat_subset_filter <- filter(dat, V2 > 0)
dat_subset_piped2 <- dat_subset_filter[,3:4]

# solution 2
dat_subset_piped2 <- select(filter(dat, V2 > 0), 
                            c("V3", "V4"))

# check. yay it works for both options
all.equal(dat_subset_piped, dat_subset_piped2)  


## More dplyr fun: mutate and summarise
?mutate
# mutate adds a new column
dat_mutated <- dat %>%
  filter(V2 > 0) %>%
  select(c("V3", "V4", "V5")) %>%
  mutate(
    sum_V4_and_V5 = V4 + V5,
    prod_V4_and_V5 = V4 * V5,
    mean_all_columns = (V3 + V4 + V5) / 3
  )
dat_mutated

# transmute does smthg similar
?transmute

dat_transmuted <- dat %>%
  filter(V2 > 0) %>%
  select(c("V3", "V4", "V5")) %>%
  transmute(
    sum_V4_and_V5 = V4 + V5,
    prod_V4_and_V5 = V4 * V5,
    mean_all_columns = (V3 + V4 + V5) / 3
  )
dat_transmuted
# this just gives you the new info, without the original 2 columns. 
# very similar to mutate (they're the same essentially)

# always look at dplyr, not plyr (plyr is older)

?summarize
dat_summarised <- dat %>%
  filter(V2 > 0) %>%
  select(c("V3", "V4", "V5")) %>%
  summarise(
    mean_V4 = mean(V4),
    sd_V4 = sd(V4),
    min_V4 = min(V4)
  )
dat_summarised

### Wrapping up piping, dply, and ggplot2 ###
p_dat_transmuted <- dat %>%
  filter(V2 > 0) %>%
  select(c("V3", "V4", "V5")) %>%
  transmute(
    sum_V4_and_V5 = V4 + V5,
    prod_V4_and_V5 = V4 * V5,
    mean_all_columns = (V3 + V4 + V5) / 3
  ) %>%
  ggplot(aes(x = sum_V4_and_V5, y = mean_all_columns, 
             colour = prod_V4_and_V5)) +
  geom_point() +
  xlab("Sum of day 4 and day 5") + 
  ylab("Mean of days 3, 4, and 5") + 
  ggtitle("Scatterpot of derived values") + 
  theme_minimal()

p_dat_transmuted

# Let's save the plot
?ggsave

ggsave("final_plot.pdf", plot = p_dat_transmuted)

# Data Wrangling in R
# 4.4 Converting Data Types

# Let's say that we have a vector of numbers representing the number of foul shots made
# by a team of basketball players

foulshots <- c(18, 22, 15, 13, 5)

# I can use the sum() function to total them up

sum(foulshots)
class(foulshots)

# But what if they are read in as strings?

foulshot_strings <- c("18", "22", "15", "13", "5")
sum(foulshot_strings)
class(foulshot_strings)

# We can fix it with as.numeric

foulshot_converted <- as.numeric(foulshot_strings)
class(foulshot_converted)
sum(foulshot_converted)

# There are also tests to help us determine whether 

is.numeric(foulshots)
is.character(foulshots)

is.numeric(foulshot_strings)
is.character(foulshot_strings)

# Let's look at a tibble containing the names of players and their team numbers

names <- c("Mike", "Rae", "Dennis", "Sally", "Ian", "Sue")
teams <- c(1,1,1,2,2,2)
assignments <- tibble(names, teams)
assignments

# That's not really correct, because team isn't a numeric value, it's a factor

assignments$teams <- as.factor(assignments$teams)
assignments

# And there is an is.factor function to test them

is.factor(assignments$names)
is.factor(assignments$teams)

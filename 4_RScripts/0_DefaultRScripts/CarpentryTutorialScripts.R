# Different functions
# Listing files with pattern


# analyze <- function(filename) {
#   # Plots the average, min, and max inflammation over time.
#   # Input is character string of a csv file.
#   dat <- read.csv(file = filename, header = TRUE)
#   avg_day_inflammation <- apply(dat, 2, mean)
#   plot(avg_day_inflammation)
#   max_day_inflammation <- apply(dat, 2, max)
#   plot(max_day_inflammation)
#   min_day_inflammation <- apply(dat, 2, min)
#   plot(min_day_inflammation)
# }
# 
# analyze("data/inflammation-01.csv")
# analyze("data/Birth rate crude.csv")


######################
# FILES BATCH reading
######################


# Reading files
list.files(path = "data", pattern = "csv")
list.files(path = "data", pattern = "Birth")
list.files(path = "data", pattern = "inflammation")

list.files(path = "data", pattern = "inflammation", full.names = TRUE)

files1 = list.files(path = "data", pattern = "csv")
files2 = list.files(path = "data", pattern = ".csv")

# ALL.EQUAL
all.equal(files1,files2)

# Reading selected files
filenames <- list.files(path = "data",  
                        # Now follows a regular expression that matches:
                        pattern = "inflammation-[0-9]{2}.csv",
                        #          |            |        the standard file extension of comma-separated values
                        #          |            the variable parts (two digits, each between 0 and 9)
                        #          the static part of the filenames
                        full.names = TRUE)

filenames <- filenames[1:3]

# Apply function on selected data!
for (f in filenames) {
  print(f)
  analyze(f)
}

# Specifying a pattern and applying function to all data with pattern
analyze_all <- function(pattern) {
  # Runs the function analyze for each file in the current working directory
  # that contains the given pattern.
  filenames <- list.files(path = "data", pattern = pattern, full.names = TRUE)
  for (f in filenames) {
    analyze(f)
  }
}


analyze_all(pattern = "inflammation-[0-2]{2}.csv")

# SMARTER: Apply FUNCTION analyze to FILENAMES
sapply(filenames, FUN = analyze)


######################
# CREATING FUNCTIONS
######################

# data = vector (dataframe with 1 column selected)
center <- function(data, desired) {
  new_data <- (data - mean(data, na.rm = TRUE)) + desired
  return(new_data)
}


scaled <- function(data) {
  new_data <- (data - mean(data, na.rm = TRUE))/sd(data, na.rm = TRUE) # 1 
  return(new_data)
}


data = read.csv(file = "data/Birth rate crude.csv", header = TRUE)
summary(data[,4])

center(data[,4],0)
scaled(data[,4])

# ALL.EQUAL
# all.equal(center(data[,4],0),scaled(data[,4]))

# Day 2 - Red-Nosed Reports

# There is certainly a better way than these complicated for-loops and
# conditionals --- will refactor soon

# two conditions for safe = all levels increasing/decreasing, two adjacent levels differ by
# at least one, at most three

library(readr)
library(stringr)

source("functions.R")

# input <- read_lines("test_input.txt")
input <- read_lines("input.txt")

safe_lines <- c()

# check safety of each line and add safe lines to vector
for (line in input){
  if (safety_checker(line) == TRUE){
    safe_lines <- append(safe_lines, line)
  }
}

#solution:
length(safe_lines)



#Part 2 - try removing levels to make a safe report

safe_lines <- c()

for (line in input) {
  if (safety_checker(line) == TRUE) {
    safe_lines <- append(safe_lines, line)
  } else { # if line is not initially safe...
    levels <- str_split_1(line, " ") |> # parse line into levels
      as.integer()
    for (i in seq(1:length(levels))) { # iterate over levels
      new_line <- get_new_line(i, levels) |> paste(collapse = " ") # create new line based on current iteration/index
      if (safety_checker(new_line) == TRUE) { # check safety of new line
        # if marked as safe, break and move to next line/input
        safe_lines <- append(safe_lines, line)
        break
      }
    }
  }
}

#solution
length(safe_lines)
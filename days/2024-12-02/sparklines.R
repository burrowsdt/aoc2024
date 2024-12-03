## Fun with sparklines? Just goofing w/the gt package

library(readr)
library(stringr)
library(gt)
library(purrr)
library(dplyr)

source("functions.R")

input <- read_lines("input.txt")
# input <- read_lines("test_input.txt")

#subset - 50 rows
input <- input[1:50]

# map input to list of integers
trend <- map(input, str_split_1, pattern = " ") |> 
  map(as.integer)

# create column of names ("report 1, report 2...")
names <- c()
for (x in seq(1:length(trend))){
  names <- append(names, paste("Report ", as.character(x)))
}

#### version 1 - plain with trendlines - (example saved as plain_sparkline.png, or plain_sparkline.html)

# make df to pass to gt
df <- tibble(
  "reports" = names,
  "trend" = trend,
)

df |> 
  gt() |> 
  cols_label(
    reports = 'Report',
    trend = 'Trend'
  ) |> 
  gtExtras::gt_plt_sparkline(
    column = trend
  )


### playing with colors based on "safe" and "unsafe" condition

# generate "safe" and "unsafe" column for input, based on part 2 of the puzzle
safe_lines <- c()
full_tracker <- c()

for (line in input) {
  if (safety_checker(line) == TRUE) {
    safe_lines <- append(safe_lines, line)
    full_tracker <- append(full_tracker, "SAFE")
  } else { # if line is not initially safe...
    levels <- str_split_1(line, " ") |> # parse line into levels
      as.integer()
    for (i in seq(1:length(levels))) { # iterate over levels
      new_line <- get_new_line(i, levels) |> paste(collapse = " ") # create new line based on current iteration/index
      if (safety_checker(new_line) == TRUE) { # check safety of new line
        # if marked as safe, break and move to next line/input
        safe_lines <- append(safe_lines, line)
        full_tracker <- append(full_tracker, "SAFE")
        break
      } else if (i == length(levels)) {
        full_tracker <- append(full_tracker, "UNSAFE")   
      } 
    }
  }
}

# df to pass to gt
df <- tibble(
  "reports" = names,
  "trend" = trend,
  "safe" = full_tracker
)

# generate sparklines but color rows -- see example png and html (color_sparkline)
df |> 
  gt() |> 
  cols_label(
    reports = 'Report',
    trend = 'Trend'
  ) |> 
  cols_hide(columns = c(safe)) |>
  data_color(
    columns = safe,
    target_columns = everything(),
    method = "auto",
    palette = c("lightgreen", "yellow")
  ) |>  
  gtExtras::gt_plt_sparkline(
    column = trend,
    type = "points",
    palette = c("black", "black", "purple", "green", "lightgrey")
  )

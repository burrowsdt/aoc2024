# `get_new_line` takes parsed line (as vector of integers) and current index, returns new line
# with current index/element removed
get_new_line <- function(i, levels){
  # if statements control what to splice based on current index
  if(i == 1) {
    new_line <- levels[2:length(levels)]
  } else if (i == length(levels)) {
    new_line <-  levels[1:(i - 1)]
  } else {
    new_line <- c(levels[1:(i - 1)], levels[(i + 1):length(levels)])
  }
  return(new_line)
}


# `safety_checker` takes line as single string (not integers), determines safety of line
# according to Part 1 conditions

# will add some comments if i refactor...
safety_checker <- function(line) {
  condition_tracker <- c()
  safety <- NULL
  levels <- str_split_1(line, " ") |>
    as.integer()
  for (i in seq(1:length(levels))) {
    if (i != length(levels)) {
      if (levels[i] < levels[i + 1]) {
        condition_tracker <- append(condition_tracker, "increasing")
        if (abs(levels[i] - levels[i + 1]) > 0 &&
            abs(levels[i] - levels[i + 1]) < 4) {
          safety <- TRUE
        } else {
          safety <- FALSE
          break
        }
      } else if (levels[i] > levels[i + 1]) {
        condition_tracker <- append(condition_tracker, "decreasing")
        if (abs(levels[i] - levels[i + 1]) > 0 &&
            abs(levels[i] - levels[i + 1]) < 4) {
          safety <- TRUE
        } else {
          safety <- FALSE
          break
        }
      } else {
        safety <- FALSE
        break
      }
    }
  }
  
  if (safety == FALSE) {
    return(FALSE)
  } else if (length(unique(condition_tracker)) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# from single string input, add buffer of "." and split to one char per element
parse <- function(x){
  split_vec <- str_split_1(x, "")
  new_line <- c(rep(".", 5), split_vec, rep(".", 5)) |>  trimws("both")
  return(new_line)
}  

construct_grid <- function(file_name){
  input <- read_lines(file_name) |>
    map(parse)
  
  buffer_row <- rep(".", length(input[[1]]))
  
  input <- c(rep(list(buffer_row), 3), input, rep(list(buffer_row), 3))
  
  grid <- do.call(rbind, input)
  
  return(grid)
}

# given coordinates i, j and a value (val), find instances of that value on surrounding coords
scanner <- function(i, j, val, grid){
  surrounding_values <- c(grid[i, j+1], grid[i+1, j+1], grid[i+1, j], grid[i+1, j-1], grid[i, j-1], grid[i-1, j-1], grid[i-1, j], grid[i-1, j+1])
  detected_indices <- which(surrounding_values == val)
  return(detected_indices)
}
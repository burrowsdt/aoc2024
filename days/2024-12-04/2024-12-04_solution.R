library(readr)
library(stringr)
library(purrr)

source("functions.R")

#### Part 1

## function reads input, adds buffer rows, creates grid matrix
grid <- construct_grid("input.txt")

## initialize final_count object
final_count <- 0

# iterate over grid
for (i in 1:nrow(grid)) {
  for (j in 1:ncol(grid)) {
    # given a coordinate [i,j] capture all surrounding coordinates (as coordinates, not values)
    surround_coords <- list(c(i, j+1), c(i+1, j+1), c(i+1, j), c(i+1, j-1), c(i, j-1), c(i-1, j-1), c(i-1, j), c(i-1, j+1))
    # value of current coordinate
    value <- grid[i, j]
    if (value == "X"){
      # store X coordinate
      X <- list(c(i, j))
      # hunt for M, starting directly right
      indices <- scanner(i,j, "M", grid)
      
      for (index in indices){
        # iterate over all instances of M --- use surround_coords to get coordinate of M
        M <- surround_coords[index]
        # determine slope
        slope <- M[[1]] - X[[1]]
        slope <- list(slope)
        # grab values that finish 4-char set
        A <- list(M[[1]] + slope[[1]])
        S <- list(A[[1]] + slope[[1]])
        # construct final phrase and test against "XMAS"
        final_phrase <- c(grid[X[[1]][1], X[[1]][2]], grid[M[[1]][1], M[[1]][2]], grid[A[[1]][1], A[[1]][2]], grid[S[[1]][1], S[[1]][2]])
        if (paste(final_phrase, collapse = "") == "XMAS")
          final_count <- final_count + 1 #add to tracker
      }
    }
  }
}

#solution
print(final_count)

##### Part 2

# Same grid set up process

grid <- construct_grid("input.txt")


# initialize tracker
x_mas_count <- 0

# iterate over grid looking for A
for (i in 1:nrow(grid)) {
  for (j in 1:ncol(grid)) {
    # surround_coords <- list(c(i, j+1), c(i+1, j+1), c(i+1, j), c(i+1, j-1), c(i, j-1), c(i-1, j-1), c(i-1, j), c(i-1, j+1))
    value <- grid[i, j]
    if (value == "A"){
      # When A is found: 
      # snag diagonals: 
      UR <- grid[[i-1, j+1]]
      LL <- grid[[i+1, j-1]]
      UL <- grid[[i-1, j-1]]
      LR <- grid[[i+1, j+1]]
      
      #construct phrases
      opt_1 <- c(UR, value, LL) |> paste(collapse = "")
      opt_2 <- c(UL, value, LR) |> paste(collapse = "")
      
      #test
      if((opt_1 == "MAS" ||  opt_1 == "SAM") && (opt_2 == "MAS" ||  opt_2 == "SAM")){
        x_mas_count <- x_mas_count + 1 #add to tracker
      }
    }
  }
}
  
#solution
print(x_mas_count)
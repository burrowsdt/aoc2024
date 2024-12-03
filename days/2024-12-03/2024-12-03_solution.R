library(readr)
library(stringr)
library(purrr)

##### Day 3 --- not elegant at all but will be *very* easy to clean up if I ever have time.

input <- "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
# input <- read_file("input.txt")

### Part 1 - parse to complete functions, multiply and sum
complete_functions <- str_extract_all(input, "mul\\(\\d{1,3},\\d{1,3}\\)")

number_sets <- str_extract_all(complete_functions, "\\d+,\\d+") |> unlist()

multiply <- function(x){
  components <- str_split(x, ",") |> unlist()
  return (as.integer(components[1]) * as.integer(components[2]))
}

products <- map_vec(number_sets, multiply)
# solution
sum(products)

### Part 2 - 

# input <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
input <- read_file("input.txt")

complete_functions <- str_extract_all(input, "mul\\(\\d{1,3},\\d{1,3}\\)|don't\\(\\)|do\\(\\)") |> unlist()

status <- "do"
final_set_to_calculate <- c()

for (x in complete_functions) {
  if (x == "do()") {
    status <- "do"
  } else if (x == "don't()") {
    status <- "don't"
  } else {
    if (status == "do") {
      final_set_to_calculate <- append(final_set_to_calculate, x)
    } else{
      next
    }
  }
}

number_sets <- str_extract_all(final_set_to_calculate, "\\d+,\\d+") |> unlist()

multiply <- function(x){
  components <- str_split(x, ",") |> unlist()
  return (as.integer(components[1]) * as.integer(components[2]))
}

products <- map_vec(number_sets, multiply)
# solution
sum(products)


## Day 1, Advent of Code 2024 - Historian Hysteria

library(readr)
library(stringr)
library(zeallot)

##### Part 1
# input <- read_lines("test_input.txt")

input <- read_lines("input.txt")

# initialize empty vectors (I know, should probably initialize with final length)
list1 <- c()
list2 <- c()

# process input into two lists
for (line in input){
  # split each line
  c(add1, add2) %<-% str_split_1(line, "  ")
  
  # add to appropriate list
  list1 <- append(list1, add1)
  list2 <- append(list2, add2)
}

# sort the lists
list1 <- as.integer(sort(list1))  
list2 <- as.integer(sort(list2))

# calculate distance between two lists
distance <- list2 - list1

# add up the distances - solution
sum(abs(distance))


##### Part 2 --- having run part 1
# Dup values from list1 to list2

# initialize empty vector for tracking count of each value
num_times <- c()

# count each occurrence of list1 in list2; add to num_times

for (number in list1){
  num_times <- append(num_times, sum(list2==number))
}

# multiply list1 by number of occurrences, sum for solution
sum(list1 * num_times)
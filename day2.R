spreadsheet <- "5 1 9 5
7 5 3 
2 4 6 8"

checksum <- function(s, sep = "\t") {
  x <- as.matrix(read.table(s, sep =sep))
  mins <- apply(x, 1, min, na.rm = TRUE)
  maxs <- apply(x, 1, max, na.rm = TRUE)
  sum(maxs - mins)
}

checksum(textConnection(spreadsheet), sep = " ")

checksum("day2_input.txt")

spreadsheet2 <- "5 9 2 8
9 4 7 3
3 8 6 5"

checksum2 <- function(s, sep = "\t") {
  x <- as.matrix(read.table(s, sep =sep))
  # Use the get_evens function on each row to return a matrix with the two
  # numbers that are divisible by eachother
  evens <- apply(x, 1, get_evens)
  
  # divide the min by the max, and sum them
  divisions <- apply(evens, 2, function(x) max(x) / min(x))
  sum(divisions)
}

# Function to find the two numbers in a vector that are evenly divisible
get_evens <- function(vec) {
  # divide each number by each other number and check which have remainder zero
  test_list <- lapply(vec, function(x) x %% vec == 0)
  
  # the element with two TRUEs (sum == 2) will have the indices of the two elements
  # of the vector that are divisible
  div_even_indices <- test_list[[which(vapply(test_list, function(x) sum(x) == 2, FUN.VALUE = logical(1)))]]
  
  # subset the original vector to get the two divisible numbers
  vec[div_even_indices]
}

checksum2(textConnection(spreadsheet2), sep = " ") # 9
checksum2("day2_input.txt")


library(testthat)

reallocate_counter <- function(x, length_of_loop = FALSE) {
  counter <- 0
  all_banks <- list()
  
  while (!any(vec_in_list(x, all_banks))) {
    all_banks <- c(all_banks, list(x))
    x <- reallocate(x)
    counter <- counter + 1
  }
  
  if (length_of_loop) {
    first <- which(vec_in_list(x, all_banks))
    return(counter - first + 1)
  }
  counter
}

vec_in_list <- function(vec, list) {
  vapply(list, function(x) all(vec == x), FUN.VALUE = logical(1))
}

reallocate <- function(x) {
  len_x <- length(x)
  max_pos <- which.max(x)
  y <- c(rep(0, max_pos), rep(1, x[max_pos]))
  len_y <- length(y)
  y <- c(y, rep(0, (ceiling(len_y/len_x) * len_x) - len_y))
  y_mat <- matrix(y, nrow = len_x)
  x[max_pos] <- 0
  x + rowSums(y_mat)
}

test_banks <- c(0, 2, 7, 0)

expect_identical(reallocate(test_banks), c(2,4,1,2))

expect_identical(reallocate_counter(test_banks), 5)

day6_input <- scan(textConnection("4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5"), numeric(), sep = "\t")

# Part 1
system.time(reallocate_counter(day6_input))
# 12841

# Part 2
expect_identical(reallocate_counter(test_banks, TRUE), 4)

system.time(reallocate_counter(day6_input, TRUE))
# 8038


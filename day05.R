library(testthat)

escape <- function(x) {

  pos <- 1
  counter <- 0
  
  while (TRUE) {
    val <- x[pos]
    if (is.na(val)) break()
    x[pos] <- x[pos] + 1
    pos <- pos + val
    counter <- counter + 1
  }
  counter
}

test_input <- c(0, 3,  0,  1,  -3)

expect_equal(escape(test_input), 5)

input <- as.integer(readLines("day05_input.txt"))

escape(input)

# Part 2

escape2 <- function(x) {
  
  pos <- 1
  counter <- 0
  
  while (TRUE) {
    val <- x[pos]
    if (is.na(val)) break()
    if (val >= 3) {
      x[pos] <- x[pos] - 1
    } else {
      x[pos] <- x[pos] + 1
    }
    counter <- counter + 1
    pos <- pos + val
  }
  counter
}

test_input <- c(0, 3,  0,  1,  -3)

expect_equal(escape2(test_input), 10)

escape2(input)

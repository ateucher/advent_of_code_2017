library(testthat)

escape <- function(x) {

  pos <- 1
  counter <- 0
  
  # I had been testing for val being NA, but as @jennybc pointed out, 
  # if the jumps are net negative and take you off the beginning of the vector
  # you will be indexing x by a negative number, which instead of returning NA, 
  # will be removing the pos'th element of x.
  # Eg: c(1,2)[3] == NA (desired), but c(1,2)[-1] == 2, and c(1,2)[-3] == c(1,2)
  while (pos > 0 && pos <= length(x)) {
    val <- x[pos]
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
  
  while (pos > 0 && pos <= length(x)) {
    val <- x[pos]
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

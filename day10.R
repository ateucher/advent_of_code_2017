library(testthat)

twister <- function(size, lengths) {
  vec <- seq(0, length.out = size)
  skip <- 0
  pos <- 1
  
  for (l in lengths) {
    if (pos > size) pos <- pos %% size
    sublist_indices <- seq(pos, length.out = l)
    sublist_indices[sublist_indices > size] <- sublist_indices[sublist_indices > size] - size
    vec[sublist_indices] <- rev(vec[sublist_indices])

    pos <- pos + l + skip
    skip <- skip + 1
  }
  
  vec
}

test_input <- c(3, 4, 1, 5)

# debug(twister)
expect_equal(twister(5, 3), c(2,1,0,3,4))
expect_equal(twister(5, test_input[1:2]), c(4,3,0,1,2))
expect_equal(twister(5, test_input[1:3]), c(4,3,0,1,2))
expect_equal(twister(5, test_input[1:4]), c(3,4,2,1,0))
expect_equal(twister(5, test_input), c(3,4,2,1,0))

# Part 1
input <- c(102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216)
output <- twister(256, input)
output[1] * output[2]

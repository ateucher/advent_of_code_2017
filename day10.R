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

# Part 2

twister2 <- function(size, lengths) {
  
  lengths <- add_suffix(to_bytes(lengths))
  vec <- seq(0, length.out = size)
  skip <- 0
  pos <- 1
  i <- 1
  for (i in 1:64) {
    for (l in lengths) {
      pos <- pos %% size
      if (pos == 0) pos <- size
      sublist_indices <- seq(pos, length.out = l)
      sublist_indices[sublist_indices > size] <- sublist_indices[sublist_indices > size] - size
      vec[sublist_indices] <- rev(vec[sublist_indices])
      
      pos <- pos + l + skip
      skip <- skip + 1
    }
    i + 1
  }
  
  sparse_hash <- split(vec, sort(rep(1:16, 16)))
  
  dense_hash <- vapply(sparse_hash, hashit, FUN.VALUE = integer(1))
  
  hex <- as.hexmode(dense_hash)
  paste(hex, collapse = "")
  
}

to_bytes <- function(x) {
  x <- paste(x, collapse = ",")
  utf8ToInt(x)
}

add_suffix <- function(x) {
  c(x, c(17, 31, 73, 47, 23))
}

hashit <- function(x) {
  Reduce(bitwXor, x)
}

expect_equal(to_bytes(1:3), c(49L, 44L, 50L, 44L, 51L))
expect_equal(add_suffix(to_bytes(1:3)), c(49,44,50,44,51,17,31,73,47,23))
expect_equal(hashit(c(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22)), 64)

expect_equal(twister2(256, ""), "a2582a3a0e66e6e86e3812dcb672a272")
expect_equal(twister2(256, "AoC 2017"),  "33efeb34ea91902bb2f59c9920caa6cd")
expect_equal(twister2(256, c(1,2,3)),  "3efbe78a8d82f29979031a4aa0b16a9d")
expect_equal(twister2(256, "1,2,4"),  "63960835bcdc130f0b66d7ff4f6a5a8e")

twister2(256, input)

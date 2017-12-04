library(testthat)

# Part 1 - ensure no duplicated words in a passphrase
is_valid <- function(phrases) {
  phrases <- strsplit(phrases, split = "\\s+")
  vapply(phrases, function(x) !any(duplicated(x)), FUN.VALUE = logical(1))
}

test_phrases <- c("aa bb cc dd ee", "aa bb cc dd aa", "aa bb cc dd aaa")
expect_equal(is_valid(test_phrases), c(TRUE, FALSE, TRUE))

# Check the real input data:
input <- readLines("day04_input.txt")

sum(is_valid(input))

# Part 2 - ensure no words in a passphrase are anagrams of each other:

is_valid2 <- function(phrases) {
  phrases <- strsplit(phrases, split = "\\s+")

  phrases <- lapply(phrases, function(x) {
    vapply(strsplit(x, split = ""), function(x) {
      paste(sort(x), collapse = "")
      }, 
      FUN.VALUE = character(1))
  })
  
  vapply(phrases, function(x) {
    !any(duplicated(x))
  }
  , FUN.VALUE = logical(1))
}

test_phrases2 <- c("abcde fghij", 
                  "abcde xyz ecdab", 
                  "a ab abc abd abf abj", 
                  "iiii oiii ooii oooi oooo", 
                  "oiii ioii iioi iiio")

expect_equal(is_valid2(test_phrases2), c(TRUE, FALSE, TRUE, TRUE, FALSE))

# Check the real input data:
input <- readLines("day04_input.txt")

sum(is_valid2(input))

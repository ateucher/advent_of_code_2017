library(testthat)
library(stringr)

garbage_score <- function(x) {
  
  x <- sanitize(x)[[1]]
  
  split_up <- x %>%
    str_split("") %>% 
    unlist()
  
  depth <- 1
  count <- 0
  prev <- ""
  for (i in split_up) {
    if (i == "{") {
      if (prev == "{") depth <- depth + 1
      count <- count + depth
    }
    
    if (i == "}" && i == prev) {
      depth <- depth - 1
    }
    prev <- i
  }
  count
}

sanitize <- function(x, removed) {
  n_orig <- nchar(x)
  n_removed <- 0
  while (str_detect(x, "!")) {
    x <- str_replace(x, "!.", "")
    n_removed <- n_removed + 2
  }
  
  # Regex to match outer angle brackets modified from here:
  # https://stackoverflow.com/a/35271017
  pattern <- "\\<(?>[^>]*(?R)?)*\\>"
  
  sanitized <- gsub(pattern, "<>", x, perl = TRUE)
  list(str_replace_all(sanitized, "<>|,", ""), 
       n_orig - (nchar(sanitized) + n_removed))
}

grp_1 <- "{}" # 1
grp_2 <- "{{{}}}" # 6
grp_3 <- "{{},{}}" # 5
grp_4 <- "{{{},{},{{}}}}" # 16
grp_5 <- "{<a>,<a>,<a>,<a>}" # 1
grp_6 <- "{{<ab>},{<ab>},{<ab>},{<ab>}}" # 9
grp_7 <- "{{<!!>},{<!!>},{<!!>},{<!!>}}" # 9
grp_8 <- "{{<a!>},{<a!>},{<a!>},{<ab>}}" # 3

expect_equal(garbage_score(grp_1), 1)
expect_equal(garbage_score(grp_2), 6)
expect_equal(garbage_score(grp_3), 5)
expect_equal(garbage_score(grp_4), 16)
expect_equal(garbage_score(grp_5), 1)
expect_equal(garbage_score(grp_6), 9)
expect_equal(garbage_score(grp_7), 9)
expect_equal(garbage_score(grp_8), 3)

input <- readLines("day09_input.txt")

garbage_score(input)
# 20530

# Part 2:

test_1 <- '{<>' # 0 characters.
test_2 <- '{<random characters>}' # 17 characters.
test_3 <- '{<<<<>}' # 3 characters.
test_4 <- '{<{!>}>}' # 2 characters.
test_5 <- '{<!!>}' # 0 characters.
test_6 <- '{<!!!>>}' # 0 characters.
test_7 <- '{<{o"i!a,<{i<a>}' # 10 characters.

expect_equal(sanitize(test_1)[[2]], 0)
expect_equal(sanitize(test_2)[[2]], 17)
expect_equal(sanitize(test_3)[[2]], 3)
expect_equal(sanitize(test_4)[[2]], 2)
expect_equal(sanitize(test_5)[[2]], 0)
expect_equal(sanitize(test_6)[[2]], 0)
expect_equal(sanitize(test_7)[[2]], 10)

sanitize(input)[[2]] # 11453 is too high (9978)

library(testthat)
library(stringr)

garbage_score <- function(x) {
  
  x <- sanitize(x)
  
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

sanitize <- function(x) {
  while (str_detect(x, "!")) {
    x <- str_replace(x, "!.", "")
  }
  
  x <- str_replace_all(x, ",", "")
  
  # Regex to match outer angle brackets modified from here:
  # https://stackoverflow.com/a/35271017
  pattern <- "\\<(?>[^>]*(?R)?)*\\>"
  
  gsub(pattern, "", x, perl = TRUE)
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

Advent of Code Day 6
================

``` r
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
  vapply(list, function(x) identical(x, vec), FUN.VALUE = logical(1))
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

day6_input <- scan(textConnection("4    10  4   1   8   4   9   14  5   1   14  15  0   15  3   5"), numeric(), sep = "\t")

# Part 1
reallocate_counter(day6_input)
```

    ## [1] 12841

``` r
# Part 2
expect_identical(reallocate_counter(test_banks, TRUE), 4)

reallocate_counter(day6_input, TRUE)
```

    ## [1] 8038

Followup note on performance
----------------------------

After looking at @jennybc's [code](https://github.com/jennybc/2017_advent-of-code/blob/master/day06.R) I realized that storing all of the numeric vectors in a list and comparing them was really inefficient (*I always realize how far I have to go when I look at her code*).

So I took a page from her book and collapsed them into a character vector and the speedup was huge. I think the reasons are twofold - one, growing a list like that in R is famously slow and inefficient (presumably growing a character vector is better?), and second, repeatedly trying to match a vector in a large list of vectors (up to almost 13000 in my case) has got to be less inefficient than searching for a match in a character vector.

``` r
reallocate_counter2 <- function(x, length_of_loop = FALSE) {
  counter <- 0
  all_banks <- character()
  
  while (!to_char(x) %in% all_banks) {
    all_banks <- c(all_banks, to_char(x))
    x <- reallocate(x)
    counter <- counter + 1
  }
  
  if (length_of_loop) {
    first <- which(to_char(x) == all_banks)
    return(counter - first + 1)
  }
  counter
}

to_char <- function(x) paste(x, collapse = " ")

system.time(ans_original <- reallocate_counter(day6_input))
```

    ##    user  system elapsed 
    ##  63.767   0.347  64.323

``` r
system.time(ans_with_char_vec <- reallocate_counter2(day6_input))
```

    ##    user  system elapsed 
    ##   1.666   0.141   1.831

``` r
all.equal(ans_original, ans_with_char_vec)
```

    ## [1] TRUE

The nature of the input appears to matter a lot as well. Here's my original implementation with my input compared to @jennybc's [input](https://github.com/jennybc/2017_advent-of-code/blob/master/day06.R). The sequence of numbers that I was given as my input took many more iterations before a match occurred.

``` r
jb_input <- "2  8   8   5   4   2   3   1   5   5   1   2   15  13  5   14"
jb_input <- as.integer(strsplit(jb_input, "\\s+")[[1]])

system.time(my_answer <- reallocate_counter(day6_input))
```

    ##    user  system elapsed 
    ##  63.267   0.338  63.786

``` r
system.time(jb_answer <- reallocate_counter(jb_input))
```

    ##    user  system elapsed 
    ##   3.825   0.029   3.871

``` r
# And for another illustration of how much faster the character vector method is:
system.time(jb_answer2 <- reallocate_counter2(jb_input))
```

    ##    user  system elapsed 
    ##   0.194   0.015   0.208

``` r
c(my_answer, jb_answer, jb_answer2)
```

    ## [1] 12841  3156  3156

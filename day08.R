library(readr)
library(dplyr)
library(testthat)

get_max_val <- function(input) {
  
  instructions <- read_delim(input, delim = " ", col_names = c("target", "direction", "value", "if", "condition_target", "condition", "condition_val"))
  instructions <- instructions %>% 
    mutate(cond_expr = sprintf("if (%s %s %s) %s <- %s %s %s", 
                               condition_target, condition, condition_val, target, target, 
                               ifelse(direction == "inc", "+", "-"), value))
  
  nms <- unique(instructions$target)
  
  # Create an environment to store the values
  val_env <- new.env()                   
  
  # Assign initial value of 0 to all objects in the environment
  lapply(nms, assign, 0, envir = val_env)
  
  # Function to get the max value of all the objects in the environment
  get_max_val <- function(env) {
    vals <- vapply(nms, function(n) {
      get(n, envir = env)
    }, FUN.VALUE = numeric(1))
    max(vals)
  }
  
  # Set an object to hold the max value (initial val should be 0)
  overall_max <- get_max_val(val_env)
  
  # Map over the instructions and evaluate each in turn in the environment
  lapply(instructions$cond_expr, function(expr) {
    eval(parse(text = expr), envir = val_env)
    # Update the current maximum value and compare it to the overall max, 
    # keeping the largest and updating overall_max accordingly
    current_max <- get_max_val(val_env)
    overall_max <<- max(current_max, overall_max)
  })
  
  c(final = get_max_val(val_env), ever = overall_max)
}

test_lines <- "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"

test <- get_max_val(test_lines)

expect_equal(test, c(final = 1,ever = 10))

input <- "day08_input.txt"

get_max_val(input)

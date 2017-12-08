library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

# lines <- readLines("day07_input.txt")
lines <- readLines("day07_test_input.txt")

## Make a tibble of the input with a column for name and weight, and a list-column for children
tbl <- str_split(lines, "\\s\\(|\\)|\\s->\\s", simplify = TRUE)[,-3] %>% 
  as_tibble() %>% 
  set_names(c("name", "wt", "children")) %>% 
  mutate(
    wt = as.numeric(wt),
    children = map(children, ~ str_split(.x, ", ")[[1]]), 
    children = rapply(children, function(x) ifelse(!nzchar(x), NA_character_, x), how = "replace"), 
    leaf = map_lgl(children, ~ ifelse(all(is.na(.x)), TRUE, FALSE)))

# Answer to part one:
# The base program should be the only one that is a parent but not a child
base_prog <- setdiff(unique(tbl$name), na.omit(unique(unlist(tbl$children))))

make_tree <- function(tbl, root_name) {
  tree <- NULL
  if (is.na(root_name)) return(tree) # exit the function when you hit a leaf
  idx <- which(tbl$name == root_name)
  tree <- list(name = tbl$name[idx], 
               wt = tbl$wt[idx],
               children = setNames(map(tbl$children[[idx]], 
                                       ~ make_tree(tbl, .x)), # Yay recursion!
                                   tbl$children[[idx]]))
  # Get rid of the empty child list if there are no children
  if (is.null(tree$children[[1]])) tree$children <- NULL
  tree
}

tree <- make_tree(tbl, base_prog)

# This is as far as I got. sum_weights will sum the immediate children of each node, but 
# doing it all the way to the tips is hard. Need to somehow start at the tips and work backwards

sum_weights <- function(x) {
  if (is.null(x$children)) return(x)
  x$sum_wt <- sum(map_dbl(x$children, ~ifelse(is.null(.x$sum_wt), .x$wt, .x$sum_wt))) + x$wt
  x$children <- lapply(x$children, sum_weights)
  x
}



# debug(sum_weights)
foo <- sum_weights(tree)

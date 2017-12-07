library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

lines <- readLines("day07_input.txt")

## tibble-driven
tbl <- str_split(lines, "\\s\\(|\\)|\\s->\\s", simplify = TRUE)[,-3] %>% 
  as_tibble() %>% 
  set_names(c("name", "wt", "children")) %>% 
  mutate(children = map(children, ~ str_split(.x, ", ")[[1]])) %>% 
  unnest()

# The base program should be the only one that is a parent but not a child
setdiff(unique(tbl$name), unique(tbl$children))




########################################################################

## list-driven
weights <- as.numeric(str_extract(lines, "\\d+"))

towers <- str_split(lines, " -> ", simplify = TRUE)[,2] %>% 
  str_split(", ") %>% 
  map(~ ifelse(.x == "", NA_character_, .x))

towers <- imap(towers, ~ list(children = list(.x, weight = weights[.y])))

names(towers) <- str_extract(lines, "^[a-zA-Z]+\\b")

leaves <- keep(towers, ~ all(is.na(.x$children)))

tree <- discard(towers, ~ all(is.na(.x$children)))

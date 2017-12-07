library(stringr)
library(purrr)
library(dplyr)

lines <- readLines("day07_test_input.txt")

## tibble-driven
tbl <- str_split(lines, "\\s\\(|\\)|\\s->\\s", simplify = TRUE)[,-3] %>% 
  as_tibble() %>% 
  set_names(c("name", "wt", "children")) %>% 
  mutate(children = map(children, ~ ifelse(.x == "", 
                                           NA_character_, 
                                           str_split(.x, ", "))), 
         is_leaf = ifelse(is.na(children), TRUE, FALSE))
  



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

spreadsheet <- "5 1 9 5
7 5 3 
2 4 6 8"

checksum <- function(s, sep = "\t") {
  x <- as.matrix(read.table(s, sep =sep))
  mins <- apply(x, 1, min, na.rm = TRUE)
  maxs <- apply(x, 1, max, na.rm = TRUE)
  sum(maxs - mins)
}

checksum(textConnection(spreadsheet), sep = " ")

checksum("day2_input.txt")

spreadsheet2 <- "5 9 2 8
9 4 7 3
3 8 6 5"

checksum2 <- function(s, sep = "\t") {
  x <- as.matrix(read.table(s, sep =sep))
 apply(x, 1, function(r) sapply(r, function(n) n %% r))
}

checksum2(textConnection(spreadsheet2), sep = " ")

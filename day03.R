test_array <- as.matrix(read.table(textConnection("17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23  24  25")))


dist <- function(x, array) {
  cent <- which(array == 1, arr.ind = TRUE)
  loc <- which(array == x, arr.ind = TRUE)
  sum(abs(cent - loc))
}

dist(1, test_array)
dist(12, test_array)
dist(23, test_array)

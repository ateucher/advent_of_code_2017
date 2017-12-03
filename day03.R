test_array <- as.matrix(read.table(textConnection("17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23  24  25")))


man_dist <- function(x, array) {
  cent <- ceiling(dim(array) / 2)
  loc <- which(array == x, arr.ind = TRUE)
  sum(abs(cent - loc))
}

man_dist(1, test_array)
man_dist(12, test_array)
man_dist(23, test_array)

wind <- function(length) {
  sq <- ceiling(sqrt(length))
  if (sq %% 2 == 0) sq <- sq + 1
  mat <- matrix(nrow = sq, ncol = sq)
  arr <- seq(length(mat))[-1]
  centre <- ceiling(sq / 2)
  
  x <- centre
  y <- centre
  dir <- "r"
  dist <- 1
  add_to_length <- FALSE
  
  mat[x,y] <- 1
  
  i <- 1
  while (i <= length) {
    if (dir == "r") {
      x_start <- x + 1
      x_end <- min(x_start + dist - 1, sq)
      x_pos <- x_start:x_end
      y_pos <- y
      x <- x + dist
    } else if (dir == "u") {
      y_start <- y - 1
      y_end <- min(y_start - dist + 1, sq)
      y_pos <- y_start:y_end
      x_pos <- x
      y <- y - dist
    } else if (dir == "l") {
      x_start <- x - 1
      x_end <- min(x_start - dist + 1, sq)
      x_pos <- x_start:x_end
      y_pos <- y
      x <- x - dist
    } else if (dir == "d") {
      y_start <- y + 1
      y_end <- min(y_start + dist - 1, sq)
      y_pos <- y_start:y_end
      x_pos <- x
      y <- y + dist
    }
    
    mat[y_pos, x_pos] <- arr[i:(min(i + dist - 1, length(arr)))]
    
    i <- i + dist
    
    if (add_to_length) dist <- dist + 1
    add_to_length <- !add_to_length

    dir <- change_direction(dir)
    
  }
  mat
}

change_direction <- function(dir) {
  dirs <- c("r", "u", "l", "d")
  if (dir == "d") return("r")
  dirs[which(dir == dirs) + 1]
}

foo <- wind(1024)
dist(1024, foo)

test_mat <- wind(325489)
dist(325489, test_mat)

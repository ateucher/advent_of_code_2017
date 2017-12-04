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
man_dist(1024, foo)

test_mat <- wind(325489)
man_dist(325489, test_mat)

# Part 2
# Need to re-write the function so that it writes the values one by one, 
# rather than one spiral arm at a time to accomodate the need to sum the neighbours
# as you go

wind2 <- function(length, type = c("sequential", "neighbour_sum")) {
  sq <- ceiling(sqrt(length))
  if (sq %% 2 == 0) sq <- sq + 1
  
  if (type == "neighbour_sum") {
    # create an outer buffer of NAs so can handle missing neighhours
    mat <- matrix(nrow = sq + 2, ncol = sq + 2)
  } else if (type == "sequential") {
    mat <- matrix(nrow = sq, ncol = sq)
  } else {
    stop("You specified an invalid type of matrix")
  }
  
  arr_len <- sq * sq - 1
  centre <- ceiling(ncol(mat) / 2)
  
  # Start off going to the right a distance of one
  dir <- "r"
  dist <- 1
  add_to_length <- FALSE
  mat[centre,centre] <- 1
  
  x <- y <- centre
  
  i <- 1
  while (i <= arr_len) {
    
    for (d in seq_len(dist)) {
      
      if (dir == "r") {
        x <- x + 1
      } else if (dir == "u") {
        y <- y - 1
      } else if (dir == "l") {
        x <- x - 1
      } else if (dir == "d") {
        y <- y + 1
      }
      
      mat_val <- switch(
        type, 
        "sequential" = i + 1, # starting at 2 since we pre-populated the centre
        "neighbour_sum"  = sum(mat[y + 1, x], 
                            mat[y - 1, x], 
                            mat[y + 1, x + 1],
                            mat[y + 1, x - 1], 
                            mat[y, x + 1], 
                            mat[y, x - 1],
                            mat[y - 1, x + 1],
                            mat[y - 1, x - 1]
                            , na.rm = TRUE))
      
      mat[y, x] <- mat_val
      i <- i + 1
      d <- d + 1
      
      # Avoid trying to write a value that doesn't exist
      if (i > arr_len) break()

    }
    
    # Increase the length of the arm every second time
    if (add_to_length) dist <- dist + 1
    add_to_length <- !add_to_length
    
    dir <- change_direction(dir)
    
  }
  mat
}

# still works with part one:
foo <- wind2(1024, "sequential")
man_dist(1024, foo)

test_mat <- wind2(325489, "sequential")
man_dist(325489, test_mat)


part_2_mat <- wind2(325489, "neighbour_sum")

min(part_2_mat[part_2_mat > 325489], na.rm = TRUE)

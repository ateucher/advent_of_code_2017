test_input <- scan("day1_input.txt", what = "character")

reverse_captcha <- function(input) {
  int_vec <- as.integer(strsplit(as.character(input), split = "")[[1]])
  
  int_vec <- c(int_vec, int_vec[1])
  
  runs <- rle(int_vec)
  sum(runs$values[runs$lengths > 1] * (runs$lengths[runs$lengths > 1] - 1))
}

reverse_captcha(1122)
reverse_captcha(1111)
reverse_captcha(1234)
reverse_captcha(91212129)
reverse_captcha(test_input)


reverse_captcha2 <- function(input) {
  int_vec <- as.integer(strsplit(as.character(input), split = "")[[1]])
  vec_len <- length(int_vec)
  halfway <- vec_len / 2
  int_vec_plus_half <- c(int_vec, int_vec[1:halfway])
  
  vals_to_sum <- vapply(seq_along(int_vec), function(i) {
    if (int_vec[i] == int_vec_plus_half[i + halfway]) {
      int_vec[i]
    } else {
      0L
    }
  }, FUN.VALUE = integer(1))
  
  sum(vals_to_sum)
}

reverse_captcha2(1212)
reverse_captcha2(1221)
reverse_captcha2(123425)
reverse_captcha2(123123)
reverse_captcha2(12131415)
reverse_captcha2(test_input)

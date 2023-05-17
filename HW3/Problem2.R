# See section 5.4.1 and the die game for examples
boardGame <- function() {
  p <- matrix(rep(0,64),nrow=8)
  onesixth <- 1/6
  onethirtysixth <- 1/36
  # Create transition matrix
  for (i in 1:8) {
    for (j in 1:6) {
      k <- i+j
      if (k > 8) k <- k - 8
      if (k != 4) {
        p[i,k] <- p[i,k] + onesixth
      }
    }

    # If can reach bonus square, update the states that can be reached from the bonus square
    if (4 - i > 0 || i - 2 >= 4) {
      p[i,1:2] <- p[i,1:2] + onethirtysixth
      p[i,5:8] <- p[i,5:8] + onethirtysixth
    }
  }
}

boardGame()

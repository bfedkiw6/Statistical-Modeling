boardGame <- function() {
  p <- matrix(rep(0,64),nrow=8)
  onesixth <- 1/6
  onethirtysixth <- 1/36
  for (i in 1:8) {
    for (j in 1:6) {
      k <- i+j
      if (k > 8) 
        k <- k - 8
      if (k != 4) {
        p[i,k] <- p[i,k] + onesixth
      }
    }
    if ((4-i) > 0) {
      p[i,1:2] <- p[i,1:2] + onethirtysixth
      p[i,5:8] <- p[i,5:8] + onethirtysixth
    }
  }
  print(p)
}

boardGame()
  # See section 5.4.1 and the die game for examples
}

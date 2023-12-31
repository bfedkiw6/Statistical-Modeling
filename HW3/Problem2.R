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

  # Get pi vector
  v <- findpi1(p)

  # Winnings = $1, multiply by time spent on square and probability passing 8. Add to not winning which is multiplied by 0 (disappears)
  exp_val <- v[1]*(1*(1/18)) + v[2]*(1*(1/18)) + v[3]*(1*(4/18)) + v[5]*(1*(3/6)) + v[6]*((1*(22/36))+(2*(1/18))) + v[7]*((1*(28/36))+(2*(1/18))) + v[8]*((1*(34/36))+(2*(1/18)))
  exp_val_squared <- (v[1]*(1^2*(1/18)) + v[2]*(1^2*(1/18)) + v[3]*(1^2*(4/18)) + v[5]*(1^2*(3/6)) + v[6]*((1^2*(22/36))+(2^2*(1/18))) + v[7]*((1^2*(28/36))+(2^2*(1/18))) + v[8]*((1^2*(34/36))+(2^2*(1/18))))
  var <- exp_val_squared - exp_val^2
  return(c(v, exp_val, var))
}

findpi1 <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p)
  imp[n,] <- rep(1,n)
  rhs <- c(rep(0,n-1),1)
  solve(imp,rhs)
}

# boardGame()

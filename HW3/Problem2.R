# See section 5.4.1 and the die game for examples
boardGame <- function() {
  p <- matrix(rep(0,64),nrow=8)
  onesixth <- 1/6
  onethirtysixth <- 1/36
  #v <- c(nreps=56)
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
  #print(p)
  v <- findpi1(p)
  exp_val <- 1*v[1]*(14/36) + 2*v[2]*(14/36) + 3*v[3]*(20/36) + 5*v[5]*(3/6) + 6*v[6]*(4/6) + 7*v[7]*(5/6) + 8*v[8]*(6/6)
  print(exp_val)
  exp_val_squared <- (1^2*v[1]*(14/36) + 2^2*v[2]*(14/36) + 3^2*v[3]*(20/36) + 5^2*v[5]*(3/6) + 6^2*v[6]*(4/6) + 7^2*v[7]*(5/6) + 8^2*v[8]*(6/6)) 
  print(exp_val_squared)
  var_val <- exp_val_squared - exp_val^2
  return(c(v,exp_val,var_val))
}

findpi1 <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p)
  imp[n,] <- rep(1,n)
  rhs <- c(rep(0,n-1),1)
  solve(imp,rhs)
}

boardGame()

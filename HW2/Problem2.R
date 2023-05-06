simEVarW <- function(d,v,p,nreps) {
  sum <- 0
  sum_squared <- 0

  for (rep in 1:nreps) {
    A <- sample(1:d,length(v),replace=FALSE)

    # Host picks door
    H <- 0

    while (H == 0) {
      # Host picks door
      host_result <- sample(1:d,1)
      # Check if door has prize, if not, let host pick that door
      if (length(which(A == host_result)) == 0) {
        H <- host_result
      }
    }

    door_change <- sample(0:1,1,prob=c(1-p,p))
    C <- 0

    # Change door
    if (door_change) {
      while (C == 0) {
        c_result <- sample(2:d,1)
        if (c_result != host_result) {
          C <- c_result
        }
      }
      # Find if new door has a prize
      index <- which(A == C)
    } 
    # Not door change
    else {
      # Find if door 1 has a prize
      index <- which(A == 1)
    }

    # Prize behind chosen door
    if (length(index) != 0) {
      sum <- sum + v[index]
      sum_squared <- sum_squared + v[index]^2
    }
  }

  return_vect <- vector(length=2)
  # E(W)
  return_vect[1] <- sum/nreps

  # Var(W)
  return_vect[2] <- sum_squared/nreps - return_vect[1]^2

  return(return_vect)
}

simEVarW(10,c(50,2,79),0.4,10)
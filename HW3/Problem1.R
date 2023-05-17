dpark <- function(p, d, i) {

  # d-1 fails, p successes for spot at destination
  if (i == 0) return((1-p)^(d-1) * p)

  # Get probabilities for spots with distances i before and i after the destination
  return(((1-p)^((d-i)-1)*p) + ((1-p)^((d+i)-1)*p))
}

ppark <- function(p, d, i) {
  sum <- 0
  for (j in 0:i) {
    sum <- sum + dpark(p, d, j)
  }
  return(sum)
}

qpark <- function(p, d, q) {
  sum <- 0
  j <- 0
  # Keep adding probabilities of distance j until q is reached
  while (TRUE) {
    if (j == 0) {
      sum <- sum + ((1-p)^(d-1)*p)
    } else {
      sum <- sum + ((1-p)^(d-j-1)*p) + ((1-p)^(d+j-1)*p)
    }
    # See if sum of probabilities reached q
    if (sum > q) {
      return(j - 1)
    } else if (sum == q) {
      return(j)
    }
    j <- j + 1
  }
}

rpark <- function(n, p, d) {
  # Simulate finding a parking spot n times
  spaces <- rgeom(n, p) + 1  # + 1 since rgeom gets the number of full parking spots
  # abs(d - spaces) gets the distance from the destination to the parking spot
  return(abs(d - spaces))
}

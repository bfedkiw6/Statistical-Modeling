dpark <- function(p, d, i) {
  return(((1-p)^((d-i)-1)*p) + ((1-p)^((d+i)-1)*p))
}

ppark <- function(p, d, i) {
  sum <- 0
  for (j in 0:i) {
    if (i == 0) {
      sum <- sum + ((1-p)^((d-j)-1)*p)
    } else {
      sum <- sum + (((1-p)^((d-j)-1)*p) + ((1-p)^((d+j)-1)*p))
    }
  }
  return(sum)
}

qpark <- function(p, d, q) {
  sum <- 0
  temp <- 0
  j <- 0
  while (TRUE) {
    if (i == 0) {
      sum <- sum + ((1-p)^((d-j)-1)*p)
    } else if (sum > q) {
      temp <- j-1
      break
    } else if (sum == q) {
      temp <- j
      break
    } else {
      sum <- sum + (((1-p)^((d-j)-1)*p) + ((1-p)^((d+j)-1)*p))
    }
    j <- j+1
  }
  return(temp)	
}

rpark <- function(n, p, d) {
  dist <- c()
  spaces <- c()
  spaces <- rgeom(n,p)
  for (i in 1:length(spaces)) {
    dist[i] <- abs(d - (spaces[i] + 1))
  }
  return(dist)
}


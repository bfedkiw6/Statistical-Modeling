tags <- function(m,k,s) {
  
  #simplifying 
  if (m <= 0 || k < 0 || s < 0) {
    return(0)
  } else if (s == 1) {
    if (m >= k) {
      return((1/m)*(m-(k-1))) #k = k-i when going through for loop
    }
    return(0)
  } else if (s > 1) {
    sum <- 0
    for (i in 1:(k-1)) {
      sum <- sum + tags(m,k-i,s-1)
    }
  }
  return(sum*(1/m)) #for the ith value in each combo

}

tags(5,4,2)

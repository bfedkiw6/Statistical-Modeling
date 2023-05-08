tags <- function(m ,k, s) {

  if (m <= 0 || k < 0 || s < 0) {
    return(0)
  } else if (s == 0) {
    if (k == 0) {
      return(1)
    }
    return(0)
  } else if (k < s) {
    # No way to get a sum of k if you need to draw more than k tags since s * 1 > k
    # and s * 1 is the minimum sum in s draws
    return(0)
  } else if (s == 1) {
    if (m >= k) {
      # Cases are tag values k or greater
      # P(tag value = m or m - 1 or m - 2 or ... or k)
      return((1/m)*(m-(k-1)))
    }
    # No tags work, case is dead (probability 0 for this recursive call)
    return(0)
  } else if (s > 1) {
    sum <- 0
    # Tags that work are the tags that won't get to a sum of k (k - 1 or less)
    # If tag value = k or greater, will reach sum of k or greater before drawing s tags,
    # breaking the condition of drawing a sum of k or greater in s draws
    if (m >= k) {
      for (i in 1:(k-1)) {
        sum <- sum + tags(m, k-i, s-1)
      }
    } else {
      for (i in 1:m) {
        sum <- sum + tags(m, k-i, s-1)
      }
    }
  }
  # Essentially the formula 1/m * (tags(m, k-1, s-1) + tags(m, k-2, s-1) + ... + tags(m, 1, s-1))
  return(sum*(1/m))

}

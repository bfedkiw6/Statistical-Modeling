tags <- function(m, k, s) {

  # Probability is 0 if any of the function parameters are 0
  if (m < 0 || k < 0 || s < 0) return (0)

  return (1)
}

print(tags(5, 9, 10))

tags <- function(m, k, s) {

  # Probability is 0 if any of the function parameters are 0
  # Base cases:
  # k < 0 (sum greater than or equal to k, need to check s in this case)
  # s = 0 (Finished drawing s tags, need to check k in this case)
  if (m < 0 || k < 0 || s < 0) return (0)

  return (1)
}

print(tags(5, 9, 10))

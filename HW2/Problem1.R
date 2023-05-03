PAMsim <- function(nGen) {

  # Starting with first and second nodes linked
  adjMat <- matrix(0, nrow=nGen + 2, ncol=nGen + 2, )
  adjMat[1, 2] <- 1
  adjMat[2, 1] <- 1

  attachHistory <- vector(length=nGen)

  # Keep track of the degrees for probabilities
  degrees <- vector(length=nGen + 2)
  degrees[1:2] <- 1

  # Simulate attaching the 3rd through nGen+2th nodes
  for (i in 3:(nGen + 2)) {
    # Gets proportions of degrees for probabilities
    p <- degrees / sum(degrees)

    # Simulate picking a node to attach to based on p
    attached <- sample(1:(nGen + 2), 1, prob=p)

    # Recording data
    adjMat[attached, i] <- 1
    adjMat[i, attached] <- 1
    degrees[i] <- degrees[i] + 1
    degrees[attached] <- degrees[attached] + 1
    attachHistory[i - 2] <- attached
  }

  # Put adjMat and attachHistory into a list and name the elements of the list
  simList <- list(adjMat, attachHistory)
  names(simList) <- c("adjMat", "attachHistory")
  return (simList)
}

PAMemaxd <- function(nGen, nReps) {

  # Call PAMsim nReps amount of times and record the max degree for each rep, then mean
  maxDegrees <- vector(length=nReps)
  for (i in 1:nReps) {
    # Simulate PAM
    PAM <- PAMsim(nGen)
    # Find the max degree
    maxDegrees[i] <- max(apply(PAM$adjMat, 2, sum))
  }

  return(mean(maxDegrees))
}

# print(PAMemaxd(5, 1000))

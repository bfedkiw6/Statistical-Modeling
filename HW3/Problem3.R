fillTM <- function(p, q, r, w) {

  # Have to add one for a 0 call state
  trans_mat <- matrix(rep(0, (r + w + 1)^2), nrow=(r + w + 1))

  # Reminder that R uses 1 based indexing
  # First get the transition probabilities for the states with no calls in the waiting room
  for (i in 1:(r + w + 1)) {
    # Deal with the 1 based indexing to represent the actual value of the state
    start_state <- i - 1

    # For no calls in the waiting room, can have 0 calls terminating up to all calls terminating
    # If calls in the waiting room, can have 0 calls up to r calls terminating
    max_terminate <- ifelse(start_state <= r, start_state, r)

    # Consider a call coming in and a call not coming in for each possible number of calls terminating
    for (num_terminate in 0:max_terminate) {
      # Get probabilities
      # All possible combinations of which "calls" terminating times the probability of that many calls terminating and the rest not terminating
      terminate_probability <- choose(max_terminate, num_terminate) * p^num_terminate * (1 - p)^(max_terminate-num_terminate)
      incoming_call_probability <-  terminate_probability * q
      no_incoming_call_probability <- terminate_probability * (1 - q)

      # How many calls left after terminating (offset by 1 for the 1 based indexing so used i instead of start_state)
      no_incoming_call_state <- i - num_terminate
      # Consider if call is declined
      if (no_incoming_call_state + 1 > r + w + 1) {
        trans_mat[i, no_incoming_call_state] <- trans_mat[i, no_incoming_call_state] + incoming_call_probability
      } else {
        trans_mat[i, no_incoming_call_state + 1] <- trans_mat[i, no_incoming_call_state + 1] + incoming_call_probability
      }
      trans_mat[i, no_incoming_call_state] <- trans_mat[i, no_incoming_call_state] + no_incoming_call_probability
    }
  }

  return(trans_mat)
}

findpi1 <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p)
  imp[n,] <- rep(1,n)
  rhs <- c(rep(0,n-1),1)
  solve(imp,rhs)
}

callCtr <- function(p, q, r, w) {

  trans_mat <- fillTM(p, q, r, w)
  pi <- findpi1(trans_mat)
  v <- rep(0, 3)

  # Long run proportion of declined calls = how often in the "max state"
  # This is a conditional probability since we're assuming a call comes in, so no need to factor in the probability of a call coming in
  # Since calls come in at the end of the epoch, the current state already accounts for calls that were terminated,
  # therefore we don't need to multiply by the probability that no calls terminate
  v[1] <- pi[r + w + 1]

  # Average number of calls on hold calculated using the expected value of number of calls in the waiting room
  # If no waiting room, don't append anything to waiting_room_calls for the waiting room
  waiting_room <- NULL
  if (w > 0) waiting_room <- c(1:w)
  waiting_room_calls <- c(rep(0, r + 1), waiting_room)
  v[2] <- sum(pi * waiting_room_calls)

  # Average number of busy operators calculated using the expected value of busy operators
  busy_operators <- c(0:r, rep(r, w))
  v[3] <- sum(pi * busy_operators)

  return(v)
}

#callCtr(0.4, 0.3, 2, 3)
#callCtr(0.4, 0.3, 1, 0)

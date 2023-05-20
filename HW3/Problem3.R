fillTM <- function(p, q, r, w) {

  # Have to add one for a 0 call state
  trans_mat <- matrix(rep(0, (r + w + 1)^2), nrow=(r + w + 1))

  # Reminder that R uses 1 based indexing
  # First get the transition probabilities for the states with no calls in the waiting room
  for (i in 1:(r + 1)) {
    # Deal with the 1 based indexing to represent the actual value of the state
    start_state <- i - 1
    # Consider a call coming in and a call not coming in for each possible number of calls terminating
    # For no calls in the waiting room, can have 0 calls terminating up to all calls terminating
    for (num_terminate in 0:start_state) {
      # Get probabilities
      # All possible combinations of which "calls" terminating times the probability of that many calls terminating and the rest not terminating
      terminate_probability <- choose(start_state, num_terminate) * p^num_terminate * (1 - p)^(start_state-num_terminate)
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

  # Get the transition probabilities when the waiting room is involved
  # Can have 0 calls terminating up to r calls terminating
#  for (i in (r + 2):(r + w + 1)) {

#  }

  return(trans_mat)
}

callCtr <- function(p, q, r, w) {

  trans_mat <- fillTM(p, q, r, w)
  print(apply(trans_mat, 1, sum))
  print(trans_mat)
}

callCtr(0.4, 0.3, 2, 3)
callCtr(0.4, 0.3, 1, 0)

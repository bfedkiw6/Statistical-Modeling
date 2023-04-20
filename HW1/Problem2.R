ExactAnalysis <- function() {
    # Settings
    m <- 5  # Time passenger gets to stop of interest
    p <- c(0.5, 0.5)  # Probabilities of delay times between successive buses
    v <- 2  # Time it takes for a bus to reach stop of interest from main station


    # Constants in the quantities we are trying to find
    k <- 1
    r <- 3
    q <- 1
    # W is how long the passenger needs to wait at the stop for a bus to arrive
    # Bus departures at X_i
    # L_d is the time between departures of any two buses (independent of each other)
    # P(L_d = j) = p_j (probability that the time between departures is a certain amount)
    # P(L_d = 1) = 0.5
    # P(L_d = 2) = 0.5

    # First bus leaves at time 0, takes 2 units to get to station
    # Second bus has a 50-50 chance of departing 1 unit or 2 units after first bus departs
    # So on for any future buses


    # Quantities vector
    q_vect <- c()

    # P(W = k) (probability that passenger has to wait k=1 unit of time)
    # Bus has to leave at t = 4 and no bus leaves at t = 3
    # P(L_1 = 2 and L_2 = 2 or L_1 = 1 and L_2 = 1 and L_3 = 2)
    # = P(L_1 = 2 and L_2 = 2) + P(L_1 = 1 and L_2 = 1 and L_3 = 2)
    # = P(L_1 = 2) * P(L_2 = 2) + P(L_1 = 1) * P(L_2 = 1) * P(L_3 = 2)

    q_vect[1] <- 0.5 * 0.5 + 0.5 * 0.5 * 0.5


    # P(X_2 = r)
    # = P(L_1 = 1 and L_2 = 2 or L_1 = 2 and L_2 = 1)
    # = P(L_1 = 1 and L_2 = 2) + P(L_1 = 2 and L_2 = 1)
    # = P(L_1 = 1) * P(L_2 = 2) + P(L_1 = 2) * P(L_2 = 1)

    q_vect[2] <- 0.5 * 0.5 * 2


    # P(W = k | L_1 = q) (probability that passenger has to wait k=1 unit of time given the delay between B_0 and B_1 = q=1)
    # = P(L_2 = 1 and L_3 = 2 | L_1 = 1)
    # = P(L_2 = 1 and L_3 = 2) (Since L_2 = 1 and L_3 = 2 is independent of L_1 = 1)

    q_vect[3] <- 0.5 * 0.5


    # P(U = 3) (B_U is bus that passenger boards)
    # B_2 must leave before t = 3
    # P(L_1 = 1 and L_2 = 1); any other combination will lead to bus 2 leaving at t=3 meaning that the passenger will board bus 2
    # = P(L_1 = 1) * P(L_2 = 1)

    q_vect[4] <- 0.5 * 0.5


    # E(W)
    # W can be 0 or 1
    # Can't be 2 since a bus would have to leave at t=3 or t=4 for a bus to leave at t=5 and the passenger would have boarded that bus
    # Max wait time is 2 since max delay is 2 (Passenger can only wait up to a max of max delay - 1?)
    # A = {0, 1}
    # 0 * P(W = 0) + 1 * P(W = 1)
    # 0 * P(L1 = 1 and L2 = 2 or L1 = 2 and L2 = 1 or L1 = 1 and L2 = 1 and L3 = 1) + 1 * P(L_1 = 2 and L_2 = 2 or L_1 = 1 and L_2 = 1 and L_3 = 2)

    q_vect[5] <- 0 * (0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5 * 0.5) + 1 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5)


    # Var(W)
    # E[X^2] = 0^2 * (0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5 * 0.5) + 1^2 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5)

    q_vect[6] <- ( 0^2 * (0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5 * 0.5) + 1^2 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5) ) - (q_vect[5])^2


    # E(B_U)
    # Passenger will only be able to board buses that have a possibility of leaving the station by t=3 or t=4
    # A = {2, 3}
    # P(B_2) = P(L_1 = 1 and L_2 = 2 or L_1 = 2 and L_2 = 2 or L_1 = 2 and L_2 = 1)
    # P(B_3) = P(L_1 = 1 and L_2 = 1 and L_3 = 1 or L_1 = 1 and L_2 = 1 and L_3 = 2)

    q_vect[7] <- 2 * (0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5) + 3 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5 * 0.5)


    # E(number of buses leaving the main station by time m) (that left the station by time m?)
    # 3, 4, 5, or 6 buses can leave by t = 5
    # P(3) = probability of buses 1 through 3 all having a delay of 2
    # P(4) = probability of buses 1 through 3 all having a delay of 2 except for one of them having a delay of 1
    # P(5) = probability of buses 1 through 4 all having a delay of 1 except for one of them having a delay of 2
    # P(6) = probability of buses 1 through 5 all having a delay of 1

    q_vect[8] <- 3 * (0.5 * 0.5 * 0.5) + 4 * (3 * (0.5 * 0.5 * 0.5)) + 5 * (4 * (0.5 * 0.5 * 0.5 * 0.5)) + 6 * (0.5 * 0.5 * 0.5 * 0.5 * 0.5)


    # Var(number of buses leaving the main station by time m)

    q_vect[9] <- ( 9 * (0.5 * 0.5 * 0.5) + 16 * (3 * (0.5 * 0.5 * 0.5)) + 25 * (4 * (0.5 * 0.5 * 0.5 * 0.5)) + 36 * (0.5 * 0.5 * 0.5 * 0.5 * 0.5) ) - q_vect[8]^2
    q_vect[11] <- (3-q_vect[8])^2 * (0.5 * 0.5 * 0.5) + (4-q_vect[8])^2 * (3 * (0.5 * 0.5 * 0.5)) + (5-q_vect[8])^2 * (4 * (0.5 * 0.5 * 0.5 * 0.5)) + (6-q_vect[8])^2 * (0.5 * 0.5 * 0.5 * 0.5 * 0.5)



    # E(number of buses leaving the main station by time m | W = k)
    # If only 3 buses leave by t=5, no bus leaves at t=3 (L_1 = 2, L_2 = 2, L_3 = 2)
    # If only 4 buses leave by t=5, 1 combo without a bus leaving at t=3 (L_1 = 2, L_2 = 2, L_3 = 1)
    # If only 5 buses leave by t=5, 1 combo without a bus leaving at t=3 (L_1 = 1, L_2 = 1, L_3 = 2, L_4 = 1)
    # No combo of delay times where 6 buses can leave the station by t=5 and no bus leaves at t=3 and one bus leaves at t=4

    q_vect[10] <- 3 * (0.5 * 0.5 * 0.5) + 4 * (0.5 * 0.5 * 0.5) + 5 * (0.5 * 0.5 * 0.5 * 0.5)

    return(q_vect)
}

# Testing ExactAnalysis
print(ExactAnalysis())


busSim <- function(m,p,v,k,r,q,nDays) {
  
  q_vect <- c()
  
  w_vals <- vector(length=nDays)  # set up space for the Ws
  x2_vals <- vector(length=nDays)
  wL_vals <- vector(length=nDays)
  l1_vals <- vector(length=nDays)
  pbus_vect <- vector(length=nDays)
  
  
  # Simulation
  
  for (day in 1:nDays) {
    # Generate L values until the passenger has boarded a bus,
    # then do the analysis using that vector of L values
    # TODO: See if this works for expected values and variance
    l_vals <- generateLVector(p, v)
    w_vals[day] <- generateW(v, p, m,l_vals)
    # x2_vals[day] <- generateXn(v, p, m, 2)
    if(l_vals[i]==q) {
      l1_vals[day] <- w_vals[day]
    }
    pbus_vect <- (left_by_m[day]*left_by_m[day])
  }
  
  
  # Analysis
  
  # P(W = k)
  q_vect[1] <- mean(w_vals == k)
  
  # P(X_2 = r)
  q_vect[2] <- mean(x2_vals == r)
  
  # P(W = k | L1 = q)
    q_vect[3] <- mean(l1_vals == k)
  # E(number of buses leaving the main station by time m)
  q_vect[8] <- mean(left_by_m)
  # Var(number of buses leaving the main station by time m)
  q_vect[9] <- mean(pbus_vect)-(q_vect[8]*q_vect[8])

  mean(w_vals)
  
  return(q_vect)
}

generateW <- function(v,m,l_vals) {
  i <- 0
  tot <- v
  while(true) {
    tot <- tot + l_vals[i]
    i <- i+1
    if (i == length(l_vals)) break
  }
  return(tot - m)
}


generateLVector <- function(p,v,m) {
  
  # e.g. if p = (0.2,0.2,0.6), choose 1 number at random from the set
  # 1,2,3, with probabilities 0.2, 0.2 and 0.6, respectively
  
  l_vect <- c()
  tot <- v
  i <- 1
  while (true) {
    l_vect[i] <- sample(1:length(p),1,prob=p)
    tot <- tot + l_vect[i]
    i <- i + 1
    
    # Passenger got onto the bus
    if (tot >= m) break
  }
  
  return(l_vect)
}


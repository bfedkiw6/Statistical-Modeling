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

    q_vect[5] <- 0 + 1 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5)


    # Var(W)

    q_vect[6] <-


    # E(B_U)
    # Passenger will only be able to board buses that have a possibility of leaving the station by t=3 or t=4
    # A = {2, 3}
    # P(B_2) = P(L_1 = 1 and L_2 = 2 or L_1 = 2 and L_2 = 2 or L_1 = 2 and L_2 = 1)
    # P(B_3) = P(L_1 = 1 and L_2 = 1 and L_3 = 1 or L_1 = 1 and L_2 = 1 and L_3 = 2)

    q_vect[7] <- 2 * (0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5) + 3 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5 * 0.5)


    # E(number of buses leaving the main station by time m) (that left the station by time m?)
    # 4, 5, or 6 buses can leave by t = 5
    # P(3) = probability of buses 1 through 3 all having a delay of 2
    # P(4) = probability of buses 1 through 3 all having a delay of 2 except for one of them having a delay of 1
    # P(5) = probability of buses 1 through 4 all having a delay of 1 except for one of them having a delay of 2
    # P(6) = probability of buses 1 through 5 all having a delay of 1

    q_vect[8] <- 3 * (0.5 * 0.5 * 0.5) + 4 * (3 * (0.5 * 0.5 * 0.5)) + 5 * (4 * (0.5 * 0.5 * 0.5 * 0.5)) + 6 * (0.5 * 0.5 * 0.5 * 0.5 * 0.5)

    # Var(number of buses leaving the main station by time m)

    q_vect[9] <-


    # E(number of buses leaving the main station by time m | W = k)
    # If only 4 buses leave by t=5, 1 combo without a bus leaving at t=3 (L_1 = 2, L_2 = 2, L_3 = 1)
    # If only 5 buses leave by t=5, 1 combo without a bus leaving at t=3 (L_1 = 1, L_2 = 1, L_3 = 2, L_4 = 1, L_5 = 1)
    # No combo of delay times where 6 buses can leave the station by t=5 and no bus leaves at t=3 and one bus leaves at t=4

    q_vect[10] <- 3 * (0.5 * 0.5 * 0.5) + 4 * (0.5 * 0.5 * 0.5) + 5 * (0.5 * 0.5 * 0.5 * 0.5)
}


busSim <- function(m,p,v,k,r,q,nDays) {

    q_vect <- c()

    w_vals <- vector(length=nDays)  # set up space for the Ws
    x2_vals <- vector(length=nDays)
    wL_vals <- vector(length=nDays)
    for (day in 1:nDays) {
        # Generate L values until the passenger has boarded a bus,
        # then do the analysis using that vector of L values
        # TODO: See if this works for expected values and variance
        w_vals[day] <- generateW(v, p, m)
        x2_vals[day] <- generateXn(v, p, m, 2)
    }

    # P(W = k)
    q_vect[1] <- mean(w_vals == k)

    # P(X_2 = r)
    q_vect[2] <- mean(x2_vals == r)

    # P(W = k | L1 = q)

}

generateW <- function(v,p,m) {

    tot <- v
    while (true) {
        tot <- tot + generateL(p)

        # Passenger got onto the bus
        if (tot >= m) break
    }

    # tot - m is the amount of time the passenger had to wait
    return(tot - m)
}

generateL <- function(p) {

    # e.g. if p = (0.2,0.2,0.6), choose 1 number at random from the set
    # 1,2,3, with probabilities 0.2, 0.2 and 0.6, respectively
    return(sample(1:length(p),1,prob=p))
}

generateXn <- function(v, p, m, n) {

    x_n <- 0
    for (i in 1:n) {
        x_n <- x_n + generateL(p)
    }
    return(x_n)
}

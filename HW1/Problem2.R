ExactAnalysis <- function() {

    # Settings
    m <- 5  # Time passenger gets to stop of interest
    p <- c(0.5, 0.5)  # Probabilities of delay times between successive buses
    v <- 2  # Time it takes for a bus to reach stop of interest from main station


    # Constants in the quantities we are trying to find
    k <- 1
    r <- 3
    q <- 1


    # Quantities vector
    q_vect <- c()

    # P(W = k) (probability that passenger has to wait k=1 unit of time)
    # Bus has to leave at t = 4 and no bus leaves at t = 3
    # P(L_1 = 2 and L_2 = 2 or L_1 = 1 and L_2 = 1 and L_3 = 2)
    # = P(L_1 = 2 and L_2 = 2) + P(L_1 = 1 and L_2 = 1 and L_3 = 2)
    # = P(L_1 = 2) * P(L_2 = 2) + P(L_1 = 1) * P(L_2 = 1) * P(L_3 = 2)

    q_vect[1] <- (0.5 * 0.5) + (0.5 * 0.5 * 0.5)


    # P(X_2 = r)
    # = P(L_1 = 1 and L_2 = 2 or L_1 = 2 and L_2 = 1)
    # = P(L_1 = 1 and L_2 = 2) + P(L_1 = 2 and L_2 = 1)
    # = P(L_1 = 1) * P(L_2 = 2) + P(L_1 = 2) * P(L_2 = 1)

    q_vect[2] <- 2 * (0.5 * 0.5)


    # P(W = k | L_1 = q) (probability that passenger has to wait k=1 unit of time given the delay between B_1 and B_2 = q)
    # W here only has the case where L_1 = 1, L_2 = 1, and L_3 = 2 since L_1 has to be 1
    # = P(L_2 = 1 and L_3 = 2 | L_1 = 1)
    # = P(L_2 = 1 and L_3 = 2) (Since L_2 = 1 and L_3 = 2 is independent of L_1 = 1)

    q_vect[3] <- 0.5 * 0.5


    # P(U = 3) (B_U is bus that passenger boards)
    # B_2 must leave before t = 3 so that the passenger misses buses 1 and 2
    # P(L_1 = 1 and L_2 = 1); any other combination will lead to bus 2 leaving at t=3 or greater meaning that the passenger will board bus 2
    # = P(L_1 = 1) * P(L_2 = 1)

    q_vect[4] <- 0.5 * 0.5


    # E(W)
    # W can be 0 or 1
    # Can't be 2 since a bus would have to leave at t=3 or t=4 for a bus to leave at t=5 and the passenger would have boarded that bus
    # Max wait time is 2 since max delay is 2 (Passenger can only wait up to a max of max delay - 1?)
    # A = {0, 1}
    # 0 * P(W = 0) + 1 * P(W = 1)
    # 0 * P(L1 = 1 and L2 = 2 or L1 = 2 and L2 = 1 or L1 = 1 and L2 = 1 and L3 = 1) + 1 * P(L_1 = 2 and L_2 = 2 or L_1 = 1 and L_2 = 1 and L_3 = 2)

    case_W0 <- 0 * (0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5 * 0.5)
    case_W1 <- 1 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5)
    q_vect[5] <- case_W0 + case_W1


    # Var(W)
    # E[X^2] = 0^2 * (0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5 * 0.5) + 1^2 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5)

    case_W0_squared <- 0^2 * (0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5 * 0.5)
    case_W1_squared <- 1^2 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5)
    q_vect[6] <- (case_W0_squared + case_W1_squared) - (q_vect[5])^2


    # E(B_U)
    # Passenger will only be able to board buses that have a possibility of leaving the station by t=3 or t=4
    # A = {2, 3}
    # P(B_2) = P(L_1 = 1 and L_2 = 2 or L_1 = 2 and L_2 = 2 or L_1 = 2 and L_2 = 1)
    # P(B_3) = P(L_1 = 1 and L_2 = 1 and L_3 = 1 or L_1 = 1 and L_2 = 1 and L_3 = 2)

    case_BU2 <- 2 * (0.5 * 0.5 + 0.5 * 0.5 + 0.5 * 0.5)
    case_BU3 <- 3 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5 * 0.5)
    q_vect[7] <- case_BU2 + case_BU3


    # E(number of buses leaving the main station by time m)
    # 2, 3, 4, or 5 buses can leave by t = 5
    # P(2) = Delays of (2, 2, 2)
    # P(3) = 3 combinations of delays (2, 1, 2), (2, 2, 1), and (1, 2, 2) or 3 combinations of delays (1, 2, 1, 2), (2, 1, 1, 2), or (1, 1, 2, 2)
    # P(4) = 4 combinations of delays (1, 1, 1, 2), (1, 1, 2, 1), (1, 2, 1, 1), or (2, 1, 1, 1) or delays of (1, 1, 1, 1, 2)
    # P(5) = Delays of (1, 1, 1, 1, 1)

    two_buses <- 2 * (0.5 * 0.5 * 0.5)
    three_buses <- 3 * (3 * (0.5 * 0.5 * 0.5) + 3 * (0.5 * 0.5 * 0.5 * 0.5))
    four_buses <- 4 * (4 * (0.5 * 0.5 * 0.5 * 0.5) + (0.5 * 0.5 * 0.5 * 0.5 * 0.5))
    five_buses <- 5 * (0.5 * 0.5 * 0.5 * 0.5 * 0.5)
    q_vect[8] <- two_buses + three_buses + four_buses + five_buses


    # Var(number of buses leaving the main station by time m)

    two_buses_squared <- 2^2 * (0.5 * 0.5 * 0.5)
    three_buses_squared <- 3^2 * (3 * (0.5 * 0.5 * 0.5) + 3 * (0.5 * 0.5 * 0.5 * 0.5))
    four_buses_squared <- 4^2 * (4 * (0.5 * 0.5 * 0.5 * 0.5) + (0.5 * 0.5 * 0.5 * 0.5 * 0.5))
    five_buses_squared <- 5^2 * (0.5 * 0.5 * 0.5 * 0.5 * 0.5)
    q_vect[9] <- (two_buses_squared + three_buses_squared + four_buses_squared + five_buses_squared) - q_vect[8]^2


    # E(number of buses leaving the main station by time m | W = k)
    # (2, 2, 2), (1, 1, 2, 2), (1, 1, 2, 1), (2, 2, 1)

    q_vect[10] <- 2 * (0.5 * 0.5 * 0.5) + 3 * (0.5 * 0.5 * 0.5 + 0.5 * 0.5 * 0.5 * 0.5) + 4 * (0.5 * 0.5 * 0.5 * 0.5)

    return(q_vect)
}

ExactAnalysis <- function() {

    # Settings
    m <- 5  # Time passenger gets to stop of interest
    p <- c(0.5, 0.5)  # Probabilities of delay times between successive buses
    v <- 2  # Time it takes for a bus to reach stop of interest from main station


    # Constants in the quantities we are trying to find
    k <- 1
    r <- 3
    q <- 1


    # Quantities vector
    q_vect <- vector(length=2)


    # P(X_2 = r)
    # = P(L_1 = 1 and L_2 = 2 or L_1 = 2 and L_2 = 1)
    # = P(L_1 = 1 and L_2 = 2) + P(L_1 = 2 and L_2 = 1)
    # = P(L_1 = 1) * P(L_2 = 2) + P(L_1 = 2) * P(L_2 = 1)

    q_vect[1] <- 2 * (0.5 * 0.5)


    # P(W = k | L_1 = q) (probability that passenger has to wait k=1 unit of time given the delay between B_1 and B_2 = q)
    # W here only has the case where L_1 = 1, L_2 = 1, and L_3 = 2 since L_1 has to be 1
    # = P(L_2 = 1 and L_3 = 2 | L_1 = 1)
    # = P(L_2 = 1 and L_3 = 2) (Since L_2 = 1 and L_3 = 2 is independent of L_1 = 1)

    q_vect[2] <- 0.5 * 0.5

    return(q_vect)
}

# Testing ExactAnalysis
print(ExactAnalysis())


# Simulates the delays for the day
generateLVector <- function(p, m) {

    # e.g. if p = (0.2,0.2,0.6), choose 1 number at random from the set
    # 1,2,3, with probabilities 0.2, 0.2 and 0.6, respectively

    l_vect <- c()
    tot <- 0
    i <- 1
    while (1) {
        l_vect[i] <- sample(1:length(p),1,prob=p)
        tot <- tot + l_vect[i]
        i <- i + 1

        # Passenger got onto the bus
        if (tot >= m) break
    }

    return(l_vect)
}

# Checks how many buses left by t=m
checkLeftByM <- function(l_vals, m) {
    t <- 0
    for (i in 1:length(l_vals)) {
        t <- t + l_vals[i]

        # i buses have left by t <= m
        if (t == m) {
          return(i)
        } else if (t > m) {
          return(i - 1)
        }
    }
}

# Checks which bus the passenger boarded
checkBusU <- function(l_vals, v, m) {

    # Account for travel time
    t <- v
    for (i in 1:length(l_vals)) {
        t <- t + l_vals[i]

        # Passenger boarded bus i if t >= m
        if (t >= m) return(i)
    }
}

busSim <- function(m,p,v,k,r,q,nDays) {

    q_vect <- vector(length=10)

    w_vals <- vector(length=nDays)
    bus_U_vals <- vector(length=nDays)
    left_by_m <- vector(length=nDays)


    # Simulation

    for (day in 1:nDays) {

        # Generate L values until the passenger has boarded a bus,
        # then do the analysis using that vector of L values

        l_vals <- generateLVector(p, m)
        bus_U_vals[day] <- checkBusU(l_vals, v, m)
        left_by_m[day] <- checkLeftByM(l_vals, m)
    }


    # Analysis

    # P(W = k)
    #q_vect[1] <- mean(w_vals == k)

    # P(X_2 = r)
    #q_vect[2] <- mean(x2_vals == r)

    # P(W = k | L1 = q)

    # P(U = 3)
    q_vect[4] <- mean(bus_U_vals == 3)

    # E(B_U)
    q_vect[7] <- mean(bus_U_vals)

    # E(number of buses leaving the main station by time m | W = k)
    q_vect[10] <- mean(left_by_m[which(w_vals == k)])

    return(q_vect)
}

busSim(5, c(0.5, 0.5), 2, 1, 3, 1, 10)

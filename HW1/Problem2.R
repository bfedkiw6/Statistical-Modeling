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
# print(ExactAnalysis())


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
    w_vals_squared <- vector(length=nDays)
    x2_vals <- vector(length=nDays)
    bus_U_vals <- vector(length=nDays)
    left_by_m <- vector(length=nDays)
    l1_vals <- vector(length=nDays)
    pbus_vect <- vector(length=nDays)


    # Simulation

    for (day in 1:nDays) {

        # Generate L values until the passenger has boarded a bus,
        # then do the analysis using that vector of L values

        l_vals <- generateLVector(p, m)
        bus_U_vals[day] <- checkBusU(l_vals, v, m)
        left_by_m[day] <- checkLeftByM(l_vals, m)
        x2_vals[day] <- sum(l_vals[1:2])
        w_vals[day] <- sum(l_vals[1:bus_U_vals[day]]) - m + v
        w_vals_squared[day] <- w_vals[day]^2
        l1_vals[day] <- l_vals[1]
        pbus_vect[day] <- left_by_m[day]^2
    }


    # Analysis

    # P(W = k)
    q_vect[1] <- mean(w_vals == k)
  
    # P(X_2 = r)
    q_vect[2] <- mean(x2_vals == r)
  
    # P(W = k | L1 = q)
    q_vect[3] <- mean(w_vals[which(l1_vals == q)])

    # P(U = 3)
    q_vect[4] <- mean(bus_U_vals == 3)
    
    # E(W)
    q_vect[5] <- mean(w_vals)
    
    # Var(W)
    q_vect[6] <- mean(w_vals_squared) - q_vect[5]^2

    # E(B_U)
    q_vect[7] <- mean(bus_U_vals)
    
    # E(number of buses leaving the main station by time m)
    q_vect[8] <- mean(left_by_m)

    # Var(number of buses leaving the main station by time m)
    q_vect[9] <- mean(pbus_vect) - q_vect[8]^2

    # E(number of buses leaving the main station by time m | W = k)
    q_vect[10] <- mean(left_by_m[which(w_vals == k)])

    return(q_vect)
}

# busSim(5, c(0.5, 0.5), 2, 1, 3, 1, 10)

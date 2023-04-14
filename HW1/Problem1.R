admissionsProbs <- function() {

    # Create probabilities vector and make the data more easily accessible
    probs <- c()
    ucba <- UCBAdmissions


    # P(admit) is the number of admitted students divided by the total
    # number of students

    probs[1] <- sum(ucba[1,,]) / sum(ucba)


    # P(admit | female) is the number of admitted students that are female
    # divided by the total number of female students

    probs[2] <- sum(ucba[1,2,]) / sum(ucba[,2,])


    # P(admit | female and Dept. B) is the number of admitted students that
    # female and in department B divided by the total number of female students
    # in department B

    probs[3] <- sum(ucba[1,2,2]) / sum(ucba[,2,2])


    # P(admit | female and Dept. C) is the same as the previous but with department C

    probs[4] <- sum(ucba[1,2,3]) / sum(ucba[,2,3])


    # P(Dept. C | female) is the number of female students in department C divided by
    # the total amount of female students

    probs[5] <- sum(ucba[,2,3]) / sum(ucba[,2,])


    # P(admit | female and (Dept. B or Dept. C)) is the number of admitted female
    # students in departments B or C divided by the total amount of female students
    # in departments B or C

    probs[6] <- sum(ucba[1,2,2:3]) / sum(ucba[,2,2:3])


    # P(female | admit) is the number of admitted female students divided by the
    # total number of admitted students

    probs[7] <- sum(ucba[1,2,]) / sum(ucba[1,,])


    # P(female and admit) is the number of admitted female students divided by
    # the total number of students

    probs[8] <- sum(ucba[1,2,]) / sum(ucba)


    # P(Dept. C or Dept. D) is equal to P(Dept. C) + P(Dept. D) since Dept. C
    # cannot be true if Dept. D is and vise versa

    probs[9] <- sum(ucba[,,3:4]) / sum(ucba)

    return(probs)
}

# Testing admissionsProbs()
# print(UCBAdmissions)
print(admissionsProbs())


#various hacker types ratio

NDBLH <- 0.45
DBALH <- 0.05
APT <- 0.5


#hackers vector
hackers = c(NDBLH,DBALH,APT)

#prior -- an overall attack probability
probTotal = 0.075

#prior per hacker type step by step
probNDBL <- probTotal * NDBLH
probDBAL <- probTotal * DBALH
probAPT <- probTotal * APT


#prior per hacker as a vector
hackerProbs <- hackers * probTotal

#mitigant influence per hacker type  step by step

effNDBL <- 0.8
effDBAL <- 0.25
effAPT <- 0

effNDBLprob <- (1-effNDBL) * probNDBL
effDBALprob <- (1-effDBAL) * probDBAL
effAPTprob <- (1-effAPT) * probAPT


#mitig as a vector
mitigProb <- c(effNDBL,effDBAL,effAPT)
#remaining hacker prob
remindProb <- function(x) {1-x}
remainHackerProbs <- remindProb(mitigProb)
#efficiency probs
effProbs = remainHackerProbs * hackerProbs



#hack probs sum step by step
effSumProb <- effNDBLprob + effDBALprob + effAPTprob

#hack probs sum by vector
effSumProbFromVector <- sum(effProbs)


eff <- effSumProb / probTotal
effVector <- effSumProbFromVector / probTotal



###
sprintf("%1.2f%%", 100*probNDBL)
sprintf("%1.2f%%", 100*probDBAL)
sprintf("%1.2f%%", 100*probAPT)

sprintf("%1.2f%%", 100*effNDBLprob)
sprintf("%1.2f%%", 100*effDBALprob)
sprintf("%1.2f%%", 100*effAPTprob)

sprintf("%1.2f%%", 100*effSumProb)

sprintf("%1.2f%%", 100*eff)
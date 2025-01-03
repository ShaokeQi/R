sim <- function(nreps) {
  count2aces <- 0 # count of number of hands with exactly 2 aces
  count4diamonds <- 0 # count of number of hands with exactly 4 diamonds
  
  for (rep in 1:nreps) {
    hand <- sample(1:52, 5, replace = FALSE) # deal hand
    aces <- intersect(c(1, 14, 27, 40), hand) # find which aces, if any, are in hand
    diamonds <- intersect(c(1:13), hand) # find which diamonds, if any, are in hand
    
    if (length(aces) == 2) count2aces <- count2aces + 1
    if (length(diamonds) == 4) count4diamonds <- count4diamonds + 1
  }
  
  p2aces <- count2aces/nreps
  p4diamonds <- count4diamonds/nreps
  
  result1 <- ifelse(p2aces > p4diamonds, "P(exactly 2 aces) is larger", "P(exactly 4 diamonds) is larger")
  return(list(result1, p2aces= p2aces,p4diamonds=p4diamonds))
}

sim(100000) # Run simulation with 100,000 repetitions
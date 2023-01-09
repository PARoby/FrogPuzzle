
getProbDistributionFrog <- function(nbPad) {
  # Setting up the matrix of probability for each jump
  mat <- matrix(data = 0, nrow = nbPad + 1, ncol = nbPad + 1)
  for(a in 1:nbPad+1){
    for(b in 1:nbPad){
      if(a>b){
        mat[a,b] <- 1/(nbPad+1-b)
      }
    }
  }
  mat[nbPad+1,nbPad+1] <- 1
  
  # Setting up the matrix for the initial state
  etat <- matrix(data = 0, nrow = nbPad + 1, ncol = 1)
  etat[1,1] <- 1
  
  # Result statistic vector
  cumul <- c(1:nbPad)
  
  # Creates the cumulative vector of the distribution
  for(i in 1:nbPad){
    etat <- mat%*%etat
    cumul[i] <- etat[nbPad+1,1]
  }
  
  # Probability vector
  prob <- cumul
  
  for(j in 2:length(prob)){
    prob[j] <- prob[j] - cumul[j-1]
  }
  
  # Calculates the mean of the distribution
  frogMean <- 0
  for(k in 1:length(prob)){
    frogMean <- frogMean + (k)*prob[k]
  }
  
  plot(c(1:length(prob)),prob, xlab = "number of jumps", ylab = "probability", main = paste("Frog jumps probability\n n =", nbPad))
  return(data.frame(matrix(c(cumul,prob,frogMean, rep(NA,nbPad-1)), nrow = 3, ncol = nbPad, byrow = TRUE), row.names = c("cumulative", "probability", "mean")))
}

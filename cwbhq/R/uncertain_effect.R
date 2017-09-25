


  # the uncertain effect function
uncertain_effect <- function(x1, u1, x2, u2) {
  h <- min(x1 + u1, x2 + u2) - max(x1 - u1, x2 - u2)
  miu <- 0.5
  if(h > u2){
    x1 <- x1 + miu * ((h / u2) - 1) * (x2 - x1)
    u1 <- u1 + miu * ((h / u2) - 1) * (u2 - u1)
  }
    # x1 <- x1 + miu * ((h / u2) - 1) * (x2 - x1)
    # u1 <- u1 + miu * ((h / u2) - 1) * (u2 - u1)
  xu_vec <- c(x1, u1)
  return(xu_vec)
}

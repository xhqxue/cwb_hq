

team <- function(num_team = 20) {
  num_worker <- c(3,4,5,6)
  team <-NULL
  for(i in 1:num_team){
    team[i] <- sample(num_worker,1)
  }
  return(team)
}

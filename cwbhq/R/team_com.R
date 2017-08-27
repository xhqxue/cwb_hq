

team_com <- function(x_vec, u_vec, workers_team) {
  x <- NULL
  u <- NULL
  for(team_id in 1:length(workers_team)){
    team_s <- ifelse(team_id == 1, 1, sum(workers_team[1:team_id-1]) + 1)
    team_e <- sum(workers_team[1:team_id])
    for(i in team_s:team_e){
      res_vec <- NULL
      for(k in team_s:team_e){

        xu_vec <- uncertain_effect(x_vec[i], u_vec[i], x_vec[k], u_vec[k])
        res_vec <- rbind(res_vec,xu_vec)
        a<-x_vec[i]+2
      }
      res <- colMeans(res_vec)
      x <- c(x, res[1])
      u <- c(u, res[2])
    }
  }
  return(list(x = x,u = u))
}



leader_com_cwb <- function(worker_vec, workers_team, i) {
  workers_team <- workers_team - 1
  n_team <- c(1:length(workers_team))
  num_com <- c(ceiling(length(n_team)*0.05):ceiling(length(n_team)*0.15))
  n_com <- sample(num_com,1)
  team_id <- worker_vec$team_id

  x <- c(rep(0,sum(workers_team)))
  u <- c(rep(0,sum(workers_team)))
  if(i > 0){
    cwb <- worker_vec$cwb[i,]
    cwb_u <- worker_vec$cwb_u[i,]

    for(i in 1:n_com){
      team_com <- sample(n_team, 2)
      # t1_s <- sum(workers_team[1:team_com[1]]) - workers_team[team_com[1]] + 1
      # t1_e <- sum(workers_team[1:team_com[1]])
      # t2_s <- sum(workers_team[1:team_com[2]]) - workers_team[team_com[2]] + 1
      # t2_e <- sum(workers_team[1:team_com[2]])
      # # t1 <- c(t1_s:t1_e)
      # # t2 <- c(t2_s:t2_e)
      # t1 <- sample(c(t1_s:t1_e), 1)
      # t2 <- sample(c(t2_s:t2_e), 1)
      t1_c <- which(team_id == team_com[1])
      t2_c <- which(team_id == team_com[2])
      t1 <- sample(t1_c,1)
      t2 <- sample(t2_c,1)
      t1_xu <- uncertain_effect(cwb[t1], cwb_u[t1], cwb[t2], cwb_u[t2])
      t2_xu <- uncertain_effect(cwb[t2], cwb_u[t2], cwb[t1], cwb_u[t1])

      x[t1] <- ifelse(x[t1] == 0, t1_xu[1], (t1_xu[1]+x[t1])/2)
      u[t1] <- ifelse(u[t1] == 0, t1_xu[2], (t1_xu[2]+u[t1])/2)

      x[t2] <- ifelse(x[t2] == 0, t2_xu[1], (t2_xu[1]+x[t2])/2)
      u[t2] <- ifelse(u[t2] == 0, t2_xu[2], (t2_xu[2]+u[t2])/2)

    }
  }
  return(list(x = x,u = u))

}

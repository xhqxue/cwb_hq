

team_com <- function(x_vec, u_vec, workers_team) {
  x <- NULL
  u <- NULL
  com_times<-c(1:5)
  # for(team_id in 1:length(workers_team)){
  #   team_s <- ifelse(team_id == 1, 1, sum(workers_team[1:(team_id-1)]) + 1)
  #   team_e <- sum(workers_team[1:team_id])
  #   for(i in team_s:team_e){
  #     res_vec <- NULL
  #     for(k in team_s:team_e){
  #       xu_vec <- uncertain_effect(x_vec[i], u_vec[i], x_vec[k], u_vec[k])
  #       res_vec <- rbind(res_vec,xu_vec)
  #     }
  #     res <- colMeans(res_vec)
  #     x <- c(x, res[1])
  #     u <- c(u, res[2])
  #   }
  # }
  x <- rep(-1,sum(workers_team))
  u <- rep(-1,sum(workers_team))
  for(team_id in 1:length(workers_team)){
    team_s <- ifelse(team_id == 1, 1, sum(workers_team[1:(team_id-1)]) + 1)
    team_e <- sum(workers_team[1:team_id])
    num_com_workers <- sample(c(2:workers_team[team_id]),1)
    num_com_times <- sample(com_times,1)
    workers <- c(team_s:team_e)
    cou_com <- sample(workers,num_com_workers)
    f1 <- NULL
    res_vec <- NULL
    for(i in 1:num_com_times){
      # browser()
      k<-NULL
      k[1] <- sample(cou_com, 1)
      k[2] <- sample(cou_com, 1)
      xu_vec1 <- uncertain_effect(x_vec[k[1]], u_vec[k[1]], x_vec[k[2]], u_vec[k[2]])
      xu_vec2 <- uncertain_effect(x_vec[k[2]], u_vec[k[2]], x_vec[k[1]], u_vec[k[1]])
      f1 <- c(f1, k)
      res_vec <- rbind(res_vec,xu_vec1)
      res_vec <- rbind(res_vec,xu_vec2)
    }
    f2 <- unique(f1)
    for(i in 1:length(f2)){
      f3 <- which(f1 == f2[i])
      res_vec_2 <- res_vec[f3,]
      if(is.null(dim(res_vec_2))){
        res <- res_vec_2
      }else{
        res <- colMeans(res_vec_2)
      }
      # res <- colMeans(res_vec_2)
      x[f2[i]] <- res[1]
      u[f2[i]] <- res[2]
    }
  }
  x <- ifelse(x == -1, x_vec, x)
  u <- ifelse(u == -1, u_vec, u)


  return(list(x = x,u = u))
}

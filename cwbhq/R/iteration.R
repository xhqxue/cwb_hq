##------------------------------------------first
# leader_gap(t) = n[1] *leader_gap(t-1)+q[1]
# mate_gap(t) = n[2] * mate_th(t-1) +q[2]
# mate_th(t) = n[3] * leader_gap(t) + n[4] * mate_gap(t) * leader_gap(t) + n[5] * mate_gap(t) +q[3]
# cwb(t) = n[6] * mate_th(t)+q[4]
##------------------------------------------second
# mate_th(t) =n[7] * mate_th(t-1) +q[5]


iteration <- function(worker_vec, workers_team, i) {
  GO_vec <- c(0.008839, 0.130610, 0.113353)
  B_vec <- c(rep(0.1,9))
  check_team <- 0.5
  R <- 0.5
  # the communication between leaders and get the threaten of some workers,and return x and u
  if(runif(1) >= 0.5){
    mate_th1 <- leader_com_th(worker_vec,workers_team,i)
    cwb1<- leader_com_cwb(worker_vec,workers_team,-1)
  }else{
    mate_th1 <- leader_com_th(worker_vec,workers_team,-1)
    cwb1 <- leader_com_cwb(worker_vec,workers_team,i)
  }
  # get interaction between workers
  workers_team <- workers_team - 1
  cwb <- worker_vec$cwb[i,]

  leader_gap <- worker_vec$l_gap[i,]
  leader_gap_u <- worker_vec$l_gap_u[i,]
  leader_gap <- 0.9 * leader_gap + 0.1 * cwb


  mate_gap <- worker_vec$m_gap[i,]
  mate_gap_u <- worker_vec$m_gap_u[i,]

  mate_th_u <- worker_vec$m_th_u[i,]

  leader_gap_xu <- team_com(leader_gap, leader_gap_u, workers_team)
  mate_gap_xu <- team_com(mate_gap, mate_gap_u, workers_team)

  mate_th2 <- 2.8671 + GO_vec[1]*check_team + GO_vec[2]*mate_gap_xu$x + GO_vec[3]*(check_team*mate_gap_xu$x)-0.6184*(leader_gap_xu$x*mate_gap_xu$x)+0.1354*(leader_gap_xu$x*mate_gap_xu$x*check_team)+R

  mate_th_x <- mate_th1$x
  mate_th <- ifelse(mate_th_x == 0, mate_th2, (mate_th2+mate_th_x)/2 )
  mate_th_u <- ifelse(mate_th_x == 0, mate_th_u, (mate_th_u+mate_th1$x)/2 )

  cwb_u <-worker_vec$cwb_u[i,]
  cwb <- 0.7688 + 0.5002*mate_th-0.1391*leader_gap_xu$x+R
  cwb <- ifelse(cwb1$x == 0, cwb, (cwb1$x+cwb)/2 )
  cwb_u <- ifelse(cwb1$x == 0, cwb_u, (cwb1$u+cwb_u)/2 )

  # b <- 0.7
  # n <- c(0.9,0.9,0.9,0.7,0.6,0.8,0.9)
  # q <- c(0.2,0.2,0.2,0.2,0.2)
  # if(b < 0.5){
  #     leader_gap <- n[1] * worker_vec$l_gap[i,] + q[1]
  #     mate_gap <- n[2] * worker_vec$m_gap[i,] + q[2]
  #     mate_th <- n[3] * leader_gap + n[4] * mate_gap * leader_gap + n[5] * mate_gap +q[3]
  #     cwb <- n[6] * mate_th + q[4]
  # }else{
  #     leader_gap <- worker_vec$l_gap[i,]
  #     mate_gap <- worker_vec$m_gap[i,]
  #     # the first method
  #     # mate_th <- n[7] * worker_vec$m_th[i,] +q[5]
  #     # the second method
  #     mate_th <- worker_vec$m_th[i,]
  #     mate_th_u <- worker_vec$m_th_u[i,]
  #     x <- NULL
  #     u <- NULL
  #     for(i in 1:length(mate_th)){
  #       res_vec <- NULL
  #       for(k in 1:length(mate_th)){
  #         xu_vec <- uncertain_effect(mate_th[i], mate_th_u[i], mate_th[k], mate_th_u[k])
  #         res_vec <- rbind(res_vec,xu_vec)
  #       }
  #       res <- colMeans(res_vec)
  #       x <- c(x, res[1])
  #       u <- c(u, res[2])
  #     }
  #     mate_th <- x
  #     mate_th_u <- u
  #     cwb <- n[6] * mate_th + q[4]
  # }
  #
  cwb <- (cwb - min(cwb))/(max(cwb)-min(cwb))
  #
  worker_vec$l_gap <- rbind(worker_vec$l_gap, leader_gap_xu$x)
  worker_vec$l_gap_u <- rbind(worker_vec$l_gap_u, leader_gap_xu$u)

  worker_vec$m_gap <- rbind(worker_vec$m_gap, mate_gap_xu$x)
  worker_vec$m_gap_u <- rbind(worker_vec$m_gap_u, mate_gap_xu$u)

  worker_vec$m_th <- rbind(worker_vec$m_th, mate_th)
  worker_vec$m_th_u <- rbind(worker_vec$m_th_u, mate_th_u)

  worker_vec$cwb <- rbind(worker_vec$cwb, cwb)
  worker_vec$cwb_u <- rbind(worker_vec$cwb_u, cwb_u)
  #
  return(worker_vec)
}

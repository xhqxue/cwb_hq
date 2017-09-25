

worker <- function(team) {
  workers <- team - 1
  worker_vec <- sum(workers)

  team_id <- NULL
  for(i in 1:length(workers)){
    id_now <- rep(i,workers[i])
    team_id <-c(team_id,id_now)
  }

  # the gap between leader and staff
  leader_gap <- rnorm(worker_vec[1], 0.826, 1.02)
  # leader_gap <- attribute_index(leader_gap)


  leader_gap_u <- rnorm(worker_vec[1],0.5,0.1)
  leader_gap_u <-ifelse(leader_gap_u > 0, leader_gap_u, leader_gap_u * -1)
  # leader_gap_u <- attribute_index(leader_gap_u)
  leader_gap_u <- matrix(leader_gap_u, nrow = 1)

  # the gap between staff
  mate_gap <- rnorm(worker_vec[1], 1.22, 0.521)
  # mate_gap <- attribute_index(mate_gap)


  mate_gap_u <- rnorm(worker_vec[1],0.8,0.2)
  mate_gap_u <-ifelse(mate_gap_u > 0, mate_gap_u, mate_gap_u * -1)
  # mate_gap_u <- attribute_index(mate_gap_u)
  mate_gap_u <- matrix(mate_gap_u, nrow = 1)

  # the threat between mate
  GO_vec <- c(0.008839, 0.130610, 0.113353)
  B_vec <- c(rep(0.1,9))
  check_team <- 0.5
  R <- 0.5
  mate_th <- 0.999+5.69 * rbeta(worker_vec[1],2.28,3.79)
  # mate_th <- 2.8671 + GO_vec[1]*check_team + GO_vec[2]*mate_gap + GO_vec[3]*(check_team*mate_gap)-0.6184*(leader_gap*mate_gap)+0.1354*(leader_gap*mate_gap*check_team)+R
  #mate_th <- attribute_index(mate_th)


  mate_th_u <- rnorm(worker_vec[1],0.2,0.1)
  mate_th_u <- attribute_index(mate_th_u)
  mate_th_u <- matrix(mate_th_u, nrow = 1)

  # cwb <- 0.999+4.95*rbeta(worker_vec[1],3.08,3.16)
  # cwb <- attribute_index(cwb)
  cwb <- 0.7688 + 0.5002*mate_th-0.1391*leader_gap+R
  cwb <- matrix(cwb, nrow = 1)

  cwb_u <- rnorm(worker_vec[1], 0.2, 0.1)
  cwb_u <- attribute_index(cwb_u)
  cwb_u <- matrix(cwb_u, nrow = 1)

  leader_gap <- matrix(leader_gap, nrow = 1)
  mate_gap <- matrix(mate_gap, nrow = 1)
  mate_th <- matrix(mate_th, nrow = 1)

  return(list(team_id = team_id, l_gap = leader_gap, m_gap = mate_gap, m_th = mate_th, cwb = cwb, cwb_u = cwb_u, l_gap_u = leader_gap_u, m_gap_u = mate_gap_u, m_th_u = mate_th_u))

}

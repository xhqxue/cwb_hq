

worker <- function(team) {
  workers <- team - 1
  worker_vec <- sum(workers)

  # the gap between leader and staff
  leader_gap <- rnorm(worker_vec[1], 0.5, 0.2)
  leader_gap <- attribute_index(leader_gap)
  leader_gap <- matrix(leader_gap, nrow = 1)

  leader_gap_u <- rnorm(worker_vec[1],0.2,0.01)
  leader_gap_u <- attribute_index(leader_gap_u, 0.3, 0.1)
  leader_gap_u <- matrix(leader_gap_u, nrow = 1)

  # the gap between staff
  mate_gap <- rnorm(worker_vec[1], 0.5, 0.2)
  mate_gap <- attribute_index(mate_gap)
  mate_gap <- matrix(mate_gap, nrow = 1)

  mate_gap_u <- rnorm(worker_vec[1],0.2,0.01)
  mate_gap_u <- attribute_index(mate_gap_u, 0.3, 0.1)
  mate_gap_u <- matrix(mate_gap_u, nrow = 1)

  # the threat between mate

  mate_th <- rnorm(worker_vec[1], 0.5, 0.2)
  mate_th <- attribute_index(mate_th)
  mate_th <- matrix(mate_th, nrow = 1)

  mate_th_u <- rnorm(worker_vec[1],0.2,0.01)
  mate_th_u <- attribute_index(mate_th_u, 0.3, 0.1)
  mate_th_u <- matrix(mate_th_u, nrow = 1)

  cwb <- rep(0,worker_vec[1])
  cwb <- matrix(cwb, nrow = 1)

  return(list(l_gap = leader_gap, m_gap = mate_gap, m_th = mate_th, cwb = cwb, l_gap_u = leader_gap_u, m_gap_u = mate_gap_u, m_th_u = mate_th_u))

}

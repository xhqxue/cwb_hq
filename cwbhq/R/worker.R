#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

worker <- function(worker_vec) {
  leader_gap <- rnorm(worker_vec[1], 0.5, 0.2)
  leader_gap[leader_gap > 1] <- 1
  leader_gap[leader_gap < 0] <- 0
  leader_gap <- matrix(leader_gap, nrow = 1)

  mate_gap <- rnorm(worker_vec[1], 0.5, 0.2)
  mate_gap[mate_gap > 1] <- 1
  mate_gap[mate_gap < 0] <- 0
  mate_gap <- matrix(mate_gap, nrow = 1)

  mate_th <- rnorm(worker_vec[1], 0.5, 0.2)
  mate_th[mate_th > 1] <- 1
  mate_th[mate_th < 0] <- 0
  mate_th <- matrix(mate_th, nrow = 1)

  cwb <- rep(0,worker_vec[1])
  cwb <- matrix(cwb, nrow = 1)

  return(list(l_gap = leader_gap, m_gap = mate_gap, m_th = mate_th, cwb = cwb))

}

##------------------------------------------first
# leader_gap(t) = n[1] *leader_gap(t-1)+q[1]
# mate_gap(t) = n[2] * mate_th(t-1) +q[2]
# mate_th(t) = n[3] * leader_gap(t) + n[4] * mate_gap(t) * leader_gap(t) + n[5] * mate_gap(t) +q[3]
# cwb(t) = n[6] * mate_th(t)+q[4]
##------------------------------------------second
# mate_th(t) =n[7] * mate_th(t-1) +q[5]


iteration <- function(worker_vec,i) {
  b <- runif(1)
  n <- c(0.9,0.9,0.9,0.7,0.6,0.8,0.9)
  q <- c(0.2,0.2,0.2,0.2,0.2)
  if(b > 0.5){
      leader_gap <- n[1] * worker_vec$l_gap[i,] + q[1]
      mate_gap <- n[2] * worker_vec$m_gap[i,] + q[2]
      mate_th <- n[3] * leader_gap + n[4] * mate_gap * leader_gap + n[5] * mate_gap +q[3]
      cwb <- n[6] * mate_th + q[4]
  }else{
      leader_gap <- worker_vec$l_gap[i,]
      mate_gap <- worker_vec$m_gap[i,]
      mate_th <- n[7] * worker_vec$m_th[i,] +q[5]
      cwb <- n[6] * mate_th + q[4]
  }

  cwb <- (cwb - min(cwb))/(max(cwb)-min(cwb))

  worker_vec$l_gap <- rbind(worker_vec$l_gap, leader_gap)
  worker_vec$m_gap <- rbind(worker_vec$m_gap, mate_gap)
  worker_vec$m_th <- rbind(worker_vec$m_th, mate_th)
  worker_vec$cwb <- rbind(worker_vec$cwb, cwb)

  return(worker_vec)
}

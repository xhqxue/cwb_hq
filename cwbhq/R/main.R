#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

main <- function(x = 10,ite = 5) {
  workers_team <- team(x)
  worker_l <- worker(workers_team)
  for(i in 1:ite){
    worker_l <- iteration(worker_l, workers_team, i)
  }
  return(worker_l)
}

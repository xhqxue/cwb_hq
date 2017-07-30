#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

main <- function(x,ite) {
  worker_l <- worker(x)
  for(i in 1:ite){
    worker_l <- iteration(worker_l,i)
  }
  return(worker_l)
}


attribute_index <- function(index_vec,index_max = 1, index_min = 0) {
  if(index_max < index_min){
    a <- index_max
    index_max <- index_min
    index_min <- a
  }
  index_vec[index_vec > index_max] <- index_max
  index_vec[index_vec < index_min] <- index_min
  return(index_vec)
}

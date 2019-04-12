random_move <- function(p_boxes){
  empty_boxes <- which(p_boxes==10)
  random_number <- as.integer(runif(1,min=1,max=length(which(p_boxes==10))+1))
  return(empty_boxes[random_number])
}

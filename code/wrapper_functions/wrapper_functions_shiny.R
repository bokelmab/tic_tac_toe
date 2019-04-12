## assign the coordinates of the middle of the field which is clicked
translate.coordinates <- function(p.coordinate){
  
  if(p.coordinate < 2/3){
    if(p.coordinate < 1/3){
      return(1/6)
    }else{
      return(1/2)
    }
  }else{
    return(5/6)
  }
}

## translate coordinates of symbols into a number representing the situation
symbols2number <- function(p.crosses, p.circles){
  
  result <- 0
  if(!is.null(p.crosses)){
    for(i in 1:nrow(p.crosses)){
      result <- result + 3^(coordinate2field(p.crosses[i,1], p.crosses[i,2])-1) 
    }
  }
  
  if(!is.null(p.circles)){
    for(i in 1:nrow(p.circles)){
      result <- result + 2 * 3^(coordinate2field(p.circles[i,1], p.circles[i,2])-1) 
    }
  }
  
  return(result)
}

coordinate2field <- function(p.xcoord, p.ycoord){
  
  p.xcoord <- (6 * p.xcoord) %/% 2 + 1
  p.ycoord <- (6 * p.ycoord) %/% 2 + 1
  
  p.ycoord <- 4 - p.ycoord
  
  return(p.xcoord + 3 * (p.ycoord-1))
  
}

field2coordinate <- function(p.field){
  
  x.coord <- p.field %% 3
  if(x.coord == 0){
    x.coord <- 3
  }
  y.coord <- (p.field - x.coord) %/% 3 + 1
  y.coord <- 4 - y.coord
  
  x.coord <- (1 + 2 * (x.coord - 1)) / 6
  y.coord <- (1 + 2 * (y.coord - 1)) / 6
  
  
  return(c(x.coord, y.coord))
}

plot_field <- function(p.crosses, p.circles){
  plot(x = c(0.33, 0.33), y = c(0,1), type = 'l', xlim = c(0,1), ylim = c(0,1), xlab = '', ylab = '', xaxt='n', yaxt='n')
  box(lty = 1, lwd = 2)
  abline(h = 0.33, lwd = 2)
  abline(v = 0.33, lwd = 2)
  abline(h = 0.66, lwd = 2)
  abline(v = 0.66, lwd = 2)
  points(p.crosses, pch = 4, cex = 10, lwd = 5)
  points(p.circles, col = 'red', cex = 10, lwd = 5)
}
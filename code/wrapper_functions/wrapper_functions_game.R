print_situation <- function(p.number){
  
  number2symbole <- function(p.short.number){
    if(p.short.number == 1){
      return('X')
    }
    if(p.short.number == 2){
      return('O')
    }
    if(p.short.number == 10){
      return(' ')
    }
  }
  situation <- number2situation(p.number)
  
  print(paste(number2symbole(situation[1]), '|', number2symbole(situation[2]), '|', number2symbole(situation[3])))
  print(paste('-', '-', '-', '-', '-'))
  print(paste(number2symbole(situation[4]), '|', number2symbole(situation[5]), '|', number2symbole(situation[6])))
  print(paste('-', '-', '-', '-', '-'))
  print(paste(number2symbole(situation[7]), '|', number2symbole(situation[8]), '|', number2symbole(situation[9])))
  
  
}

situation2number = function(p_situation){
  p_situation[which(p_situation==10)] <- 0
  result <- 0
  for(i in 1:9){
    result <- result + p_situation[i] * 3^(i-1) 
  }
  return(result+1)
}

number2situation = function(p_number){
  
  situation <- rep(10,9)
  rest <- p_number-1
  for(i in 1:9){
    situation[i] <- rest %% 3
    rest <- (rest - situation[i])/3
  }
  situation[which(situation==0 | is.na(situation))] <- 10
  
  return(situation)
}

get_allowed_moves = function(p_situation,p_symbole){
  
  empty_fields <- which(p_situation==10)
  allowed_moves <- rep(NULL,length(empty_fields))
  for(i in 1:length(empty_fields)){
    new_situation <- p_situation
    if(p_symbole=='cross'){
      new_situation[empty_fields[i]] <- 1
    }else{
      new_situation[empty_fields[i]] <- 2
    }
    
    allowed_moves[i] <- situation2number(new_situation)
  }
  return(allowed_moves)
  
}
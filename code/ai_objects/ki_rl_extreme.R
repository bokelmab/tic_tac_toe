## required files
source('code/wrapper_functions/wrapper_functions_game.R')

## ai object
ki_rl <- function(){
  
  ki_env <- environment()
  value_function <- rep(1/2, 3^9)
  #game_history <- rep(0,3^9)
  symbole <- 'cross'
  alpha <- 0.7 ## learning rate
  
  output <- list(
    
    env <- ki_env,
    
    make_move = function(p_situation, p_exploit = F){
      
      allowed_moves <- get_allowed_moves(p_situation,get('symbole',ki_env)) ## get all allowed moves
      
      if(p_exploit){
        idx_best <- which(value_function[allowed_moves] == max(value_function[allowed_moves]))
        if(length(idx_best) > 1){
          idx_best <- idx_best[sample(1:length(idx_best),1)]
        }
        return(which(!(p_situation - number2situation(allowed_moves[idx_best]) == 0)))
      }
      ## with 80% probability, chose maximum of value function
      ## with 20% probability, make random move
      if(runif(1)<0.8){
        idx_best <- which(value_function[allowed_moves] == max(value_function[allowed_moves]))
        if(length(idx_best) > 1){
          idx_best <- idx_best[sample(1:length(idx_best),1)]
        }
        return(which(!(p_situation - number2situation(allowed_moves[idx_best]) == 0)))
      }else{
        ## chose move randomly
        return(which(!(p_situation-number2situation(allowed_moves[sample(1:length(allowed_moves),1)])==0)))
      }
      

    },
    
    get_value_function = function(){
      return(get('value_function',ki_env))
    },
    
    update_value_function = function(p_situation, p_previous_situation){
      
      value_function <- get('value_function', ki_env)
      idx_prev <- situation2number(p_previous_situation)
      idx_current <- situation2number(p_situation)
      own_symbole <- get('symbole', ki_env)
      competitor_symbole <- setdiff(c('cross', 'circle'), own_symbole) 
      
      ## update value function of current situation
      if(game$get_finished()){
        if(game$get_winner() == own_symbole){
          value_function[idx_current] <- 1
        }
        if(game$get_winner() == 'tie'){
          value_function[idx_current] <- 1/2
        }
        if(game$get_winner() == competitor_symbole){
          value_function[idx_current] <- 0
        }
      }
      
      ## this is the extreme learning part
      if(value_function[idx_current] == 0){
        value_function[idx_prev] <- 0
      }else{
        value_function[idx_prev] <- value_function[idx_prev] + get('alpha', ki_env) * (value_function[idx_current] - value_function[idx_prev])
      }
      
      assign('value_function', value_function, ki_env)
      
    },
    
    update_symbole = function(p_symbole){
      assign('symbole',p_symbole,ki_env)
    }
    
    
  )
  
  
  
}
## required files
source('code/wrapper_functions/wrapper_functions_game.R')

## ai object
ki <- function(){
  
  ki_env <- environment()
  experience <- matrix(0,nrow=3^9,ncol=3)
  game_history <- rep(0,3^9)
  symbole <- 'cross'
  
  output <- list(
    
    env <- ki_env,
    
    make_move = function(p_situation){
      
      allowed_moves <- get_allowed_moves(p_situation,get('symbole',ki_env))
      score <- rep(1/2,length(allowed_moves))
      experience <- get('experience',ki_env)
      symbole <- get('symbole',ki_env)
      
      for(i in 1:length(allowed_moves)){
        total <- sum(experience[allowed_moves[i],])
        
        ties <- experience[allowed_moves[i],2]
        if(symbole=='cross'){
          succesful <- experience[allowed_moves[i],1]
          lost <- experience[allowed_moves[i],3]
        }else{
          succesful <- experience[allowed_moves[i],3]
          lost <- experience[allowed_moves[i],1]
        }
        
        if(total>50){
          score[i] <- max((ties^2 + succesful^2) / total,0.0001)
        }
        
        
      }
      #idx_max_prob <- which(probabilities==max(probabilities))[as.integer(runif(1,min=1,max=length(which(probabilities==max(probabilities)))+1))]
      idx_move <- sample(1:length(allowed_moves),1,prob=score/sum(score))
      
      #save new situation in game history
      game_history <- get('game_history',ki_env)
      game_history[allowed_moves[idx_move]] <- 1
      assign('game_history',game_history,ki_env)
      
      return(which(!(p_situation-number2situation(allowed_moves[idx_move])==0)))
    },
    
    get_experience = function(){
      return(get('experience',ki_env))
    },
    
    update_experience = function(p_result){
      experience <- get('experience',ki_env)
      game_history <- get('game_history',ki_env)
      #symbole <- get('symbole',ki_env)
      if(p_result=='cross'){
        experience[which(game_history==1),1] <- experience[which(game_history==1),1] + 1
      }else{
        if(p_result=='tie'){
          experience[which(game_history==1),2] <- experience[which(game_history==1),2] + 1
        }else{
          experience[which(game_history==1),3] <- experience[which(game_history==1),3] + 1
        }
          
      }
        
      assign('game_history',rep(0,3^9),ki_env)
      assign('experience',experience,ki_env)
    },
    
    update_symbole = function(p_symbole){
      assign('symbol',p_symbole,ki_env)
    }
    
    
  )
  
  
  
}
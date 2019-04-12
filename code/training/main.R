#required files
source('code/wrapper_functions/wrapper_functions_game.R')
source('code/game_implementation/game.R')
source('code/ai_objects/random_player.R')
source('code/ai_objects/ki.R')
source('code/ai_objects/ki_rl_extreme.R')

score <- c(0,0)
number_games <- 100000
competitor <- ki_rl()
ruediger <- ki_rl()
#ruediger <- readRDS('ruediger.RDS')

## train AI
for(i in 1:number_games){
  #game
  game <- tick_tack_toe()
  
  #assign symbols
  #if(i%%2==0){
    ruediger$update_symbole('cross')
    competitor$update_symbole('circle')
  # }else{
  #   ruediger$update_symbole('circle')
  #   competitor$update_symbole('cross')
  # }
  previous_situation_ruediger <- NULL
  previous_situation_competitor <- NULL
    
  while(!game$get_finished()){
    
    ## Ruediger moves
    game$move(ruediger$make_move(game$get_boxes(), T))
    if(!is.null(previous_situation_ruediger)){
       
    }
    previous_situation_ruediger <- game$get_boxes()
    
    ## competitor reacts
    game$move(competitor$make_move(game$get_boxes(), T)) 
    if(!is.null(previous_situation_competitor)){
      competitor$update_value_function(game$get_boxes(), previous_situation_competitor) ## update experience 
    }
    previous_situation_competitor <- game$get_boxes()
    
    ## update Ruedigers experience if competitor won
    if(game$get_winner()=='circle'){
      ruediger$update_value_function(game$get_boxes(), previous_situation_ruediger) ## update experience
    }
    
  }
  
  #competitor$update_experience(game$get_winner())
  
  #update score
  if(!is.null(game$get_winner())){
    if(game$get_winner()=='cross'){
      score[1] <- score[1] + 1
    }
    if(game$get_winner()=='circle'){
      score[2] <- score[2] + 1
    }
  }
  
  #intermediate results
  if(i%%1000==0){
    print(i)
    print(score)
    print(1000-sum(score))
    print('############')
    score = c(0,0)
    
  }
  
}

saveRDS(ruediger, 'ki_cross.RDS')
saveRDS(competitor, 'ki_circle.RDS')

## real game
game <- tick_tack_toe()
game$move(ruediger$make_move(game$get_boxes()))
game$get_boxes()
game$move(6)
game$move(ruediger$make_move(game$get_boxes()))
game$get_boxes()
game$move(2)
game$move(ruediger$make_move(game$get_boxes()))
game$get_boxes()
game$move(7)
game$move(ruediger$make_move(game$get_boxes()))
game$get_boxes()
game$move(2)
game$get_winner()
ruediger$update_experience(game$get_winner()=='circle')

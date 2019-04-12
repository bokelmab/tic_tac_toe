tick_tack_toe <- function(){
  
  tick_tack_env <- environment()
  boxes <- rep(10,9)
  winner <- 'tie'
  finished <- F
  
  output <- list(
    
    env = tick_tack_env,
    
    move = function(p_box){
      
      #break if game has finished
      if(get('finished',tick_tack_env)){
        return()
      }
      
      #get boxes
      boxes <- get('boxes',tick_tack_env)
      
      #warning if unallowed move
      if(!boxes[p_box]==10){
        print('Unallowed move!',col='red')
        return()
      }
      
      #move
      if(length(which(boxes==10))%%2==1){
        boxes[p_box] <- 1
      }else{
        boxes[p_box] <- 2
      }
      
      #assign boxes
      assign('boxes',boxes,tick_tack_env)
      
      #test whether game finished
      if(sum(boxes[1:3])==3 | sum(boxes[4:6])==3 | sum(boxes[7:9])==3 | (boxes[1]+boxes[4]+boxes[7])==3
         | (boxes[2]+boxes[5]+boxes[8])==3 | (boxes[3]+boxes[6]+boxes[9])==3
         | (boxes[1]+boxes[5]+boxes[9])==3 | (boxes[3]+boxes[5]+boxes[7])==3){
        assign('finished',T,tick_tack_env)
        assign('winner','cross',tick_tack_env)
      }else if(sum(boxes[1:3])==6 | sum(boxes[4:6])==6 | sum(boxes[7:9])==6 | (boxes[1]+boxes[4]+boxes[7])==6
          | (boxes[2]+boxes[5]+boxes[8])==6 | (boxes[3]+boxes[6]+boxes[9])==6
          | (boxes[1]+boxes[5]+boxes[9])==6 | (boxes[3]+boxes[5]+boxes[7])==6){
        assign('finished',T,tick_tack_env)
        assign('winner','circle',tick_tack_env)
      }else if(length(which(boxes==10))==0){
        assign('finished',T,tick_tack_env)
      }
      
    },
    
    get_boxes = function(){
      return(get('boxes',tick_tack_env))
    },
    
    get_winner = function(){
      return(get('winner',tick_tack_env))
    },
    
    get_finished = function(){
      return(get('finished',tick_tack_env))
    }
    
  )
  
}


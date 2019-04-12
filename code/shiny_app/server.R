
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
## libraries
library(shiny)
library(xtable)
library(magrittr)

## source files
source('../wrapper_functions/wrapper_functions_shiny.R')
source('../wrapper_functions/wrapper_functions_game.R')
source('../ai_objects/ki_rl.R')
source('../game_implementation/game.R')

## environment
myenv = environment()
circles <- NULL ## coordinates of circles
crosses <- NULL ## coordinates of crosses
game <- tick_tack_toe() ## game object
ruediger <- NULL
chosen.symbole <- 'empty'

shinyServer(function(input, output, session) {
  
  server.env <- environment() ## get environment name
  
  observeEvent(input$circle,{
    if(input$circle & chosen.symbole == 'empty'){
      assign('chosen.symbole', 'circle', myenv)
      output$board <- renderPlot({
        
        assign('ruediger', readRDS('../../trained_ai/ki_cross.RDS'), myenv) ## load KI player
        field.number <- ruediger$make_move(game$get_boxes(), T)
        game$move(field.number)
        crosses %<>% rbind(field2coordinate(field.number))
        assign('crosses', crosses, server.env)
        
        plot_field(crosses, circles)
      })
    }
    
  })
  observeEvent(input$cross,{
    if(input$cross & chosen.symbole == 'empty'){
      assign('ruediger', readRDS('../../trained_ai/ki_circle.RDS'), myenv) ## load KI player
      assign('chosen.symbole', 'cross', myenv)
    }
  })
  
  ## create board before first click
  output$board <- renderPlot({
    plot_field(crosses, circles)
  })
  
  ### interactive part of game (player makes moves) --------------------
  observeEvent(input$plot_click, {
    
    ## check whether symbol was choosen
    if(xor(input$cross, input$circle )){
      
      ## check whether circle has won
      if(!game$get_finished()){
        
        ### add new coordinates --------------------------------------------
        mouse <- input$plot_click
        mouse.x <- translate.coordinates(mouse$x) ## mid point of chosen box
        mouse.y <- translate.coordinates(mouse$y) ## mid point of chosen box
        
        ## add new symbole to game
        game$move(coordinate2field(mouse.x, mouse.y)) ## the move of the player
        if(chosen.symbole == 'cross'){
          crosses %<>% rbind(c(mouse.x, mouse.y)) ## add mouse coordinates to cross coordinates
        }
        if(chosen.symbole == 'circle'){
          circles %<>% rbind(c(mouse.x, mouse.y)) ## add mouse coordinates to circle coordinates
        }
      }
      
      ## check whether cross has won
      if(!game$get_finished()){
        
        ## add new circle to field
        field.number <- ruediger$make_move(game$get_boxes(), T)
        game$move(field.number)
        if(chosen.symbole == 'circle'){
          crosses %<>% rbind(field2coordinate(field.number))
        }else{
          circles %<>% rbind(field2coordinate(field.number))
        }
        
        ## check whether circle has won
        if(!game$get_winner() %in% c('tie', chosen.symbole)){
          output$request <- renderText({'Sorry, Ruediger beat you!'})
        }
      }else{
        if(game$get_winner() == chosen.symbole){
          output$request <- renderText({'Congratulation, you won!'})
        }
      }
      
      assign('crosses', crosses, server.env)
      assign('circles', circles, server.env)
    }
    
    ## plot board with symbols ------------------------------------------------
    output$board <- renderPlot({
      plot_field(crosses, circles)
    })
    
})
    
  #rsconnect::deployApp('C:/Users/Björn/Documents/Tennis/Hallentraining/Organisation_public')
  observeEvent(input$cross,{
    if(!(input$cross | input$circle) & !game$get_finished()){
      output$request <- renderText({
        'Choose a symbole!'
      })
    }else if(!game$get_finished()){
      output$request <- renderText({''})
    }
    })
  observeEvent(input$circle,{
    if(!(input$cross | input$circle) & !game$get_finished()){
      output$request <- renderText({
        'Choose a symbole!'
      })
    }else if(!game$get_finished()){
      output$request <- renderText({''})
    }
  })
  

})

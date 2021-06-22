source('chess_fun.R')
source('NN_fun.R')
chess.data <- getChessData(num=100000)                                          #Returns processed chess data with valid chess games
positionScores <- getPositionScores(chess.data)                                 #First column is a 1 or 0 for white win or black win. Remaining 64 columns is a Square List board representation. 


input.layer.size <- 64                                                          #Input Layer Size
degrees <- c(2,3,4,5)                                                           #Degree polynomial added to data set
hidden.layer.size <- 64                                                         #Size of each hidden layer in neural network
maxiter =  500                                                                  #Total number of training iterations
lambda = c(0,0.00001,0.00003)                                                   #Regularization values
LRL <- c(0.003,0.001)                                                           #Learning Rate values




model2 <- getModel(positionScores,input.layer.size,degrees,hidden.layer.size,maxiter,lambda,LRL)   #Returns model parameters and model size.


temp_model <- list(model2[[1]],model2[[2]],model2[[3]],3,normalization)
model1 <- list(nn.params,input.layer.size,hidden.layer.size,degree,normalization)

chss <- Chess$new()                                                             #Creates chess environment
game <- list(0.5,chss)
depth=5                                                                        #Depth of search



while(!chss$game_over()){
  
  if(chss$turn()=='w'){
    print("White's Turn")
    alpha <- -10
    beta <- 10
    result <- alphabeta.white(node=chss, depth=depth, maximizingPlayer=T,       #returns move
                              model=temp_model, alpha=alpha, beta=beta)
    chss$move(result[[2]])                                                      #makes moves
    print(paste(result[[2]],result[[1]]))                                       #prints the model's position score and move
    print(chss$plot())                                                          #prints chess board
    
  }else{
    
    print("Black's Turn")
    alpha <- 10
    beta <- -10
    
    result <- alphabeta.black(node=chss, depth=depth, minimizingPlayer=T,
                              model=model2, alpha=alpha, beta=beta)
    chss$move(result[[2]])
    print(paste(result[[2]],result[[1]]))
    print(chss$plot())
  }
  
  
  
  
}


#Whicchss$move("Qe6+hever model wins takes the crown as best model

if(chss$in_checkmate()){
  print("CHECK MATE")
  if(chss$turn == 'w'){
    best.model <- model2
  }else{
    best.model <- model1
  }
  
}else{
  print("DRAW")
}


tic <- Sys.time()
depth=5                                                                        #Depth of search
alpha <- -10
beta <- 10
print(alphabeta.white(node=chss, depth=depth, maximizingPlayer=T,       #returns move
                          model=temp_model, alpha=alpha, beta=beta))
toc <- Sys.time()
print(toc-tic)

chss$move("e4")
chss$plot()
chss$move("e5")
chss$plot()
chss$move("Qg4")
chss$plot()
chss$move("Nf6")
chss$plot()
chss$move("Qg5")
chss$plot()
chss$move("h6")
chss$plot()
chss$move("Qxe5+")
chss$plot()
chss$move("Be7")
chss$plot()
chss$move("Bc4")
chss$plot()
chss$move("O-O")
chss$plot()
chss$move("Nc3")
chss$plot()
chss$move("Nc6")
chss$plot()
chss$move("Qf4")
chss$plot()
chss$move("d6")
chss$plot()
chss$move("Nge2")
chss$plot()
chss$move("Be6")
chss$plot()
chss$move("Bxe6")
chss$plot()
chss$move("fxe6")
chss$plot()
chss$move("Qg3")
chss$plot()
chss$move("e5")
chss$plot()
chss$move("Qg6")
chss$plot()
chss$move("Qe8")
chss$plot()
chss$move("Qf5")
chss$plot()
chss$move("Nd5")
chss$plot()
chss$move("Qe6+")
chss$plot()
chss$move("Kh8")
chss$plot()
chss$move("O-O")
chss$plot()
chss$move("Nxc3")
chss$plot()
chss$move("dxc3")
chss$plot()
chss$move("Qf7")
chss$plot()
chss$move("Qg4")
chss$plot()
chss$move("Rad8")
chss$plot()
chss$move("Be3")
chss$plot()
chss$move("b6")
chss$plot()
chss$move("Ng3")
chss$plot()
chss$move("d5")
chss$plot()

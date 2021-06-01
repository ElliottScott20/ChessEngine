
library(vroom)
library(rchess)
library(dplyr)
library(purrr)

getChessData <- function(num=100000){
  
  head <- 'n date result welo belo len date_c resu_c welo_c belo_c edate_c setup fen resu2_c oyrange bad_len'   #Name of features
  head <- unlist(strsplit(head, split = ' '))                                   #split each word into its own character vector item
  
  df <- vroom('dataset2/all_with_filtered_anotations_since1998.txt',skip=5, delim = '###', col_names = c('header','game'))  #read in chess data set, separate data into all and game feature columns
  
  games <- na.omit(df[1:num,1:2])  #remove na values
  moves <- games$game                                                           #create variable moves from game feature vector
  game <- NULL
  
  print('Cleaning PGN')
  
  for(i in 1:length(moves)){                                                    #for each moves feature instance check if its NA. if not, then split moves into individual character vector items.
    
    if(round(i/length(moves),3)>round((i-1)/length(moves),3)){
      print(paste0((round(i/length(moves),3)*100),' %'))
    }
    
    out <- moves[i]
    if(is.na(out)){
      next
    }
    out <- unlist(strsplit(out, split = ' '))
    for(j in 1:length(out)){                                                    #For each character vector item, separate by '.' and keep the right side, the move. 
      out[j] <- unlist(strsplit(out[j], split='.',fixed=T))[[2]]
    }
    game[i] <- paste0(out,collapse=' ')                                         #Collapse the separate character vector items into a single character string seperated by a space.
  }
  
  games <- unlist(strsplit(games$header,split =" "))                               #split game into separate character vector items.
  games <- as.data.frame(split(games,head))                                     #create data frame with head has column names
  games <- games %>% select(n, date, result, welo, belo, len, date_c, resu_c, welo_c, belo_c, edate_c, setup, fen, resu2_c, oyrange, bad_len)     #Reorder columns
  games$moves <- game                                                           #add moves column with cleaned moves
  
  chss <- Chess$new()                                                           #create chess environment 
  
     
  
  print('Validating Games')
  
  for( i in 1:nrow(games)){
    if(round(i/nrow(games),3)>round((i-1)/nrow(games),3)){
      print(paste0((round(i/nrow(games),3)*100),' %'))
    }
    games[chss$load_pgn(games$moves[i]),]                                       #For each game check if PGN is a valid game of chess
  }                                                   
  
  
  
  
  
  return(games)                                                                 #return cleaned game data
  
}

playRandomMove <- function(chss){
  moves <- chss$moves()
  choice <- moves[round(runif(1,min=1,length(moves)))]
  chss$move(choice)
  return(chss)
}

playRandom <- function(){
  chss <- Chess$new()
  
  while(!chss$in_checkmate()){
    chss <- Chess$new()
    while(!chss$game_over()){
      moves <- chss$moves()
      choice <- moves[round(runif(1,min=1,length(moves)))]
      chss$move(choice)
    }
  }
  
  if(chss$turn()=='b'){
      return(list(1,chss))
    }else{
      return(list(-1,chss))
    }
  
}

getPositionScores <- function(chess.data){
  
  chss <- Chess$new()
  positionScores <- data.frame()
  j=1
  
  while(j <= nrow(chess.data)){
    if(j %% 100 == 0){
      print(j)
    }
    
    if(chess.data$result[j] == '1-0'){
      chss$load_pgn(chess.data$moves[j])
      game <- (list(1,chss))
      positionVector <- getBoardPosition(game)
      score <- game[[1]]
      positionScores <- rbind(positionScores,c(score,positionVector))
    }else if(chess.data$result[j]=='0-1'){
      chss$load_pgn(chess.data$moves[j])
      game <- (list(0,chss))
      positionVector <- getBoardPosition(game)
      score <- game[[1]]
      positionScores <- rbind(positionScores,c(score,positionVector))
    }
    j <- j+1
  }
  
  return(positionScores)
}

getBoardPosition <- function(game){
  board <- c('a1','b1','c1','d1','e1','f1','g1','h1',
             'a2','b2','c2','d2','e2','f2','g2','h2',
             'a3','b3','c3','d3','e3','f3','g3','h3',
             'a4','b4','c4','d4','e4','f4','g4','h4',
             'a5','b5','c5','d5','e5','f5','g5','h5',
             'a6','b6','c6','d6','e6','f6','g6','h6',
             'a7','b7','c7','d7','e7','f7','g7','h7',
             'a8','b8','c8','d8','e8','f8','g8','h8')
  positionVector <- vector()
  
  for(i in 1:length(board)){
    position <- game[[2]]$get(board[i])
    if(is.null(position)){
      positionVector[i] <- 0
    }else if(position$type == 'p'){
      if(position$color == 'w'){
        positionVector[i] <- 1
      }else{
        positionVector[i] <- -1
      }
    }else if(position$type == 'n'){
      if(position$color == 'w'){
        positionVector[i] <- 2
      }else{
        positionVector[i] <- -2
      }
    }else if(position$type == 'b'){
      if(position$color == 'w'){
        positionVector[i] <- 3
      }else{
        positionVector[i] <- -3
      }
    }else if(position$type == 'r'){
      if(position$color == 'w'){
        positionVector[i] <- 4
      }else{
        positionVector[i] <- -4
      }
    }else if(position$type == 'q'){
      if(position$color == 'w'){
        positionVector[i] <- 5
      }else{
        positionVector[i] <- -5
      }
    }else if(position$type == 'k'){
      if(position$color == 'w'){
        positionVector[i] <- 6
      }else{
        positionVector[i] <- -6
      }
    }
  }
  return(positionVector)
}

getPositionEvaluator <- function(chess.data){
  
  source('NN_fun.R')
  
  positionScores <- getPositionScores(chess.data)
  
  white <- positionScores[positionScores[,1]==1,]
  black <- positionScores[positionScores[,1]==0,]
  df1 <- as.data.frame(ones(nrow(black)*2,ncol(black)))
  w <- 1
  b <- 1
  for(i in 1:(nrow(black)*2)){
   
    if(i %% 2 ==0){
      df1[i,] <- black[b,]
      b <- b+1
    }else{
      df1[i,] <- white[w,]
      w <- w+1
    }
  }
  
  
  library(caTools)
  train.split <- sample.split(df1$V1, SplitRatio = 0.6)
   
  train <- subset(df1, train.split==T)
  test.cv <- subset(df1, train.split==F)
  
  test.split <- sample.split(test.cv$V1, SplitRatio = 0.5)
  
  cross.validation <- subset(test.cv, test.split == F)
  test <- subset(test.cv, test.split ==T)
  
  
  X <- as.matrix(train[,c(2:length(train))],rownames.force = F)
  y <- as.matrix(train[,1],rownames.force = F)
  
  
  Xval <- as.matrix(cross.validation[,c(2:length(train))], rownames.force = F)
  yval <- as.matrix(cross.validation[,1], rownames.force = F)
  
  Xtest <- as.matrix(test[,c(2:length(train))],rownames.force = F)
  ytest <- as.matrix(test[,1], rownames.force = F)     
  
  
  
  m = nrow(X)
  
  X <- cbind(ones(nrow(X),1),X)
  Xval <- cbind(ones(nrow(Xval),1),Xval)
  Xtest <- cbind(ones(nrow(Xtest),1),Xtest)
  
  num.labels <- 1
  input.layer.size <- 64
  
  hidden.layer.size <- 128
  maxiter = 3000
  lambda = c(0)
  LRL <- c(0.01,0.03,0.1,0.3)
  
  
  j =1
  results <- list()
  temp <- list()
  
  for(learn.rate in LRL){
    print(j)
    i=1
    for(lam in lambda){
      print(i)
      
      length.Theta.1 <- (input.layer.size +1) * hidden.layer.size
      length.Theta.2 <- length.Theta.1 + ((hidden.layer.size+1)* hidden.layer.size)
      length.Thete.3 <- length.Theta.2 + ((hidden.layer.size + 1) * num.labels)
      
      initial.Theta1 <- matrix(runif(1:length.Theta.1, min = -1 ,max = 1), nrow = hidden.layer.size, ncol = (input.layer.size +1))
      initial.Theta2 <- matrix(runif(length.Theta.1:length.Theta.2, min = -1, max =1), nrow = hidden.layer.size, ncol = (hidden.layer.size+1))
      initial.Theta3 <- matrix(runif(length.Theta.2:length.Thete.3, min = -1, max = 1), nrow = num.labels, ncol = (hidden.layer.size+1))
      
      initial.Theta1.Unrolled <- as.vector(initial.Theta1)
      initial.Theta2.Unrolled <- as.vector(initial.Theta2)
      initial.Theta3.Unrolled <- as.vector(initial.Theta3)
      
      nn.params <- c(initial.Theta1.Unrolled,initial.Theta2.Unrolled,initial.Theta3.Unrolled)
      
      temp[[i]] <- gradientDescent(nn.params,input.layer.size, hidden.layer.size, num.labels, X, y, lam, maxiter,learn.rate)
    }
    results[[j]] <-temp
    temp <-list()
    j <- j+1
  }

  for(i in 1:length(LRL)){
    for(j in 1:length(lambda)){
      print(paste( 'Learning Rate: ', LRL[i],' Lambda', lambda[j], '  Cost: ',min(results[[i]][[j]][[1]])))
    }
  }
  
  nn.params <- results[[1]][[1]][[2]]
  
  Theta1 <- matrix(nn.params[1:length.Theta.1], nrow=hidden.layer.size, ncol= (input.layer.size+1))
  
  Theta2 <- matrix(nn.params[(length.Theta.1+1):length.Theta.2], nrow=hidden.layer.size, ncol = (hidden.layer.size+1))
  
  Theta3 <- matrix(nn.params[(length.Theta.2+1):length.Thete.3], nrow=num.labels, ncol= (hidden.layer.size+1))
  
  prediction <- nn.predict(Theta1, Theta2, Theta3, X)
  
  MSE.nn <- sum((y - prediction)^2)/nrow(y)
  MSE.nn
  table(prediction,y)
  
  error.table <- cbind(y,prediction)
  
  precision <- (sum(y==1 & prediction==1))/(sum(prediction==1))
  precision
  
  recall <- (sum(y==1 & prediction==1))/(sum(y==1))
  recall
  
  f1.score <- 2 * ((precision*recall)/(precision + recall))
  f1.score
  
  
  
  prediction <- nn.predict(Theta1, Theta2, Theta3, Xval)
  
  MSE.nn <- sum((yval - prediction)^2)/nrow(yval)
  MSE.nn
  table(prediction,yval)
  
  error.table <- cbind(yval,prediction)
  
  precision <- (sum(yval==1 & prediction==1))/(sum(prediction==1))
  precision
  
  recall <- (sum(yval==1 & prediction==1))/(sum(yval==1))
  recall
  
  f1.score <- 2 * ((precision*recall)/(precision + recall))
  f1.score
  
  
  
  prediction <- nn.predict(Theta1, Theta2, Theta3, Xtest)
  
  MSE.nn <- sum((ytest - prediction)^2)/nrow(ytest)
  MSE.nn
  table(prediction,ytest)
  
  error.table <- cbind(ytest,prediction)
  
  precision <- (sum(ytest==1 & prediction==1))/(sum(prediction==1))
  precision
  
  recall <- (sum(ytest==1 & prediction==1))/(sum(ytest==1))
  recall
  
  f1.score <- 2 * ((precision*recall)/(precision + recall))
  f1.score
  
  
  errors <- learningCurve(X,y,Xval,yval,lambda,input.layer.size,num.labels,hidden.layer.size)
  
  return(nn.params)
  
  
  
}  
  
evalutate <- function(model, position){
  
  nn.params <- model
  
  num.labels <- 1
  input.layer.size <- 64
  hidden.layer.size <- 128
  
  length.Theta.1 <- (input.layer.size +1) * hidden.layer.size
  length.Theta.2 <- length.Theta.1 + ((hidden.layer.size+1)* hidden.layer.size)
  length.Thete.3 <- length.Theta.2 + ((hidden.layer.size + 1) * num.labels)
  
  Theta1 <- matrix(nn.params[1:length.Theta.1], nrow=hidden.layer.size, ncol= (input.layer.size+1))
  Theta2 <- matrix(nn.params[(length.Theta.1+1):length.Theta.2], nrow=hidden.layer.size, ncol = (hidden.layer.size+1))
  Theta3 <- matrix(nn.params[(length.Theta.2+1):length.Thete.3], nrow=num.labels, ncol= (hidden.layer.size+1))
  
  m <- 1
  
  p <- zeros(1,1)
  position <- c(1,position)
  
  
  a1 <- position
  
  Z2 <- a1 %*% t(Theta1)
  a2 <- sig(Z2)
  a2 <- cbind(ones(nrow(a2),1),a2)
  
  Z3 <- a2 %*% t(Theta2)
  a3 <- sig(Z3)
  a3 <- cbind(ones(nrow(a3),1),a3)
  
  Z4 <- a3 %*% t(Theta3)
  a4 <- sig(Z4)
  
  H <- a4
  
  return(H)
  
}















































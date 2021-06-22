
library(vroom)
library(rchess)
library(dplyr)
library(purrr)

getChessData <- function(num=100000){
  
  head <- 'n date result welo belo len date_c resu_c welo_c belo_c edate_c setup fen resu2_c oyrange bad_len'   #Name of features
  head <- unlist(strsplit(head, split = ' '))                                   #split each word into its own character vector item
  
  df <- vroom('dataset2/all_with_filtered_anotations_since1998.txt',skip=5,     #read in chess data set, separate data into all and game feature columns
              delim = '###', col_names = c('header','game'))  
  
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
  
  games <- unlist(strsplit(games$header,split =" "))                            #split game into separate character vector items.
  games <- as.data.frame(split(games,head))                                     #create data frame with head has column names
  games <- games %>% select(n, date, result, welo, belo, len, date_c,           #Reorder columns
                            resu_c, welo_c, belo_c, edate_c, setup, fen,
                            resu2_c, oyrange, bad_len)     
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
  #Plays a random chess moves. Returns chess environment.
  
  
  moves <- chss$moves()                                                         #array of available legal moves
  choice <- moves[round(runif(1,min=1,length(moves)))]                          #randomly chooses a move from moves
  chss$move(choice)                                                             #moves chosen piece
  return(chss)                                                                  #returns entire chess environment
}

playRandom <- function(){
  #Plays chess with random moves. Will only return a game that ends in a 
  #checkmate. Will restart if ends in draw. Returns 1 or 0 for white win
  #black win and chess environment.
  
  
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
      return(list(0,chss))
    }
  
}

getPositionScores <- function(chess.data){
  #returns position vectors wtih either a 1 or 0 at the front of the vector
  #indicating a white win or black win.
  
  
  chss <- Chess$new()
  positionScores <- data.frame()
  j=1
  
  while(j <= nrow(chess.data)){
    if(j %% 100 == 0){
      #updates progress every 100 iterations
      print(j)
    }
    
    if(chess.data$result[j] == '1-0'){
      #white won
      chss$load_pgn(chess.data$moves[j])
      game <- (list(1,chss))
      positionVector <- getBoardPosition(game)                                  #returns a 1X64 length vector representing the chess board.
      score <- game[[1]]
      positionScores <- rbind(positionScores,c(score,positionVector))
    }else if(chess.data$result[j]=='0-1'){
      #black won
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
  
  #returns a 1x64 vector representing the chess board. Each piece if given a value.
  #White pieces are positive values and black pieces are negative values. An empty
  #square is zero.
  
  board <- c('a1','b1','c1','d1','e1','f1','g1','h1',
             'a2','b2','c2','d2','e2','f2','g2','h2',
             'a3','b3','c3','d3','e3','f3','g3','h3',
             'a4','b4','c4','d4','e4','f4','g4','h4',
             'a5','b5','c5','d5','e5','f5','g5','h5',
             'a6','b6','c6','d6','e6','f6','g6','h6',
             'a7','b7','c7','d7','e7','f7','g7','h7',
             'a8','b8','c8','d8','e8','f8','g8','h8')
  
  positionVector <- vector()
  
  #Traverses the board and adds piece values into vector called positionVector.
  
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

getModel <- function(positionScores,input.layer.size,degrees,hidden.layer.size,maxiter,lambda,LRL){
  
  #Returns a board evaluation model. The model parameters are nn.params, 
  #with input.layer.size and hidden.layer.size also returned. input.layer.size is dependent on 
  #degrees which affects what polynomial degree is added to the data set. This is important information
  #for fitting together nn.params.
  
  source('NN_fun.R')                                                            #Sources a bunch of Neural Network functions used to create the model
  
  white <- positionScores[positionScores[,1]==1,]
  black <- positionScores[positionScores[,1]==0,]
  df1 <- as.data.frame(ones(nrow(black)*2,ncol(black)))
  w <- 1
  b <- 1
  
  print("Getting White and Black Wins")
  
  #Due to the data being largely white wins, the model was having a hard time predicting black wins. This 
  #loop iterates between black and white and so there is an even split. 
  
  for(i in 1:(nrow(black)*2)){
   
    if(i %% 2 ==0){
      df1[i,] <- black[b,]
      b <- b+1
    }else{
      df1[i,] <- white[w,]
      w <- w+1
    }
  }
  
 
  
  z <- 1
  result <- list()
  
  df2 <- df1
  best <- list(0,NULL,NULL,NULL)
  
  for(degree in degrees){                                                       #Each degree polynomial is tested for the best F1 Score
    
    print(z)
    print(degree)
    
    df1 <- df2
    
    print("Getting Polynomials")
    
    df1 <- cbind(df1,getPolynomials(df1[,2:ncol(df1)], degree))                 #returns polynomial of specified degree.
    
    print("Splitting Data")
    
    library(caTools)                                                            #splits data into 60% training data, 20% cross validating data, and 20% testing data 
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
    
    X.mu <- NULL
    X.max <- NULL
    X.min <- NULL
    
    #Normalization of data
    
    for( i in 1:ncol(X)){
      X.mu[i] <- mean(X[,i])
      X.max[i] <- max(X[,i])
      X.min[i] <- min(X[,i])
      
      X[,i] <- (X[,i]-X.mu[i])/(X.max[i]-X.min[i])
      Xval[,i] <- (Xval[,i]-X.mu[i])/(X.max[i]-X.min[i])
      Xtest[,i] <- (Xtest[,i]-X.mu[i])/(X.max[i]-X.min[i])
    }
    
    normalization <- list(X.mu,X.max,X.min)
    
    m <- nrow(X)
    
    #Adds bias unit
    
    X <- cbind(ones(nrow(X),1),X)
    Xval <- cbind(ones(nrow(Xval),1),Xval)
    Xtest <- cbind(ones(nrow(Xtest),1),Xtest)
    
    num.labels <- 1
    input.layer.size <- 64 * degree
    
    hidden.layer.size <- hidden.layer.size
    maxiter <- maxiter
    lambda <- lambda
    LRL <- LRL
    
    
    # errors <- stochasticLearningCurve(X,y,Xval,yval,lambda,input.layer.size,num.labels,hidden.layer.size,maxiter,LRL)
    
    # errors <- learningCurve(X,y,Xval,yval,lambda,input.layer.size,num.labels,hidden.layer.size)
    
    
    j <- 1
    temp1 <- list()
    temp2 <- list()
    
    print("Starting Training")
    
    for(learn.rate in LRL){                                                    #Each learning rate is tested for best F1 Score
      print(j)
      i<-1
      for(lam in lambda){                                                       #Each lambda regularization variable it tested for best F1 Score
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
        
        
        #temp1 stochasticGradientDescent returns the trained nn.params and a loss
        temp1 <- stochasticGradientDescent(nn.params,input.layer.size, hidden.layer.size, num.labels, X, y, lam, maxiter,learn.rate)
        nn.params <- temp1[[2]]
        
        #nn.params is rebuilt for predictions
        
        Theta1 <- matrix(nn.params[1:length.Theta.1], nrow=hidden.layer.size, ncol= (input.layer.size+1))
        Theta2 <- matrix(nn.params[(length.Theta.1+1):length.Theta.2], nrow=hidden.layer.size, ncol = (hidden.layer.size+1))
        Theta3 <- matrix(nn.params[(length.Theta.2+1):length.Thete.3], nrow=num.labels, ncol= (hidden.layer.size+1))
        
        
        prediction <- nn.predict(Theta1, Theta2, Theta3, Xval)                  #returns a value between 0,1. 0 for black is win, 1 for white win
        
        MSE.nn <- sum((yval - prediction)^2)/nrow(yval)
        MSE.nn
        table(prediction,yval)
        
        precision <- (sum(yval==1 & prediction==1))/(sum(prediction==1))
        precision
        
        recall <- (sum(yval==1 & prediction==1))/(sum(yval==1))
        recall
        
        f1.score <- 2 * ((precision*recall)/(precision + recall))
        f1.score
        
        if(f1.score > best[[1]]){
          #if the F1 Score is better than the previous best, F1 Score becomes the new best.
          print(paste("New Best F1.Score: ",f1.score))
          best <- list(f1.score, nn.params,input.layer.size,hidden.layer.size,degree,normalization)      # information about the reconstruction of nn.params is recorded in best.
        }
        
      }
      
      j <- j+1
    }
    
    z <- z+1
    
  }
  
  
  
  # prediction <- nn.predict(Theta1, Theta2, Theta3, X)
  # 
  # MSE.nn <- sum((y - prediction)^2)/nrow(y)
  # MSE.nn
  # table(prediction,y)
  # 
  # error.table <- cbind(y,prediction)
  # 
  # precision <- (sum(y==1 & prediction==1))/(sum(prediction==1))
  # precision
  # 
  # recall <- (sum(y==1 & prediction==1))/(sum(y==1))
  # recall
  # 
  # f1.score <- 2 * ((precision*recall)/(precision + recall))
  # f1.score
  

  # prediction <- nn.predict(Theta1, Theta2, Theta3, Xtest)
  # 
  # MSE.nn <- sum((ytest - prediction)^2)/nrow(ytest)
  # MSE.nn
  # table(prediction,ytest)
  # 
  # error.table <- cbind(ytest,prediction)
  # 
  # precision <- (sum(ytest==1 & prediction==1))/(sum(prediction==1))
  # precision
  # 
  # recall <- (sum(ytest==1 & prediction==1))/(sum(ytest==1))
  # recall
  # 
  # f1.score <- 2 * ((precision*recall)/(precision + recall))
  # f1.score
  
  #After testing each degree,learning rate, and lambda the best values are extracted and returned.
  
  nn.params <- best[[2]]
  input.layer.size <- best[[3]]
  hidden.layer.size <- best[[4]]
  degree <- best[[5]]
  normalization <- best[[6]]
  
  
  return(list(nn.params,input.layer.size,hidden.layer.size,degree,normalization))
  
  
  
}  
  
evaluate <- function(model, position){
  
  #Returns the predicted heuristic value of the position via the model
  
  print(rev(position))
  
  #Extracts information from model to rebuild nn.params
  nn.params <- model[[1]]
  input.layer.size <- model[[2]]
  hidden.layer.size <- model[[3]]
  degree <- model[[4]]
  normalization <- model[[5]]
  
  num.labels <- 1
  
  X.mu <- normalization[[1]]
  X.max <- normalization[[2]]
  X.min <- normalization[[3]]
  
  #gets polynomials of new position
  position <- append(position,getPolynomials(position, degree))
  
  
  
  #normalizes data of new position
  for( i in 1:length(position)){
    position[i] <- (position[i]-X.mu[i])/(X.max[i]-X.min[i])
  }
  

  #Rebuilds 
  
  length.Theta.1 <- (input.layer.size +1) * hidden.layer.size
  length.Theta.2 <- length.Theta.1 + ((hidden.layer.size+1)* hidden.layer.size)
  length.Thete.3 <- length.Theta.2 + ((hidden.layer.size + 1) * num.labels)
  
  Theta1 <- matrix(nn.params[1:length.Theta.1], nrow=hidden.layer.size, ncol= (input.layer.size+1))
  Theta2 <- matrix(nn.params[(length.Theta.1+1):length.Theta.2], nrow=hidden.layer.size, ncol = (hidden.layer.size+1))
  Theta3 <- matrix(nn.params[(length.Theta.2+1):length.Thete.3], nrow=num.labels, ncol= (hidden.layer.size+1))

  
  p <- zeros(1,1)
  position <- c(1,position)
  
  #Begins prediction
  
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
  
  #Returns prediction
  
  return(H)
  
}

alphabeta.white <- function(node, depth, maximizingPlayer, model, alpha, beta){
  
  #Alpha Beta Pruning Search of chess moves. Uses the 
  #model to evaluate positions and prunes positions which are unfavorable from search tree.
  
  if(depth == 0){
    positionScore <- qsearch.white(node,alpha,beta,model)   #qsearch searches specific moves from last position to prevent the horizon effect
    
    return(c(positionScore,node))                           #returns evaluation of position
    
  }
  
  
  #Sets positionScore to worst possible score
  
  if(maximizingPlayer){
    positionScore <- -10
  }else{
    positionScore <- 10
  }
  action.target <-""
  
  
  moves <- node$moves()
  mates <- NULL
  captures <- NULL
  promotion <- NULL
  check <- NULL
  other<- NULL
  
  #Move ordering helps alphabeta pruning discover the most promising positions 
  #sooner rather than searching through loads of bad positions first.
  #Moves are ordered as follows: Captures, promotions, checks, then other.
  #Each position in each category is evaluated and sorted within the category only.
  
  for(move in moves){
    if(length(move[grepl('#',move)])>0){
      node$move(move)
      game <- list(0.5,node)
      position <- getBoardPosition(game)
      mates[move] <- 1
      node$undo()
    }else if(length(move[grepl('x',move)])>0){
      node$move(move)
      game <- list(0.5,node)
      position <- getBoardPosition(game)
      captures[move] <- evaluate(model,position)
      node$undo()
    }else if(length(move[grepl('=',move)])>0){
      node$move(move)
      game <- list(0.5,node)
      position <- getBoardPosition(game)
      promotion[move] <- evaluate(model,position)
      node$undo()
    }else if(length(move[grepl(as.character("\\+"),move)])>0){
      node$move(move)
      game <- list(0.5,node)
      position <- getBoardPosition(game)
      check[move] <- evaluate(model,position)
      node$undo()
    }else{
      node$move(move)
      game <- list(0.5,node)
      position <- getBoardPosition(game)
      other[move] <- evaluate(model,position)
      node$undo()
    }
  }
  
  mates <- sort(mates, decreasing=F)
  captures <- sort(captures, decreasing=F)
  promotion <- sort(promotion, decreasing=F)
  check <- sort(check,decreasing=F)
  other <- sort(other,decreasing=F)
  
  
  #The sorted moves are stored in moves
  
  moves <- c(mates,captures,promotion,check,other)
  
  #Each move is evaluated to a set depth.
  for(move in names(moves)){
    
    child <- node$move(move)
    child.state <- alphabeta.white(node=child, depth=depth-1, maximizingPlayer=(!maximizingPlayer), model=model, alpha=alpha, beta=beta)
    
    if(maximizingPlayer && (positionScore < child.state[[1]])){
      positionScore <- child.state[[1]]
      action.target <- move
      if(positionScore>alpha){
        alpha <- positionScore
      }
      if(beta <= alpha){
        node$undo()
        break
      }
    }else if(!maximizingPlayer && (positionScore > child.state[[1]])){
      positionScore <- child.state[[1]]
      action.target <- move
      if(positionScore<beta){
        beta <- positionScore
      }
      if(beta <=alpha){
        node$undo()
        break
      }
    }
    node$undo()
  }
  
  return (c(positionScore,action.target))
  
}

alphabeta.black <- function(node, depth, minimizingPlayer, model, alpha, beta){
  
  #This is the same function as alphabeta.white but with the inequalities flipped.
  
  if(depth == 0){
    
    positionScore <- qsearch.black(node,alpha,beta,model)
    
    return(c(positionScore,node))
    
  }
  
  if(minimizingPlayer){
    positionScore <- 10
  }else{
    positionScore <- -10
  }
  action.target <-""
  
  moves <- node$moves()
  captures <- NULL
  promotion <- NULL
  check <- NULL
  other<- NULL
  
  for(move in moves){
    if(length(move[grepl('x',move)])>0){
      node$move(move)
      game <- list(0.5,node)
      position <- getBoardPosition(game)
      captures[move] <- evaluate(model,position)
      node$undo()
    }else if(length(move[grepl('=',move)])>0){
      node$move(move)
      game <- list(0.5,node)
      position <- getBoardPosition(game)
      promotion[move] <- evaluate(model,position)
      node$undo()
    }else if(length(move[grepl(as.character("\\+"),move)])>0){
      node$move(move)
      game <- list(0.5,node)
      position <- getBoardPosition(game)
      check[move] <- evaluate(model,position)
      node$undo()
    }else{
      node$move(move)
      game <- list(0.5,node)
      position <- getBoardPosition(game)
      other[move] <- evaluate(model,position)
      node$undo()
    }
  }
  
  captures <- sort(captures, decreasing=F)
  promotion <- sort(promotion, decreasing=F)
  check <- sort(check,decreasing=F)
  other <- sort(other,decreasing=F)
  
  moves <- c(captures,promotion,check,other)
  print("Sorted Moves: ")
  print(moves)
  
  
  for(move in names(moves)){
    child <- node$move(move)
    child.state <- alphabeta.black(node=child, depth=depth-1, minimizingPlayer=(!minimizingPlayer), model=model, alpha=alpha, beta=beta)
    
    if(minimizingPlayer && (positionScore > child.state[[1]])){
      positionScore <- child.state[[1]]
      action.target <- move
      if(positionScore>alpha){
        alpha <- positionScore
      }
      if(beta >= alpha){
        node$undo()
        break
      }
    }else if(!minimizingPlayer && (positionScore < child.state[[1]])){
      positionScore <- child.state[[1]]
      action.target <- move
      if(positionScore>beta){
        beta <- positionScore
      }
      if(beta >=alpha){
        node$undo()
        break
      }
    }
    node$undo()
  }
  
  return (c(positionScore,action.target))
  
}

qsearch.white <- function(node,alpha,beta,model){
  # qsearch prevents the 'horizon' effect. Via chessprogramming.org: The Horizon 
  # Effect is caused by the depth limitation of the search algorithm, and became 
  # manifest when some negative event is inevitable but postponable. Because only 
  # a partial game tree has been analyzed, it will appear to the system that the 
  # event can be avoided when in fact this is not the case.
  
  
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  
  game <- list(0.5,node)
  position <- getBoardPosition(game)
  stand.pat <- evaluate(model,position)
  
  if(stand.pat >= beta){
    return(beta)
  }
  
  if(alpha < stand.pat){
    alpha <- stand.pat
  }
  
  for(capture in node$moves()[grepl('x',node$moves())]){
    
    node$move(capture)
    
    tmp1 <- (0-1)*(beta)
    tmp2 <- (0-1)*(alpha)
    
    score <- -qsearch.white(node,tmp1,tmp2,model)
    node$undo()
    
    if(score >= beta){
      return(beta)
    }
    
    if(score > alpha){
      alpha <- score
    }
  }
  return(alpha)
  
}

qsearch.black <- function(node,alpha,beta,model){
  
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  
  game <- list(0.5,node)
  position <- getBoardPosition(game)
  stand.pat <- evaluate(model,position)
  
  if(stand.pat <= beta){
    return(beta)
  }
  
  if(alpha > stand.pat){
    alpha <- stand.pat
  }
  
  for(capture in node$moves()[grepl('x',node$moves())]){
    
    node$move(capture)
    
    tmp1 <- (0-1)*(beta)
    tmp2 <- (0-1)*(alpha)
    
    score <- -qsearch.black(node,tmp1,tmp2,model)
    node$undo()
    
    if(score <= beta){
      return(beta)
    }
    
    if(score < alpha){
      alpha <- score
    }
  }
  return(alpha)
  
}






































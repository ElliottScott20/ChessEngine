library(ggplot2)
library(dplyr)
library(plotly)
########## FUNCTIONS ###########################################################



ones <- function(height,width){
  return(matrix(1,nrow = height,ncol=width))
}

zeros <- function(height,width){
  return(matrix(0,nrow=height,ncol=width))
}

featureNormalize <- function(X){
  
  for(feature in 1:ncol(X)){
    X[,feature] <- (X[,feature]- mean(X[,feature]))/(max(X[,feature])-min(X[,feature]))
  }
  
  return(X)
  
}

nnCostFunction <- function(nn.params,input.layer.size, hidden.layer.size, num.labels, X,y,lambda){
  
  
  length.Theta.1 <- (input.layer.size +1) * hidden.layer.size
  length.Theta.2 <- length.Theta.1 + ((hidden.layer.size+1)* hidden.layer.size)
  length.Thete.3 <- length.Theta.2 + ((hidden.layer.size + 1) * num.labels)
  
  
  Theta1 <- matrix(nn.params[1:length.Theta.1], nrow=hidden.layer.size, ncol= (input.layer.size+1))
  
  Theta2 <- matrix(nn.params[(length.Theta.1+1):length.Theta.2], nrow=hidden.layer.size, ncol = (hidden.layer.size+1))
  
  Theta3 <- matrix(nn.params[(length.Theta.2+1):length.Thete.3], nrow=num.labels, ncol= (hidden.layer.size+1))
  
  if(is.null(nrow(X))){
    m <- 1
  }else{
    m <- nrow(X)
  }
  
  
  J <- 0;
  
  Theta1.grad <- zeros(nrow(Theta1),ncol(Theta1))
  Theta2.grad <- zeros(nrow(Theta2),ncol(Theta2))
  Theta3.grad <- zeros(nrow(Theta3),ncol(Theta3))
  # FEED FORWARD PROPAGATION
  
  a1 <- X
  
  Z2 <- a1 %*% t(Theta1)
  a2 <- sig(Z2)
  a2 <- cbind(ones(nrow(a2),1),a2)
  
  Z3 <- a2 %*% t(Theta2)
  a3 <- sig(Z3)
  a3 <- cbind(ones(nrow(a3),1),a3)
  
  Z4 <- a3 %*%t(Theta3)
  a4 <-sig(Z4)
  
  H <- a4
  
  
  #COST FUNCTION
  
  yVec <- YVEC(num.labels,y)
  
  
  #H is a 15167 x 1 vector
  #y is a 15167 x 1
  
  temp1 <- (-yVec * log(H))
  
  if(log(1-H) == -Inf){
    temp2 <- (1-yVec)*-99999
  }else{
    temp2 <- (1-yVec)*log(1-H)
  }
  
  
  
  
  temp3 <- temp1-temp2
  if(is.null(ncol(temp3))){
    temp4 <- sum(temp3)
  }else{
    temp4 <- colSums(temp3)
  }
  
  temp5 <- sum(temp4)
  J <- (1/m) * temp5
  
  
  J
  
  # J = (1/m) * sum(sum((-yVec * log(h_of_x)) - (1-yVec)*log(1-h_of_x)));
  
  
  #Regularization
  
  temp1 <-(lambda/(2*m))
  
  temp2 <- colSums(Theta1[,2:ncol(Theta1)]^2)
  temp3 <- sum(temp2)
  
  if(nrow(Theta2)!= 1){
    temp4 <- colSums(Theta2[,2:ncol(Theta2)]^2)
  }else{
    temp4 <- sum(Theta2[,2:ncol(Theta2)]^2)
  }
  
  temp5 <- sum(temp4)
  
  if(nrow(Theta3)!= 1){
    temp6 <- colSums(Theta3[,2:ncol(Theta3)]^2)
  }else{
    temp6 <- sum(Theta3[,2:ncol(Theta3)]^2) 
  }
  temp7 <- sum(temp6)
  
  
  
  
  J <- J + temp1 * (temp3 + temp5 +temp7) 
  
  J
  
  
  # J <- J + (lambda/(2*m)) * (sum(sum(Theta1[,2:ncol(Theta1)]^2)) + sum(sum(Theta2[,2:ncol(Theta2)]^2)) )
  
  # + sum(sum(Theta3[,2:ncol(Theta3)]^2)))
  
  #Back Propagation
  
  delta4 <- a4 - yVec
  
  delta3 <- (delta4 %*% Theta3) * cbind(ones(nrow(Z3),1), sigmoidGradient(Z3))
  delta3 <- delta3[,2:ncol(delta3)]
  
  delta2 <- (delta3 %*% Theta2) * cbind(ones(nrow(Z2),1), sigmoidGradient(Z2))
  
  
  
  if(is.null(ncol(a1))){
    delta2 <- delta2[2:length(delta2)]
    Theta1.grad = ((1/m) * (delta2 %*% t(a1))) + (lambda/m) * cbind(zeros(nrow(Theta1),1),Theta1[,2:ncol(Theta1)])
  }else{
    delta2 <- delta2[,2:ncol(delta2)]
    Theta1.grad = ((1/m) * (t(delta2) %*% a1)) + (lambda/m) * cbind(zeros(nrow(Theta1),1),Theta1[,2:ncol(Theta1)])
    
  }
  
  
  
  
  if(nrow(Theta2)!=1){
    if(is.null(ncol(delta3))){
      Theta2.grad = ((1/m) * (delta3 %*% a2)) + (lambda/m) * cbind(zeros(nrow(Theta2),1),Theta2[,2:ncol(Theta2)])
    }else{
      Theta2.grad = ((1/m) * (t(delta3) %*% a2)) + (lambda/m) * cbind(zeros(nrow(Theta2),1),Theta2[,2:ncol(Theta2)])
    }
    
  }else{
    if(is.null(ncol(delta3))){
      Theta2.grad = ((1/m) * (delta3 %*% a2)) + (lambda/m) * cbind(zeros(nrow(Theta2),1),Theta2[,2:ncol(Theta2)])
    }else{
      Theta2.grad = ((1/m) * (t(delta3) %*% a2)) + (lambda/m) * cbind(zeros(nrow(Theta2),1),Theta2[,2:ncol(Theta2)])
    }
  }
  
  if(nrow(Theta3)!=1){
    Theta3.grad = ((1/m) * (t(delta4) %*% a3)) + (lambda/m) * cbind(zeros(nrow(Theta3),1),Theta3[,2:ncol(Theta3)])
  }else{
    Theta3.grad = ((1/m) * (t(delta4) %*% a3)) + (lambda/m) * c(0,Theta3[,2:ncol(Theta3)])
  }
  
  
  
  Theta1.grad.unrolled <- as.vector(Theta1.grad)
  Theta2.grad.unrolled <- as.vector(Theta2.grad)
  Theta3.grad.unrolled <- as.vector(Theta3.grad)
  
  
  grad <- c(Theta1.grad.unrolled,Theta2.grad.unrolled,Theta3.grad.unrolled)
  
  J_grad <- list('J'=J, 'grad'=grad)
  
  
  return(J_grad)
  
  
}

YVEC <- function(num.labels,y){
  out <- matrix(data=NA, nrow=length(y), ncol = num.labels)
  for(i in 1:length(y)){
    for(j in 1:num.labels){
      if(y[i]==j){
        out[i,j] <- 1
      }else{
        out[i,j] <- 0
      }
    }
  }
  # out
  return(out)
}

sig <- function(Z){
  g = 1/(1+exp(-Z))
  return(g)
}

sigmoidGradient <- function(Z){
  return(sig(Z) * (1-sig(Z)))
}

checkNNGradients <- function(lambda){
  input.layer.size <-3
  hidden.layer.size <-5
  num.labels <- 2
  m <- 5
  
  Theta1 <- debugInitializeWeights(hidden.layer.size, input.layer.size)
  Theta2 <- debugInitializeWeights(hidden.layer.size, hidden.layer.size)
  Theta3 <- debugInitializeWeights(num.labels, hidden.layer.size)
  
  X <- debugInitializeWeights(m, input.layer.size-1)
  X <- cbind(ones(nrow(X),1),X)
  y <- 1+ (1:m %% num.labels)
  
  Theta1_unrolled <- as.vector(Theta1)
  Theta2_unrolled <- as.vector(Theta2)
  Theta3_unrolled <- as.vector(Theta3)
  
  nn.params <- c(Theta1_unrolled,Theta2_unrolled,Theta3_unrolled)
  
  cost.grad <- nnCostFunction(nn.params,input.layer.size,hidden.layer.size,num.labels,X,y,lambda)
  num.grad <- computeNumericalGradient(nn.params,input.layer.size,hidden.layer.size,num.labels,X,y,lambda)
  df <- cbind(cost.grad[[2]],num.grad)
  print(df)
  avg.diff <- mean(cost.grad[[2]] -num.grad)
  print(paste("Average difference between calculated gradient and numerical gradient: ",avg.diff))
}

debugInitializeWeights <- function(fan.out, fan.in){
  
  W = zeros(fan.out, 1+fan.in)
  W = matrix(sin(1:length(W)),dim(W))/10
  return(W)
}

computeNumericalGradient <- function(theta,input.layer.size,hidden.layer.size,num.labels,X,y,lambda){
  numgrad <- zeros(length(theta),1)
  perturb <- zeros(length(theta),1)
  e <- 1*10^-4
  
  for(p in 1:length(theta)){
    perturb[p] <- e
    loss1 <- nnCostFunction(theta-perturb,input.layer.size,hidden.layer.size,num.labels,X,y,lambda)
    loss2 <- nnCostFunction(theta+perturb,input.layer.size,hidden.layer.size,num.labels,X,y,lambda)
    numgrad[p] = (loss2[[1]]-loss1[[1]])/(2*e)
    perturb[p]<- 0
  }
  
  return(numgrad)
}

gradientDescent <- function(nn.params,input.layer.size, hidden.layer.size, num.labels, X, y, lambda, maxiter, learn.rate){
  
  J.History <- zeros(maxiter,1)
  
  for(iter in 1:maxiter){
    J_grad <- nnCostFunction(nn.params,input.layer.size,hidden.layer.size,num.labels,X,y,lambda)
    m <- nrow(X)
    J.History[iter] <- J_grad[[1]]
    grad <- J_grad[[2]]
    nn.params <- nn.params - (learn.rate * grad)
    
    print(paste('Iteration',paste(iter,paste('    Cost: ',J.History[iter]))))
  }
  print(paste('Final Cost: ', J.History[maxiter]))
  #plot(J.History[(nrow(J.History)-100):nrow(J.History)])
  
  results <- list(J.History,nn.params)
  
  return(results)
}

nn.predict <- function(Theta1, Theta2, Theta3, X){
  
  m <- nrow(X)
  num.labels <- 2
  
  p <- zeros(nrow(X),1)
  
  a1 <- X
  
  Z2 <- a1 %*% t(Theta1)
  a2 <- sig(Z2)
  a2 <- cbind(ones(nrow(a2),1),a2)
  
  Z3 <- a2 %*% t(Theta2)
  a3 <- sig(Z3)
  a3 <- cbind(ones(nrow(a3),1),a3)
  
  Z4 <- a3 %*% t(Theta3)
  a4 <- sig(Z4)
  
  H <- a4
  
  
  for(i in 1:nrow(H)){
    if(H[i]<0.5){
      p[i] <- 0
    }else{
      p[i] <- 1
    }
  }
  return(p)
}

learningCurve <- function(X,y,Xval,yval,lambda,input.layer.size,num.labels,hidden.layer.size){
  
  m <- nrow(X)
  step = 25
  
  length.Theta.1 <- (input.layer.size +1) * hidden.layer.size
  length.Theta.2 <- length.Theta.1 + ((hidden.layer.size+1)* hidden.layer.size)
  length.Thete.3 <- length.Theta.2 + ((hidden.layer.size + 1) * num.labels)
  
  initial.Theta1 <- matrix(runif(1:length.Theta.1, min = -1, max = 1), nrow = hidden.layer.size, ncol = (input.layer.size +1))
  initial.Theta2 <- matrix(runif(length.Theta.1:length.Theta.2, min = -1, max =1), nrow = hidden.layer.size, ncol = (hidden.layer.size+1))
  initial.Theta3 <- matrix(runif(length.Theta.2:length.Thete.3, min = -1, max = 1), nrow = num.labels, ncol = (hidden.layer.size+1))
  
  initial.Theta1.Unrolled <- as.vector(initial.Theta1)
  initial.Theta2.Unrolled <- as.vector(initial.Theta2)
  initial.Theta3.Unrolled <- as.vector(initial.Theta3)
  
  nn.params <- c(initial.Theta1.Unrolled,initial.Theta2.Unrolled,initial.Theta3.Unrolled)
  
  
  error_train <- zeros(m/step,1)
  error_val <- zeros(m/step,1)
  i=1
  j=1
  while(i <= m){
    
    print(i)
    print(j)
    
    X_train = X[1:i,]
    y_train = y[1:i,]
    
    
    J_grad <- gradientDescent(nn.params,input.layer.size, hidden.layer.size, num.labels, X_train, y_train, lambda = 0, maxiter =100,learn.rate = 0.01)
    nn.params = J_grad[[2]]
    
    ert <- nnCostFunction(nn.params,input.layer.size, hidden.layer.size, num.labels, X_train,y_train,lambda)
    error_train[j,] <- ert[[1]]
    
    erv <- nnCostFunction(nn.params,input.layer.size, hidden.layer.size, num.labels, Xval,yval,lambda)
    error_val[j,] <- erv[[1]]
    
    i<- i+step
    j<- j+1
    
  }
  
  library(ggplot2)
  
  errors <- as.data.frame(cbind(error_train,error_val))
  x_er = 1:nrow(errors)
  
  pl <- ggplot() + geom_line(data = errors, aes(x= x_er, y= V1), color = 'red') + geom_line(data = errors, aes(x= x_er, y= V2), color = 'blue')
  print(pl)
}

getPolynomials <- function(data, degree){
  
  out <- NULL
  n <- ncol(data)
  for(i in 1:n){
    for(j in 2:degree){
      poly <- data[,i]^j
      out <- cbind(out,poly)
    }
  }
  
  colnames(out) <- paste0('Poly', seq_along(colnames(out) =='poly'))
  
  
  return(out)
  
}
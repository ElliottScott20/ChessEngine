source('chess_fun.R')

chess.data <- getChessData(num=100000)

model <- getPositionEvaluator()

chss <- Chess$new()
game <- list(0.5,chss)

position <- getBoardPosition(game)


positionScore <- evalutate(model, position)
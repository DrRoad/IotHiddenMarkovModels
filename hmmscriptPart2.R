library('HMM')
library('mhsmm')
library('Metrics')
df <- read.csv(file="E:\\iot project\\achaluv.csv", head = FALSE)

hmm <- initHMM(c('A','B','C'), c(1,2,3,4,5))

train <- df[1:1600,1]
test <- df[1601:2000,1]

# bw = baumWelch(hmm,train,100)
# 
# predict <- simHMM(bw$hmm, 1)
# predict$observation

# bw = baumWelch(hmm,train,100)
# #print(bw$hmm)
# 
# 
# predict <- simHMM(bw$hmm, 400)
# predictObs <- predict$observation
# 
# 
# x <- test
# y <- predictObs
# 
# sxx <- sum((x-mean(x))^2)
# sxy <-  sum((x-mean(x))*(y-mean(y)))
# bb <- sxy/sxx
# aa <- mean(y) - bb*mean(x)
# 
# ee <- y - aa - bb*x
# sse <- sum(ee^2)
# sst <- sum((y-mean(y))^2)
# ssr <- sst-sse
# 
# Rsquared <- ssr/sst
# 
# RMSE <- rmse(test, predictObs)
# 
# 
# 
# plot(test[300:400], type = 'l', col="red")
# lines(predictObs[300:400],col="green")



getPredictedData <- function(train, n){
  hmm <- initHMM(c('A','B','C'), c(1,2,3,4,5))
  data <- train
  nIter <- 30
  while(length(data) < 2000-n){
    trainData <- tail(data,1600)
    
    bw = baumWelch(hmm,trainData,nIter)
    #print(bw$hmm)
    
    
    predict <- simHMM(bw$hmm, n)
    predictObs <- predict$observation
    
    data <- append(data,predictObs)
  }
  
  remLen = 2000 - length(data)
  trainData <- tail(data,1600)
  
  
  bw = baumWelch(hmm,trainData,nIter)
  #print(bw$hmm)
  
  
  predict <- simHMM(bw$hmm, remLen)
  predictObs <- predict$observation
  
  data <- append(data,predictObs)
  
  #RMSE <- rmse(test, tail(data,400))
  
  return(tail(data,400))
}

print(getPredictedData(train,1))

SSEvalues = vector(mode="numeric", length=0)
RMSEvalues = vector(mode="numeric", length=0)
RsquaredValues = vector(mode="numeric", length=0)


for (i in seq(from=10, to=100, by=10)){
  print(i)
  predictObs <- getPredictedData(train, i)
  x <- test
  y <- predictObs
  
  sxx <- sum((x-mean(x))^2)
  sxy <-  sum((x-mean(x))*(y-mean(y)))
  bb <- sxy/sxx
  aa <- mean(y) - bb*mean(x)
  
  ee <- y - aa - bb*x
  sse <- sum(ee^2)
  
  SSEvalues <- append(SSEvalues, sse)
  
  sst <- sum((y-mean(y))^2)
  ssr <- sst-sse
  
  Rsquared <- ssr/sst
  RsquaredValues <- append(RsquaredValues, Rsquared)
  
  RMSE <- rmse(test, predictObs)
  RMSEvalues <- append(RMSEvalues, RMSE)
  
}

cat("RMSE values ", RMSEvalues)
cat("SSE values ", SSEvalues)
cat("Rsquared values ", RsquaredValues)


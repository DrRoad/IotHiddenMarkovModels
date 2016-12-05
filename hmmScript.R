library('HMM')
library('mhsmm')
library('Metrics')
df <- read.csv(file="E:\\iot project\\achaluv.csv", head = FALSE)

hmm <- initHMM(c('A','B','C'), c(1,2,3,4,5))

train <- df[1:1600,1]
test <- df[1601:2000,1]

bw = baumWelch(hmm,train,100)
#print(bw$hmm)


predict <- simHMM(bw$hmm, 400)
predictObs <- predict$observation


x <- test
y <- predictObs

sxx <- sum((x-mean(x))^2)
sxy <-  sum((x-mean(x))*(y-mean(y)))
bb <- sxy/sxx
aa <- mean(y) - bb*mean(x)

ee <- y - aa - bb*x
sse <- sum(ee^2)
sst <- sum((y-mean(y))^2)
ssr <- sst-sse

Rsquared <- ssr/sst

RMSE <- rmse(test, predictObs)






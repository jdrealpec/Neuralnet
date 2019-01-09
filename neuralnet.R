library(caTools)
#set.seed(500)
library(MASS)

data <- Boston

apply(data, 2, function(x) sum(is.na(x)))

sample = sample.split(data, SplitRatio = 0.75)

train = subset(data,sample ==TRUE)
test = subset(data,sample ==TRUE)

lm.fit <- lm(medv ~., data=train)

summary(lm.fit)

pr.lm <- predict(lm.fit,test)

RMSE.lm <- (sum((pr.lm - test$medv)^2)/nrow(test))^0.5

SSE = sum((test$medv - pr.lm)^2)
SST = sum((test$medv - mean(data$medv))^2)
lm.r2 = 1 - SSE/SST

library(neuralnet)
library(BBmisc) #normalize

dataN <- normalize(data, method = "range", range = c(0, 1))

sample = sample.split(dataN, SplitRatio = 0.75)

trainN = subset(dataN,sample ==TRUE)
testN = subset(dataN,sample ==TRUE)

n <- names(trainN)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

nn <- neuralnet(f,data=trainN,hidden=c(5,3),linear.output=T)

plot(nn)

pr.nn <- compute(nn,testN[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (testN$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

RMSE.nn <- (sum((test.r - pr.nn_)^2)/nrow(testN))^0.5

SSE = sum((test$medv - pr.nn_)^2)
SST = sum((test$medv - mean(data$medv))^2)
nn.r2 = 1 - SSE/SST


print(paste(RMSE.lm,RMSE.nn))

print(paste(lm.r2,nn.r2))

ggplot() + geom_point(aes(x=test$medv, y= pr.nn_))
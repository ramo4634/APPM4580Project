library(readxl)
library(e1071)


filename <- "P3_kitchen_area_air_quality_03_20_2019.xlsx"
dat<- read_excel(filename)

y <- dat$TotalPM25ugcubicmeter_PT 
feature_list = c("OCmassugcubicmeter","ECmassugcubicmeter","TCmassugcubicmeter","ECOCmassconc","Mean_bcor_1","Mean_bcor_2","Mean_temp","Mean_rh","Mean_CO","Mean_CO2","Mean_MCE","Var_bcor_1")#,"CoverageClass","season") 
X <- dat[feature_list]
#Replace NA values with mean - note this only works for numeric data
for(i in colnames(X)){
  print(mean(X[[i]], na.rm=TRUE))
  X[[i]][is.na(X[[i]])] <- mean(X[[i]],na.rm=TRUE)
}

X$y <- y

## 80% of the sample size for training 20% for testing
smp_size <- floor(0.8 * length(y))
print(smp_size)


set.seed(42)
train_ind <- sample(seq_len(length(y)), size = smp_size)

X.train <- X[train_ind,]
y.train <- y[train_ind]

X.temp <- X[-train_ind,]
y.temp <- y[-train_ind]

#X.test <- X.temp[test_ind,]
#y.test <- y.temp[test_ind]
#X.ver <- X.temp[-test_ind,]
#y.ver <- y.temp[-test_ind]
X.test <- X.temp
y.test <- y.temp


fit <- svm(y~.,data=X.train,kernel='polynomial',cost=2)
y.pred <- predict(fit, X.test)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- fit$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778
print(predictionRMSE)
#Grid Search
tuneResult <- tune(svm, y ~ .,  data = X.train, ranges = list(kernel=list('radial','linear'),epsilon = seq(0,1,0.1), cost = 2^(2:8)))
print(tuneResult)
plot(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, X.test) 

error <- y.test - tunedModelY  

tunedModelRMSE <- rmse(tunedModel$residuals)  
print(tunedModelRMSE)


library(readxl)
library(e1071)


filename <- #FILENAME HERE
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

# ============================================================
#  Setting up Training, Testing, and Validation sets
# ============================================================


## 60% of the sample size for training 20% for testing and 20% for verification
smp_size <- floor(0.6 * length(y))

set.seed(42)
train_ind <- sample(seq_len(length(y)), size = smp_size)

X.train <- X[train_ind,]
y.train <- y[train_ind]

X.temp <- X[-train_ind,]
y.temp <- y[-train_ind]

smp_size <- floor(0.5*length(y.temp))
test_ind <- sample(seq_len(length(y.temp)), size = smp_size)

X.test <- X.temp[test_ind,]
y.test <- y.temp[test_ind]
X.ver <- X.temp[-test_ind,]
y.test <- y.temp[-test_ind]


# ============================================================
#  Fitting the model
# ============================================================

fit <- svm(y~.,data=X.train,kernel='linear',cost=2)
y.pred <- predict(fit, X.test)

rmse <- function(error)
{
  sqrt(mean(error^2))
}


# ============================================================
#  Finding best model parameters
# ============================================================
error <- fit$residuals  
predictionRMSE <- rmse(error)  
print(predictionRMSE)
#Grid Search
tuneResult <- tune(svm, y ~ .,  data = X.train, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, X.test) 

error <- y.test - tunedModelY  

tunedModelRMSE <- rmse(error)  



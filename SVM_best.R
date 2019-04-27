library(readxl)
library(e1071)

#========================================
# Setting up the data
#========================================

filename <- "P3_kitchen_area_air_quality_03_20_2019.xlsx"
dat<- read_excel(filename)

y <- dat$TotalPM25ugcubicmeter_PT 
feature_list = c("OCmassugcubicmeter","ECmassugcubicmeter","TCmassugcubicmeter","ECOCmassconc","Mean_bcor_1","Mean_bcor_2","Mean_temp","Mean_rh","Mean_CO","Mean_CO2","Mean_MCE","Var_bcor_1")#,"CoverageClass","season") 
X <- dat[feature_list]
#Replace NA values with mean - note this only works for numeric data
for(i in colnames(X)){
  X[[i]][is.na(X[[i]])] <- mean(X[[i]],na.rm=TRUE)
}

X$y <- y


#========================================
# Train/Test Split
#========================================

## 80% of the sample size for training 20% for testing
smp_size <- floor(0.8 * length(y))


set.seed(42)
train_ind <- sample(seq_len(length(y)), size = smp_size)

X.train <- X[train_ind,]
y.train <- y[train_ind]

X.test <- X[-train_ind,]
y.test <- y[-train_ind]


#========================================
# Train/Test Split
#========================================

fit <- svm(y~.,data=X.train,kernel='radial',cost=4,gamma=0.6, epsilon=0)
y.pred <- predict(fit, X.test)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- fit$residuals  
predictionRMSE <- rmse(error)   
print(predictionRMSE)



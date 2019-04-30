################################################################################################
## Ridge Regression
################################################################################################
library(readxl)
library(glmnet)
library(coefplot)

#========================================
# Setting up the data
#========================================

filename <- "P3_kitchen_area_air_quality_03_20_2019.xlsx"
dat<- read_excel(filename)
dat = dat[c(-17,-18,-37),]

y <- dat$TotalPM25ugcubicmeter_PT 
feature_list = c("OCmassugcubicmeter","ECmassugcubicmeter","TCmassugcubicmeter","ECOCmassconc","Mean_bcor_1","Mean_bcor_2","Mean_temp","Mean_rh","Mean_CO","Mean_CO2","Mean_MCE","Var_bcor_1")#,"CoverageClass","season") 
X <- dat[feature_list]
#Replace NA values with mean - note this only works for numeric data
for(i in colnames(X)){
  X[[i]][is.na(X[[i]])] <- mean(X[[i]],na.rm=TRUE)
}
#X$y <- y


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
# Ridge Regression using glmnet
#========================================

X.train <- as.matrix(X.train)
y.train <-as.matrix(y.train)
X.test <- as.matrix(X.test)
y.test <- as.matrix(y.test)

lambdas <- 10^seq(10, -2, by = -.1)
rr.test <- cv.glmnet(X.train,y.train,alpha=0,lambda=lambdas,intercept=FALSE)
lambda.opt <- rr.test$lambda.min
jpeg("rr_lambda_vals.jpg")
plot(rr.test)
dev.off()

rr.final <- glmnet(X.train,y.train,alpha=0,lambda=lambda.opt)

rr.pred <- predict(rr.final,s=0.1,newx=as.matrix(X.test))

mean( (mean(y.test) - y.test)^2 ) 
mean( (rr.pred - y.test)^2 ) 
rmse <- function(error)
{
  sqrt(mean(error^2))
}

rrRMSE = rmse(y.test-rr.pred)
print(rrRMSE)


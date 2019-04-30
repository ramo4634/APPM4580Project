################################################################################################
## The lasso
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
# Lasso via glmnet
#========================================

X.train <- as.matrix(X.train)
X.test <- as.matrix(X.test)

lasso.mod <- cv.glmnet(X.train,y.train,alpha=1,lambda=10^seq(-2,5,length.out=100),intercept=FALSE) # alpha=1 => lasso

jpeg("lasso_lambda_vals.jpg")
plot(lasso.mod,xvar="lambda")
dev.off()

lambda.opt <- lasso.mod$lambda.min


lasso.final <- glmnet(X.train,y.train,alpha=1,lambda=lambda.opt)
## Predict on test data for given lambda, compare to null model
lasso.pred <- predict(lasso.final,newx=as.matrix(X.test))

mean( (mean(y.test) - y.test)^2 ) # null model (beta_0 is only term)
mean( (lasso.pred - y.test)^2 ) # lasso predictions
rmse <- function(error)
{
  sqrt(mean(error^2))
}

lasso.rmse = rmse(y.test-lasso.pred)
print(lasso.rmse)
#jpeg("coef_plot_lasso.jpg")
#coefplot(lasso.final)
#dev.off()



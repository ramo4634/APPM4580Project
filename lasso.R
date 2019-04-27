################################################################################################
## The lasso
################################################################################################

library(glmnet)

filename <- "~/Desktop/Spring Semester 2019/APPM 4580/Final Project/APPM4580Project/P3_kitchen_area_air_quality_03_20_2019.xlsx"
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

is(dat)
names(dat)
str(dat)
summary(dat)
pairs(TotalPM25ugcubicmeter_PT~ECOCmassconc+log(Mean_bcor_1)+log(Mean_bcor_2)+Mean_temp+Mean_rh+Mean_CO+Mean_CO2+Mean_MCE,data = dat)
cor(select(dat,TotalPM25ugcubicmeter_PT,OCmassugcubicmeter,ECmassugcubicmeter,TCmassugcubicmeter,ECOCmassconc,Mean_bcor_1,Mean_bcor_2,Mean_temp,Mean_rh,Mean_CO,Mean_CO2,Mean_MCE))

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

##
## lasso via glmnet
##

lasso.mod <- glmnet(as.matrix(X.train),y.train,alpha=1,lambda=10^seq(-2,2,length.out=100)) # alpha=1 => lasso
plot(lasso.mod,xvar="lambda")



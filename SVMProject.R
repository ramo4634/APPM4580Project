dat= read.table('ProjectData.csv', sep="\t", header=TRUE)

dim(dat)
dat = list(N=118,var1=as.vector(dat$var1),var2=as.vector(dat$var2),var3=as.vector(dat$var3),Y=as.vector(dat$Y))
fixEfFit <- stan(file = "Model1.stan", data = dat, iter = 2000, chains = 4)


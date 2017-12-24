#set working directory to where the file can be found
setwd("C:/Users/admin/Desktop/STA 108/Project1")

#read the data file and place in a variable
mydata = read.table("UN.txt", header = T) #header=F if no first row.

#Display some of the data
head(mydata)

#Display all of the data
mydata

#Set x and y to mydata's height and weight
x=mydata$PPgdp
y=mydata$Fertility

#Display some of the data for mydata
str(x)
str(y)
str(mydata)

#1
#plot x and y, label the axes
plot(x,y,xlab="PPgdp",ylab="Fertility",main="Fertility vs. PPgdp")

#2
#create variables for the log transformations
xlog = 1/log(x)

plot(xlog, y, xlab="1/logx",ylab="y")

#3a
#xbar and ybar
ybar=mean(y)
xbar=mean(xlog)

#n, the number of variables
n=length(x)

#betahat1 and betahat0
betahat1=sum((xlog-xbar)*(y-ybar))/sum((xlog-xbar)^2)
betahat0=ybar-betahat1*xbar
betahat0
betahat1

#plot a straight line for the data using slope and intercept
abline(a = betahat0, b = betahat1)

#yhat
yhat = betahat0+betahat1*xlog

#SSR
SSR = sum((yhat-ybar)^2)

#SSE
SSE = sum((y-yhat)^2)

#SSTO
SSTO = sum((y-ybar)^2)

#R^2 and rsquared
R2 = SSR/SSTO
R2
rsquared = (1 - SSE/SSTO)
rsquared

#3b
#lm function
model = lm(y~xlog)
summary(model)

plot(xlog,y)
#plot fitted line
abline(model$coefficients)

#3c
#matrix manipulation
head(mydata)
X=as.matrix(cbind(rep(1,n),1/log(mydata[,3])))
View(X)
str(X)

XTX = t(X)%*%X
XTXinv = solve(XTX)
Y = as.matrix(mydata[,2])
View(Y)
XTY = t(X)%*%Y
betahatMatrix = XTXinv%*%XTY
betahatMatrix

plot(xlog,y)
#draw fitted matrix line
abline(betahatMatrix)

Yhat = X%*%betahatMatrix
head(Yhat)

res = as.vector(Y - Yhat)
head(res)
SSE = res%*%res

SSTO = sum((Y-mean(Y))^2)

Rsquaredmatrix = 1 - SSE/SSTO
Rsquaredmatrix

#4
model = lm(y~xlog)
plot(model, which=1)
#The dispersion of the residuals seem to be smaller at first, but they spread out as
#fitted values increase. At the end they shrink again, but the change is not severe.
stdres=rstandard(model)
hist(stdres)
#The histogram appears unimodal, with the standard deviation of residuals
#going to -3 and 3. 
qqnorm(stdres)
qqline(stdres)
#The QQ plot lies mostly along the line, so there is a mostly normal distribution.

#5
#test whether beta1 = 0 at 0.05 significance level
#H0: b1 = 0 v.s. H1: b1 =/= 0
#T.S. t* = (b1hat - 0)/SE(b1hat)
summary(model)
#for the lm model, the slope has a p-value of < 2*10^-16, therefore the conclusion
#is to reject the null hypothesis

#6
MSE = summary(model)$sigma^2
Xh = 1/log(20000)
Yh=betahat0+betahat1*Xh
Yh+c(-1,1)*qt((1-0.01/2),n-2)*sqrt(MSE*(1/n + ((Xh-mean(xlog))^2)/sum((xlog-mean(xlog))^2)))
#1.438266 2.038231

#7
xseq = seq(min(mydata[,3]), max(mydata[,3]), 0.1)
xlogseq = 1/log(xseq)
W = sqrt(2*qf(1-0.05, 2, n-2))
yseq=betahat0+betahat1*xlogseq
se.y.seq = sqrt(MSE*(1/n + ((xlogseq-mean(xlog))^2)/sum((xlog-mean(xlog))^2)))
low = yseq - W*se.y.seq
high = yseq + W*se.y.seq

plot(xlog, y)
abline(betahat0,betahat1)
lines(xlogseq,low, col="red")
lines(xlogseq,high, col="blue")

#8
MSE = summary(model)$sigma^2
Xh = 1/log(25000)
Yh=betahat0+betahat1*Xh
Yh+c(-1,1)*qt((1-0.01/2),n-2)*sqrt(MSE*(1/n + 1 + ((Xh-mean(xlog))^2)/sum((xlog-mean(xlog))^2)))
#-1.057710  4.344765

#9
#based on the part 4 diagnostic plots, the equal variance and normality are okay.
#also the other hypothesis tests and inferences made sense in accordance with
#the results that were given.

#different models
# abline(model$coefficients)
# par(mfrow=c(2,2))
# plot(model)
# 
# plot(model)
# model = lm(mydata$Fertility~mydata$PPgdp)
# par(mfrow=c(2,2))
# plot(model)
# 
# Y = log(Y)
# X = log(X)
# 
# transform.plot = function(x,y){
#   par(mfrow=c(5,5))
#   x.list = vector("list", 6)
#   y.list = vector("list", 6)
#   x.list[[1]]=x; x.list[[2]] = x^2; x.list[[3]] = sqrt(x[y>0]); x.list[[4]]=1/x; x.list[[5]]=log(x[y>0]); x.list[[6]]=exp(x)
#   y.list[[1]]=y; y.list[[2]] = y^2; y.list[[3]] = sqrt(y[y>0]); y.list[[4]]=1/y; y.list[[5]]=log(y[y>0]); y.list[[6]]=exp(y)
#   transform.x = c("original x", "x^2", "sqrt(x)", "1/x", "log(x)", "exp(x)")
#   transform.y = c("original y", "y^2", "sqrt(y)", "1/y", "log(y)", "exp(y)")
#   for (i in 1:6){
#     for (j in 1:6){
#       if (i!=4 & j!=4){
#         plot(x.list[[i]], y.list[[j]], xlab=transform.x[i], ylab=transform.y[j])
#       }
#     }
#   }
# }
# 
# transform.plot(X[Y>0],Y[Y>0])
# 
# fit1 = lm(Y~X)
# library(MASS)
# boxcox(fit1)
# par(mfrow=c(1,1))
# plot(log(X), log(Y))
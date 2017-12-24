#Set the working directory
setwd("C:/Users/jyqq9/Desktop/STA 108/Project 2")

#Read in the data from the text file
mydata = read.table("diabetes.txt", header=T)

#Examine the data
head(mydata)
dim(mydata)

#Examples of qualitative predictor variables are gender (male,female), purchase status
#(puchase,no purchase), and disability status (not disabled,partly disabled,fully disabled)

#1
#The quantiative variables are chol, stab.glu, hdl, ratio, glyhb, age, height, weight,
#bp.1s, bp.1d, waist, hip, time.ppn. The qualitative variables are location, gender, and frame.

#histogram for each quantiative variable
#chol's distribution
quantvar1 = subset(mydata, select = c(chol))
hist(quantvar1$chol)
#The distribution for the histogram of the variable 'chol' appears unimodal, with
#the majority of the data around 200. It also appears somewhat skewed to the right.
#There appears to be some more outlying data around 400 also.

#stab.glu's distribution
quantvar2 = subset(mydata, select = c(stab.glu))
hist(quantvar2$stab.glu)
#The distribution appears unimodal, and mostly below or near 100. It is also heavily
#skewed towards the right side.

#hdl's distribution
quantvar3 = subset(mydata, select = c(hdl))
hist(quantvar3$hdl)
#The distribution appears unimodal, with the majority of the data collecting
#around 40-50. There is also a skew to the right. It stretches much further to
#the right than it does to the left.

#ratio's distribution
quantvar4 = subset(mydata, select = c(ratio))
hist(quantvar4$ratio)
#The distribution appears unimodal, with most of the data collecting around 5
#or slightly below. The distribution is also skewed to the right. It stretches
#much further to the right than it does left.

#glyhb's distribution
quantvar5 = subset(mydata, select = c(glyhb))
hist(quantvar5$glyhb)
#The distribution is unimodal, however it seems to be quite sharp at around 5 or
#slightly below that value. There is also a skew to the right.

#age's distribution
quantvar6 = subset(mydata, select = c(age))
hist(quantvar6$age)
#The distribution appears unimodal, however it is a very large gathering of data
#around a rather wide range. Therefore the curve looks quite round. It has a
#somewhat slight skew towards the right, but it is not extreme.

#height's distribution
quantvar7 = subset(mydata, select = c(height))
hist(quantvar7$height)
#The distribution is unimodal, however it is also quite round in shape. Therefore
#it seems that the data gathers around 60-70 in an almost equal distribution.
#This time the skew is towards the left, but it is also not very obvious or extreme.
#note!
#is it skewed?

#weight's distribution
quantvar8 = subset(mydata, select = c(weight))
hist(quantvar8$weight)
#The distribution is unimodal, and the skew is towards the right. Most of the data
#collects around 150-200, and then stretches towards 350.

#bp.1s' distribution
quantvar9 = subset(mydata, select = c(bp.1s))
hist(quantvar9$bp.1s)
#The distribution of bp.1s is unimodal, and skewed to the right. Most of the data
#is collecting around 100-150. There are some more extreme values around 250.

#bp.1d's distribution
quantvar10 = subset(mydata, select = c(bp.1d))
hist(quantvar10$bp.1d)
#The distribution is unimodal, with most of the data collecting around 80. It also
#seems quite even with no noticable skew in the data.

#waist's distribution
quantvar11 = subset(mydata, select = c(waist))
hist(quantvar11$waist)
#The distribution is unimodal, with most of the data collecting around 30-40. The
#data is somewhat skewed to the right, but it is not too extreme.

#hip's distribution
quantvar12 = subset(mydata, select = c(hip))
hist(quantvar12$hip)
#The distribution for the data is unimodal, with most of the data collecting around
#35-45. The data also is skewed towards the right side.

#time.ppn's distribution
quantvar13 = subset(mydata, select = c(time.ppn))
hist(quantvar13$time.ppn)
#The distribution appears mostly unimodal, but also slightly bimodal due to the
#slight spike in the data around 750. The data is mostly collecting below 500.
#There is also a heavy skew towards the right side.

#pie chart for each qualitative variable
#Load MASS package for the table function
library(MASS)

#location's distribution
qualvar1 = subset(mydata, select = c(location))
location = qualvar1$location
location.pie = table(location)
pie(location.pie)
#The distribution for the location is roughly equal, with about half of the
#people in the data coming from Buckingham, and slightly more coming from Louisa.

#gender's distribution
qualvar2 = subset(mydata, select = c(gender))
gender = qualvar2$gender
gender.pie = table(gender)
pie(gender.pie)
#The distribution of the gender is heavier for females. There are less males in the
#data, however the difference is not too severe. It is something that is noticable however.

#frame's distribution
qualvar3 = subset(mydata, select = c(frame))
frame = qualvar3$frame
frame.pie = table(frame)
pie(frame.pie)
#The distribution is over three types of frames, large, medium, and small. The majority
#of the data is collected around the medium type. This makes up approximately half of the
#data in the entire sample. The rest of the data is split roughly evenly in the other
#half of the data between large and small.

#draw a scatterplot matrix and obtain the pairwise correlation matrix for all
#quantiative variables in the data
quantvarmatrix = mydata[c(1:5,7,9:10,12:16)]
pairs(quantvarmatrix)
#The most clear linear relationship between different quantitative variables are those
#that are between the physical characteristics of weight, waist, and hip. It makes sense
#that in a study, these are the most obvious connections to identify. It make sense
#to think that people who are heavier will have larger waist and hip sizes.The variables
#hdl and ratio also seem to have a correlation that is negative. Therefore multicollinearity
#exists between weight, waist, and hip along with hdl and ratio. There is some slight
#relationship between bp.1s and bp.1d, however there is a large spread for the data.


#2
#regress glybh on all predictor variables
Model1 = lm(glyhb~.,data=mydata)
plot(Model1)
#plot(mydata$glyhb)
#lambda = 2
#create quantitative matrix without glyhb

#still need to comment on model

#3
library(MASS)
boxcox(mydata$glyhb~.,data=mydata)
Ystar = 1/mydata$glyhb
Model2 = lm(Ystar~., data=mydata[,-5])
boxcox(Model2)
#The lambda for the data now says that it is fine at lambda = 1, therefore the
#response variable Y is at an ideal transformed state.
plot(Model2)
#do hist and qqplot

#4
set.seed(10) #set seed for random number generator
            #so everyone gets the same split of the data.
N=nrow(mydata) #number of observations in the data 
index=sample(1:N, size=N/2, replace=FALSE) #randomly sample 
                #N/2 observation to form the training data.
data.t=mydata[index,] #get the training data set
data.v=mydata[-index,] #the remaining N/2 observations form the validation set

#5
Ystar2 = 1/data.t$glyhb #find the Ystar for the different data set
Model3 = lm(Ystar2~., data=data.t[,-5])
summary(Model3)
length(Model3$coefficients) #of regression coefficients
summary(Model3)$sigma^2 #MSE from this model
#fits frame for medium and large, no small
#There are 17 regression coefficients
#The MSE is 0.001383855
#Doesn't match, keeps changing

#6
library(leaps)

best = regsubsets(Ystar2~., data=data.t[,-5], nbest=1, nvmax=16)
sum_sub=summary(best)
sum_sub$which
n=nrow(data.t)
n
p.m=2:17
sse=sum_sub$rss
sse
aic=n*log(sse)+2*p.m-n*log(n)
aic
bic=n*log(sse)+log(n)*p.m-n*log(n)
bic
res_sub=cbind(sum_sub$which,sse,sum_sub$rsq,sum_sub$adjr2,sum_sub$cp,aic, bic)
fit0=lm(Ystar2~1,data=data.t[,-5]) # fit the model with only intercept
sse1=sum(fit0$residuals^2)
p=1
c1=sse1/0.001384-(n-2*p)
aic1=n*log(sse1)+2*p-n*log(n)
bic1=n*log(sse1)+log(n)*p-n*log(n)
none=c(1,rep(0,16),sse1,0,0,c1,aic1,bic1)
res_sub=rbind(none,res_sub) # combine the results with other models
colnames(res_sub)=c(colnames(sum_sub$which),"sse", "R^2", "R^2_a", "Cp", "aic", "bic")
which.min(res_sub[,21])
res_sub
#lowest SSE 16 0.2297200
#highest R^2_a 6 0.5302352
#highest R^2 16 0.5546893
#lowest Cp 5 0.05337754
#Explanation for lowest Cp?
#lowest aic 5 -1205.022
#lowest bic 3 -1191.471

#Model 3.1, 3.2, 3.3
Model3.1=lm(Ystar~.,data=mydata[c(2,4,6,11,14)]) #AIC selected
Model3.2=lm(Ystar~.,data=mydata[c(2,6,14)]) #BIC selected
Model3.3=lm(Ystar~.,data=mydata[c(2,4,6,11,14,16)]) #AdjR^2 selected

#7
#Model4=lm(Ystar~.^2,data=mydata)
Model4=lm(Ystar2~.^2,data=data.t[,-5])
#should the X's be squared?
length(Model4$coefficients) #136
summary(Model4)$sigma^2 #0.001036088
#overfitting is a concern, too many predictors for so few betas

#8
best = regsubsets(Ystar2~., data=data.t[,-5], nbest=1, nvmax=16)
fit0=lm(Ystar~1,data=mydata)
fs1=stepAIC(fit0,scope=list(upper=Model4,lower=~1),direction="both",k=2)
# Step:  AIC=-2436.1
# Ystar ~ stab.glu + age + ratio + waist + time.ppn + location + 
#   stab.glu:time.ppn + stab.glu:age + age:ratio
sse.fs1=sum(fs1$residuals^2)
p.fs1=length(fs1$coefficients)
#Model 3.1 = -1205.022
#FS1 has a much smaller AIC, so it is better to choose the fs1 model for AIC.
#So the two-way interaction allows for much better subsets according to AIC criterion.

#9
fs2=stepAIC(Model3,scope=list(upper=Model4,lower=~1),direction="both",k=2)
# Step:  AIC=-1230.61
# Ystar2 ~ chol + stab.glu + hdl + ratio + age + gender + height + 
#   weight + bp.1s + bp.1d + waist + hip + time.ppn + stab.glu:gender + 
#   hdl:ratio + age:bp.1d + weight:bp.1s + age:hip + hip:time.ppn + 
#   gender:height + stab.glu:bp.1s + stab.glu:time.ppn + stab.glu:waist + 
#   age:waist + chol:time.ppn + hdl:weight + bp.1d:waist + weight:hip

#fs1 has a much better AIC, so it is ideal to choose fs1 over fs2's subset
#this is because it has a lower AIC than fs2. The model fs1 is -2436.1
#while fs2 is only -1230.61

#10
n = nrow(data.t[,-5])
bic.fs1=n*log(sse.fs1)+p.fs1*log(n)-n*log(n)
bic.fs1 #-1049.11
#repeat with fs2, and compare the two
#compare with 3.1
sse.fs2=sum(fs2$residuals^2)
p.fs2=length(fs2$coefficients)
bic.fs2=n*log(sse.fs2)+p.fs2*log(n)-n*log(n)
bic.fs2 #-1137.536
#The BIC model for fs2 is lower than fs1, so it is better. The BIC for fs1 is
#-1049.11, and the BIC for fs2 is -1137.536

Model4.1 = fs1
Model4.2 = fs2

#11
PRESSModel3.1 = sum(Model3.1$residuals^2/(1-influence(Model3.1)$hat)^2)
PRESSModel3.2 = sum(Model3.2$residuals^2/(1-influence(Model3.2)$hat)^2)
PRESSModel3.3 = sum(Model3.3$residuals^2/(1-influence(Model3.3)$hat)^2)
PRESSModel4.1 = sum(Model4.1$residuals^2/(1-influence(Model4.1)$hat)^2)
PRESSModel4.2 = sum(Model4.2$residuals^2/(1-influence(Model4.2)$hat)^2)
PRESSModel3.1 #0.5322445
PRESSModel3.2 #0.5428011
PRESSModel3.3 #0.5309113
PRESSModel4.1 #0.4742118
PRESSModel4.2 #0.2171946
#Model 4.2 has the best PRESS value because it is lower than all the others.

#12
Yhat.3.1 = predict(Model3.1, newdata=data.v)
Y.3.1 = 1/data.v[,5]
m = nrow(data.v)
MSPR.3.1 = sum((Yhat.3.1-Y.3.1)^2)/m
MSPR.3.1 #0.001332268
Yhat.3.2 = predict(Model3.2, newdata=data.v)
Y.3.2 = 1/data.v[,5]
MSPR.3.2 = sum((Yhat.3.2-Y.3.2)^2)/m
MSPR.3.2 #0.001423891
Yhat.3.3 = predict(Model3.3, newdata=data.v)
Y.3.3 = 1/data.v[,5]
MSPR.3.3 = sum((Yhat.3.3-Y.3.3)^2)/m
MSPR.3.3 #0.001323436
Yhat.4.1 = predict(Model4.1, newdata=data.v)
Y.4.1 = 1/data.v[,5]
MSPR.4.1 = sum((Yhat.4.1-Y.4.1)^2)/m
MSPR.4.1 #0.001159244
Yhat.4.2 = predict(Model4.2, newdata=data.v)
Y.4.2 = 1/data.v[,5]
MSPR.4.2 = sum((Yhat.4.2-Y.4.2)^2)/m
MSPR.4.2 #0.001797609
m = nrow(data.v)
PRESSModel3.1/m #0.00290844
PRESSModel3.2/m #0.002966126
PRESSModel3.3/m #0.002901155
PRESSModel4.1/m #0.002591321
PRESSModel4.2/m #0.001186856
#All of the MSPR's are lower than the PRESS/n except for
#Model 4.2 which is second to Model 4.1 from MSPR
#Model 4.1 has the smallest MSPR.
#PRESS/n has larger values due to the...?

#13
#I wouldn't necessarily use Model 4.2 even though it has the best outcome for
#MSPR. The model has a high prediction power, however the model is too complicated
#since it uses so many predictors. Therefore it's hard to interpret the model.
#To balance this out, I would use Model 4.1 as a predictor since it has the lowest MSPR.
#My goal is also for prediction, therefore I would use MSPR as the indicator. It also
#did well in the PRESS/n test.

Model4.1
mydata$glyhb

Model4.1reg = lm(1/mydata$glyhb ~ stab.glu + age + ratio + waist + time.ppn + location + stab.glu:time.ppn + stab.glu:age + age:ratio, data=mydata)
summary(Model4.1reg)$coef
anova(Model4.1reg)

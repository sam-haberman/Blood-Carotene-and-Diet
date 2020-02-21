attach(data)
library(ISLR) 
library(leaps)
library(gam)
library(glmnet)

######################################################
####                 Question 1                   ####
######################################################

dim(data)
summary(data)
cor(data[,c(-2,-3,-5)])

######################################################
####                 Question 2                   ####
######################################################

model1 <- glm(caroBlood~alcohol)
summary(model1)
plot(alcohol,caroBlood)
abline(model1)

#We see an outlier, so we want to see what happens when we delete the outlier
no_outlier <- subset(data, alcohol!=203)
model2 <- glm(caroBlood~alcohol, data=no_outlier)
summary(model2)
plot(no_outlier$alcohol,no_outlier$caroBlood)
abline(model2)

######################################################
####                 Question 3                   ####
######################################################

model3 <- glm(caroBlood~gender, family = binomial(link="logit"))
summary(model3)
plot(caroBlood~gender)

######################################################
####                 Question 4                   ####
######################################################

#We make a train and test set
set.seed(1)
train = sample(1:dim(data)[1],dim(data)[1]*1/2)
test = (-train)

#we do forward selection for variables
regfit.fwd=regsubsets(caroBlood~. ,data[train,],method="forward")
summary(regfit.fwd)
plot(summary(regfit.fwd)$bic,type="b")
plot(regfit.fwd, scale="bic")
#We want to keep weightOverHeightSQ, vitaSuppNo, chol and caroDiet

#making the linear model
lm1 <- lm(caroBlood~vitaSuppl +ns(weightOverHeightSQ,3) +ns(chol,3)+ ns(caroDiet,3) ,data=data,subset=train)

#testing linear model on the test set
predlm1 = predict(lm1, newdata=data[test,])
mselm1 = mean((predlm1-data[test, "caroBlood"])^2)

#making the ridge regression model
x=model.matrix(caroBlood~.,data)
y=data$caroBlood
grid=10^seq(10,-2,length=100) 
ridge1=glmnet(x,y,alpha=0,lambda=grid)
coef=coef(ridge1)[-1,]
l2=sqrt(apply(coef*coef,2,sum))
plot(log(ridge1$lambda),l2,type='l')
plot(ridge1)

#testing the ridge regression model on the test set
#we do cross validation to find the optimal lambda value for our ridge regression model
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)#the variability in the CV is shown as well as two thresholds
minlam=cv.out$lambda.min
selam=cv.out$lambda.1se
predridge1=predict(ridge1,s=minlam,newx=x[test,])
mseridge1 = mean((predridge1-y[test])^2)
predridge2=predict(ridge1,s=selam,newx=x[test,])
mseridge2 = mean((predridge2-y[test])^2)
#the first model has the lowest MSE, so this lambda is the optimal one.


#making the GAM
gam1 = gam(caroBlood~vitaSuppl +s(weightOverHeightSQ,3) +s(chol,3)+ s(caroDiet,3) ,data=data,subset=train)
par(mfrow=c(2,2))
plot(gam1,se=TRUE,col="red")

#testing GAM on the test set
predgam1 = predict(gam1, newdata=data[test,])
msegam1 = mean((predgam1-data[test, "caroBlood"])^2)

#compare the MSE over all models
compare<-c(mselm1, mseridge1, msegam1)
xnames <- c("lm", "ridge", "GAM")
x <- 1:3
plot(x,compare, xaxt='n', ylab="MSE")
axis(1, x, labels=xnames)

######################################################
####                 Question 5                   ####
######################################################

x=model.matrix(caroBlood~.,data)
y=data$caroBlood
bestmodel=glmnet(x,y,alpha=0)
predict(bestmodel,type="coefficients",s=minlam)
#fiber is the best dietary variable

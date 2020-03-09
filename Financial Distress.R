Financial.Distress <- read.csv("Financial Distress.csv")
View(Financial.Distress)
d=sample(c(1:nrow(Financial.Distress)),size = 500)
data1=Financial.Distress[d,c(3:ncol(Financial.Distress))]
colnames(data1)[1]<-c('FinDistress')
t=sample(c(1:500),size = 0.8*nrow(data))
train=c(1:500) %in% t
test=!train
trainData=data[train,]
testData=data[test,]

x<-model.matrix(FinDistress~.,data1)[,-1]
y<-data1$FinDistress
lambda<-10^seq(10, -2, length = 100)

library(glmnet)
set.seed(123)
t = sample(1:nrow(x), nrow(x)/2)
train=c(1:500) %in% t
test = !train
ytest = y[test]

#OLS
fit1 <- lm(FinDistress~., data1,subset=train)
# important parameters: x4,x10,x12,x18,x23,x25,x47,x50,x57,x62,x63,x64,x65,x69,x71,x72,x73,x77,x83
pred_lm=predict(fit1,newx = x[test,])
mse_lm=mean((pred_lm-ytest)^2)

# ridge alpha = 0
fit2<-glmnet(x, y, alpha = 0, lambda = lambda)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
bestlam <- cv.out$lambda.min
# ridge best model, prediction
ridge.pred <- predict(fit2, s = bestlam, newx = x[test,])
mse_ridge=mean((ridge.pred-ytest)^2)


# lasso alpha = 1
fit3<-glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
cv.out2 <- cv.glmnet(x[train,], y[train], alpha = 1)
bestlam2<-cv.out2$lambda.min
# lasso best model, prediction
lasso.pred <- predict(fit3, s = bestlam2, newx = x[test,])
mse_lasso=mean((lasso.pred-ytest)^2)
# check the parameters
lasso.coef  <- predict(fit3, type = 'coefficients', s = bestlam2)
# here we can see only some of the parameters are selected

# check mse for the three model:
print("OLS MSE:")
print(mse_lm)
print("Ridge MSE:")
print(mse_ridge)
print("Lasso MSE:")
print(mse_lasso)
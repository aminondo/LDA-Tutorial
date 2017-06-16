# =====================================================================================================================
# Ridge Regression
# =====================================================================================================================

#To use this model you have to install the following packages:

install.packages("glmnet")
install.packages("lars")
install.packages("foreach")
install.packages("lasso2")
install.packages("MASS")
install.packages("ISLR")

library(MASS)
library(ISLR)
View(Hitters)
hitters = na.omit(Hitters)
View(hitters)
with(hitters, sum(is.na(Salary)))


x=model.matrix(Salary~.-1,data=hitters) 
y=hitters$Salary

fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge)
plot(fit.ridge,xvar="lambda",label=TRUE)

fit.lasso=glmnet(x,y,alpha=1, lambda = seq(0,100,1))
plot(fit.lasso,xvar="lambda",label=TRUE)

# To perform Ridge Regression in R, we will use the glmnet package, developed by the creators of the algorithm.

library(glmnet)
library(lars)
library(foreach)

# Data = for this modle we??ll use the swiss dataset.
# We remore the intercept column, store the independent variable as y, and create a vector of lambda values. 

View(swiss)
swiss <- datasets::swiss
x <- model.matrix(Fertility~., swiss)[,-1]
y <- swiss$Fertility
lambda <- 10^seq(10, -2, length = 100)

#

set.seed(489)

train = sample(1:nrow(x), nrow(x)*.8)
test = (-train)
ytest = y[test]  


# Fit your models 
#OLS

swisslm <- lm(Fertility~., data = swiss)
coef(swisslm)

#ridge
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
plot(ridge.mod, label = T)
predict(ridge.mod, s = 0, exact = T, type = 'coefficients')[1:6,]

# The differences here are nominal. Let's see if we can use ridge to improve on the OLS estimate.

swisslm <- lm(Fertility~., data = swiss, subset = train)
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)

#find the best lambda from our list via cross-validation

cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)

## Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations
## per fold

bestlam <- cv.out$lambda.min

#make predictions

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
s.pred <- predict(swisslm, newdata = swiss[test,])

#check MSE

mean((s.pred-ytest)^2)

mean((ridge.pred-ytest)^2)

# Ridge performs better for this data according to the MSE.

#a look at the coefficients

out = glmnet(x[train,],y[train],alpha = 0)
predict(ridge.mod, type = "coefficients", s = bestlam)[1:6,]
coef(swisslm)

# As expected, most of the coefficient estimates are more conservative.

# Let's have a look at the lasso. The big difference here is in the shrinkage term 
# the lasso takes the absolute value of the coefficient estimates.

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred-ytest)^2)

# The MSE is a bit higher for the lasso estimate. Let's check out the coefficients.
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)[1:6,]



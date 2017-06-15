attach(iris)
library(e1071)

x <- subset(iris, select=-Species)
y <- Species

svm_model <- svm(Species ~ ., data=iris)
summary(svm_model)

svm_model1 <- svm(x,y)
summary(svm_model1)

pred <- predict(svm_model1,x)
system.time(pred <- predict(svm_model1,x))

table(pred,y)

svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))

table(pred, y)

# ---------------------THIS IS WHAT I WILL ACTUALLY PRESENT----------------------------
# Red Blue Dataset  -----------------------------------------------------------------
library(e1071)

load("../../iX-rModules/lectures/red-blue-points.RData")

# Splitting into trainsets and testsets
known[,'train'] <- ifelse(runif(nrow(known))<0.8,1,0)
trainset <- known[known$train==1,]
testset <- known[known$train==0,]

trainColNum <- grep('train',names(trainset))  #remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]
# typeColNum <- grep('colour',names(known))
# finish splitting

# creating the model
svm_model <- svm(colour~., data=trainset, type="C-classification", kernel='radial')

# predicting for the trainset
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$colour)    # probability correct

#predicting for the testset
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$colour)     # probability correct

# Confusion matrix 
table(testset$colour, pred_test)

# plot of svm_model
plot(svm_model, trainset, x ~ y)

# plot of all data points
ggplot(known, aes(x, y, color = colour)) + 
  geom_point()

# table(trainset$ colour, pred_train)

# trying to tune --------------------------------- don't really need tuning for this data set
tune_out <- tune.svm(x~y,data = trainset, gamma=seq(0,2, .1),cost=c(0.01,0.1,1,10,100,1000),kernel='radial')
#print best values of cost and gamma
tune_out$best.parameters$cost
tune_out$best.parameters$gamma

svm_model <- svm(colour~., data=trainset, type='C-classification', kernel='radial', cost = tune_out$best.parameters$cost, gamma = tune_out$best.parameters$gamma)

plot(svm_model, trainset, x ~ y)

ggplot(known, aes(x, y, color = colour)) + 
  geom_point()

# Example 2 -------------------------------------------------------------------------

# ideal Iris set - - - - - - - - - - - - - - - - - - - - - - - - 
#load required library
library(e1071)
#load built-in iris dataset
data(iris)
#set seed to ensure reproducible results
set.seed(42)
#split into training and test sets
iris[,'train'] <- ifelse(runif(nrow(iris))<0.8,1,0)   #GOOD EXAMPLE OF SPLITTING DATA SETS
#separate training and test sets
trainset <- iris[iris$train==1,]
testset <- iris[iris$train==0,]
#get column index of train flag
trainColNum <- grep('train',names(trainset))
#remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]
#get column index of predicted variable in dataset
typeColNum <- grep('Species',names(iris))
#build model - linear kernel and C-classification (soft margin) with default cost (C=1)
svm_model <- svm(Species~ ., data=trainset, method='C-classification', kernel='linear')

#training set predictions
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$colour)

#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$colour)

plot(svm_model, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

table(testset$Species, pred_test)


# real world set - - - - - - - - - - - - - - - - - - -  
#load required library (assuming e1071 is already loaded)
library(mlbench)
#load Sonar dataset
data(Sonar)
#set seed to ensure reproducible results
set.seed(42)
#split into training and test sets
Sonar[,'train'] <- ifelse(runif(nrow(Sonar))<0.8,1,0)
#separate training and test sets
trainset <- Sonar[Sonar$train==1,]
testset <- Sonar[Sonar$train==0,]
#get column index of train flag
trainColNum <- grep('train',names(trainset))
#remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]
#get column index of predicted variable in dataset
typeColNum <- grep('Class',names(Sonar))
#build model - linear kernel and C-classification with default cost (C=1)
svm_model <- svm(Class~ ., data=trainset, method='C-classification', kernel='linear')

#training set predictions
pred_train <-predict(x = svm_model,data = trainset)
mean(pred_train==trainset$Class)

#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Class)


#build model: radial kernel, default params
svm_model <- svm(Class~ ., data=trainset, method='C-classification', kernel='radial')

#print params
svm_model$cost
svm_model$gamma

#training set predictions
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Class)

#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Class)

#find optimal parameters in a specified range
tune_out <- tune.svm(x=trainset[,-typeColNum],y=trainset[,typeColNum],gamma=10^(-3:3),cost=c(0.01,0.1,1,10,100,1000),kernel='radial')
#print best values of cost and gamma
tune_out$best.parameters$cost
tune_out$best.parameters$gamma

#build model
svm_model <- svm(Class~ ., data=trainset, method='C-classification', kernel='radial',cost=tune_out$best.parameters$cost,gamma=tune_out$best.parameters$gamma)
#training set predictions
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Class)

#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Class)

table(testset$Class, pred_test)

plot(svm_model, Sonar, V1 ~ V2)

# plot test -------------------------------------------------
day = c(0,1,2,3,4,5,6)
weather = c(1,0,0,0,0,0,0)
happy = factor(c(T,F,F,F,F,F,F))

d = data.frame(day=day, weather=weather, happy=happy)
model = svm(happy ~ day + weather, data = d)
plot(model, d)

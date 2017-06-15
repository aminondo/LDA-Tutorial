# =====================================================================================================================
# ARTIFICIAL NEURAL NETWORKS
# =====================================================================================================================
#
library(neuralnet)
#
#
# ---------------------------------------------------------------------------------------------------------------------
# SQUARE ROOT
# ---------------------------------------------------------------------------------------------------------------------

#Going to create a neural network to perform square rooting

#Generate 50 random numbers uniformly distributed between 0 and 100

traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)


trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

# Now we are going to train our neural network

net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10)

#Plot the neural network
plot(net.sqrt)

#Test the neural network 
testdata <- as.data.frame((1:10)^2) 
net.results <- compute(net.sqrt, testdata) 

#Lets see the results
print(net.results$net.result)

output <- cbind(testdata,sqrt(testdata),as.data.frame(net.results$net.result))

colnames(output) <- c("Input","Expected Output","Neural Net Output")

print(output)

#Test the neural network 
testdata <- as.data.frame((11:20)^2) 
net.results <- compute(net.sqrt, testdata) 

#Lets see the results
print(net.results$net.result)

output <- cbind(testdata,sqrt(testdata),as.data.frame(net.results$net.result))

colnames(output) <- c("Input","Expected Output","Neural Net Output")

print(output)

#BE CAREFUL WITH EXTRAPOLATING!! 
# ---------------------------------------------------------------------------------------------------------------------
# BOSTON HOUSING
# ---------------------------------------------------------------------------------------------------------------------

# We are going to make a neural network to predict the median value homes (in $1000)
set.seed(500)
library(MASS)

housing <- Boston
head(housing)

# When taking in a previous dataset ensure there are no missing datapoints
apply(housing,2,function(x) sum(is.na(x)))

# Split into test and train data
index <- sample(1:nrow(housing),round(0.8*nrow(housing)))
train <- housing[index,]
test <- housing[-index,]

# Now we are going to normalize our data
maxs <- apply(housing, 2, max) 
mins <- apply(housing, 2, min)

# Now we are going to scale our data
scaled <- as.data.frame(scale(housing, center = mins, scale = maxs - mins))

train.s <- scaled[index,]
test.s <- scaled[-index,]

# Now we are going ot train the neural network
# the neural network function cannot accept formulas in the traditional R manner (y~.)
# We get around this by doing the following:
n <- names(train.s[,-14])
form <- as.formula(paste("medv ~", paste(n, collapse = " + ")))
form
nn <- neuralnet(form,data=train.s,hidden=c(5,3),linear.output=T)
plot(nn)

# Compute() in neuralnet pkg predicts the results using the neural network
pr.nn <- compute(nn,test.s[,-14])

# Now we need to scale back our values
pr.nn_ <- pr.nn$net.result*(max(housing$medv)-min(housing$medv))+min(housing$medv)

#This is the real values of the test data
test.r <- (test.s$medv)*(max(housing$medv)-min(housing$medv))+min(housing$medv)

#now we calculate the Mean Squared Error
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test.s)
MSE.nn


# Compare this to a Linear Model

lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
MSE.lm
print(paste(MSE.lm,MSE.nn))

# Plot it

par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

# ---------------------------------------------------------------------------------------------------------------------
# COLLEGES
# ---------------------------------------------------------------------------------------------------------------------
#Using the College dataset from the ISLR package try to make a neural net to predict whether a college is public or private
library(ISLR)
college_data = College
head(college_data)

#Let's make Private a binary value so it can be mapped to the neural network, 1=Yes, 0=No
college_data$Private = as.numeric(college_data$Private)-1

#Your first step will be to scale your data so it's easier to train the neural network
#Try applying the max min scaling method we used in the previous example


#Now Split the data into Test and Training Sets (See Solutions for an alternate way to do splitting)


#Now make your formula for the neuralnet function. Try using the method we used in the previous example

#Now train the neuralnet and plot it using neuralnet

#Now use the trained neuralnet to predict the tested values


#Make a confusion matrix comparing the actual values for Private and the Predicted


# ---------------------------------------------------------------------------------------------------------------------
# COLLEGES SOLUTIONS
# ---------------------------------------------------------------------------------------------------------------------
library(ISLR)
college_data = College

college_data$Private = as.numeric(college_data$Private)-1

#Scaling
maxs <- apply(college_data[,2:18], 2, max)
mins <- apply(college_data[,2:18], 2, min)
scaled.data <- as.data.frame(scale(college_data[,2:18],center = mins, scale = maxs - mins))
print(head(scaled.data,2))

#Splitting
library(caTools)
set.seed(101)

split = sample.split(college_data$Private, SplitRatio = 0.70)
train = subset(college_data, split == TRUE)
test = subset(college_data, split == FALSE)

#Formula
feats <- names(scaled.data)
f <- paste(feats,collapse=' + ')
f <- paste('Private ~',f)
f <- as.formula(f)
f

#Training the Neural Net
nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)
plot(nn)

#Predicting
predicted.nn.values <- compute(nn,test[2:18])

print(head(predicted.nn.values$net.result))
View(predicted.nn.values)
#Note that the values are rounded as some of the values produced by the neural net are not exactly 0 or 1
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)

#Confusion Matrix
table(test$Private,predicted.nn.values$net.result)

table(test$Private)
# ---------------------------------------------------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------------------------------------------------

# 1.http://gekkoquant.com/2012/05/26/neural-networks-with-r-simple-example/
# 2.https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
# 3.http://www.kdnuggets.com/2016/08/begineers-guide-neural-networks-r.html
# 4.http://blog.revolutionanalytics.com/2017/03/neural-networks-r.html
# 5.https://stats.stackexchange.com/questions/181/how-to-choose-the-number-of-hidden-layers-and-nodes-in-a-feedforward-neural-netw
#   The fourth link has a really good video attached for those interested
#   The fifth link has more information on how to choose the right number of hidden layers and nodes when creating your neral network.

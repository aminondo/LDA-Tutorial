library(ggplot2)
library(ISLR)

install.packages("gam")
library(gam)
## Split data set
Wage = Wage
plot(wage ~ year,data = Wage )
Wage = Wage[Wage$wage <175,]
Wage = Wage[Wage$wage > 50,]

plot(wage ~ year, data = Wage)
train.index = sample(c(T, F), nrow(Wage), replace = TRUE, prob = c(0.8, 0.2))
Wage = split(Wage, train.index)
names(Wage) = c("test", "train")
remove(train.index)

##Examine the different categories to try to determine which ones
##have some kind of statistical relevance
plot(wage ~ education, data = Wage$train)

##

plot(wage ~ year, data = Wage$train)
year0 =gam(wage ~ education, data = Wage$train)
year1 =gam(wage ~ education + s(year, 1), data =  Wage$train)
year2 =gam(wage ~ education + s(year, 2), data = Wage$train)
year3 =gam(wage ~ education + s(year, 3), data = Wage$train)
year4 =gam(wage ~ education + s(year, 4), data = Wage$train)
anova(year0,year1,year2,year3,year4)

##

## check if age is statistically significant ##
plot(wage ~ age)
age0 =gam(wage ~ education + s(year,1), data = Wage$train)
age1 =gam(wage ~ education + s(year,1)+s(age,1), data = Wage$train)
age2 =gam(wage ~ education + s(year,1)+s(age,2), data = Wage$train)
age3 =gam(wage ~ education + s(year,1)+s(age,3), data = Wage$train)
age4 =gam(wage ~ education + s(year,1)+s(age,4), data = Wage$train)
age5 = gam(wage ~ education + s(year,1)+s(age,5), data = Wage$train)
anova(age0,age1,age2,age3,age4,age5)

##

plot(wage ~ race)
race0 =gam(wage ~ education + s(year,1) + s(age, 3), data = Wage$train)
race1 =gam(wage ~ education + s(year,1) + s(age, 3) + race, data = Wage$train)
anova(race0,race1)

##
##check if health is statistically significant given that health is categorical

plot(wage ~ health)

##
##check if sex is statistically significant

plot(wage ~ sex)

##
error = function(actual, prediction){
  abs(prediction - actual)/actual
}

fit_f = gam(wage ~ education + s(year,1) + s(age, 3) + health + race, data = Wage$train)
class(fit_f)
par(mfrow=c(2,2), gam:: plot.gam(fit_f))
Wage$test$predictions = predict(fit_f, newdata = Wage$test)
Wage$test$error = error(Wage$test$wage, Wage$test$predictions)
avg_error = mean(Wage$test$error)
avg_error
nrow(Wage$test[Wage$test$error < .20, ])
nrow(Wage$test)

####################################################################################
################################# Red Blue #########################################
####################################################################################

##LOAD RED BLUE Data##
#load("C:/Users/Student/Downloads/red-blue-points.RData")
View(known)
known$bin = ifelse(known$colour == "blue", TRUE, FALSE)
train.index = sample(c(T, F), nrow(known), replace = TRUE, prob = c(0.8, 0.2))
known = split(known, train.index)
names(known) = c("test", "train")
remove(train.index)

ggplot(known$train, aes(x=x,y=y, color = colour)) +geom_point()
x0 = gam(bin ~ y, data = known$train, family = binomial)
x1 = gam(bin ~ y + poly(x,1), data = known$train, family = binomial)
x2 = gam(bin ~ y + poly(x,2), data = known$train, family = binomial)
x3 = gam(bin ~ y + poly(x,3), data = known$train, family = binomial)
x4 = gam(bin ~ y + poly(x,4), data = known$train, family = binomial)
x5 = gam(bin ~ y + poly(x,5), data = known$train, family = binomial)
x6 = gam(bin ~ y + poly(x,6), data = known$train, family = binomial)

anova(x0,x1,x2,x3,x4,x5,x6)

y0 = gam(bin ~ poly(x,4), data = known$train, family = binomial)
y1 = gam(bin ~ poly(x,4) + poly(y,1), data = known$train, family = binomial)
y2 = gam(bin ~ poly(x,4) + poly(y,2), data = known$train, family = binomial)
y3 = gam(bin ~ poly(x,4) + poly(y,3), data = known$train, family = binomial)
y4 = gam(bin ~ poly(x,4) + poly(y,4), data = known$train, family = binomial)
y5 = gam(bin ~ poly(x,4) + poly(y,5), data = known$train, family = binomial)
y6 = gam(bin ~ poly(x,4) + poly(y,6), data = known$train, family = binomial)
y7 = gam(bin ~ poly(x,4) + poly(y,7), data = known$train, family = binomial)

anova(y0,y1,y2,y3,y4,y5,y6,y7)
## y2 and y4 look decent

known$test$preds = format(round(predict(y2, newdata = known$test, type = "response"), 4), nsmall =  4)
known$test$color_pred = ifelse(known$test$preds > .50, 'blue','red')
table(known$test$colour,known$test$color_pred)

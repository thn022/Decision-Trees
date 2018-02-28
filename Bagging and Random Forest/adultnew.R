adultnew <- read.csv("adultnew.csv")
adult.test <- read.csv("adulttest.csv")

sum(is.na(adultnew))
sum(is.na(adult.test))
adultnew[adultnew == 9999] = NA
adult.test[adult.test == 9999] = NA
nrow(adultnew)
nrow(adult.test)
adultnew = na.omit(adultnew)
adult.test =na.omit(adult.test)
str(adultnew)
str(adult.test)

#adultnew <- na.omit(adultnew)

#put the variables in order
levels(adultnew$workclass)
table(adultnew$workclass)
adultnew$workclass <- factor(adultnew$workclass, 
                  levels = levels(adultnew$workclass)[c(4,6,5,1,2,7,8,3,9)])

adultnew <- droplevels(adultnew)


levels(adult.test$workclass)
table(adult.test$workclass)
adult.test$workclass <- factor(adult.test$workclass, 
                             levels = levels(adult.test$workclass)[c(4,6,5,1,2,7,8,3,9)])
adult.test <- droplevels(adult.test)


levels(adultnew$education)
adultnew$education <- factor(adultnew$education, 
                  levels = levels(adultnew$education)[c(10,16,2,12,15,8,9,7,6,3,13,4,1,11,5,14)])

levels(adult.test$education)
adult.test$education <- factor(adult.test$education, 
                             levels = levels(adult.test$education)[c(10,16,2,12,15,8,9,7,6,3,13,4,1,11,5,14)])

levels(adultnew$marital.status)
adultnew$marital.status <- factor(adultnew$marital.status, 
              levels = levels(adultnew$marital.status)[c(3,1,5,6,7,4,2)])

levels(adult.test$marital.status)
adult.test$marital.status <- factor(adult.test$marital.status, 
                                  levels = levels(adult.test$marital.status)[c(3,1,5,6,7,4,2)])


levels(adultnew$occupation)
adultnew$occupation <- factor(adultnew$occupation, 
                  levels = levels(adultnew$occupation)[c(13,3,8,12,4,10,6,7,1,5,14,9,11,2,15)])

levels(adult.test$occupation)
adult.test$occupation <- factor(adult.test$occupation, 
                              levels = levels(adult.test$occupation)[c(13,3,8,12,4,10,6,7,1,5,14,9,11,2,15)])


levels(adultnew$relationship)
adultnew$relationship <- factor(adultnew$relationship, 
                  levels = levels(adultnew$relationship)[c(6, 4, 1, 2, 3, 5)])

levels(adult.test$relationship)
adult.test$relationship <- factor(adult.test$relationship, 
                                levels = levels(adult.test$relationship)[c(6, 4, 1, 2, 3, 5)])



levels(adultnew$race)
adultnew$race <- factor(adultnew$race, 
                  levels = levels(adultnew$race)[c(5,2,1,4,3)])

levels(adult.test$race)
adult.test$race <- factor(adult.test$race, 
                        levels = levels(adult.test$race)[c(5,2,1,4,3)])


levels(adultnew$native.country)
adultnew$native.country <- factor(adultnew$native.country, 
                levels = levels(adultnew$native.country)[c(39,1,9,33,2,11,28,19,24,12,
                                                           35,3,5,20,16,30,22,31,23,40,
                                                           26,32,21,10,6,25,7,36,14,4,
                                                           18,13,27,34,37,41,8,38,29,17,15,42)])
levels(adult.test$native.country)
adult.test$native.country <- factor(adult.test$native.country, 
                                  levels = levels(adult.test$native.country)[c(39,1,9,33,2,11,28,19,24,12,
                                                                             35,3,5,20,16,30,22,31,23,40,
                                                                             26,32,21,10,6,25,7,36,14,4,
                                                                            18,13,27,34,37,41,8,38,29,17,15,42)])


str(adultnew)

levels(adult.test$Age) <- levels(adultnew$Age)
levels(adult.test$workclass) <- levels(adultnew$workclass)
levels(adult.test$fnlwgt) <- levels(adultnew$fnlwgt)
levels(adult.test$education) <- levels(adultnew$education)
levels(adult.test$education.num) <- levels(adultnew$education.num)
levels(adult.test$marital.status) <- levels(adultnew$marital.status)
levels(adult.test$occupation) <- levels(adultnew$occupation)
levels(adult.test$relationship) <- levels(adultnew$relationship)
levels(adult.test$race) <- levels(adultnew$race)
levels(adult.test$capital.gain) <- levels(adultnew$capital.gain)
levels(adult.test$sex) <- levels(adultnew$sex)
levels(adult.test$capital.loss) <- levels(adultnew$capital.loss)
levels(adult.test$hours.per.week) <- levels(adultnew$hours.per.week)
levels(adult.test$native.country) <- levels(adultnew$native.country)
levels(adult.test$Y) <- levels(adultnew$Y)






#fit and plot tree

library(rpart)
fit <- rpart(Y~., method = "class", data = adultnew)
summary(fit)


library(rattle)
fancyRpartPlot(fit,sub="")

#test tree
adult.pred <- predict(fit, newdata = adult.test, type = "class")
Y = adult.test[,15]
#Y.Test = Y[15]
table(Y,adult.pred)

sum(diag(prop.table(table(Y,adult.pred))))



#bagging
library(randomForest)
set.seed(18)
rf.bag = randomForest(Y~., data = adultnew,mtry=14, importance=TRUE)
rf.bag

importance(rf.bag)

pred.adult.bag = predict(rf.bag, newdata = adult.test, type="class")
sum(diag(prop.table(table(Y,pred.adult.bag))))

(11293+989)/15060



#randomForest
set.seed(18)
rf.adult = randomForest(Y~.,data = adultnew, importance=TRUE)
rf.adult

rf.pred = predict(rf.adult, newdata = adult.test, type="class")
sum(diag(prop.table(table(Y,rf.pred))))

(11322+991)/15060

#summary


(10772+1863)/15060 #Fit model
(11279+1038)/15060 #bagged model
(11322+991)/15060 #random forest







#rfimpute and then randomforest

#adult.imputed <- rfImpute(Y~., adultnew)

#rf.adult <-(Y~., data = adult.imputed)





#bagging
library(ipred)
set.seed(18)
bag.adult <- bagging(Y~., nbagg= 100, data = adultnew, coob=TRUE)
bag.adult
#.1575


set.seed(18)
bag.pred <- predict(bag.adult, newdata = adult.test)
sum(diag(prop.table(table(Y,bag.pred))))
(10783+2046)/15060 


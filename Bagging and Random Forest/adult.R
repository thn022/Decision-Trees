adult <- read.csv('adult.csv')
str(adult)
sum(is.na(adult))

adultnew <- read.csv("adultnew.csv")
 
adultnew[adultnew == 9999] = NA
sum(is.na(adultnew))
str(adultnew)

library(tree)
adult.tree <- tree(Y~., adultnew)
str(adultnew)

adultnew$relationship <- factor(adultnew$relationship, 
                                levels = levels(adultnew$relationship)[c(6, 4, 1, 2, 3, 5)])

library(rpart)
fit1 <- rpart(Y~., method = "class", data = adultnew)

summary(fit1)

plot(fit1, uniform = TRUE)
text(fit1)

table(adultnew$Y)

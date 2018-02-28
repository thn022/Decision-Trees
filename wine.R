wine <- read.csv('wine.csv')

table(wine$quality)

wine$quality <- gsub(0, 'poor', wine$quality)
wine$quality <- gsub(1, 'poor', wine$quality)
wine$quality <- gsub(2, 'poor', wine$quality)
wine$quality <- gsub(3, 'poor', wine$quality)
wine$quality <- gsub(4, 'poor', wine$quality)
wine$quality <- gsub(5, 'normal', wine$quality)
wine$quality <- gsub(6, 'normal', wine$quality)
wine$quality <- gsub(7, 'normal', wine$quality)
wine$quality <- gsub(8, 'excellent', wine$quality)
wine$quality <- gsub(9, 'excellent', wine$quality)
wine$quality <- gsub(10, 'excellent', wine$quality)
str(wine)
table(wine$quality)
sum(is.na(wine))

wine$quality = as.factor(wine$quality)

library(rpart)
fit1 <- rpart(quality~., method = "class", data = wine )
summary(fit1)

plot(fit1, uniform = TRUE)


library(tree)
wine.tree <- tree(quality~., wine)
summary(wine.tree)

plot(wine.tree)
text(wine.tree)



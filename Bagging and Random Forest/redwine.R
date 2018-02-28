redwine <- read.csv('redwine.csv')

redwine$quality <- gsub(3, 'poor', redwine$quality)
redwine$quality <- gsub(4, 'poor', redwine$quality)
redwine$quality <- gsub(5, 'normal', redwine$quality)
redwine$quality <- gsub(6, 'normal', redwine$quality)
redwine$quality <- gsub(7, 'normal', redwine$quality)
redwine$quality <- gsub(8, 'excellent', redwine$quality)

redwine$quality = as.factor(redwine$quality)

str(redwine)

library(tree)
redwine.tree <- tree(quality~.,redwine)
summary(redwine.tree)

plot(redwine.tree)
text(redwine.tree)



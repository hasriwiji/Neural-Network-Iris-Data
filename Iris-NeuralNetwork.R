data(iris)
iris$setosa <- iris$Species=="setosa"
iris$virginica <- iris$Species == "virginica"
iris$versicolor <- iris$Species == "versicolor"
iris.train.idx <- sample(x = nrow(iris), size = nrow(iris)*0.75)
training_set <- iris[iris.train.idx,]
test_set <- iris[-iris.train.idx,]
nrow(training_set)
nrow(test_set)
library(neuralnet)
iris.net <- neuralnet(setosa+versicolor+virginica ~ 
                        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                      data=training_set, hidden=c(10,10),rep=2, err.fct = "ce", 
                      linear.output = F, lifesign = "minimal", stepmax = 1000000,
                      threshold = 0.001)
plot(iris.net, rep="best")
iris.prediction <- compute(iris.net, test_set[-5:-8])
idx <- apply(iris.prediction$net.result, 1, which.max)
predicted <- c('setosa', 'versicolor', 'virginica')[idx]
cm=table(predicted, test_set$Species)
library(caret)
confusionMatrix(cm)

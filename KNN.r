library(class)
library(ggvis)
library(gmodels)
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE)
iris
names(iris)<-c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
#iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
summary(iris)
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
iris_norm
#iris_norm %>% ggvis(~Sepal.Length, ~Sepal.Width) %>% layer_points()
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.training <- iris_norm[ind==1, 1:4]
iris.training
iris.test <- iris_norm[ind==2, 1:4]
iris.test
iris.trainLabels <- iris[ind==1, 5]
iris.trainLabels
iris.testLabels <- iris[ind==2, 5]
iris.testLabels

iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
iris_pred
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)


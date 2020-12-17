options(stringsAsFactors = FALSE)
Abalone <- read.csv("TestData\\abalone.csv")
abalone_y <- Abalone[,1]
abalone_X <- data.matrix(Abalone[, 2:length(Abalone)])
training_X <- abalone_X[1:4000,]
training_y <- abalone_y[1:4000]
test_X <- abalone_X[4001:4100,]
test_y <- abalone_y[4001:4100]
predicted <- knn(training_X, training_y, test_X, 10)
sum(test_y == moda(predicted), na.rm = TRUE)
#test_y <- training_y
#predicted <- knn(training_X, training_y, training_X, 1)
#sum(test_y == predicted, na.rm = TRUE)

abalone <- read.csv("TestData\\abalone.csv")
californiahousing <- read.csv("TestData\\californiahousing.csv")
redwine <- read.csv("TestData\\winequality-red.csv")
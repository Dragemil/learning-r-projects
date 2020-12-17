necessaryConditionForKnn <- function(dataSet, p = 2) {
  labels <- dataSet[,1]
  variables <- data.matrix(dataSet[, -1])
  
  resultLabels <- knn(variables, labels, variables, 1, p)
  
  print(paste("Warunek konieczny dla p =", p, " spelniony: ", all(labels == resultLabels, sep = "")))
}

plotAbsoluteErrors <- function(dataSet, n, m, k, aggregator = moda, p = 2){
  nm <- n + m
  sampledData <- dataSet[sample(nm, size = nm, replace = FALSE), 1:3]
  dataNames <- names(sampledData)
  trainingSet <- data.matrix(sampledData[1:n, -1])
  trainingLabels <- sampledData[1:n, 1]
  testSet <- data.matrix(sampledData[n+1:m, -1])
  correctLabels <- sampledData[n+1:m, 1]
  
  resultLabels <- aggregator(knn(trainingSet, trainingLabels, testSet, k, p))
  correctPercent <- round(sum(resultLabels == correctLabels) / m * 100, 1)
  result <- data.frame(cbind(abs(correctLabels - resultLabels), testSet))
  
  ggplot(result, aes(x=result[, 2],y=result[, 3], col=result[, 1])) +
    geom_point() +
    ggtitle(paste("Wykres bledu bezwzglednego predykcji etykiety (", correctPercent, "% poprawnych)", sep = "")) +
    xlab(dataNames[2]) +
    ylab(dataNames[3]) +
    scale_color_gradient(low="green", high="red", name = paste("Blad bezwzgledny predykcji etykiety", dataNames[1]))
}
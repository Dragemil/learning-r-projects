getDistanceFunction <- function(p) {
  if (is.infinite(p))
    function(trainingSet, testData, p){
      apply(trainingSet, 1, function(trainingData){
        max(abs(trainingData - testData))
      })
    }
  else
    function(trainingSet, testData, p){
      apply(trainingSet, 1, function(trainingData){
        sum(abs(trainingData - testData)^p)^(1/p)
      })
    }
}

firstKMinimal <- function(testDistances, trainingLabels, k){
  head(trainingLabels[order(testDistances)], n = k)
}

knn <- function(trainingSet, trainingLabels, testSet, k, p = 2){
  distFun <- getDistanceFunction(p)
  allDistances <- apply(testSet, 1, distFun, trainingSet = trainingSet, p = p)
  t(apply(t(allDistances), 1, firstKMinimal, trainingLabels = trainingLabels, k = k))
}

knnWithCheck <- function(trainingSet, trainingLabels, testSet, k, p = 2){
  stopifnot(
    is.matrix(trainingSet),
    is.atomic(trainingLabels),
    is.matrix(testSet),
    k %% 1 == 0,
    length(k) == 1,
    p %% 1 == 0 | is.infinite(p),
    length(p) == 1
  )
  
  n <- length(trainingLabels)
  
  stopifnot(
    n == dim(trainingSet)[1],
    all.equal(colnames(trainingSet), colnames(testSet)),
    1 <= k && k <= n,
    p >= 1)

  knn(trainingSet, trainingLabels, testSet, k, p)
}

knn_c <- compiler::cmpfun(knn)
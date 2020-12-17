library(dplyr)
library(tidyr)
library(randomForest)

times5Crossvalidation <- function(dataSet, aggregator, k, p = 2) {
  n <- nrow(dataSet)
  sampledData <- dataSet[sample(n, size = n, replace = FALSE),]
  err <- 0
  mad <- 0
  mse <- 0
  labels <- sampledData[,1]
  variables <- data.matrix(sampledData[, -1])
  
  for (i in 0:4) {
    start <- floor(i / 5 * n) + 1
    end <- floor((i + 1) / 5 * n)
    len <- end - start 
    
    trainingSet <- variables[-end:-start,]
    trainingLabels <- labels[-end:-start]
    testSet <- variables[start:end,]
    correctLabels <- labels[start:end]
    resultLabels <- aggregator(knn(trainingSet, trainingLabels, testSet, k, p))
    
    err <- err + sum(resultLabels != correctLabels) / len
    mad <- mad + sum(abs(resultLabels - correctLabels)) / len
    mse <- mse + sum((resultLabels - correctLabels) ^ 2) / len
  }
  
  ret <- c(err, mad, mse)/5
  
  print(paste("Procent blednej klasyfikacji:", round((err * 100), 3), "%"))
  print(paste("Blad bezwzgledny:", round(mad, 4)))
  print(paste("Blad srendiokwadratowy:", round(mse, 4)))
}

shortenKs <- function(ks, num) {
  t(apply(ks, 1, head, n = num))
}

createKnnBenchmark <- function(fileName) {
  dataSet <- abalone <- read.csv(paste("TestData\\", fileName, sep = ""))
  
  aggregators <- c(moda, srednia_a, mediana, minkara1.5, minkara3.0, wazonyrand)
  aggNames <- c("moda", "srednia_a", "mediana", "minkara1.5", "minkara3.0", "wazonyrand")
  ks <- seq(1, 19, 2)
  maxk <- max(ks)
  ps <- c(1, 2, Inf)
  results <- list()
  titles <- vector()
  params <- matrix(nrow = 0, ncol = 3)
  result <- matrix(nrow = 0, ncol = 3)
  
  n <- nrow(dataSet)
  sampledData <- dataSet[sample(n, size = n, replace = FALSE),]
  labels <- sampledData[,1]
  variables <- data.matrix(sampledData[, -1])
  
  for (i in 0:4) {
    results[[i+1]] <- matrix(nrow = 0, ncol = 3)
    
    start <- floor(i / 5 * n) + 1
    end <- floor((i + 1) / 5 * n)
    len <- end - start 
    
    trainingSet <- variables[-end:-start,]
    trainingLabels <- labels[-end:-start]
    testSet <- variables[start:end,]
    correctLabels <- labels[start:end]
    
    for (p in ps) {
      maxkLabels <- knn(trainingSet, trainingLabels, testSet, maxk, p)
      
      for (k in ks) {
        kLabels <- shortenKs(maxkLabels, k)
        
        for (aggId in 1:6) {
          resultLabels <- aggregators[[aggId]](kLabels)
          
          err <- sum(resultLabels != correctLabels) / len / 5
          mad <- sum(abs(resultLabels - correctLabels)) / len / 5
          mse <- sum((resultLabels - correctLabels) ^ 2) / len / 5
          
          results[[i+1]] <- rbind(results[[i+1]], c(err, mad, mse))

          if (i==0) {
            params <- rbind(params, c(p, k, aggId))
            titles <- append(titles, paste("knn dla p=", p, ", k=", k, ", agregator ", aggNames[aggId], sep = ""))
          }
        }
      }
    }
    
  }
  
  for (i in 1:5) {
    if (i == 1) {
      result <- results[[i]]
    }
    else {
      result <- result + results[[i]]
    }
  }
  
  result <- t(apply(result, 1, function(r) c(round((r[1] * 100), 3), round(r[2], 4), round(r[3], 4))))

  result <- cbind(result, params)
  colnames(result) <- c("err", "mad", "mse", "p", "k", "aggId")
  rownames(result) <- titles
  
  write.csv(result, file = fileName)
}

plotKErrs <- function(dataSet) {
  aggregatedSet <- group_by(dataSet, k) %>%
    summarise(err = mean(err/100), mad = mean(mad), mse = mean(mse))
  
  ggplot(aggregatedSet, aes(x=k)) +
    geom_line(aes(y=err, colour = "err")) +
    geom_line(aes(y=mad, colour = "mad")) +
    geom_line(aes(y=mse, colour = "mse")) +
    labs(title = "Analiza bledow wzgledem parametru k", y="Wartosc bledu", x="k", caption="") +
    scale_colour_manual("",
                        breaks = c("err", "mad", "mse"),
                        values = c("red", "orange", "yellow"))
}

plotAggErrs <- function(dataSet) {
  grouped <- dataSet %>%
    mutate(agg = aggNames[aggId]) %>%
    group_by(agg) %>%
    summarise(err=mean(err/100),
              mad=mean(mad),
              mse=mean(mse))
  
  grLong <- grouped %>%
    gather("Blad","Wartosc",-agg)
  
  ggplot(grLong) + geom_col(aes(x=agg, y=Wartosc, fill=Blad), position='dodge')
}

plotPErrs <- function(dataSet) {
  dataSet$p = as.factor(dataSet$p)
  
  grouped <- dataSet %>%
    group_by(p) %>%
    summarise(err=mean(err/100),
              mad=mean(mad),
              mse=mean(mse))

  grLong <- grouped %>%
    gather("Blad","Wartosc",-p)

  ggplot(grLong) + geom_col(aes(x=p, y=Wartosc, fill=Blad), position='dodge')
}

forestCrossvalidation <- function(dataSet) {
  n <- nrow(dataSet)
  sampledData <- dataSet[sample(n, size = n, replace = FALSE),]
  err <- 0
  mad <- 0
  mse <- 0
  labels <- sampledData[,1]
  variables <- data.matrix(sampledData[, -1])
  
  for (i in 0:4) {
    start <- floor(i / 5 * n) + 1
    end <- floor((i + 1) / 5 * n)
    len <- end - start 
    
    trainingSet <- sampledData[-end:-start,]
    testSet <- variables[start:end,]
    correctLabels <- labels[start:end]

    forest <- randomForest(response ~ ., data = trainingSet)
    resultLabels <- round(predict(forest, testSet))
    
    err <- err + sum(resultLabels != correctLabels) / len / 5
    mad <- mad + sum(abs(resultLabels - correctLabels)) / len / 5
    mse <- mse + sum((resultLabels - correctLabels) ^ 2) / len / 5
  }

  print("Wyniki dla klasyfikacji dla lasu losowego")  
  print(paste("Procent blednej klasyfikacji: ", round((err * 100), 3), "%", sep = ""))
  print(paste("Blad bezwzgledny: ", round(mad, 4), sep = ""))
  print(paste("Blad srendiokwadratowy: ", round(mse, 4), sep = ""))
}

benchmarkedKnnCrossvalidation <- function(bDataSet) {
  errors <- sapply(bDataSet[,c("err", "mad", "mse")], mean)
  
  print("Wyniki dla klasyfikacji dla zagregowanego benchmarku knn")  
  print(paste("Procent blednej klasyfikacji: ", round(errors[1], 3), "%", sep = ""))
  print(paste("Blad bezwzgledny: ", round(errors[2], 4), sep = ""))
  print(paste("Blad srendiokwadratowy: ", round(errors[3], 4), sep = ""))
}


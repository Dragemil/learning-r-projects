
moda <- function(testLabels){
  if(is.vector(testLabels))
    testLabels
  else
    apply(testLabels, 1, function(labels){
      t <- table(labels)
      as.integer(sample(names(t)[t == max(t)], 1))
    })
}

srednia_a <- function(testLabels){
  if(dim(testLabels)[1] == 1)
    testLabels
  else
    apply(testLabels, 1, function(labels){
      mea <- mean(labels)
      ret1 <- labels[which.min(abs(labels-mea))]
      ret2 <- 2 * mea - ret1
      if (any(labels == ret2)) sample(c(ret1, ret2), 1) else ret1
    })
}

mediana <- function(testLabels){
  if(is.vector(testLabels))
    testLabels
  else
    apply(testLabels, 1, function(labels){
      med <- median(labels)
      if (med %% 1 == 0)
        med
      else {
        ret1 <- labels[which.min(abs(labels-med))]
        ret2 <- 2 * med - ret1
        if (any(labels == ret2)) sample(c(ret1, ret2), 1) else ret1
      }})
}

minkara1.5 <- function(testLabels){
  if(is.vector(testLabels))
    testLabels
  else
    apply(testLabels, 1, function(labels){
      labels[which.min(apply(as.matrix(labels), 1, function(label){
        abs(sum((labels-label)^1.5))
      }))]
  })
}

minkara3.0 <- function(testLabels){
  if(is.vector(testLabels))
    testLabels
  else
    apply(testLabels, 1, function(labels){
      labels[which.min(apply(as.matrix(labels), 1, function(label){
        abs(sum((labels-label)^3))
      }))]
  })
}

wazonyrand <- function(testLabels){
  if(is.vector(testLabels))
    testLabels
  else
    apply(testLabels, 1, function(labels){
      sample(labels, 1, prob = length(labels):1)
    })
}
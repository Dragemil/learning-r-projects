library(ggplot2)

source("knn.R")
source("aggregates.R")
source("testHelpers.R")
source("reportHelpers.R")

set.seed(1)

aggNames <- c("moda", "srednia_a", "mediana", "minkara1.5", "minkara3.0", "wazonyrand")

abalone <- read.csv("TestData\\abalone.csv")
californiahousing <- read.csv("TestData\\californiahousing.csv")
kinematics <- read.csv("TestData\\kinematics.csv")
redwine <- read.csv("TestData\\winequality-red.csv")

abaloneKnnBenchmarks <- read.csv("abalone.csv")
kinematicsKnnBenchmarks <- read.csv("kinematics.csv")
redwineKnnBenchmarks <- read.csv("winequality-red.csv")

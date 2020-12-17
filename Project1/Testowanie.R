library(sqldf)
library(dplyr)
library(data.table)
library(microbenchmark)

TestAndBenchmark <- function(expected, baseSol, dplyrSol, dataTableSol, withOrder = TRUE) {
  print("Checking each solution...")
  
  baseSolEq = all_equal(expected(), baseSol(), ignore_row_order = !withOrder)
  print(paste("Base solution correct:", baseSolEq))
  
  dplyrSolEq = all_equal(expected(), dplyrSol(), ignore_row_order = !withOrder)
  print(paste("Dplyr solution correct:", dplyrSolEq))
  
  dataTableSolEq = all_equal(expected(), dataTableSol(), ignore_row_order = !withOrder)
  print(paste("Data.table solution correct:", dataTableSolEq))
  
  print("Benchmarking each solution...")
  microbenchmark::microbenchmark(
    sqldf=expected(),
    base=baseSol(),
    dplyr=dplyrSol(),
    data.table=dataTableSol(),
    times = 10L
  )
}
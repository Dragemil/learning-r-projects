library("dplyr")
jc01 <- read.csv("data/JC-201901-citibike-tripdata.csv")
jc02 <- read.csv("data/JC-201902-citibike-tripdata.csv")
jc03 <- read.csv("data/JC-201903-citibike-tripdata.csv")
jc04 <- read.csv("data/JC-201904-citibike-tripdata.csv")
jc05 <- read.csv("data/JC-201905-citibike-tripdata.csv")
jc06 <- read.csv("data/JC-201906-citibike-tripdata.csv")
jc07 <- read.csv("data/JC-201907-citibike-tripdata.csv")
jc08 <- read.csv("data/JC-201908-citibike-tripdata.csv")
jc09 <- read.csv("data/JC-201909-citibike-tripdata.csv")
jc10 <- read.csv("data/JC-201910-citibike-tripdata.csv")
jc11 <- read.csv("data/JC-201911-citibike-tripdata.csv")
jc12 <- read.csv("data/JC-201912-citibike-tripdata.csv")

datum <- list(jc01, jc02, jc03, jc04, jc05, jc06, jc07, jc08, jc09, jc10, jc11, jc12)

aggregatedBikes <- function(hourStart, hourEnd, month) {
  start <- datum[[month]] %>%
    dplyr::filter(as.integer(substr(starttime, 11, 13)) >= hourStart 
                  & as.integer(substr(starttime, 11, 13)) <= hourEnd) %>%
    group_by(
      station.name = start.station.name, 
      station.latitude = start.station.latitude, 
      station.longitude = start.station.longitude) %>%
    summarise(count = n()) %>%
    arrange(station.name)
  
  end <- datum[[month]] %>%
    dplyr::filter(as.integer(substr(starttime, 11, 13)) >= hourStart 
                  & as.integer(substr(starttime, 11, 13)) <= hourEnd) %>%
    group_by(
      station.name = end.station.name, 
      station.latitude = end.station.latitude, 
      station.longitude = end.station.longitude) %>%
    summarise(count = n()) %>%
    arrange(station.name)
  
  allBikes <- full_join(
    start,
    end,
    by=c("station.name", "station.latitude", "station.longitude"),
    suffix=c(".s", ".e"))
  
  allBikes$count.s[is.na(allBikes$count.s)] <- 0
  allBikes$count.e[is.na(allBikes$count.e)] <- 0
    
  allBikes$difference = allBikes$count.e - allBikes$count.s
  
  select(allBikes, station.name, station.latitude, station.longitude, difference)
}

mostFrequentStations <- function(hourStart, hourEnd, month) {
  start <- datum[[month]] %>%
    dplyr::filter(as.integer(substr(starttime, 11, 13)) >= hourStart 
                  & as.integer(substr(starttime, 11, 13)) <= hourEnd) %>%
    group_by(station.name = start.station.name) %>%
    summarise(count = n())

  end <- datum[[month]] %>%
    dplyr::filter(as.integer(substr(starttime, 11, 13)) >= hourStart 
                  & as.integer(substr(starttime, 11, 13)) <= hourEnd) %>%
    group_by(station.name = end.station.name) %>%
    summarise(count = n())

    bind_rows(start, end) %>%
      dplyr::group_by(station.name) %>%
      summarise_all(funs(sum(., na.rm=TRUE))) %>%
      dplyr::arrange(desc(count)) %>%
      head(10)
}

freeriders <- function(hourStart, hourEnd, month) {
  datum[[month]] %>%
    dplyr::filter(as.integer(substr(starttime, 11, 13)) >= hourStart 
                  & as.integer(substr(starttime, 11, 13)) <= hourEnd) %>%
    dplyr::filter(tripduration > 300) %>%
    mutate(dayOfTheWeek = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                            "Friday", "Saturday")[as.POSIXlt(starttime)$wday + 1]) %>%
    mutate(freeride = as.integer(start.station.id == end.station.id)) %>%
    group_by(dayOfTheWeek, usertype) %>%
    summarise(fraction = sum(freeride)/n() * 100) %>%
    dplyr::ungroup()
}

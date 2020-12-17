library(sqldf)
library(dplyr)
library(data.table)

#2

source("WczytanieDanych.R")

##2.1
Query2_1 <- function() {
  sqldf(
    "SELECT
      Users.DisplayName,
      Users.Age,
      Users.Location,
      SUM(Posts.FavoriteCount) AS FavoriteTotal,
      Posts.Title AS MostFavoriteQuestion,
      MAX(Posts.FavoriteCount) AS MostFavoriteQuestionLikes
    FROM Posts
    JOIN Users ON Users.Id=Posts.OwnerUserId
    WHERE Posts.PostTypeId=1
    GROUP BY OwnerUserId
    ORDER BY FavoriteTotal DESC
    LIMIT 10"
  )
}

##2.2

Query2_2 <- function() {
  Query2_2 <- subset(Posts, PostTypeId == 1) 
  Query2_2 <- merge(Query2_2, Users, by.x = "OwnerUserId", by.y = "Id")
  
  Query2_2 <- do.call(rbind, lapply(split(Query2_2, as.factor(Query2_2$OwnerUserId)),  function(x) {
    cbind(
      data.frame(x[which.max(x$FavoriteCount)[1],]),
      data.frame(FavoriteTotal = sum(x$FavoriteCount, na.rm = TRUE)))}))
  Query2_2 <- Query2_2[order(-Query2_2$FavoriteTotal),]
  Query2_2 <- head(Query2_2, 10)
  Query2_2[c("MostFavoriteQuestion","MostFavoriteQuestionLikes")] <- Query2_2[c("Title", "FavoriteCount")]
  Query2_2 <- Query2_2[,c("DisplayName", "Age", "Location", "FavoriteTotal", "MostFavoriteQuestion", "MostFavoriteQuestionLikes")]
}

print(all_equal(Query2_1, Query2_2, ignore_row_order = FALSE))

##2.3

Query2_3 <- function() {
  Query2_3 <- Posts %>%
    filter(PostTypeId==1) %>%
    inner_join(Users, by = c("OwnerUserId" = "Id")) %>%
    group_by(OwnerUserId) %>%
    mutate(FavoriteTotal = sum(FavoriteCount, na.rm = TRUE)) %>%
    slice(which.max(FavoriteCount)) %>%
    ungroup() %>%
    select(DisplayName, Age, Location, FavoriteTotal, MostFavoriteQuestion = Title, MostFavoriteQuestionLikes = FavoriteCount) %>%
    arrange(desc(FavoriteTotal)) %>%
    head(10)
}

print(all_equal(Query2_1, Query2_3, ignore_row_order = FALSE))

##2.4

Query2_4 <- function() {
  Query2_4 <- merge(as.data.table(Posts)[PostTypeId == 1], as.data.table(Users), by.x = "OwnerUserId", by.y = "Id")
  Query2_4 <- Query2_4[,.(
    DisplayName,
    Age,
    Location,
    FavoriteTotal = sum(FavoriteCount, na.rm = TRUE),
    MostFavoriteQuestion = Title,
    FavoriteCount), by = OwnerUserId]
  Query2_4 <- Query2_4[,.SD[which.max(FavoriteCount)], by = OwnerUserId]
  Query2_4 <- setorder(Query2_4, -FavoriteTotal, na.last = TRUE)
  Query2_4 <- Query2_4[1:10, .(DisplayName, Age, Location, FavoriteTotal, MostFavoriteQuestion, MostFavoriteQuestionLikes = FavoriteCount)]
}

print(all_equal(Query2_1, Query2_4, ignore_row_order = FALSE))



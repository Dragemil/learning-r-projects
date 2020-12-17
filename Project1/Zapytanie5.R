library(sqldf)
library(dplyr)
library(data.table)

#5

source("WczytanieDanych.R")

##5.1

Query5_1 <- function() {
  sqldf(
    "SELECT
    Posts.Title,
    CmtTotScr.CommentsTotalScore
    FROM (
      SELECT
      PostID,
      UserID,
      SUM(Score) AS CommentsTotalScore
      FROM Comments
      GROUP BY PostID, UserID
    ) AS CmtTotScr
    JOIN Posts ON Posts.ID=CmtTotScr.PostID AND Posts.OwnerUserId=CmtTotScr.UserID
    WHERE Posts.PostTypeId=1
    ORDER BY CmtTotScr.CommentsTotalScore DESC
    LIMIT 10"
  )
}

##5.2

Query5_2 <- function() {
  CmtTotScr <- aggregate(Comments$Score, by = Comments[, c("UserId", "PostId")], sum)
  CmtTotScr["CommentsTotalScore"] <- CmtTotScr["x"]
  CmtTotScr <- CmtTotScr[,-3]
  
  Query5_2 <- merge(Posts[Posts$PostTypeId == 1,], CmtTotScr, by.x = c("Id", "OwnerUserId"), by.y = c("PostId", "UserId"))
  Query5_2 <- Query5_2[order(-Query5_2$CommentsTotalScore),] %>% head(10)
  Query5_2 <- Query5_2[,c("Title", "CommentsTotalScore")]
}

print(all_equal(Query5_1, Query5_2, ignore_row_order = FALSE))

##5.3

Query5_3 <- function() {
  CmtTotScr <- Comments %>%
    group_by(PostId, UserId) %>%
    summarise(CommentsTotalScore = sum(Score))
  
  Query5_3 <- Posts %>%
    filter(PostTypeId == 1) %>%
    inner_join(CmtTotScr, by = c("Id" = "PostId", "OwnerUserId" = "UserId")) %>%
    arrange(desc(CommentsTotalScore)) %>%
    head(10) %>%
    select(Title, CommentsTotalScore)
}

print(all_equal(Query5_1, Query5_3, ignore_row_order = FALSE))

##5.4

Query5_4 <- function() {
  CmtTotScr <- as.data.table(Comments)
  CmtTotScr <- CmtTotScr[, .(CommentsTotalScore = sum(Score)), keyby = .(UserId, PostId)]
  
  Query5_4 <- merge(as.data.table(Posts)[PostTypeId == 1], CmtTotScr, by.x = c("Id", "OwnerUserId"), by.y = c("PostId", "UserId"))
  Query5_4 <- setorder(Query5_4, -CommentsTotalScore)
  Query5_4 <- Query5_4[1:10, .(Title, CommentsTotalScore)]
}

print(all_equal(Query5_1, Query5_4, ignore_row_order = FALSE))
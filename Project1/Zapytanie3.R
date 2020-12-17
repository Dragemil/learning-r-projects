library(sqldf)
library(dplyr)
library(data.table)

#3

source("WczytanieDanych.R")

##3.1

Query3_1 <- function() {
  sqldf(
    "SELECT
    Posts.ID,
    Posts.Title,
    Posts2.PositiveAnswerCount
    FROM Posts
    JOIN (
      SELECT
      Posts.ParentID,
      COUNT(*) AS PositiveAnswerCount
      FROM Posts
      WHERE Posts.PostTypeID=2 AND Posts.Score>0
      GROUP BY Posts.ParentID
    ) AS Posts2
    ON Posts.ID=Posts2.ParentID
    ORDER BY Posts2.PositiveAnswerCount DESC
    LIMIT 10"
  )
}

##3.2

Query3_2 <- function() {
  Posts2 = subset(Posts, PostTypeId == 2 & Score > 0)
  Posts2 <- aggregate(Posts2, by = list(Posts2$ParentId), length)
  Posts2 <- transform(Posts2, PositiveAnswerCount = Posts2[,2])
  Posts2["ParentId"] = Posts2[,1]
  Posts2 <- Posts2[,c("ParentId", "PositiveAnswerCount")]
  
  Query3_2 <- merge(Posts, Posts2, by.x = "Id", by.y = "ParentId")
  Query3_2 <- Query3_2[,c("Id", "Title", "PositiveAnswerCount")]
  Query3_2 <- Query3_2[order(-Query3_2$PositiveAnswerCount),]
  Query3_2 <- head(Query3_2, 10)
}

print(all_equal(Query3_1, Query3_2, ignore_row_order = FALSE))

##3.3

Query3_3 <- function() {
  Posts2 = Posts %>%
    filter(PostTypeId == 2 & Score > 0) %>%
    group_by(ParentId) %>%
    summarise(PositiveAnswerCount = n())
  
  Query3_3 = inner_join(Posts, Posts2, by = c("Id" = "ParentId")) %>%
    arrange(desc(PositiveAnswerCount)) %>%
    head(10) %>%
    select(Id, Title, PositiveAnswerCount)
}

print(all_equal(Query3_1, Query3_3, ignore_row_order = FALSE))

##3.4

Query3_4 <- function() {
  Posts2 <- as.data.table(Posts)
  Posts2 <- Posts2[PostTypeId == 2 & Score > 0, .(PositiveAnswerCount = .N), keyby = ParentId]
  
  Query3_4 <- merge(as.data.table(Posts), Posts2, by.x = "Id", by.y = "ParentId")
  Query3_4 <- Query3_4[,.(Id, Title, PositiveAnswerCount)]
  Query3_4 <- setorder(Query3_4, -PositiveAnswerCount)
  Query3_4 <- Query3_4[1:10]
}

print(all_equal(Query3_1, Query3_4, ignore_row_order = FALSE))


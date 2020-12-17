library(sqldf)
library(dplyr)
library(data.table)

#1

source("WczytanieDanych.R")

##1.1

Query1_1 <- function() {
  sqldf(
    "SELECT
    Posts.Title,
    UpVotesPerYear.Year,
    MAX(UpVotesPerYear.Count) AS Count
    FROM (
      SELECT
      PostId,
      COUNT(*) AS Count,
      STRFTIME('%Y', Votes.CreationDate) AS Year
      FROM Votes
      WHERE VoteTypeId=2
      GROUP BY PostId, Year
    ) AS UpVotesPerYear
    JOIN Posts ON Posts.Id=UpVotesPerYear.PostId
    WHERE Posts.PostTypeId=1
    GROUP BY Year"
  )
}

##1.2

Query1_2 <- function() {
  UpVotesPerYear <- subset(Votes, VoteTypeId == 2)
  UpVotesPerYear <- transform(UpVotesPerYear, Year=substring(CreationDate, 1, 4))
  UpVotesPerYear <- UpVotesPerYear[,c("PostId","Year")]
  UpVotesPerYear <- aggregate(UpVotesPerYear, by = UpVotesPerYear[, c("Year", "PostId")], length)
  UpVotesPerYear <- transform(UpVotesPerYear, Count = UpVotesPerYear[,3])
  UpVotesPerYear <- UpVotesPerYear[,c("PostId", "Count", "Year")]
  
  Query1_2 <- merge(UpVotesPerYear, Posts[Posts$PostTypeId==1,], by.x = "PostId", by.y = "Id")
  Query1_2 <- do.call(rbind, lapply(split(Query1_2, as.factor(Query1_2$Year)), function(z) {z[which.max(z$Count),]}))
  Query1_2 <- Query1_2[,c("Title", "Year", "Count")]
}

print(all_equal(Query1_1, Query1_2))

##1.3

Query1_3 <- function() {
  UpVotesPerYear <- Votes %>%
    filter(VoteTypeId == 2) %>%
    mutate(Year = substring(CreationDate, 1, 4)) %>%
    select(PostId, Year) %>%
    group_by(PostId, Year) %>%
    summarise(Count = n())
  
  Query1_3 = UpVotesPerYear %>%
    inner_join(Posts, by = c("PostId" = "Id")) %>%
    filter(PostTypeId == 1) %>%
    group_by(Year) %>%
    filter(Count == max(Count)) %>%
    select(Title, Count, Year)
}

print(all_equal(Query1_1, Query1_3))

##1.4

Query1_4 <- function() {
  UpVotesPerYear <- as.data.table(Votes)
  UpVotesPerYear <- UpVotesPerYear[VoteTypeId == 2, .(PostId, Year = substring(CreationDate, 1, 4))]
  UpVotesPerYear <- UpVotesPerYear[, Count := .N, by = .(PostId, Year)][, .SD[1], by = .(PostId, Year)]
  
  Query1_4 <- merge(as.data.table(Posts)[PostTypeId == 1], UpVotesPerYear, by.x = "Id", by.y = "PostId")
  Query1_4 <- Query1_4[, .(Title, Year, Count)][,.SD[which.max(Count)], by = Year]
}

print(all_equal(Query1_1, Query1_4))

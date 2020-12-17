library(sqldf)
library(dplyr)
library(data.table)

#7

source("WczytanieDanych.R")

##7.1

Query7_1 <- function() {
  sqldf(
    "SELECT
    Posts.Title,
    VotesByAge2.OldVotes
    FROM Posts
    JOIN (
      SELECT
      PostId,
      MAX(CASE WHEN VoteDate = 'new' THEN Total ELSE 0 END) NewVotes,
      MAX(CASE WHEN VoteDate = 'old' THEN Total ELSE 0 END) OldVotes,
      SUM(Total) AS Votes
      FROM (
        SELECT
        PostId,
        CASE STRFTIME('%Y', CreationDate)
        WHEN '2017' THEN 'new'
        WHEN '2016' THEN 'new'
        ELSE 'old'
        END VoteDate,
        COUNT(*) AS Total
        FROM Votes
        WHERE VoteTypeId=2
        GROUP BY PostId, VoteDate
      ) AS VotesByAge
      GROUP BY VotesByAge.PostId
      HAVING NewVotes=0
    ) AS VotesByAge2 ON VotesByAge2.PostId=Posts.ID
    WHERE Posts.PostTypeId=1
    ORDER BY VotesByAge2.OldVotes DESC
    LIMIT 10"
  )
}

##7.2

Query7_2 <- function() {
  VotesByAge <- Votes[Votes$VoteTypeId == 2, c("CreationDate", "PostId")]
  VotesByAge <- transform(VotesByAge, VoteDate = substring(CreationDate, 1, 4))
  VotesByAge <- transform(VotesByAge, VoteDate = ifelse(VoteDate == 2016 | VoteDate == 2017, "new", "old"))
  VotesByAge <- aggregate(VotesByAge, by = VotesByAge[,c("PostId", "VoteDate")], length)
  VotesByAge["Total"] <- VotesByAge[,3]
  VotesByAge <- VotesByAge[,c("PostId", "VoteDate", "Total")]
  
  NewVotes <- aggregate(ifelse(VotesByAge$VoteDate == "new", VotesByAge$Total, 0L), by = list(VotesByAge$PostId), max)
  OldVotes <- aggregate(ifelse(VotesByAge$VoteDate == "old", VotesByAge$Total, 0L), by = list(VotesByAge$PostId), max)
  VotesByAge2 <- cbind(NewVotes, OldVotes[,2])
  colnames(VotesByAge2) = c("PostId", "NewVotes", "OldVotes")
  VotesByAge2 <- subset(VotesByAge2, NewVotes == 0)
  
  Query7_2 <- subset(Posts, PostTypeId == 1)
  Query7_2 <- merge(Query7_2, VotesByAge2, by.x = "Id", by.y = "PostId")
  Query7_2 <- Query7_2[order(-Query7_2$OldVotes),]
  Query7_2 <- head(Query7_2, 10)
  Query7_2 <- Query7_2[,c("Title", "OldVotes")]
}

print(all_equal(Query7_1, Query7_2, ignore_row_order = FALSE))

##7.3

Query7_3 <- function() {
  VotesByAge <- Votes %>%
    filter(VoteTypeId == 2) %>%
    select(CreationDate, PostId) %>%
    mutate(VoteDate = substring(CreationDate, 1, 4)) %>%
    mutate(VoteDate = case_when (VoteDate == 2016 | VoteDate == 2017 ~ "new", TRUE ~ "old" )) %>%
    group_by(PostId, VoteDate) %>%
    summarise(Total = n()) %>%
    ungroup()
  
  VotesByAge2 <- VotesByAge %>%
    group_by(PostId) %>%
    mutate(
      NewVotes = max(case_when(VoteDate == "new" ~ Total, TRUE ~ 0L)),
      OldVotes = max(case_when(VoteDate == "old" ~ Total, TRUE ~ 0L))) %>%
    filter(NewVotes == 0) %>%
    select(PostId, NewVotes, OldVotes)
  
  Query7_3 = Posts %>%
    filter(PostTypeId == 1) %>%
    inner_join(VotesByAge2, by = c("Id" = "PostId")) %>%
    arrange(desc(OldVotes)) %>%
    head(10) %>%
    select(Title, OldVotes)
}
  
print(all_equal(Query7_1, Query7_3, ignore_row_order = FALSE))

##7.4

Query7_4 <- function() {
  VotesByAge <- as.data.table(Votes)[VoteTypeId == 2, .(CreationDate, PostId)]
  VotesByAge <- VotesByAge[,.(PostId, VoteDate = substring(CreationDate, 1, 4))]
  VotesByAge <- VotesByAge[,.(PostId, VoteDate = fifelse(VoteDate == 2016 | VoteDate == 2017, "new", "old"))]
  VotesByAge <- VotesByAge[,.N, by = .(PostId, VoteDate)][,.(PostId, VoteDate, Total = N)]
  
  VotesByAge2 <- VotesByAge[, .(NewVotes = max(fifelse(VoteDate == "new", Total, 0L)), OldVotes = max(fifelse(VoteDate == "old", Total, 0L))), by = .(PostId)]
  VotesByAge2 <- VotesByAge2[NewVotes == 0]
  
  Query7_4 <- as.data.table(Posts)[PostTypeId == 1]
  Query7_4 <- merge(Query7_4, VotesByAge2, by.x = "Id", by.y = "PostId")
  Query7_4 <- setorder(Query7_4, -OldVotes)
  Query7_4 <- Query7_4[1:10, .(Title, OldVotes)]
}

print(all_equal(Query7_1, Query7_4, ignore_row_order = FALSE))

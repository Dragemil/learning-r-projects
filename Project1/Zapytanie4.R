library(sqldf)

#4

source("WczytanieDanych.R")

##4.1

Query4_1 <- function() {
  sqldf(
    "SELECT
    Questions.Id,
    Questions.Title,
    BestAnswers.MaxScore,
    Posts.Score AS AcceptedScore,
    BestAnswers.MaxScore-Posts.Score AS Difference
    FROM (
      SELECT Id, ParentId, MAX(Score) AS MaxScore
      FROM Posts
      WHERE PostTypeId==2
      GROUP BY ParentId
    ) AS BestAnswers
    JOIN (
      SELECT * FROM Posts
      WHERE PostTypeId==1
    ) AS Questions
    ON Questions.Id=BestAnswers.ParentId
    JOIN Posts ON Questions.AcceptedAnswerId=Posts.Id
    WHERE Difference>50
    ORDER BY Difference DESC"
  )
}

##4.2

Query4_2 <- function() {
  BestAnswers <- Posts[Posts$PostTypeId == 2,]
  BestAnswers <- do.call(rbind, lapply(split(BestAnswers, as.factor(BestAnswers$ParentId)), function(z) {z[which.max(z$Score),]}))
  BestAnswers["MaxScore"] = BestAnswers["Score"]
  BestAnswers <- BestAnswers[,c("Id","ParentId","MaxScore")]
  
  Questions <- Posts[Posts$PostTypeId == 1,]
  
  Query4_2 <- merge(BestAnswers, Questions, by.x = "ParentId", by.y = "Id")
  Query4_2 <- merge(Query4_2, Posts, by.x = "AcceptedAnswerId", by.y = "Id")
  Query4_2 <- transform(Query4_2, Difference = MaxScore - Score.y) 
  Query4_2 <- subset(Query4_2, Difference > 50)
  Query4_2 <- transform(Query4_2, Title = Title.x, AcceptedScore = Score.y, Id = ParentId.x)
  Query4_2 <- Query4_2[,c("Id", "Title", "MaxScore", "AcceptedScore", "Difference")]
  Query4_2 <- Query4_2[order(-Query4_2$Difference),]
}

print(all_equal(Query4_1, Query4_2, ignore_row_order = FALSE))

##4.3

Query4_3 <- function() {
  BestAnswers <- Posts %>%
    filter(PostTypeId == 2) %>%
    group_by(ParentId) %>%
    slice(which.max(Score)) %>%
    select(Id, ParentId, MaxScore = Score)
  
  Questions <- filter(Posts, PostTypeId == 1)
  
  Query4_3 <- inner_join(BestAnswers, Questions, by = c("ParentId" = "Id")) %>%
    inner_join(Posts, by = c("AcceptedAnswerId" = "Id")) %>%
    select(Id = ParentId.x, Title = Title.x, MaxScore, AcceptedScore = Score.y) %>%
    mutate(Difference = MaxScore - AcceptedScore) %>%
    filter(Difference > 50) %>%
    arrange(desc(Difference))
}

print(all_equal(Query4_1, Query4_3, ignore_row_order = FALSE))

##4.4

Query4_4 <- function() {
  BestAnswers <- as.data.table(Posts)[PostTypeId == 2, .(Id, ParentId, MaxScore = max(Score)), keyby = Score]
  BestAnswers <- BestAnswers[, .(Id, ParentId, MaxScore)]
  
  Questions <- as.data.table(Posts)[PostTypeId==1]
  
  Query4_4 <- merge(BestAnswers, Questions, by.x = "ParentId", by.y = "Id")
  Query4_4 <- merge(Query4_4, as.data.table(Posts), by.x = "AcceptedAnswerId", by.y = "Id")
  Query4_4 <- Query4_4[,.(Id = ParentId.x, Title = Title.x, MaxScore, AcceptedScore = Score.y, Difference = MaxScore - Score.y)]
  Query4_4 <- Query4_4[Difference > 50]
  Query4_4 <- setorder(Query4_4, -Difference)[, .SD[1], by = "Id"]
}

print(all_equal(Query4_1, Query4_4, ignore_row_order = FALSE))

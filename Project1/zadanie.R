library(sqldf)

# Wczytanie danych

options(stringsAsFactors=FALSE)

Badges <- read.csv("travel_stackexchange_com\\Badges.csv")
Comments <- read.csv("travel_stackexchange_com\\Comments.csv")
PostLinks <- read.csv("travel_stackexchange_com\\PostLinks.csv")
Posts <- read.csv("travel_stackexchange_com\\Posts.csv")
Tags <- read.csv("travel_stackexchange_com\\Tags.csv")
Users <- read.csv("travel_stackexchange_com\\Users.csv")
Votes <- read.csv("travel_stackexchange_com\\Votes.csv")

#1

##1.1

Query1_1 = sqldf(
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

#2

##2.1

Query2_1 = sqldf(
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

#3

##3.1

Query3_1 = sqldf(
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

#4

##4.1

Query4_1 = sqldf(
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

#5

##5.1

Query5_1 = sqldf(
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

#6

##6.1

Query6_1 = sqldf(
  "SELECT DISTINCT
  Users.Id,
  Users.DisplayName,
  Users.Reputation,
  Users.Age,
  Users.Location
  FROM (
    SELECT
    Name, UserID
    FROM Badges
    WHERE Name IN (
      SELECT
      Name
      FROM Badges
      WHERE Class=1
      GROUP BY Name
      HAVING COUNT(*) BETWEEN 2 AND 10
    )
    AND Class=1
  ) AS ValuableBadges
  JOIN Users ON ValuableBadges.UserId=Users.Id"
)

#7

##7.1

Query7_1 = sqldf(
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
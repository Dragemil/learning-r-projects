library(sqldf)
library(dplyr)
library(data.table)

#6

source("WczytanieDanych.R")

##6.1

Query6_1 <- function() {
  sqldf(
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
}
  

##6.2

Query6_2 <- function() {
  Badges1 <- Badges[Badges$Class == 1, "Name"]
  Badges1 <- aggregate(Badges1, by = list(Badges1), length)
  Badges1["Name"] <- Badges1[,1]
  Badges1 <- Badges1[Badges1[,2] >= 2 & Badges1[,2] <= 10, "Name"]
  
  ValuableBadges <- Badges[Badges$Class == 1 &  Badges$Name %in% Badges1, c("Name", "UserId")]
  
  Query6_2 <- merge(Users, ValuableBadges, by.x = "Id", by.y = "UserId")
  Query6_2 <- unique(Query6_2[,c("Id", "DisplayName", "Reputation", "Age", "Location")])
}

print(all_equal(Query6_1(), Query6_2()))

##6.3

Query6_3 <- function() {
  Badges1 <- simplify2array(Badges %>%
    filter(Class == 1) %>%
    group_by(Name) %>%
    summarise(Cnt = n()) %>%
    filter(Cnt >= 2 & Cnt <= 10) %>%
    select(Name))
  
  ValuableBadges <- Badges %>%
    filter(Class == 1 & Name %in% Badges1) %>%
    select(Name, UserId)
  
  Query6_3 <- inner_join(Users, ValuableBadges, by = c("Id" = "UserId")) %>%
    select(Id, DisplayName, Reputation, Age, Location) %>%
    distinct()
}

print(all_equal(Query6_1(), Query6_3()))

##6.4

Query6_4 <- function() {
  Badges1 <- as.data.table(Badges)[Class == 1, .N, keyby = Name]
  Badges1 <- Badges1[N >= 2 & N <= 10, Name]
  
  ValuableBadges <- as.data.table(Badges)[Class == 1 & Name %in% Badges1, .(Name, UserId)]
  
  Query6_4 <- merge(as.data.table(Users), ValuableBadges, by.x = "Id", by.y = "UserId")
  Query6_4 <- Query6_4[, .SD[1], by = .(Id, DisplayName, Reputation, Age, Location)]
  Query6_4 <- Query6_4[,.(Id, DisplayName, Reputation, Age, Location)]
}

print(all_equal(Query6_1(), Query6_4()))

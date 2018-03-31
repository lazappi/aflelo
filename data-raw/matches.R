library("tidyverse")
library("devtools")

url <- "https://afltables.com/afl/stats/biglists/bg3.txt"
matches_raw <- read_table(url, skip = 2, col_names = FALSE,
                          col_types = cols(
                              .default = col_character(),
                              X1       = col_double()
                         ))

matches <- matches_raw %>%
    rename(ID = X1, Date = X2, Round = X3, HomeTeam = X4, HomeScore = X5,
           AwayTeam = X6, AwayScore = X7, Ground = X8) %>%
    mutate(Date = as.Date(Date, format = "%e-%b-%Y")) %>%
    mutate(Season = format(Date, "%Y")) %>%
    mutate(HomeTeam = if_else(HomeTeam == "Brisbane Lions", "Brisbane",
                              HomeTeam)) %>%
    mutate(HomeTeam = if_else(HomeTeam == "Kangaroos", "North Melbourne",
                              HomeTeam)) %>%
    mutate(HomeTeam = if_else(HomeTeam == "Western Bulldog", "Western Bulldogs",
                              HomeTeam)) %>%
    mutate(AwayTeam = if_else(AwayTeam == "Brisbane Lions", "Brisbane",
                              AwayTeam)) %>%
    mutate(AwayTeam = if_else(AwayTeam == "Kangaroos", "North Melbourne",
                              AwayTeam)) %>%
    mutate(AwayTeam = if_else(AwayTeam == "Western Bulldog", "Western Bulldogs",
                              AwayTeam)) %>%
    separate(HomeScore, c("HomeGoals", "HomeBehinds", "HomeTotal")) %>%
    separate(AwayScore, c("AwayGoals", "AwayBehinds", "AwayTotal")) %>%
    mutate_at(c("Season", "HomeGoals", "HomeBehinds", "HomeTotal",
                "AwayGoals", "AwayBehinds", "AwayTotal"), as.integer) %>%
    select(Date, Season, everything(), -ID) %>%
    filter(Season >= 1997, Season <= 2017) %>%
    as.data.frame()

use_data(matches, overwrite = TRUE)

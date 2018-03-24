#' Matches
#'
#' Details of AFL matches from the 2000 season onwards
#'
#' @format `matches` is a data.frame containing the details of AFL matches from
#' the 2000 season onwards obtained from afltables.com
#'
#' @details Some changes have been made to what is available on afltables.com:
#'
#' * A season column has been added
#' * Scores have been split into goals, behinds and totals columns
#' * "Brisbane Lions" have been renamed to "Brisbane"
#' * "Kangaroos" have been renamed to "North Melbourne"
#' * "Western Bulldog" have been renamed to "Western Bulldogs"
#'
#' @source
#' ```
#' library(tidyverse)
#' url <- "https://afltables.com/afl/stats/biglists/bg3.txt"
#' matches_raw <- read_table(url, skip = 2, col_names = FALSE,
#'                           col_types = cols(
#'                               .default = col_character(),
#'                               X1       = col_double()
#'                          ))
#'
# matches <- matches_raw %>%
#     rename(ID = X1, Date = X2, Round = X3, HomeTeam = X4, HomeScore = X5,
#            AwayTeam = X6, AwayScore = X7, Ground = X8) %>%
#     mutate(Date = as.Date(Date, format = "%e-%b-%Y")) %>%
#     mutate(Season = format(Date, "%Y")) %>%
#     mutate(HomeTeam = if_else(HomeTeam == "Brisbane Lions", "Brisbane",
#                               HomeTeam)) %>%
#     mutate(HomeTeam = if_else(HomeTeam == "Kangaroos", "North Melbourne",
#                               HomeTeam)) %>%
#     mutate(HomeTeam = if_else(HomeTeam == "Western Bulldog", "Western Bulldogs",
#                               HomeTeam)) %>%
#     mutate(AwayTeam = if_else(AwayTeam == "Brisbane Lions", "Brisbane",
#                               AwayTeam)) %>%
#     mutate(AwayTeam = if_else(AwayTeam == "Kangaroos", "North Melbourne",
#                               AwayTeam)) %>%
#     mutate(AwayTeam = if_else(AwayTeam == "Western Bulldog", "Western Bulldogs",
#                               AwayTeam)) %>%
#     separate(HomeScore, c("HomeGoals", "HomeBehinds", "HomeTotal")) %>%
#     separate(AwayScore, c("AwayGoals", "AwayBehinds", "AwayTotal")) %>%
#     mutate_at(c("Season", "HomeGoals", "HomeBehinds", "HomeTotal",
#                 "AwayGoals", "AwayBehinds", "AwayTotal"), as.integer) %>%
#     select(Date, Season, everything(), -ID) %>%
#     filter(Season >= 2000) %>%
#     as.data.frame()
#' ```
"matches"

#' Distances
#'
#' Distances between teams and grounds
#'
#' @format `distances` is a matrix containing the great circle distance in
#' kilometres between the home city of AFL teams and the city of AFL grounds
"distances"
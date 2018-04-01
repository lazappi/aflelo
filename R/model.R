#' New AFLELO Model
#'
#' Create a new AFLELO Model
#'
#' @param params aflelo_params object containing parameters for the model
#'
#' @return An aflelo_model object
new_aflelo_model <- function(params = new_aflelo_params()) {

    distances <- NA
    matches <- NA
    utils::data("distances", package = "aflelo", envir = environment())
    utils::data("matches", package = "aflelo", envir = environment())

    teams <- sort(unique(matches$HomeTeam)[1:16])

    ratings <- data.frame(Team = teams, Rating = 1500, Points = 0, PtsFor = 0,
                          PtsAgainst = 0, Percentage = 0,
                          stringsAsFactors = FALSE)

    rating_history <- matrix(, nrow = 16, ncol = 0, dimnames = list(teams))
    mode(rating_history) <- "numeric"

    structure(
        list(
            params = params,
            ratings = ratings,
            season = 1997,
            round = "Preseason",
            distances = distances,
            rating_history = rating_history,
            match_history = data.frame(Season    = character(),
                                       Round     = character(),
                                       HomeTeam  = character(),
                                       AwayTeam  = character(),
                                       Ground    = character(),
                                       Predicted = numeric(),
                                       Margin    = numeric(),
                                       stringsAsFactors = FALSE)
        ),
        class = "aflelo_model"
    )
}


#' Validate AFLELO Model
#'
#' Validate an AFLELO Model object
#'
#' @param model The aflelo_model to validate
#'
#' @return Raises an error if invalid, otherwise returns the aflelo_model
validate_aflelo_model <- function(model) {
    checkmate::assert_class(model, "aflelo_model")
    validate_aflelo_params(model$params)
    checkmate::assert_data_frame(model$ratings, ncol = 6)
    checkmate::assert_int(model$season, 2000)
    checkmate::assert_character(model$round, len = 1)
    checkmate::assert_matrix(model$distances, mode = "numeric",
                             any.missing = FALSE)
    checkmate::assert_matrix(model$rating_history, nrows = nrow(model$ratings),
                             mode = "numeric", any.missing = FALSE)
    checkmate::assert_data_frame(model$match_history, ncols = 7)

    return(model)
}


#' AFLELO Model
#'
#' Object for storing information about the AFLELO Model
#'
#' @param params aflelo_params object containing parameters for the model
#' @param ... Any other parts of the model to set
#'
#' @return A valid aflelo_model object
#' @examples
#' model <- aflelo_model()
#'
#' @export
aflelo_model <- function(params = new_aflelo_params(), ...) {

    model <- new_aflelo_model(params = params)

    dots <- list(...)

    for (dot in names(dots)) {
        if (dot %in% names(model)) {
            model[[dot]] <- dots[[dot]]
        } else {
            stop(dot, " is not a valid parameter")
        }
    }

    validate_aflelo_model(model)
}


#' Print AFLELO Model
#'
#' Print an AFLELO Model
#'
#' @param x aflelo_model object to print
#' @param ... additional arguments, not used
#'
#' @return Prints object
print.aflelo_model <- function(x, ...) {
    cat("AFLELO Model", "\n\n")

    cat(crayon::bold("Parameters"), "\n")
    print(x$params, compact = TRUE)
    cat("\n\n")

    cat(crayon::bold("Season:"), x$season, crayon::bold("Round:"), x$round,
        "\n\n")

    ratings <- x$ratings
    ratings <- ratings[order(ratings$Rating, decreasing = TRUE), ]
    ratings$Rating <- round(ratings$Rating)
    ratings$RatingStr <- ratings$Rating
    ratings$Gap <- (max(nchar(ratings$Team)) + 2) - nchar(ratings$Team)
    is_pos <- ratings$Rating > 1500
    ratings$RatingStr[is_pos] <- crayon::green(ratings$Rating[is_pos])
    is_neg <- ratings$Rating < 1500
    ratings$RatingStr[is_neg] <- crayon::red(ratings$Rating[is_neg])

    cat(crayon::bold("Ratings"), "\n")
    for (i in seq_len(nrow(ratings))) {
        cat(ratings[i, "Team"], strrep(" ", ratings[i, "Gap"]),
            ratings[i, "RatingStr"], "\t",
            ratings[i, "Points"], "\t",
            paste0(round(ratings[i, "Percentage"], 2), "%"), "\n")
    }
    cat("\n")

    cat(crayon::bold("Distances"), "\n")
    cat("Distances between", nrow(x$distances), "teams and",
        ncol(x$distances), "grounds")
    cat("\n\n")

    cat(crayon::bold("Rating history"), "\n")
    cat("Ratings for", nrow(x$rating_history), "teams across",
        ncol(x$rating_history), "rounds")
    cat("\n\n")

    cat(crayon::bold("Match history"), "\n")
    cat("Match details for", nrow(x$match_history), "matches across",
        length(unique(x$match_history$Season)), "seasons")
}


#' Update ratings
#'
#' Update the ratings for teams in an AFLELO Model
#'
#' @param model aflelo_model to update
#' @param new_ratings vector of new ratings
#'
#' @return aflelo_model with updated ratings
#' @examples
#' model <- aflelo_model()
#' aflelo:::update_ratings(model, rep(1600, 16))
update_ratings <- function(model, new_ratings) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_numeric(new_ratings, lower = 0, finite = TRUE,
                              any.missing = FALSE, len = nrow(model$ratings))

    model$ratings$Rating <- new_ratings

    validate_aflelo_model(model)
}


#' Update rating
#'
#' Update rating information for a single team in an AFLELO Model
#'
#' @param model aflelo_model to update
#' @param team name of team to update
#' @param new_rating new rating value
#' @param points number of premiership points to add
#' @param pts_for number of points scored by the team
#' @param pts_against number of points scored against the team
#'
#' @return afelo_object with updated rating
#' @examples
#' model <- aflelo_model()
#' aflelo:::update_rating(model, "Richmond", 1600, 4, 110, 100)
update_rating <- function(model, team, new_rating, points, pts_for,
                          pts_against) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::check_character(team, len = 1)
    checkmate::assert_number(new_rating, lower = 0, finite = TRUE)
    checkmate::assert_int(points, lower = 0, upper = 4)
    checkmate::assert_int(pts_for, lower = 0)
    checkmate::assert_int(pts_against, lower = 0)

    team_idx <- which(model$ratings$Team == team)
    model$ratings$Rating[team_idx] <- new_rating
    model$ratings$Points[team_idx] <- model$ratings$Points[team_idx] + points
    model$ratings$PtsFor[team_idx] <- model$ratings$PtsFor[team_idx] + pts_for
    model$ratings$PtsAgainst[team_idx] <- model$ratings$PtsAgains[team_idx] +
                                            pts_against
    model$ratings$Percentage[team_idx] <- (model$ratings$PtsFor[team_idx] /
                                          model$ratings$PtsAgainst[team_idx]) *
                                          100

    validate_aflelo_model(model)
}


#' Update rating history
#'
#' Add the current ratings to the rating history
#'
#' @param model aflelo_model to update
#'
#' @return aflelo_model with updated rating history
#' @examples
#' model <- aflelo_model()
#' aflelo:::update_rating_history(model)
update_rating_history <- function(model) {
    checkmate::assert_class(model, "aflelo_model")

    ratings <- model$ratings$Rating
    names(ratings) <- model$ratings$Team
    history_names <- colnames(model$rating_history)
    model$rating_history <- cbind(model$rating_history,
                                  ratings[rownames(model$rating_history)])
    colnames(model$rating_history) <- c(history_names,
                                        paste0(model$round, model$season))

    validate_aflelo_model(model)
}


#' Add team
#'
#' Add a new team to an AFLELO Model
#'
#' @param model aflelo_model to add team to
#' @param team name of the new team to add
#'
#' @return aflelo_model with new team
#' @examples
#' model <- aflelo_model()
#' aflelo:::add_team(model, "Gold Coast")
add_team  <- function(model, team) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_character(team, len = 1)

    diff <- 1500 - model$params$new_team_rating

    new_ratings <-  model$ratings$Rating + (diff / nrow(model$ratings))

    model <- update_ratings(model, new_ratings)

    model$ratings <- rbind(model$ratings,
                           data.frame(Team = team,
                                      Rating = model$params$new_team_rating,
                                      Points = 0,
                                      PtsFor = 0,
                                      PtsAgainst = 0,
                                      Percentage = 0))

    team_names <- rownames(model$rating_history)
    model$rating_history <- rbind(model$rating_history,
                                  rep(0, ncol(model$rating_history)))
    rownames(model$rating_history) <- c(team_names, team)

    validate_aflelo_model(model)
}


#' New season
#'
#' Start a new season in an AFLELO Model
#'
#' @param model aflelo_model to start new season in
#'
#' @return aflelo_model at beginning of new season
#' @examples
#' model <- aflelo_model()
#' aflelo:::new_season(model)
new_season <- function(model) {
    checkmate::assert_class(model, "aflelo_model")

    model <- update_rating_history(model)

    new_season <- model$season + 1
    model$season <- new_season

    # Regress ratings
    adjustments <- (1500 - model$ratings$Rating) *
        model$param$new_season_adjustment
    new_ratings <- model$ratings$Rating + adjustments

    model <- update_ratings(model, new_ratings)

    model$ratings$Points <- 0
    model$ratings$PtsFor <- 0
    model$ratings$PtsAgainst <- 0
    model$ratings$Percentage <- 0

    if (new_season == 2011) {
        model <- add_team(model, "Gold Coast")
    }

    if (new_season == 2012) {
        model <- add_team(model, "GW Sydney")
    }

    model$round <- "Preseason"
    model <- update_rating_history(model)

    validate_aflelo_model(model)
}


new_aflelo_model <- function(params = new_aflelo_params()) {

    teams <- sort(unique(matches$HomeTeam)[1:16])

    ratings <- data.frame(Team = teams, Rating = 1500,
                          stringsAsFactors = FALSE)

    utils::data("distances", package = "aflelo", envir = environment())

    structure(
        list(
            params = params,
            ratings = ratings,
            season = 2000,
            distances = distances,
            rating_history = matrix(ratings$Rating,
                                    dimnames = list(teams, "Initial")),
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


validate_aflelo_model <- function(model) {
    checkmate::assert_class(model, "aflelo_model")
    validate_aflelo_params(model$params)
    checkmate::assert_data_frame(model$ratings, ncol = 2)
    checkmate::assert_int(model$season, 2000)
    checkmate::assert_matrix(model$distances, mode = "numeric",
                             any.missing = FALSE)
    checkmate::assert_matrix(model$rating_history, nrows = nrow(model$ratings),
                             mode = "numeric", any.missing = FALSE)
    checkmate::assert_data_frame(model$match_history, ncols = 7)

    return(model)
}


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


print.aflelo_model <- function(x, ...) {
    cat("AFLELO Model", "\n\n")

    cat(crayon::bold("Parameters"), "\n")
    print(x$params, compact = TRUE)
    cat("\n\n")

    cat(crayon::bold("Season"), x$season, "\n\n")

    ratings <- x$ratings
    ratings$Rating <- round(ratings$Rating)
    ratings$Gap <- (max(nchar(ratings$Team)) + 2) - nchar(ratings$Team)
    is_pos <- ratings$Rating > 1500
    ratings$Rating[is_pos] <- crayon::green(ratings$Rating[is_pos])
    is_neg <- ratings$Rating < 1500
    ratings$Rating[is_neg] <- crayon::red(ratings$Rating[is_neg])

    cat(crayon::bold("Ratings"), "\n")
    for (i in seq_len(nrow(ratings))) {
        cat(ratings[i, "Team"], strrep(" ", ratings[i, "Gap"]),
            ratings[i, "Rating"], "\n")
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


update_ratings <- function(model, new_ratings) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::check_character(round, len = 1)
    checkmate::assert_numeric(new_ratings, lower = 0, finite = TRUE,
                              any.missing = FALSE, len = nrow(model$ratings))

    model$ratings$Rating <- new_ratings
    model$ratings <- model$ratings[order(model$ratings$Rating,
                                         decreasing = TRUE), ]

    validate_aflelo_model(model)
}


update_rating_history <- function(model, round) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::check_character(round, len = 1)

    history_names <- colnames(model$rating_history)
    model$rating_history <- cbind(model$rating_history, model$ratings$Rating)
    colnames(model$rating_history) <- c(history_names,
                                        paste0(round, model$season))

    validate_aflelo_model(model)
}


add_team  <- function(model, team) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_character(team, len = 1)

    diff <- 1500 - model$params$new_team_rating

    new_ratings <-  model$ratings$Rating + (diff / nrow(model$ratings))

    model <- update_ratings(model, new_ratings)

    model$ratings <- rbind(model$ratings,
                           data.frame(Team = team,
                                      Rating = model$params$new_team_rating))

    team_names <- rownames(model$rating_history)
    model$rating_history <- rbind(model$rating_history,
                                  rep(0, ncol(model$rating_history)))
    rownames(model$rating_history) <- c(team_names, team)

    validate_aflelo_model(model)
}


new_season <- function(model) {
    checkmate::assert_class(model, "aflelo_model")

    new_season <- model$season + 1
    model$season <- new_season

    # Regress ratings
    adjustments <- (1500 - model$ratings$Rating) *
        model$param$new_season_adjustment
    new_ratings <- model$ratings$Rating + adjustments

    model <- update_ratings(model, new_ratings)

    if (new_season == 2011) {
        model <- add_team(model, "Gold Coast")
    }

    if (new_season == 2012) {
        model <- add_team(model, "GW Sydney")
    }

    model <- update_rating_history(model, "Preseason")

    validate_aflelo_model(model)
}
#' Calculate Home Ground Advantage
#'
#' Calculate the Home Ground Advantage between two teams playing at a particular
#' ground
#'
#' @param model aflelo_model object
#' @param home name of the home team
#' @param away name of the away team
#' @param ground name of the ground
#'
#' @return Value giving the advantage of the home team in ratings points
#' @examples
#' model <- aflelo_model()
#' aflelo:::calc_hga(model, "West Coast", "Collingwood", "Subiaco")
calc_hga <- function(model, home, away, ground) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_character(home, len = 1)
    checkmate::assert_character(away, len = 1)
    checkmate::assert_character(ground, len = 1)

    current_season <- model$season
    seasons <- model$match_history$Season
    is_recent <- seasons >= current_season - 2
    recent_matches <- model$match_history[is_recent, ]
    recent_matches <- recent_matches[recent_matches$Ground == ground, ]

    home_exp <- sum(recent_matches$HomeTeam == home) +
                sum(recent_matches$AwayTeam == home)
    away_exp <- sum(recent_matches$HomeTeam == away) +
        sum(recent_matches$AwayTeam == away)

    exp <- log(home_exp + 1) - log(away_exp + 1)

    home_dist <- model$distances[home, ground]
    away_dist <- model$distances[away, ground]

    travel <- sign(away_dist - home_dist) * abs(away_dist - home_dist) ^ 0.33

    hga <- (model$params$hga_alpha * travel) + (model$params$hga_beta * exp)

    return(hga)
}


#' Predict match
#'
#' Predict the result and margin of a match between two teams at a particular
#' ground
#'
#' @param model afelo_model object
#' @param home name of the home team
#' @param away name of the away team
#' @param ground name of the ground
#'
#' @return Probability of victory and margin for the home team
#'
#' @examples
#' model <- aflelo_model()
#' predict_match(model, "Brisbane", "Western Bulldogs", "Gabba")
#'
#' @export
predict_match <- function(model, home, away, ground) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_character(home, len = 1)
    checkmate::assert_character(away, len = 1)
    checkmate::assert_character(ground, len = 1)

    pred_result <- predict_result(model, home, away, ground)
    pred_margin <- predict_margin(model, pred_result)

    return(c(Result = pred_result, Margin = pred_margin))
}


#' Predict result
#'
#' Predict the result of a match between two teams at a particular ground
#'
#' @param model afelo_model object
#' @param home name of the home team
#' @param away name of the away team
#' @param ground name of the ground
#'
#' @return Probability of victory for the home team
#' @examples
#' model <- aflelo_model()
#' aflelo:::predict_result(model, "Brisbane", "Western Bulldogs", "Gabba")
predict_result <- function(model, home, away, ground) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_character(home, len = 1)
    checkmate::assert_character(away, len = 1)
    checkmate::assert_character(ground, len = 1)

    hga <- calc_hga(model, home, away, ground)

    home_rating <- model$ratings$Rating[model$ratings$Team == home]
    away_rating <- model$ratings$Rating[model$ratings$Team == away]

    m <- model$params$pred_m
    pred_result <- 1 / (10 ^ ((-(home_rating - away_rating + hga) / m)) + 1)

    return(pred_result)
}


#' Predict margin
#'
#' Convert a predicted result to a predicted margin
#'
#' @param model aflelo_model object
#' @param pred_result probablilty of home win
#'
#' @return Margin for the home team
#' @examples
#' model <- aflelo_model()
#' pred_result <- aflelo:::predict_result(model, "Brisbane", "Western Bulldogs",
#'                         "Gabba")
#' aflelo:::predict_margin(model, pred_result)
predict_margin <- function(model, pred_result) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_number(pred_result, lower = 0, upper = 1)

    pred_margin <- -log((1 - pred_result) / pred_result) / model$params$pred_p

    return(pred_margin)
}


#' Convert margin
#'
#' Convert a real margin to a result probabilty
#'
#' @param model aflelo_model object
#' @param margin real margin for the home team
#'
#' @return Probability of the home team winning
#' @examples
#' model <- aflelo_model()
#' aflelo:::convert_margin(model, 30)
convert_margin <- function(model, margin) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_number(margin)

    result <- 1 / (1 + exp(-model$params$pred_p * margin))

    return(result)
}

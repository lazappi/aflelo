#' Traing AFLELO Model
#'
#' Train an AFLELO Model
#'
#' @param model aflelo_model object
#' @param matches data.frame containg matche results to train using, see
#'        [matches] for an example
#'
#' @return aflelo_model with ratings history based on the provided matches
#' @examples
#' data("matches")
#' matches1997 <- matches[matches$Season == 1997, ]
#' model <- aflelo_model()
#' train_model(model, matches1997)
#'
#' @export
train_model <- function(model, matches) {

    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_data_frame(matches, min.rows = 1)

    for (i in seq_len(nrow(matches))) {

        match_row <- matches[i, ]

        home   <- match_row$HomeTeam
        away   <- match_row$AwayTeam
        ground  <- match_row$Ground
        season <- match_row$Season
        round  <- match_row$Round

        if (season > model$season) {
            model <- new_season(model)
            model$round <- round
        }

        if (round != model$round) {
            model <- update_rating_history(model)
            model$round <- round
        }

        pred_result <- predict_result(model, home, away, ground)

        pred_margin <- predict_margin(model, pred_result)

        match <- aflelo_match(season      = season,
                              round       = round,
                              home        = home,
                              away        = away,
                              ground      = ground,
                              pred_margin = pred_margin,
                              home_total  = match_row$HomeTotal,
                              away_total  = match_row$AwayTotal)

        model <- add_match(model, match)
    }

    return(model)

}


#' Add match
#'
#' Add the results of a match to an AFLELO Model
#'
#' @param model afelo_model object
#' @param match aflelo_match object
#'
#' @return aflelo_model with ratings updated based on the match
#' @examples
#' model <- aflelo_model()
#' match <- aflelo_match(2000, "R1", "Melbourne", "Adelaide", "M.C.G.",
#'                       10, 110, 102)
#' aflelo:::add_match(model, match)
add_match <- function(model, match) {

    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_class(match, "aflelo_match")

    if (match$season > model$season) {
        model <- new_season(model)
        model$round <- match$round
    }

    if (match$round != model$round) {
        model <- update_rating_history(model)
        model$round <- match$round
    }

    pred_result <- convert_margin(model, match$pred_margin)
    real_result <- convert_margin(model, match$real_margin)

    new_home_rating <- calc_new_rating(model, match$home, real_result,
                                       pred_result)
    new_away_rating <- calc_new_rating(model, match$away, 1 - real_result,
                                       1 - pred_result)

    if (!(match$round %in% c("QF", "EF", "SF", "PF", "GF"))) {
        home_points <- 0
        away_points <- 0
        if (match$real_margin > 0) {
            home_points <- 4
        } else if (match$real_margin < 0) {
            away_points <- 4
        } else if(match$real_margin == 0) {
            home_points <- 2
            away_points <- 2
        }

        model <- update_ladder(model, match$home, home_points, match$home_total,
                               match$away_total)
        model <- update_ladder(model, match$away, away_points, match$away_total,
                               match$home_total)
    }

    model <- update_rating(model, match$home, new_home_rating)
    model <- update_rating(model, match$away, new_away_rating)

    model$match_history <- rbind(model$match_history,
                                 data.frame(Season = model$season,
                                            Round = model$round,
                                            HomeTeam = match$home,
                                            AwayTeam = match$away,
                                            Ground = match$ground,
                                            Predicted = match$pred_margin,
                                            Margin = match$real_margin))

    validate_aflelo_model(model)
}


#' Calculate new rating
#'
#' Calculate new rating for a team based on predicted and real results
#'
#' @param model aflelo_model object
#' @param team name of team to calculate new rating for
#' @param real_result real result probability
#' @param pred_result predicted result probability
#'
#' @return New rating value for the team
#' @examples
#' model <- aflelo_model()
#' match <- aflelo_match(2000, "R1", "Melbourne", "Adelaide", "M.C.G.",
#'                       10, 110, 102)
#' real_result <- aflelo:::convert_margin(model, match$real_margin)
#' pred_result <- aflelo:::convert_margin(model, match$pred_margin)
#' aflelo:::calc_new_rating(model, "Melbourne", real_result, pred_result)
calc_new_rating <- function(model, team, real_result, pred_result) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_character(team, len = 1)
    checkmate::assert_number(real_result, lower = 0, upper = 1)
    checkmate::assert_number(pred_result, lower = 0, upper = 1)

    if (model$round %in% c("R1", "R2", "R3", "R4", "R5")) {
        adjust_k <- model$params$adjust_k_early
    } else if (model$round %in% c("QF", "EF", "SF", "PF", "GF")) {
        adjust_k <- model$params$adjust_k_finals
    } else {
        adjust_k <- model$params$adjust_k_normal
    }

    old_rating <- model$ratings$Rating[model$ratings$Team == team]

    new_rating <- old_rating + adjust_k * (real_result - pred_result)

    return(new_rating)
}

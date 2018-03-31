train_model <- function(model, matches) {

    for (i in seq_len(nrow(matches))) {

        match_row <- matches[i, ]

        home   <- match_row$HomeTeam
        away   <- match_row$AwayTeam
        gound  <- match_row$Ground
        season <- match_row$Season
        round  <- match_row$Round

        if (season > model$season) {
            model <- new_season(model)
            model$round <- match$round
        }

        if (round != model$round) {
            model <- update_rating_history(model)
            model$round <- round
        }

        real_margin <- match_row$HomeTotal - match_row$AwayTotal
        pred_result <- predict_result(model, home, away, ground)

        pred_margin <- predict_margin(model, pred_result)

        match <- aflelo_match(season      = season,
                              round       = round,
                              home        = home,
                              away        = away,
                              ground      = ground,
                              pred_margin = pred_margin,
                              real_margin = real_margin)

        model <- add_match(model, match)
    }

    return(model)

}

add_match <- function(model, match) {


    if (match$season > model$season) {
        model <- new_season(model)
        model$round <- match$round
    }

    match_round <- match$round
    if (match_round != model$round) {
        model <- update_rating_history(model)
        model$round <- match_round
    }

    pred_result <- convert_margin(model, match$pred_margin)
    real_result <- convert_margin(model, match$real_margin)

    new_home_rating <- calc_new_rating(model, match$home, real_result,
                                       pred_result)
    new_away_rating <- calc_new_rating(model, match$away, 1 - real_result,
                                       1 - pred_result)

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
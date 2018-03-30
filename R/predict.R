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


predict_margin <- function(model, pred_result) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_number(pred_result, lower = 0, upper = 1)

    pred_margin <- -log((1 - pred_result) / pred_result) / model$params$pred_p

    return(pred_margin)
}


convert_margin <- function(model, margin) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_number(margin)

    result <- 1 / (1 + exp(-model$params$pred_p * margin))

    return(result)
}


calc_new_rating <- function(model, team, real_result, pred_result) {
    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_character(team, len = 1)
    checkmate::assert_number(real_result, lower = 0, upper = 1)
    checkmate::assert_number(pred_result, lower = 0, upper = 1)

    old_rating <- model$ratings$Rating[model$ratings$Team == team]

    new_rating <- old_rating + model$params$adjust_k *
        (real_result - pred_result)

    return(new_rating)
}
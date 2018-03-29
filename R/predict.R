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

    print(c(home_exp, away_exp, exp, home_dist, away_dist, travel, hga))

    return(hga)
}
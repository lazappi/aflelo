simulate_matches <- function(model, matches, n = 10000, n_cores = 1, seed = 1) {

    set.seed(seed)

    `%dopar%` <- foreach::`%dopar%`

    cl <- snow::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    pb <- txtProgressBar(max = n, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    sim_ratings <- foreach::foreach(seq_len(n), .combine = cbind,
                                    .export = c("new_season",
                                                "update_rating_history",
                                                "validate_aflelo_model",
                                                "validate_aflelo_params",
                                                "validate_aflelo_match",
                                                "new_aflelo_match",
                                                "update_ratings",
                                                "predict_result",
                                                "predict_margin",
                                                "calc_hga",
                                                "aflelo_match",
                                                "add_team",
                                                "add_match",
                                                "convert_margin",
                                                "calc_new_rating",
                                                "update_rating"),
                                    .options.snow = opts) %dopar% {

        sim_model <- model

        for (j in seq_len(nrow(matches))) {

            match_row <- matches[j, ]

            home   <- match_row$HomeTeam
            away   <- match_row$AwayTeam
            ground <- match_row$Ground
            season <- match_row$Season
            round  <- match_row$Round

            if (season > sim_model$season) {
                sim_model <- new_season(sim_model)
                sim_model$round <- round
            }

            if (round != sim_model$round) {
                sim_model <- update_rating_history(sim_model)
                sim_model$round <- round
            }

            pred_result <- predict_result(sim_model, home, away, ground)
            pred_margin <- predict_margin(sim_model, pred_result)

            real_margin <- rnorm(1, mean = pred_margin,
                                 sd = sim_model$params$sim_sigma)

            match <- aflelo_match(season      = season,
                                  round       = round,
                                  home        = home,
                                  away        = away,
                                  ground      = ground,
                                  pred_margin = pred_margin,
                                  real_margin = real_margin)

            sim_model <- add_match(sim_model, match)
        }

        sim_model <- update_rating_history(sim_model)

        ratings <- sim_model$rating_history[, ncol(sim_model$rating_history)]

        ratings
    }

    close(pb)
    snow::stopCluster(cl)

    return(sim_ratings)
}
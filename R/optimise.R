new_population <- function(n) {

    population <- vector("list", n)

    for (i in seq_len(n)) {
        population[[i]] <- generate_params()
    }

    return(population)
}

generate_params <- function() {

    aflelo_params(
        new_team_rating = runif(1, 800, 1400),
        new_season_adjustment = runif(1, 0, 1),
        hga_alpha = runif(1, 0, 20),
        hga_beta = runif(1, 0, 20),
        pred_p = runif(1, 0, 1),
        adjust_k_early = runif(1, 0, 100),
        adjust_k_normal = runif(1, 0, 100),
        adjust_k_finals = runif(1, 0, 100)
    )
}

mutate_params <- function(params, prob = 0.1) {

    for (param in c("new_team_rating", "new_season_adjustment", "hga_alpha",
                    "hga_beta", "pred_p", "adjust_k_early", "adjust_k_normal",
                    "adjust_k_finals")) {
        mutate <- rbinom(1, 1, prob) == 1

        if (mutate) {
            multiplier <- rnorm(1, mean = 1, sd = 0.1)
            params[[param]] <- params[[param]] * multiplier
        }
    }

    validate_aflelo_params(params)
}

breed_params <- function(population, n) {

    new_pop <- lapply(seq_len(n), function(x) {

        params1 <- population[[sample(seq_along(population), 1)]]
        params2 <- population[[sample(seq_along(population), 1)]]

        params <- aflelo_params(
            new_team_rating = mean(c(params1$new_team_rating,
                                     params2$new_team_rating)),
            new_season_adjustment = mean(c(params1$new_season_adjustment,
                                           params2$new_season_adjustment)),
            hga_alpha = mean(c(params1$hga_alpha, params2$hga_alpha)),
            hga_beta = mean(c(params1$hga_beta, params2$hga_beta)),
            pred_p = mean(c(params1$pred_p, params2$pred_p)),
            adjust_k_early = mean(c(params1$adjust_k_early,
                                    params2$adjust_k_early)),
            adjust_k_normal = mean(c(params1$adjust_k_normal,
                                     params2$adjust_k_normal)),
            adjust_k_finals = mean(c(params1$adjust_k_finals,
                                     params2$adjust_k_finals))
        )

        params <- mutate_params(params)
    })

    return(new_pop)
}

evaluate_population <- function(population, matches, start_eval = 2000,
                                end_eval = 2016, n_cores = 1) {

    `%dopar%` <- foreach::`%dopar%`

    cl <- snow::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    pb <- utils::txtProgressBar(max = length(population), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    matches <- matches[matches$Season <= end_eval, ]

    fitness <- foreach::foreach(params = population,
                                .combine = rbind,
                                .packages = "aflelo",
                                .options.snow = opts) %dopar% {

        model <- aflelo_model(params = params)
        model <- train_model(model, matches)

        match_history <- model$match_history
        match_history <- match_history[match_history$Season >= start_eval, ]
        match_history <- match_history[match_history$Season <= end_eval, ]

        correct <- sign(match_history$Predicted) == sign(match_history$Margin)
        pct_correct <- sum(correct) / nrow(match_history)

        diff <- abs(match_history$Predicted - match_history$Margin)
        avg_diff <- mean(diff)

        c(PctCorrect = pct_correct, AvgDiff = avg_diff,
          Fitness = 0.8 * pct_correct + 0.2 * (1 / avg_diff))
    }

    close(pb)
    snow::stopCluster(cl)

    return(as.data.frame(fitness))
}

optimise_params <- function(matches, start_eval = 2000, end_eval = 2016,
                            n = 100, top = 10, generations = 10, n_cores = 1) {

    population <- new_population(n)

    hall_of_fame <- list()

    for (i in seq_len(generations)) {
        message("Generation ", i)
        fitness <- evaluate_population(population, matches, start_eval,
                                       end_eval, n_cores)
        fitness$Individual <- seq_len(nrow(fitness))
        fitness <- fitness[order(fitness$Fitness, decreasing = TRUE), ]

        top_params <- population[fitness$Individual[seq_len(top)]]
        top_params_flat <- t(sapply(top_params, unlist))

        hall_of_fame <- c(hall_of_fame,
                          list(TopParams = top_params_flat, Fitness = fitness))

        population <- breed_params(top_params, n)
    }

    return(hall_of_fame)
}
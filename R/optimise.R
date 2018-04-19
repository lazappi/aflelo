#' New population
#'
#' Randomly generate a population of AFLELO Params
#'
#' @param n size of the population
#'
#' @return list of aflelo_params objects
#' @examples
#' population <- aflelo:::new_population(5)
new_population <- function(n) {
    checkmate::assert_int(n, lower = 1)

    population <- vector("list", n)

    for (i in seq_len(n)) {
        population[[i]] <- generate_params()
    }

    return(population)
}


#' Generate parameters
#'
#' Randomly generate a single AFLELO Params
#'
#' @return aflelo_params object
#'
#' @examples
#' aflelo:::generate_params()
#'
#' @importFrom stats runif
generate_params <- function() {

    aflelo_params(
        new_team_rating       = runif(1, 800, 1400),
        new_season_adjustment = runif(1, 0, 1),
        hga_alpha             = runif(1, 0, 20),
        hga_beta              = runif(1, 0, 20),
        pred_p                = runif(1, 0, 0.1),
        adjust_k_early        = runif(1, 0, 100),
        adjust_k_normal       = runif(1, 0, 100),
        adjust_k_finals       = runif(1, 0, 100)
    )
}


#' Muatate AFLELO Params
#'
#' Mutate AFLELO Params
#'
#' @param params aflelo_params object to mutate
#' @param prob probability that an individual parameter is mutated
#'
#' @return mutated aflelo_params
#'
#' @examples
#' params <- aflelo_params()
#' aflelo:::mutate_params(params)
#'
#' @importFrom stats rbinom rnorm
mutate_params <- function(params, prob = 0.1) {
    checkmate::assert_class(params, "aflelo_params")
    checkmate::assert_number(prob, lower = 0, upper = 1)

    for (param in c("new_team_rating", "new_season_adjustment", "hga_alpha",
                    "hga_beta", "pred_p", "adjust_k_early", "adjust_k_normal",
                    "adjust_k_finals")) {

        mutate <- rbinom(1, 1, prob) == 1

        if (mutate) {

            max <- Inf
            if (param %in% c("new_season_adjustment", "pred_p")) {
                max <- 1
            }

            multiplier <- rnorm(1, mean = 1, sd = 0.1)
            new_value <-  squeeze_value(params[[param]] * multiplier, 0, max)
            params[[param]] <- new_value
        }
    }

    validate_aflelo_params(params)
}


#' Squeeze value
#'
#' Force a value to be within a range
#'
#' @param value value to squeeze
#' @param lower lower bound
#' @param upper upper bound
#'
#' @return squeezed value
#' @examples
#' aflelo:::squeeze_value(10, 0, 10)
#' aflelo:::squeeze_value(10, 0, 5)
#' aflelo:::squeeze_value(10, 15, 20)
squeeze_value <- function(value, lower, upper) {
    checkmate::assert_number(value)
    checkmate::assert_number(lower, upper = upper)
    checkmate::assert_number(upper, lower = lower)

    if (value < lower) {
        return(lower)
    }

    if (value > upper) {
        return(upper)
    }

    return(value)
}


#' Breed AFLELO Params
#'
#' Create a new population of AFLELO Params through crossover of an existing
#' population
#'
#' @param population list of aflelo_params
#' @param fitness vector giving the fitness of each aflelo_params
#' @param n size of new population
#'
#' @return list of alfelo_params
#' @examples
#' population <- aflelo:::new_population(5)
#' aflelo:::breed_params(population, rep(0.5, 5), 5)
breed_params <- function(population, fitness, n) {
    checkmate::assert_list(population, types = "aflelo_params")
    checkmate::assert_numeric(fitness, len = length(population),
                              any.missing = FALSE)
    checkmate::assert_int(n, lower = 1)

    weights <- fitness / sum(fitness)

    new_pop <- lapply(seq_len(n), function(x) {

        idx1 <- sample(seq_along(population), 1, prob = weights)
        idx2 <- sample(seq_along(population), 1, prob = weights)
        params1 <- population[[idx1]]
        params2 <- population[[idx2]]

        params <- aflelo_params(
            new_team_rating = mean(params1$new_team_rating,
                                   params2$new_team_rating),
            new_season_adjustment = mean(params1$new_season_adjustment,
                                         params2$new_season_adjustment),
            hga_alpha = mean(params1$hga_alpha, params2$hga_alpha),
            hga_beta = mean(params1$hga_beta, params2$hga_beta),
            pred_p = mean(params1$pred_p, params2$pred_p),
            adjust_k_early = mean(params1$adjust_k_early,
                                  params2$adjust_k_early),
            adjust_k_normal = mean(params1$adjust_k_normal,
                                   params2$adjust_k_normal),
            adjust_k_finals = mean(params1$adjust_k_finals,
                                   params2$adjust_k_finals)
        )

        # params <- aflelo_params(
        #     new_team_rating = ifelse(rbinom(1, 1, 0.5),
        #                              params1$new_team_rating,
        #                              params2$new_team_rating),
        #     new_season_adjustment = ifelse(rbinom(1, 1, 0.5),
        #                                    params1$new_season_adjustment,
        #                                    params2$new_season_adjustment),
        #     hga_alpha = ifelse(rbinom(1, 1, 0.5),
        #                        params1$hga_alpha,
        #                        params2$hga_alpha),
        #     hga_beta = ifelse(rbinom(1, 1, 0.5),
        #                       params1$hga_beta,
        #                       params2$hga_beta),
        #     pred_p = ifelse(rbinom(1, 1, 0.5),
        #                     params1$pred_p,
        #                     params2$pred_p),
        #     adjust_k_early = ifelse(rbinom(1, 1, 0.5),
        #                             params1$adjust_k_early,
        #                             params2$adjust_k_early),
        #     adjust_k_normal = ifelse(rbinom(1, 1, 0.5),
        #                              params1$adjust_k_normal,
        #                              params2$adjust_k_normal),
        #     adjust_k_finals = ifelse(rbinom(1, 1, 0.5),
        #                              params1$adjust_k_finals,
        #                              params2$adjust_k_finals)
        #)

        params <- mutate_params(params)
    })

    return(new_pop)
}


#' Evaluate population
#'
#' Evaluate a population of AFLELO Params object
#'
#' @param population list of aflelo_params to evaluate
#' @param matches data.frame of matches for training and evaluation
#' @param start_eval start season for calculating fitness
#' @param end_eval end season for calculating fitness
#' @param pred_weight weight given to prediction accuracy when calculating
#'        fitness
#' @param n_cores number of cores to use
#'
#' @return data.frame giving the fitness of each aflelo_params
#' @examples
#' data("matches")
#' population <- aflelo:::new_population(5)
#' evaluate_population(population, matches, start_eval = 1997, end_eval = 1998)
#' @export
evaluate_population <- function(population, matches, start_eval = 2000,
                                end_eval = 2016, pred_weight = 0.8,
                                n_cores = 1) {
    checkmate::assert_list(population, types = "aflelo_params")
    checkmate::assert_data_frame(matches)
    checkmate::assert_int(start_eval, lower = min(matches$Season))
    checkmate::assert_int(end_eval, lower = min(matches$Season))
    checkmate::assert_number(pred_weight, lower = 0, upper = 1)
    checkmate::assert_int(n_cores, lower = 1)

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

        mae <- mean(abs(match_history$Predicted - match_history$Margin))

        c(PctCorrect = pct_correct,
          MarginMAE = mae,
          FitnessRaw = pred_weight * pct_correct + (1 - pred_weight) * (1 / mae))
    }

    close(pb)
    snow::stopCluster(cl)

    fitness <- as.data.frame(fitness)

    #mae_floor <- floor(min(fitness$MarginMAE))
    #fitness$MAEAdjusted <- fitness$MarginMAE / mae_floor

    #fitness$Fitness <- pred_weight * fitness$PctCorrect +
    #    (1 - pred_weight) * (1 / fitness$MAEAdjusted)

    mae <- fitness$MarginMAE
    mae[mae > 60] <- 60
    mae[mae < 20] <- 20
    mae_adj <- (mae - 20) / 40

    fitness$MAEAdjusted <- mae_adj

    fitness$Fitness <- pred_weight * fitness$PctCorrect +
        (1 - pred_weight) * (1 - mae_adj)

    return(fitness)
}


#' Optimise AFLELO Params
#'
#' Attempt to find an optimal set of AFLELO Params using a genetic algorithm
#'
#' @param matches data.frame of matches for training and evaluation
#' @param start_eval start season for calculating fitness
#' @param end_eval end season for calculating fitness
#' @param n population size
#' @param prop_new proportion of new individuals added at each generation
#' @param generations number of generations
#' @param pred_weight weight given to prediction accuracy when calculating
#'        fitness
#' @param n_cores number of cores to use
#'
#' @return list of data.frames describing parameters and fitness, one for each
#'         generation
#'
#' @examples
#' data("matches")
#' optimise_params(matches, start_eval = 1997, end_eval = 1998, n = 5,
#'                 generations = 1)
#'
#' @importFrom stats median sd
#' @export
optimise_params <- function(matches, start_eval = 2000, end_eval = 2016,
                            n = 100, prop_new = 0.1, generations = 10,
                            pred_weight = 0.8, n_cores = 1) {
    checkmate::assert_data_frame(matches)
    checkmate::assert_int(start_eval, lower = min(matches$Season))
    checkmate::assert_int(end_eval, lower = min(matches$Season))
    checkmate::assert_int(n, lower = 1)
    checkmate::assert_number(prop_new, lower = 0, upper = 1)
    checkmate::assert_int(generations, lower = 1)
    checkmate::assert_number(pred_weight, lower = 0, upper = 1)
    checkmate::assert_int(n_cores, lower = 1)

    new_n <- round(n * prop_new)
    population <- new_population(n)

    hall_of_fame <- list()

    for (i in seq_len(generations)) {
        message("[", Sys.time(), "] ", "Generation ", i, " of ", generations)
        fitness <- evaluate_population(population, matches, start_eval,
                                       end_eval, pred_weight, n_cores)

        pop_flat <- t(sapply(population, unlist))

        hall_of_fame[[i]] <- data.frame(pop_flat, fitness, row.names = NULL)

        if (new_n > 0) {
            new_pop <- new_population(new_n)
        } else {
            new_pop <- list()
        }
        breed_pop <- breed_params(population, fitness$Fitness, n - new_n)
        population <- c(breed_pop, new_pop)

        message("Prediction    ",
                "Min: ", round(min(fitness$PctCorrect), 2), ", ",
                "Median: ", round(median(fitness$PctCorrect), 2), ", ",
                "Max: ", round(max(fitness$PctCorrect), 2), ", ",
                "Mean: ", round(mean(fitness$PctCorrect), 2), ", ",
                "SD: ", round(sd(fitness$PctCorrect), 2))

        message("Margin        ",
                "Min: ", round(min(fitness$MarginMAE), 2), ", ",
                "Median: ", round(median(fitness$MarginMAE), 2), ", ",
                "Max: ", round(max(fitness$MarginMAE), 2), ", ",
                "Mean: ", round(mean(fitness$MarginMAE), 2), ", ",
                "SD: ", round(sd(fitness$MarginMAE), 2))

        message("Fitness       ",
                "Min: ", round(min(fitness$Fitness), 2), ", ",
                "Median: ", round(median(fitness$Fitness), 2), ", ",
                "Max: ", round(max(fitness$Fitness), 2), ", ",
                "Mean: ", round(mean(fitness$Fitness), 2), ", ",
                "SD: ", round(sd(fitness$Fitness), 2))
    }

    return(hall_of_fame)
}
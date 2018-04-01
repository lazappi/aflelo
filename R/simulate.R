#' Simulate matches
#'
#' Simulate a series of AFL matches
#'
#' @param model trained aflelo_model object
#' @param matches data.frame containg matches to simulate, see [matches] for an
#'        example
#' @param n number of times to run the simulation
#' @param n_cores number of cores to use
#' @param seed random seed set before beginning simulations
#'
#' @return Ratings for teams at the end of each simulation
#' @examples
#' data("matches")
#' matches1997 <- matches[matches$Season == 1997, ]
#' model <- aflelo_model()
#' simulate_matches(model, matches1997, n = 1)
#'
#' @export
simulate_matches <- function(model, matches, n = 10000, n_cores = 1, seed = 1) {

    checkmate::assert_class(model, "aflelo_model")
    checkmate::assert_data_frame(matches, min.rows = 1)
    checkmate::assert_int(n, lower = 1)
    checkmate::assert_int(n_cores, lower = 1)
    checkmate::assert_number(seed)

    set.seed(seed)

    `%dopar%` <- foreach::`%dopar%`

    cl <- snow::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    pb <- utils::txtProgressBar(max = n, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    sim_ratings <- foreach::foreach(seq_len(n),
                                    .packages = "aflelo",
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

            real_margin <- round(stats::rnorm(1, mean = pred_margin,
                                              sd = sim_model$params$sim_sigma))

            home_total <- 75
            away_total <- 75
            if (real_margin > 0) {
                home_total <- 75 + real_margin
            } else if (real_margin < 0) {
                away_total <- 75 + (-real_margin)
            }

            match <- aflelo_match(season      = season,
                                  round       = round,
                                  home        = home,
                                  away        = away,
                                  ground      = ground,
                                  pred_margin = pred_margin,
                                  home_total  = home_total,
                                  away_total  = away_total)

            sim_model <- add_match(sim_model, match)
        }

        data.frame(sim_model$ratings, sim_model$ladder[, -1])
    }

    close(pb)
    snow::stopCluster(cl)

    return(sim_ratings)
}
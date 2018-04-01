#' New AFLELO Params
#'
#' Create a new AFLELO Params object
#'
#' @param new_team_rating start rating for new teams
#' @param new_season_adjustment amount to regress at beginning of new season,
#'        in range 0-1
#' @param hga_alpha alpha parameter for calculating home ground advantage,
#'        controls the weighting given to travel
#' @param hga_beta beta parameter for calculating home ground advantage,
#'        controls the weighting given to ground experience
#' @param pred_m m parameter for predicting results, controls how difference in
#'        rating effects predicted result
#' @param pred_p p parameter for predicting margins, controls how predicted
#'        results are converted to margins
#' @param adjust_k_early k value for early rounds, bigger values cause bigger
#'        changes in ratings
#' @param adjust_k_normal k value for normal rounds
#' @param adjust_k_finals k value for finals
#' @param sim_sigma standard deviation used when simulating margins
#'
#' @return An aflelo_params object
new_aflelo_params <- function(new_team_rating       = 1090,
                              new_season_adjustment = 0.1,
                              hga_alpha             = 6,
                              hga_beta              = 15,
                              pred_m                = 400,
                              pred_p                = 0.0464,
                              adjust_k_early        = 82,
                              adjust_k_normal       = 62,
                              adjust_k_finals       = 72,
                              sim_sigma             = 37.1) {

    structure(
        list(
            new_team_rating       = new_team_rating,
            new_season_adjustment = new_season_adjustment,
            hga_alpha             = hga_alpha,
            hga_beta              = hga_beta,
            pred_m                = pred_m,
            pred_p                = pred_p,
            adjust_k_early        = adjust_k_early,
            adjust_k_normal       = adjust_k_normal,
            adjust_k_finals       = adjust_k_finals,
            sim_sigma             = sim_sigma
        ),
        class = "aflelo_params"
    )
}


#' Validate AFLELO Params
#'
#' Validate and AFLELO Params object
#'
#' @param params aflelo_params to validate
#'
#' @return Raises an error if invalid, otherwise returns the aflelo_params
validate_aflelo_params <- function(params) {
    checkmate::assert_class(params, "aflelo_params")
    checkmate::assert_number(params$new_team_rating, lower = 0, finite = TRUE)
    checkmate::assert_number(params$new_season_adjustment, lower = 0, upper = 1)
    checkmate::assert_number(params$hga_alpha, lower = 0, finite = TRUE)
    checkmate::assert_number(params$hga_beta, lower = 0, finite = TRUE)
    checkmate::assert_number(params$pred_m, lower = 0, finite = TRUE)
    checkmate::assert_number(params$pred_p, lower = 0, upper = 1)
    checkmate::assert_number(params$adjust_k_early, lower = 0, finite = TRUE)
    checkmate::assert_number(params$adjust_k_normal, lower = 0, finite = TRUE)
    checkmate::assert_number(params$adjust_k_finals, lower = 0, finite = TRUE)
    checkmate::assert_number(params$sim_sigma, lower = 0, finite = TRUE)

    return(params)
}


#' AFLELO Params
#'
#' Object for storing parameters for an AFLELO Model
#'
#' @param new_team_rating start rating for new teams
#' @param new_season_adjustment amount to regress at beginning of new season,
#'        in range 0-1
#' @param hga_alpha alpha parameter for calculating home ground advantage,
#'        controls the weighting given to travel
#' @param hga_beta beta parameter for calculating home ground advantage,
#'        controls the weighting given to ground experience
#' @param pred_m m parameter for predicting results, controls how difference in
#'        rating effects predicted result
#' @param pred_p p parameter for predicting margins, controls how predicted
#'        results are converted to margins
#' @param adjust_k_early k value for early rounds, bigger values cause bigger
#'        changes in ratings
#' @param adjust_k_normal k value for normal rounds
#' @param adjust_k_finals k value for finals
#' @param sim_sigma standard deviation used when simulating margins
#'
#' @return A valid aflelo_params object
#' @examples
#' aflelo_params()
#'
#' @export
aflelo_params <- function(new_team_rating       = 1090,
                          new_season_adjustment = 0.1,
                          hga_alpha             = 6,
                          hga_beta              = 15,
                          pred_m                = 400,
                          pred_p                = 0.0464,
                          adjust_k_early        = 82,
                          adjust_k_normal       = 62,
                          adjust_k_finals       = 72,
                          sim_sigma             = 37.1) {

    params <- new_aflelo_params(new_team_rating,
                                new_season_adjustment,
                                hga_alpha,
                                hga_beta,
                                pred_m,
                                pred_p,
                                adjust_k_early,
                                adjust_k_normal,
                                adjust_k_finals,
                                sim_sigma)

    validate_aflelo_params(params)
}


#' Print AFLELO Params
#'
#' Print an AFLELO Params
#'
#' @param x aflelo_params to print
#' @param compact whether to print in compact format
#' @param ... additional arguments, not used
#'
#' @return Prints object
print.aflelo_params <- function(x, compact = FALSE, ...) {

    if (compact) {
        cat(crayon::bold("New team\t"), "Rating:", x$new_team_rating,
            "\n")
        cat(crayon::bold("New season\t"),
            "Adjustment:", x$new_season_adjustment,
            "\n")
        cat(crayon::bold("Home ground \t"), "Alpha:", x$hga_alpha,
                                            "Beta:", x$hga_beta,
            "\n")
        cat(crayon::bold("Prediction\t"), "m:", x$pred_m,
                                          "p:", x$pred_p,
            "\n")
        cat(crayon::bold("Adjustment\t"), "k_early:", x$adjust_k_early,
            "k_normal:", x$adjust_k_normal, "k_finals:", x$adjust_k_finals,
            "\n")
        cat(crayon::bold("Simulation\t"), "Sigma:", x$sim_sigma)
    } else {
        cat("AFLELO Parameters", "\n")

        cat(crayon::bold("New team"), "\n")
        cat("Rating:", x$new_team_rating, "\n")
        cat("\n")

        cat(crayon::bold("New season"), "\n")
        cat("Adjustment:", x$new_season_adjustment, "\n")
        cat("\n")

        cat(crayon::bold("Home ground advantage"), "\n")
        cat(paste0("Alpha: ", x$hga_alpha, ", Beta: ", x$hga_beta), "\n")
        cat("\n")

        cat(crayon::bold("Predictions"), "\n")
        cat(paste0("m: ", x$pred_m, ", p: ", x$pred_p), "\n")
        cat("\n")

        cat(crayon::bold("Rating adjustments"), "\n")
        cat(paste0("k_early: ", x$adjust_k_early,
                   ", k_normal: ", x$adjust_k_normal,
                   ", k_finals: ",x$adjust_k_finals), "\n")
        cat("\n")

        cat(crayon::bold("Simulations"), "\n")
        cat(paste0("Sigma: ", x$sim_sigma), "\n")
        cat("\n")
    }

}
new_aflelo_params <- function(new_team_rating = 1090,
                              new_season_adjustment = 0.1,
                              hga_alpha = 6,
                              hga_beta = 15,
                              pred_m = 400,
                              pred_p = 0.0464,
                              adjust_k = 62,
                              sim_sigma = 37.1) {

    structure(
        list(
            new_team_rating       = new_team_rating,
            new_season_adjustment = new_season_adjustment,
            hga_alpha             = hga_alpha,
            hga_beta              = hga_beta,
            pred_m                = pred_m,
            pred_p                = pred_p,
            adjust_k              = adjust_k,
            sim_sigma             = sim_sigma
        ),
        class = "aflelo_params"
    )
}


validate_aflelo_params <- function(params) {
    checkmate::assert_class(params, "aflelo_params")
    checkmate::assert_number(params$new_team_rating, lower = 0, finite = TRUE)
    checkmate::assert_number(params$new_season_adjustment, lower = 0, upper = 1)
    checkmate::assert_number(params$hga_alpha, lower = 0, finite = TRUE)
    checkmate::assert_number(params$hga_beta, lower = 0, finite = TRUE)
    checkmate::assert_number(params$pred_m, lower = 0, finite = TRUE)
    checkmate::assert_number(params$pred_p, lower = 0, upper = 1)
    checkmate::assert_number(params$sim_sigma, lower = 0, finite = TRUE)

    return(params)
}


aflelo_params <- function(...) {

    params <- new_aflelo_params()

    dots <- list(...)

    for (dot in names(dots)) {
        if (dot %in% names(params)) {
            params[[dot]] <- dots[[dot]]
        } else {
            stop(dot, " is not a valid parameter")
        }
    }

    validate_aflelo_params(params)
}


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
        cat(crayon::bold("Adjustment\t"), "k:", x$adjust_k,
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
        cat(paste0("k: ", x$adjust_k), "\n")
        cat("\n")

        cat(crayon::bold("Simulations"), "\n")
        cat(paste0("Sigma: ", x$sim_sigma), "\n")
        cat("\n")
    }

}
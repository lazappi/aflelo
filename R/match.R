#' New AFLELO Match
#'
#' Create a new AFLELO Match
#'
#' @param season Integer giving season of the match
#' @param round String giving the round of the match
#' @param home String giving the name of the home team
#' @param away String giving the name of the away team
#' @param ground String giving the name of the ground
#' @param pred_margin Number giving the predicted margin
#' @param real_margin Number giving the real margin
#' @param home_total Number giving the total score for the home team
#' @param away_total Number giving the total score for the away team
#'
#' @return An aflelo_match object
new_aflelo_match <- function(season, round, home, away, ground, pred_margin,
                             real_margin, home_total, away_total) {

    structure(
        list(
            season = season,
            round = round,
            home = home,
            away = away,
            ground = ground,
            pred_margin = pred_margin,
            real_margin = real_margin,
            home_total = home_total,
            away_total = away_total
        ),
        class = "aflelo_match"
    )

}


#' Validate AFLELO Match
#'
#' Validate an AFLELO Match object
#'
#' @param match The aflelo_match to validate
#'
#' @return Raises an error if invalid, otherwise returns the aflelo_match
validate_aflelo_match <- function(match) {
    checkmate::assert_class(match, "aflelo_match")
    checkmate::assert_int(match$season)
    checkmate::assert_character(match$round, len = 1)
    checkmate::assert_character(match$home, len = 1)
    checkmate::assert_character(match$away, len = 1)
    checkmate::assert_character(match$ground, len = 1)
    checkmate::assert_number(match$pred_margin)
    checkmate::assert_number(match$real_margin)
    checkmate::assert_int(match$home_total, lower = 0)
    checkmate::assert_int(match$away_total, lower = 0)

    return(match)
}


#' AFLELO Match
#'
#' Object for storing information about AFL matches
#'
#' @param season Integer giving season of the match
#' @param round String giving the round of the match
#' @param home String giving the name of the home team
#' @param away String giving the name of the away team
#' @param ground String giving the name of the ground
#' @param pred_margin Number giving the predicted margin
#' @param home_total Number giving the total score for the home team
#' @param away_total Number giving the total score for the away team
#'
#' @return A valid aflelo_match object
#' @examples
#' aflelo_match(2000, "R1", "Melbourne", "Adelaide", "M.C.G.", 10, 110, 102)
#'
#' @export
aflelo_match <- function(season, round, home, away, ground, pred_margin,
                         home_total, away_total) {

    real_margin <- home_total - away_total

    match <- new_aflelo_match(season, round, home, away, ground, pred_margin,
                              real_margin, home_total, away_total)

    validate_aflelo_match(match)
}


#' Print AFLELO Match
#'
#' Print an AFLELO Match
#'
#' @param x aflelo_match object to print
#' @param ... additional arguments, not used
#'
#' @return Prints object
print.aflelo_match <- function(x, ...) {

    cat("AFLELO Match", "\n")

    cat(paste0(crayon::bold("Season: "), x$season, ", ",
               crayon::bold("Round: "), x$round, "\n"))
    cat(crayon::green(x$home), crayon::bold("vs"), crayon::red(x$away),
        crayon::bold("at"), x$ground, "\n")

    cat(crayon::bold("Predicted result: "))
    if (x$pred_margin > 0) {
        cat(crayon::green(x$home), "by", crayon::green(x$pred_margin), "pts")
    } else if (x$pred_margin < 0) {
        cat(crayon::red(x$away), "by", crayon::red(-x$pred_margin), "pts")
    } else {
        cat("Draw")
    }
    cat("\n")

    cat(crayon::bold("Actual result: "))
    cat(crayon::green(x$home), x$home_total,
        crayon::red(x$away), paste0(x$away_total, ", "))
    if (x$real_margin > 0) {
        cat(crayon::green(x$home), "by", crayon::green(x$real_margin), "pts")
    } else if (x$real_margin < 0) {
        cat(crayon::red(x$away), "by", crayon::red(-x$real_margin), "pts")
    } else {
        cat("Draw")
    }
}

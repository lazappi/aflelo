new_aflelo_match <- function(season, round, home, away, ground, predicted,
                             margin) {

    structure(
        list(
            season = season,
            round = round,
            home = home,
            away = away,
            ground = ground,
            predicted = predicted,
            margin = margin
        ),
        class = "aflelo_match"
    )

}


validate_aflelo_match <- function(match) {
    checkmate::assert_class(match, "aflelo_match")
    checkmate::assert_int(match$season)
    checkmate::assert_character(match$round, len = 1)
    checkmate::assert_character(match$home, len = 1)
    checkmate::assert_character(match$away, len = 1)
    checkmate::assert_character(match$ground, len = 1)
    checkmate::assert_number(match$predicted)
    checkmate::assert_number(match$margin)

    return(match)
}

aflelo_match <- function(season, round, home, away, ground, predicted,
                         margin) {

    match <- new_aflelo_match(season, round, home, away, ground, predicted,
                              margin)

    validate_aflelo_match(match)
}

print.aflelo_match <- function(x, ...) {

    cat("AFLELO Match", "\n")

    cat(paste0(crayon::bold("Season: "), x$season, ", ",
               crayon::bold("Round: "), x$round, "\n"))
    cat(crayon::green(x$home), crayon::bold("vs"), crayon::red(x$away),
        crayon::bold("at"), x$ground, "\n")

    cat(crayon::bold("Predicted result: "))
    if (x$predicted > 0) {
        cat(crayon::green(x$home), "by", crayon::green(x$predicted), "pts")
    } else if (x$predicted < 0) {
        cat(crayon::red(x$away), "by", crayon::red(-x$predicted), "pts")
    } else {
        cat("Draw")
    }
    cat("\n")

    cat(crayon::bold("Actual result: "))
    if (x$margin > 0) {
        cat(crayon::green(x$home), "by", crayon::green(x$margin), "pts")
    } else if (x$margin < 0) {
        cat(crayon::red(x$away), "by", crayon::red(-x$actual), "pts")
    } else {
        cat("Draw")
    }
}
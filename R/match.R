new_aflelo_match <- function(season, round, home, away, ground, pred_margin,
                             real_margin) {

    structure(
        list(
            season = season,
            round = round,
            home = home,
            away = away,
            ground = ground,
            pred_margin = pred_margin,
            real_margin = real_margin
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
    checkmate::assert_number(match$pred_margin)
    checkmate::assert_number(match$real_margin)

    return(match)
}

aflelo_match <- function(season, round, home, away, ground, pred_margin,
                         real_margin) {

    match <- new_aflelo_match(season, round, home, away, ground, pred_margin,
                              real_margin)

    validate_aflelo_match(match)
}

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
    if (x$real_margin > 0) {
        cat(crayon::green(x$home), "by", crayon::green(x$real_margin), "pts")
    } else if (x$real_margin < 0) {
        cat(crayon::red(x$away), "by", crayon::red(-x$real_margin), "pts")
    } else {
        cat("Draw")
    }
}
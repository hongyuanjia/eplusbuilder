# with_eplusr_option {{{
#' Evaluate an expression with temporary eplusr options
#'
#' @param opts A list of valid input for `eplusr::eplusr_option()`.
#' @param expr An expression to be evaluated.
#' @importFrom eplusr eplusr_option
#' @export
with_eplusr_option <- function (opts, expr) {
    # get options
    ori <- eplusr::eplusr_option()

    if (!is.list(opts) || is.null(names(opts))) {
        stop("`opts` should be a named list.")
    }

    if (any(!names(opts) %in% names(ori))) {
        stop("Invalid eplusr option found: ", sQuote(names(opts)[!names(opts) %in% names(ori)]))
    }

    # set new option values
    on.exit(do.call(eplusr::eplusr_option, ori), add = TRUE)
    do.call(eplusr::eplusr_option, opts)

    force(expr)
}
# }}}

# with_check_disabled {{{
#' Evaluate an expression with eplusr checking components disabled
#'
#' @param disable Names of checking components to disable during evaluation of
#' `expr`.
#' @param expr An expression to be evaluated with specified checking components
#' disabled.
#' @importFrom eplusr eplusr_option level_checks
#' @export
with_check_disabled <- function (disable, expr) {
    # get current validate level
    lvl <- eplusr::eplusr_option("validate_level")
    on.exit(eplusr::eplusr_option(validate_level = lvl), add = TRUE)

    # get checkings included
    chk <- eplusr::level_checks(lvl)

    # stop if invaid checking name found
    if (any(!disable %in% names(chk))) {
        invld <- disable[!disable %in% names(chk)]
        stop("Invalid checking names found\n:",
            paste0(" #", which(disable == invld), "| ", sQuote(invld), "")
        )
    }

    chk[disable] <- FALSE
    eplusr::eplusr_option(validate_level = chk)
    force(expr)
}
# }}}

# with_no_verbose {{{
#' Evaluate an expression with no verbose information
#'
#' @param expr An expression to be evaluated
#' @importFrom eplusr eplusr_option
#' @export
with_no_verbose <- function (expr) {
    with_eplusr_option(list(verbose_info = FALSE), expr)
}
# }}}

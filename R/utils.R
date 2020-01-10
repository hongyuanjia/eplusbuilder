# std_choice {{{
std_choice <- function (x, choices) {
    choices[match(tolower(x), tolower(choices))]
}
# }}}

# get_idd_env {{{
get_idd_env <- function (idf) {
    idf$.__enclos_env__$private$idd_env()
}
# }}}

# get_idf_env {{{
get_idf_env <- function (idf) {
    idf$.__enclos_env__$private$idf_env()
}
# }}}

# pad {{{
pad <- function(x, char = " ", width = NULL, direction = "left") {
    if (!length(x)) return(x)
    w <- nchar(x, type = "width")
    if (is.null(width)) width <- max(w)
    paste0(strrep(char, pmax(width - w, 0)), x)
}
# }}}

# na_to_empty {{{
na_to_empty <- function (x, quote = TRUE) {
    x <- as.character(x)
    if (quote) x[!is.na(x)] <- paste0("'", x[!is.na(x)], "'")
    x[is.na(x)] <- ""
    x
}
# }}}

# assert_no_error {{{
assert_no_error <- function (idd_env, idf_env, obj, val, unique_name = TRUE,
                             extensible = TRUE, required_field = TRUE,
                             autofield = TRUE, type = TRUE, choice = TRUE,
                             range = TRUE, reference = TRUE) {
    # stop if there are any errors
    valid <- eplusr:::validate_objects(idd_env, idf_env,
        dt_object = unique(obj, by = c("object_id")),
        dt_value = unique(val, by = c("object_id", "value_id")),
        unique_name = unique_name,
        extensible = extensible,
        required_field = required_field,
        autofield = autofield,
        type = type,
        choice = choice,
        range = range,
        reference = reference
    )
    if (eplusr:::count_check_error(valid) > 0L) {
        on.exit(print(valid), add = TRUE)
        stop("All errors need to be corrected before proceed.", call. = FALSE)
    }
    TRUE
}
# }}}

# assert_no_blank {{{
assert_no_blank <- function (val) {
    # get rid of R CMD check NOTE
    value_chr <- NULL

    if (nrow(miss <- val[is.na(value_chr)])) {
        stop("Object(s) below has blank fields:\n",
            paste0(sprintf(" #%s| Object %s [ID: %i]",
                pad(seq_along(miss$object_id), "0"), na_to_empty(miss$object_name), miss$object_id
            ), collapse = "\n"),
        )
    }
    TRUE
}
# }}}

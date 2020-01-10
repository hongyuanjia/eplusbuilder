# toCamelCase {{{
#' Change to CamelCase
#'
#' @param x A character vector
#'
#' @return A character vector changed to CamelCase
#' @examples
#' toCamelCase(c("a b", "a b-c_d"))
#' @export
toCamelCase <- function (x) {
    x <- gsub("[\\s-_](\\w)", "\\U\\1\\L", tolower(x), perl = TRUE)
    gsub("^(\\w)", "\\U\\1\\L", x, perl = TRUE)
}
# }}}

# as_fun {{{
as_fun <- function (x) {
    if (is.null(x)) {
        identity
    } else if (is.function(x)) {
        x
    } else if (inherits(x, "formula")) {
        if (length(x) > 2) {
            stop("Can't convert a two-sided formula to a function")
        }

        fun <- function (){}
        # have to use this ugly approach to get rid of R CMD check NOTE
        eval(parse(text = "formals(fun) <- alist(... =, .x = ..1, .y = ..2, . = ..1)"))

        body(fun) <- x[[2L]]
        fun
    } else if (is.character(x)) {
        if (anyNA(x)) {
            stop(sprintf("When '%s' is a character vector, it cannot contain any NA",
                deparse(substitute(x))
            ))
        }
        function (class) {
            vapply(class, function (cls) {
                nm <- names(which(x == cls))
                if (length(nm)) return(nm)
                else cls
            }, character(1), USE.NAMES = FALSE)
        }
    } else {
        stop(sprintf("'%s' should be one of NULL, a function or a character vector",
            deparse(substitute(x))
        ))
    }

}
# }}}

# rename_per_field {{{
#' Rename object in specified class based on zone or zone list it belongs to
#'
#' `rename_per_field()` renames all objects in specified class in format
#' `class-field`, where `class` and `field` are character vectors created by
#' using class names and `class_fun`, and field values and `field_fun`,
#' respectivly.
#'
#' @param idf An [eplusr::Idf]
#'
#' @param class A character vector of classes names
#'
#' @param field An integer or a single string specifying the fields whose values
#' will be used as input passed to `field_fun`
#'
#' @param class_fun,field_fun A function to take the corresponding class names
#' or field values as input, process them and return a character vector with the
#' same length. It can also be a named character vector. In this case, when
#' specified class names or field values match element value, the element name
#' will be used as part of the new name. If `NULL`, the class names or field
#' values will be directly used. For `class_fun`, default is to
#' use [base::abbreviate()] to abbreviate them into 10 character width. For
#' `field_fun`, default is `NULL`.
#'
#' @param strict If `TRUE`, an error will be issued if the newly generated
#' object names contain duplications. If `FALSE`, [make.unique] will be called
#' to make sure there are no duplications. Default: `FALSE`
#'
#' @return The modified [eplusr::Idf] object itself, invisiblly
#' @export
rename_per_field <- function (idf, class, field, class_fun = function (x) abbreviate(x, 10L),
                              field_fun = NULL, strict = FALSE) {
    # get rid of R CMD check NOTE
    J <- has_name <- object_id <- class_name <- value <- NULL

    if (length(field) != 1L) {
        stop("'field' should be a length-1 vector")
    }

    class <- verify_class(idf, class)

    has_nm <- get_idd_env(idf)$class[J(class), on = "class_name", has_name]
    if (any(!has_nm)) {
        warning("Class(es) that does not have name attributes will be ignored: ",
            paste0("'", class[!has_nm], "'", collapse = ", ")
        )
        class <- class[has_nm]
    }

    if (!length(class)) {
        message("No objects can be renamed. Skip...")
        return(invisible(idf))
    }

    idd_env <- get_idd_env(idf)
    idf_env <- get_idf_env(idf)
    val <- eplusr:::get_idf_value(idd_env, idf_env, class, field = rep(field, length(class)))

    class <- as_fun(class_fun)(val$class_name)
    suffix <- as_fun(field_fun)(val$value_chr)

    if (length(class) != nrow(val)) {
        stop(sprintf("'class_fun' should return a character vector with length %i but not %i",
            nrow(val), length(class)
        ))
    }
    if (length(suffix) != nrow(val)) {
        stop(sprintf("'field_fun' should return a character vector with length %i but not %i",
            nrow(val), length(suffix)
        ))
    }

    data.table::set(val, NULL, "value", paste(class, suffix, sep = "-"))

    if (!strict) {
        data.table::set(val, NULL, "value", make.unique(val$value, sep = "-"))
    }

    # update
    idf$update(val[, list(id = object_id, class = class_name, index = 1L, value)])

    invisible(idf)
}
# }}}

# rename_per_fun {{{
#' Rename objects using speicified function
#'
#' @param idf An [eplusr::Idf] object
#'
#' @param class A character vector of classes names
#'
#' @param fun A function to take the corresponding object names as input,
#' process them and return a character vector with the same length. `fun`
#' can be a named character vector. In this case, when specified object names
#' match element value, the element name will be used as the new name. If
#' `NULL`, nothing will be done. Default: [toCamelCase]
#'
#' @param strict If `TRUE`, an error will be issued if the newly generated
#' object names contain duplications. If `FALSE`, [make.unique] will be called
#' to make sure there are no duplications. Default: `FALSE`
#'
#' @return The modified [eplusr::Idf] object, invisibly
#' @export
rename_per_fun <- function (idf, class, fun = toCamelCase, strict = FALSE) {
    # get rid of R CMD check NOTE
    J <- has_name <- index <- NULL

    class <- verify_class(idf, class)

    has_nm <- get_idd_env(idf)$class[J(class), on = "class_name", has_name]
    if (any(!has_nm)) {
        warning("Class(es) that does not have name attributes will be ignored: ",
            paste0("'", class[!has_nm], "'", collapse = ", ")
        )
        class <- class[has_nm]
    }

    if (!length(class)) {
        message("No objects to be renamed. Skip...")
        return(invisible(idf))
    }

    dt <- idf$to_table(class = class)[index == 1L]

    new_nm <- as_fun(fun)(dt$value)

    if (length(new_nm) != nrow(dt)) {
        stop(sprintf("'fun' should return a character vector with length %i but not %i",
            nrow(dt), length(new_nm)
        ))
    }

    data.table::set(dt, NULL, "value", new_nm)

    if (!strict) {
        data.table::set(dt, NULL, "value", make.unique(dt$value, sep = "-"))
    }

    idf$update(dt)

    invisible(idf)
}
# }}}

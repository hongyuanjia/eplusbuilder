# verify_group {{{
#' Verify input group names and return only existings
#'
#' @param idf An [eplusr::Idf] object
#' @param group A character vector of group names to verify
#'
#' @return A character vector
#' @export
verify_group <- function (idf, group) {
    # make sure input group is a valid group name in current IDD
    if (any(!idf$is_valid_group(group, all = TRUE))) {
        invld <- group[!idf$is_valid_group(group, all = TRUE)]
        stop("Invalid group name found: ", paste0("'", invld, "'", collapse = ", "), ".")
    }

    group[idf$is_valid_group(group)]
}
# }}}

# verify_class {{{
#' Verify input class names and return only existings
#'
#' @param idf An [eplusr::Idf] object
#' @param class A character vector of class names to verify
#'
#' @return A character vector
#' @export
verify_class <- function (idf, class) {
    # make sure input class is a valid class name in current IDD
    if (any(!idf$is_valid_class(class, all = TRUE))) {
        invld <- class[!idf$is_valid_class(class, all = TRUE)]
        stop("Invalid class name found: ", paste0("'", invld, "'", collapse = ", "), ".")
    }

    class[idf$is_valid_class(class)]
}
# }}}

# purge_object {{{
#' Purge objects that are not referenced by any others
#'
#' @param idf An [eplusr::Idf]
#' @param class A character vector of classes where objects to purge
#'
#' @return The modified [eplusr::Idf] object itself, invisiblly
#'
#' @examples
#' \dontrun{
#' purge_object(idf, "ScheduleTypeLimits")
#' }
#'
#' @export
#' @importFrom eplusr eplusr_option
purge_object <- function(idf, class) {
    class <- verify_class(idf, class)

    if (!length(class)) {
        message("No objects can be purged. Skip...")
        return(invisible(idf))
    }

    # get object IDs
    ids <- idf$object_id(class, simplify = TRUE)

    # use internal functions to handle reference
    idd_env <- get_idd_env(idf)
    idf_env <- get_idf_env(idf)
    ref <- eplusr:::get_idfobj_relation(idd_env, idf_env, object_id = ids,
        name = FALSE, direction = "ref_by")$ref_by

    # check if referenced
    no_ref <- !ids %in% ref$src_object_id

    # get id of objects without reference
    ids_del <- ids[no_ref]

    if (!length(ids_del)) {
        message("No objects can be purged. Skip...")
        return(invisible(idf))
    }

    if (eplusr::eplusr_option("verbose_info")) {
        nms_del <- idf$object_name(class, simplify = TRUE)[no_ref]

        cls_del <- eplusr:::get_idf_object(idd_env, idf_env, object = ids_del)$class_name

        message("Object(s) below has been purged:\n", paste0(
            sprintf(" #%s| Object %s [ID: %i] in class '%s'",
                pad(seq_along(ids_del), "0"), na_to_empty(nms_del), ids_del, cls_del
            ), collapse = "\n"
        ))
    }

    with_no_verbose(idf$del(ids_del))

    invisible(idf)
}
# }}}

# merge_object {{{
#' Merge redundant objects into one
#'
#' @details
#' Redundant objects here refer to objects that have different names but all
#' other field values are the same.
#'
#' `merge_object()` will only keep the first object and remove all redundant
#' objects. The field value references are handled automatically, i.e. all
#' field values that originally refer to the redundant objects will be
#' redirected to the object that is kept.
#'
#' @param idf An [eplusr::Idf] object
#'
#' @param class A character vector of class names in current [eplusr::Idf]. If
#' `NULL`, all classes in current [eplusr::Idf] object will be used.
#'
#' @return The modified object itself, invisibly
#' @examples
#' \dontrun{
#' merge_object(idf, "ScheduleTypeLimits")
#' }
#' @export
merge_object <- function (idf, class) {
    # get rid of R CMD check NOTE
    id <- name <- id_del <- name_del <- .SD <- field_index <- i.field_index <- NULL
    src_object_id <- i.id <- i.value_id <- i.value_chr <- i.value_num <- NULL
    i.src_value_chr <- i.src_value_num <- i.src_value_id <- i.src_object_id <- NULL

    # use internal functions
    idd_env <- get_idd_env(idf)
    idf_env <- get_idf_env(idf)

    # make sure every object in class has the same number of fields
    dt <- idf$to_table(class = class, align = TRUE)

    # change to lower case for comparison
    data.table::set(dt, NULL, "value", tolower(dt$value))

    # get ID of objects to keep and delete
    dt_merge <- lapply(split(dt, by = "class"), function(d) {
        # dcast to compare
        d <- data.table::dcast(d, class + id + name ~ index, value.var = "value")

        d[, list(class = class[[1L]], id = id[[1L]], name = name[[1L]],
                 id_del = list(id[-1L]), name_del = list(name[-1L])
            ), by = c(setdiff(names(d), c("class", "id", "name", "1")))][
          , list(class, id, name, id_del, name_del)]
    })
    dt_merge <- data.table::rbindlist(dt_merge)[, lapply(.SD, unlist), by = c("class", "id", "name")]

    # get referenced field index of object to be deleted
    ref <- eplusr:::get_idfobj_relation(idd_env, idf_env, object_id = dt_merge$id_del,
        name = FALSE, direction = "ref_by")$ref_by
    val <- eplusr:::get_idf_value(idd_env, idf_env, object = dt_merge$id_del)
    ref[val, on = c("src_object_id" = "object_id", "src_value_id" = "value_id"),
        field_index := i.field_index]
    # update the referenced object id
    ref[dt_merge, on = c("src_object_id" = "id_del"), src_object_id := i.id]
    # update the reference value id
    val <- eplusr:::get_idf_value(idd_env, idf_env, object = unique(dt_merge$id))
    ref[val, on = c("src_object_id" = "object_id", "field_index"), `:=`(
        "src_value_id" = i.value_id, "src_value_chr" = i.value_chr, "src_value_num" = i.value_num
    )]

    # update referenced value
    idf_env$value[ref, on = c("object_id", "value_id"), `:=`(
        value_chr = i.src_value_chr, value_num = i.src_value_num
    )]
    # update reference dict
    idf_env$reference[ref, on = c("object_id", "value_id"), `:=`(
        src_object_id = i.src_object_id, src_value_id = i.src_value_id
    )]

    if (eplusr::eplusr_option("verbose_info")) {
        message(dt_merge[, by = c("class", "id", "name"), paste0(
            sprintf("Objects in class '%s' below have been merged into object '%s' [ID: %i]:\n",
                class, name, id
            ),
            paste0(sprintf(" #%s| Object %s [ID: %i]",
                pad(seq_along(id_del), "0"), na_to_empty(name_del), id_del
            ), collapse = "\n"),
            "\n\n"
        )]$V1)
    }

    # delete objects
    if (nrow(dt_merge)) {
        with_no_verbose(idf$del(dt_merge$id_del, .force = TRUE))
    }

    invisible(idf)
}
# }}}

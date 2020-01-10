# SCH_DAYTYPE {{{
SCH_DAYTYPE <- list(
    designday = c("SummerDesignDay", "WinterDesignDay"),
    weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
    weekends = c("Saturday", "Sunday"),
    holiday = c("Holiday"),
    customday = c("CustomDay1", "CustomDay2")
)
# }}}

# schedule_year_to_compact {{{
#' Convert `Schedule:Year` objects to `Schedule:Compact` objects
#'
#' @param idf An [eplusr::Idf] object
#'
#' @param object An integer vector of IDs or a character vector of names of
#' objects in `Schedule:Year` to be converted
#'
#' @return The modifed `Idf` object, indivisibly
#'
#' @examples
#' \dontrun{
#' schedule_year_to_compact(idf)
#' }
#' @importFrom data.table data.table ":=" setnames setcolorder
#' @importFrom eplusr eplusr_option
#' @export
schedule_year_to_compact <- function (idf, object = NULL) {
    # get rid of R CMD check NOTE
    .N <- .SD <- `1` <- `2` <- `3` <- `4` <- `5` <- dates <- end_date <- field_index <- NULL
    i.class_id <- i.class_name <- i.new_object_id <- i.new_value_id <- NULL
    i.value_chr <- i.value_id <- index <- object_id <- object_name <- NULL
    src_class_id <- src_class_name <- through <- value_id <- week_sch <- NULL
    start_date <- whole_year <- weeksch_type <- yearsch_type <- NULL

    # use internal functions
    idd_env <- get_idd_env(idf)
    idf_env <- get_idf_env(idf)

    obj <- eplusr:::get_idf_object(idd_env, idf_env, class = "Schedule:Year", object = object)
    # make sure rleid is correct
    val <- eplusr:::get_idf_value(idd_env, idf_env, object = obj$object_id)

    # stop if there are any errors
    assert_no_error(idd_env, idf_env, obj, val)

    # stop if there are missing values
    assert_no_blank(val[field_index > 2L])

    # order the schedule by date and make sure a whole year is covered
    span <- val[field_index > 2L][, index := seq.int(.N), by = "object_id"]
    span[index > 5L, index := index %% 5L][index == 0L, index := 5L]
    span[, week := (seq.int(.N) - 1L) %/% 5L + 1L, by = "object_id"]

    span <- lapply(split(span, by = "object_id"),
        function (d) {
            # dcast for year coverage validation
            d <- data.table::dcast(d, object_name + object_id + week ~ index, value.var = "value_chr")

            # change time specifications to integers
            mon_day <- setdiff(names(d), c("object_name", "object_id", "week", "1"))
            for (j in mon_day) {
                data.table::set(d, NULL, j, as.integer(d[[j]]))
            }

            # order by start date
            data.table::setorderv(d, c("2", "3"))

            # get start and end date for each period
            d[, `:=`(
                start_date = as.Date(paste("1970", `2`, `3`, sep = "-")),
                end_date = as.Date(paste("1970", `4`, `5`, sep = "-"))
            )][, list(object_id, object_name, week, week_name = `1`, start_date, end_date)]
        }
    )
    span <- data.table::rbindlist(span)

    # check year coverage
    year_seq <- as.Date(0:364, origin = "1970-01-01")
    span[, `:=`(dates = list(seq(start_date, end_date, by = "1 day"))), by = c("object_id", "week")]
    year_cov <- span[, list(whole_year = all(year_seq %in% Reduce(c, dates))), by = c("object_id", "object_name")]
    if (any(!year_cov$whole_year)) {
        invld <- year_cov[whole_year == FALSE]
        stop("Object(s) below does not cover a whole year in the schedule specification\n",
            paste0(sprintf(" #%s| Object %s [ID: %i]",
                pad(seq_along(invld$object_id), "0"), na_to_empty(invld$object_name), invld$object_id
            ), collapse = "\n"),
        )
    }

    # remove helper colomn
    data.table::set(span, NULL, "dates", NULL)

    # add field index and value id
    span[, field_index := (week - 1L) * 5L + 3L]
    span[val, on = c("object_id", "field_index"), value_id := i.value_id]

    # get week schedule object ID
    ref <- eplusr:::get_idf_relation(idd_env, idf_env, value_id = span$value_id)
    ref[idf_env$object, on = c("src_object_id" = "object_id"), src_class_id := i.class_id]
    ref[idd_env$class, on = c("src_class_id" = "class_id"), src_class_name := i.class_name]
    if (any(ref$src_class_name != "Schedule:Week:Daily")) {
        stop("Currently, only 'Schedule:Week:Daily' is supported")
    }
    # get week schedule piece
    week <- schedule_week_daily_to_compact_piece(idf, ref$src_object_id)

    # add week schedule type
    data.table::set(span, NULL, "weeksch_type", week$schedule_type)
    # add year schedule type
    span[val[field_index == 2L], on = "object_id", yearsch_type := i.value_chr]
    # make sure schedule type match
    if (nrow(mismatch <- span[tolower(yearsch_type) != tolower(weeksch_type)])) {
        warning("Mismatched schedule type limit between year schedule and its week schedule. ",
            "Year schedule type limit will be used:\n",
            paste0(sprintf(" #%s| Object %s [ID: %i] with type limits: %s (year) vs %s (week)",
                pad(seq_along(mismatch$object_id), "0"),
                na_to_empty(mismatch$object_name),
                mismatch$object_id,
                na_to_empty(mismatch$yearsch_type),
                na_to_empty(mismatch$weeksch_type)
            ), collapse = "\n"),
        )
    }
    # add week schedule
    data.table::set(span, NULL, "week_sch", week$value_chr)

    # build compact schedule
    span[, through := sprintf("Through: %i/%i", data.table::month(end_date), data.table::mday(end_date))]
    sch <- span[, by = "object_id", {
        sch <- Map(function (through, week) c(through, week), through, week_sch)
        list(value = list(c(object_name[[1L]], yearsch_type[[1L]], unlist(sch, use.names = FALSE))))
    }][, lapply(.SD, unlist), by = "object_id"]
    data.table::set(sch, NULL, "class", "Schedule:Compact")
    data.table::set(sch, NULL, "index", data.table::rowidv(sch$object_id))
    data.table::setnames(sch, "object_id", "id")

    # add new one
    compact <- idf$load(sch)

    # get new object ID
    compact_id <- vapply(compact, function (sch) sch$id(), integer(1))

    # get old name value id
    year_val <- val[field_index == 1L]
    # get name value id
    compact_val <- eplusr:::get_idf_value(idd_env, idf_env, object = compact_id, field = rep(1L, length(compact_id)))
    # combine old and new value id
    year_compact <- cbind(
        year_val[, list(object_id, value_id)],
        compact_val[, list(new_object_id = object_id, new_value_id = value_id)]
    )

    # redirect reference
    idf_env$reference[year_compact, on = c("src_object_id" = "object_id", "src_value_id" = "value_id"),
        `:=`(src_object_id = i.new_object_id, src_value_id = i.new_value_id)
    ]

    # delete all year, week and day schedule objects
    with_no_verbose(idf$del(obj$object_id, week$object_id, unique(unlist(week$daysch_id)), .force = TRUE))

    # message
    if (eplusr::eplusr_option("verbose_info")) {
        message("Complete conversion from year schedules to compact schedules ",
            "All related week and day schedules have been removed.\n",
            paste0(sprintf(" #%s| %s: Old [ID: %i] --> New [ID: %i]",
                pad(seq_along(year_compact$object_id), "0"),
                na_to_empty(obj$object_name),
                year_compact$object_id, year_compact$new_object_id
            ), collapse = "\n")
        )
    }

    invisible(idf)
}
# }}}

# schedule_day_interval_to_compact_piece {{{
schedule_day_interval_to_compact_piece <- function (idf, object = NULL) {
    # get rid of R CMD check NOTE
    field_index <- J <- object_id <- value_chr <- schedule_type <- i.value_chr <- NULL
    rleid <- class_name <- object_name <- NULL

    class <- "Schedule:Day:Interval"

    # use internal functions
    idd_env <- get_idd_env(idf)
    idf_env <- get_idf_env(idf)

    obj <- eplusr:::get_idf_object(idd_env, idf_env, class = class, object = object)
    # make sure rleid is correct
    val <- eplusr:::get_idf_value(idd_env, idf_env, object = obj$object_id)

    # stop if there are any errors
    assert_no_error(idd_env, idf_env, obj, val)

    # stop if there are missing values
    assert_no_blank(val[field_index > 2L])

    # add column of schedule limit
    type <- val[J(2L), on = "field_index", list(object_id, value_chr)]
    val[type, on = "object_id", schedule_type := i.value_chr]

    # add prefix
    val[J(3L), on = "field_index", value_chr := paste("Interpolate:", value_chr)]
    val[field_index > 3L & (field_index - 3L) %% 2L == 1L, value_chr := paste("Until:", value_chr)]

    val[field_index > 2L, list(rleid, class_name, object_id, object_name, schedule_type, value_chr)]
}
# }}}

# schedule_week_daily_to_compact_piece {{{
schedule_week_daily_to_compact_piece <- function (idf, object = NULL) {
    # get rid of R CMD check NOTE
    J <- class_name <- field_index <- field_name <- i.class_id <- i.class_name <- NULL
    i.daysch_id <- i.daysch_type <- i.daysch <- i.src_object_id <- need_split <- NULL
    object_id <- object_name <- rleid <- schedule_type <- src_class_id <- NULL
    src_class_name <- src_object_id <- value_chr <- value_id <- NULL

    class <- "Schedule:Week:Daily"

    # use internal functions
    idd_env <- get_idd_env(idf)
    idf_env <- get_idf_env(idf)

    obj <- eplusr:::get_idf_object(idd_env, idf_env, class = class, object = object)
    # make sure rleid is correct
    val <- eplusr:::get_idf_value(idd_env, idf_env, object = obj$object_id)
    ref <- eplusr:::get_idf_relation(idd_env, idf_env, value_id = val$value_id)

    # stop if there are any errors
    assert_no_error(idd_env, idf_env, obj, val)

    # stop if there are missing values
    assert_no_blank(val[field_index > 2L])

    # get referenced day schedule type
    ref[idf_env$object, on = c("src_object_id" = "object_id"), src_class_id := i.class_id]
    ref[idd_env$class, on = c("src_class_id" = "class_id"), src_class_name := i.class_name]

    # convert day schedules into compact schedule pieces
    day <- unique(ref[, list(src_class_id, src_class_name, src_object_id)])

    val[field_index > 1L, `:=`(
        value_chr = tolower(value_chr),
        daytype = gsub(" Schedule:Day Name", "", field_name, fixed = TRUE)
    )]

    # merge day type
    week <- val[field_index > 1L, by = c("object_id", "value_chr"), {
        if (all(unlist(SCH_DAYTYPE) %in% daytype)) {
            return(list(daytype = list("AllDays"), value_id = value_id[[1L]], need_split = FALSE))
        }

        if (all(SCH_DAYTYPE$weekdays %in% daytype)) {
            daytype <- c("Weekdays", daytype[!daytype %in% SCH_DAYTYPE$weekdays])
        }
        if (all(SCH_DAYTYPE$weekends %in% daytype)) {
            daytype <- c("Weekends", daytype[!daytype %in% SCH_DAYTYPE$weekends])
        }
        if (all(c(SCH_DAYTYPE$holiday, SCH_DAYTYPE$customday) %in% daytype)) {
            daytype <- c("AllOtherDays", daytype[!daytype %in% c(SCH_DAYTYPE$holiday, SCH_DAYTYPE$customday)])
        }

        list(daytype = list(daytype), value_id = value_id[[1L]],
             need_split = all(c("Weekdays", "AllOtherDays") %in% daytype)
        )
    }]

    # split weekdays and allotherdays
    other <- week[J(TRUE), on = "need_split"][, by = "object_id",
        `:=`(daytype = list(daytype[[1L]][daytype[[1L]] != "Weekdays"]))]
    week[J(TRUE), on = "need_split", `:=`(daytype = list("Weekdays"))]
    week <- data.table::rbindlist(list(week, other))[, need_split := NULL]

    # get day schedule object id
    week[ref, on = c("value_id" = "value_id"), daysch_id := i.src_object_id]
    # get day schedule piece
    day <- schedule_day_interval_to_compact_piece(idf, week$daysch_id)[
        , list(rleid, object_id, schedule_type, value_chr)]
    # split by object
    day <- split(day, by = "rleid", keep.by = FALSE)
    # add day schedule type
    data.table::set(week, NULL, "daysch_type", vapply(day, function (x) x$schedule_type[[1L]], character(1)))
    # add day schedule
    data.table::set(week, NULL, "daysch", lapply(day, "[[", "value_chr"))

    week <- week[, by = c("object_id"), {
        all_daytype <- c(
            SCH_DAYTYPE$designday,
           "Weekdays", SCH_DAYTYPE$weekdays,
           "Weekends", SCH_DAYTYPE$weekends,
           "AllOtherDays", SCH_DAYTYPE$holiday, SCH_DAYTYPE$customday
        )
        # order each group
        grp_ord <- order(vapply(daytype, function (type) max(match(type, all_daytype)), integer(1)))
        daytype <- daytype[grp_ord]
        daysch <- daysch[grp_ord]
        daysch_id <- daysch_id[grp_ord]

        # order day type and combine in each group
        daytype <- vapply(daytype, function (type) {
            paste("For:", paste(all_daytype[all_daytype %in% type], collapse = " "))
        }, character(1))

        # combine day type and schedule
        daysch <- unlist(Map(function(type, sch) c(type, sch), daytype, daysch), use.names = FALSE)

        # only use the first schedule type
        daysch_type <- unique(daysch_type)
        if (length(unique(tolower(daysch_type))) > 1L) {
            warning(sprintf("Multiple schedule type limits found during converting '%'. Only the first will be used.", class))
        }
        daysch_type <- daysch_type[[1L]]

        list(daysch_id = list(daysch_id), daysch_type = daysch_type, daysch = list(daysch))
    }]

    # merge into the main value table
    obj[week, on = "object_id", `:=`(schedule_type = i.daysch_type, daysch_id = i.daysch_id, value_chr = i.daysch)][
        , list(rleid, class_name, object_id, object_name, schedule_type, daysch_id, value_chr)]
}
# }}}

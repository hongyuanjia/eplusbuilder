test_that("clean", {
    eplusr_option(verbose_info = FALSE)
    idf <- eplusr::empty_idf(8.8)
    idf$add(ScheduleTypeLimits = list("Any Number"))
    idf$dup(rep(2, 5))

    expect_silent(merge_object(idf, "ScheduleTypeLimits"))
    expect_equal(idf$object_name("ScheduleTypeLimits", simplify = TRUE), "Any Number")

    purge_object(idf, "ScheduleTypeLimits")
    expect_equal(verify_class(idf, "ScheduleTypeLimits"), character(0))
})


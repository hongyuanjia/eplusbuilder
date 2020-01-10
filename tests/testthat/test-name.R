test_that("toCamelCase", {
    expect_equal(toCamelCase(c("ab", "a b", "a-b c")), c("Ab", "AB", "ABC"))
})

test_that("as_fun", {
    expect_equal(as_fun(NULL)(c("a", "b")), c("a", "b"))

    expect_equal(as_fun(~.)("a"), "a")
    expect_equal(as_fun(~.x)("a"), "a")
    expect_equal(as_fun(~1)("a"), 1)
    expect_equal(as_fun(identity)("a"), "a")
    expect_equal(as_fun(~gsub("a", "b", .))("a"), "b")

    expect_equal(as_fun(c(A = "a"))(c("a", "b")), c("A", "b"))
})

test_that("rename_per_field", {
    eplusr_option(verbose_info = FALSE)
    idf <- eplusr::empty_idf(8.8)
    idf$add(ScheduleTypeLimits = list("Fraction", 0, 1, "Continuous", "Dimensionless"))
    idf$dup(rep(2, 5))

    expect_silent(rename_per_field(idf, "ScheduleTypeLimits", 2))

    expect_equal(idf$object_name("ScheduleTypeLimits", simplify = TRUE),
        c("SchdlTypLm-0", paste("SchdlTypLm-0", 1:5, sep = "-"))
    )
})

test_that("rename_per_fun", {
    eplusr_option(verbose_info = FALSE)
    idf <- eplusr::empty_idf(8.8)
    idf$add(ScheduleTypeLimits = list("Fraction", 0, 1, "Continuous", "Dimensionless"))
    idf$dup(rep(2, 5))

    expect_silent(rename_per_fun(idf, "ScheduleTypeLimits", c(Frac = "Fraction_5")))

    expect_equal(idf$object_name("ScheduleTypeLimits", simplify = TRUE)[6], "Frac")
})

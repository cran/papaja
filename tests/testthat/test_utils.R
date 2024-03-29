context("Utility functions")

test_that(
  "add_equals()"
  , {
    x <- list(
      character = add_equals(c(".4", "=1.6", "<.001", ">.99"))
      , length_zero = add_equals(character(0))
      , no_substitution = add_equals(c(">.4", "=1.6"))
    )
    expect_identical(
      object = x
      , expected = list(
        character = c("= .4", "=1.6", "<.001", ">.99")
        , length_zero = character(0)
        , no_substitution = c(">.4", "=1.6")
      )
    )
  }
)


test_that(
  "apa_confint()"
  , {
    x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
    y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)

    cor_test <- cor.test(x, y)
    apa_confint_res <- apa_confint(cor_test$conf.int, conf.int = 0.95, enclose_math = TRUE)
    expect_is(apa_confint_res, "character")
    expect_equal(apa_confint_res, "95\\% CI $[-0.15, 0.90]$")

    apa_confint_res <- apa_confint(cor_test$conf.int, gt1 = FALSE, enclose_math = TRUE)
    expect_equal(apa_confint_res, "95\\% CI $[-.15, .90]$")

    apa_confint_res <- apa_confint(cor_test$conf.int, enclose_math = TRUE)
    expect_equal(apa_confint_res, "95\\% CI $[-0.15, 0.90]$")

    apa_confint_res <- apa_confint(c(1, 2), enclose_math = TRUE)
    expect_equal(apa_confint_res, "$[1.00, 2.00]$")

    conf_int <- confint(lm(x ~ y))
    apa_confint_res <- apa_confint(conf_int, enclose_math = TRUE)

    expect_is(apa_confint_res, "list")
    expect_equal(length(apa_confint_res), nrow(conf_int))
    expect_equal(names(apa_confint_res), c("Intercept", "y"))
    expect_equal(apa_confint_res$Intercept, "95\\% CI $[19.52, 51.78]$")
    expect_equal(apa_confint_res$y, "95\\% CI $[-0.95, 7.67]$")

    apa_confint(c(-Inf, 0), enclose_math = TRUE)


  }
)

test_that(
  "in_paren()"
  , {
    expect_equal(in_paren("$t(1) = 1$"), "$t[1] = 1$")
    expect_equal(in_paren("$\\chi^2(1, n = 100) = 1$"), "$\\chi^2[1, n = 100] = 1$")
    expect_equal(in_paren("95% CI $[123, 123]$"), "95% CI $[123, 123]$")
    expect_equal(in_paren("$F(1, 234) = 1$"), "$F[1, 234] = 1$")

    expect_identical(
      in_paren(list(estimate = "$t(1) = 1$"))
      , expected = list(estimate = "$t[1] = 1$")
    )
  }
)

test_that(
  "determine_within_between"
  , {
    test <- determine_within_between(data = npk, id = "block", factors = c("N", "P", "K"))
    expect_equal(test, list(within = c("N", "P", "K"), between = NULL))

    data <- npk[2:16, ]
    test <- determine_within_between(data = data, id = "block", factors = c("N", "P", "K"))
    expect_identical(test, list(within = c("N", "P", "K"), between = NULL))
  }
)

test_that(
  "complete_observations"
  , {
    test <- complete_observations(data = npk, id = "block", dv = "yield", within = c("N", "P"))

    expect_identical(
      object = test
      , expected = npk
    )

    data <- npk
    data$yield[5] <- NA
    test <- papaja:::complete_observations(data = data, id = "block", dv = "yield", within = c("N", "K"))

    expect_identical(
      object = test
      , expected = structure(
        list(
          block = structure(
            c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L)
            , .Label = c("1", "3", "4", "5", "6")
            , class = "factor"
          )
          , N = structure(
            c(1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L)
            , .Label = c("0", "1"), class = "factor"
          )
          , P = structure(
            c(2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L)
            , .Label = c("0", "1")
            , class = "factor"
          )
          , K = structure(
            c(2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L)
            , .Label = c("0", "1")
            , class = "factor"
          )
          , yield = c(49.5, 62.8, 46.8, 57, 62.8, 55.8, 69.5, 55, 62, 48.8, 45.5, 44.2, 52, 51.5, 49.8, 48.8, 57.2, 59, 53.2, 56)
        )
        , .Names = c("block", "N", "P", "K", "yield")
        , row.names = c(1L, 2L, 3L, 4L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L)
        , class = "data.frame"
        , removed_cases_explicit_NA = c("2")
      )
    )
  }
)

test_that(
  "canonize"
  , {
    expect_warning(
      out <- papaja:::canonize(npk)
      , regexp = "Some columns could not be renamed: 'block', 'N', 'P', 'K', 'yield'"
    )
    expect_identical(object = out, expected = npk)
  }
)

context("apa_print.lm()")

test_that(
  "Linear regression: lm()-fit"
  , {
    ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
    trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
    group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
    weight <- c(ctl, trt)
    lm_fit <- lm(weight ~ group)

    lm_fit_output <- apa_print(lm_fit)

    expect_apa_results(
      lm_fit_output
      , labels = list(
        term        = "Predictor"
        , estimate  = "$b$"
        , conf.int  = "95\\% CI"
        , statistic = "$t$"
        , df        = "$\\mathit{df}$"
        , p.value   = "$p$"
      )
    )

    # stat
    expect_identical(length(lm_fit_output$stat), 3L)
    expect_identical(names(lm_fit_output$stat), c("Intercept", "groupTrt", "modelfit"))
    expect_identical(length(lm_fit_output$stat$modelfit), 1L)
    expect_identical(names(lm_fit_output$stat$modelfit), "r2")

    expect_identical(lm_fit_output$stat$Intercept, "$t(18) = 22.85$, $p < .001$")
    expect_identical(lm_fit_output$stat$groupTrt, "$t(18) = -1.19$, $p = .249$")
    expect_identical(lm_fit_output$stat$modelfit$r2, "$F(1, 18) = 1.42$, $p = .249$")

    # est
    expect_identical(names(lm_fit_output$est), c("Intercept", "groupTrt", "modelfit"))
    expect_identical(length(lm_fit_output$est$modelfit), 4L)
    expect_identical(names(lm_fit_output$est$modelfit), c("r2", "r2_adj", "aic", "bic"))

    expect_identical(lm_fit_output$est$Intercept, "$b = 5.03$, 95\\% CI $[4.57, 5.49]$")
    expect_identical(lm_fit_output$est$groupTrt, "$b = -0.37$, 95\\% CI $[-1.03, 0.28]$")
    expect_identical(lm_fit_output$est$modelfit$r2, "$R^2 = .07$, 90\\% CI $[0.00, 0.33]$")
    expect_identical(lm_fit_output$est$modelfit$r2_adj, "$R^2_{adj} = .02$")
    expect_identical(lm_fit_output$est$modelfit$aic, "$\\mathrm{AIC} = 46.18$")
    expect_identical(lm_fit_output$est$modelfit$bic, "$\\mathrm{BIC} = 49.16$")

    # full
    expect_identical(names(lm_fit_output$full), c("Intercept", "groupTrt", "modelfit"))
    expect_identical(length(lm_fit_output$full$modelfit), 1L)
    expect_identical(names(lm_fit_output$full$modelfit), "r2")


    expect_identical(lm_fit_output$full$Intercept, "$b = 5.03$, 95\\% CI $[4.57, 5.49]$, $t(18) = 22.85$, $p < .001$")
    expect_identical(lm_fit_output$full$groupTrt, "$b = -0.37$, 95\\% CI $[-1.03, 0.28]$, $t(18) = -1.19$, $p = .249$")
    expect_identical(lm_fit_output$full$modelfit$r2, "$R^2 = .07$, 90\\% CI $[0.00, 0.33]$, $F(1, 18) = 1.42$, $p = .249$")

    # table
    expect_identical(nrow(lm_fit_output$table), 2L)

    # Manual CI
    lm_fit_output <- apa_print(lm_fit, conf.int = matrix(c(1, 2), ncol = 2, nrow = 2, byrow = TRUE, dimnames = list(names(lm_fit$coefficients), c("2.5 \\%", "97.5 \\%"))))
    expect_identical(lm_fit_output$full$Intercept, "$b = 5.03$, 95\\% CI $[1.00, 2.00]$, $t(18) = 22.85$, $p < .001$")
    expect_identical(lm_fit_output$full$groupTrt, "$b = -0.37$, 95\\% CI $[1.00, 2.00]$, $t(18) = -1.19$, $p = .249$")
    expect_identical(lm_fit_output$full$modelfit$r2, "$R^2 = .07$, 90\\% CI $[0.00, 0.33]$, $F(1, 18) = 1.42$, $p = .249$")
    expect_apa_results(
      lm_fit_output
      , labels = list(
        term        = "Predictor"
        , estimate  = "$b$"
        , conf.int  = "95\\% CI"
        , statistic = "$t$"
        , df        = "$\\mathit{df}$"
        , p.value   = "$p$"
      )
    )

    # Set name of estimate
    lm_fit_output <- apa_print(lm_fit, est_name = "\\beta")
    expect_identical(lm_fit_output$est$Intercept, "$\\beta = 5.03$, 95\\% CI $[4.57, 5.49]$")
    expect_identical(lm_fit_output$est$groupTrt, "$\\beta = -0.37$, 95\\% CI $[-1.03, 0.28]$")
    expect_apa_results(
      lm_fit_output
      , labels = list(
        term        = "Predictor"
        , estimate  = "$\\beta$"
        , conf.int  = "95\\% CI"
        , statistic = "$t$"
        , df        = "$\\mathit{df}$"
        , p.value   = "$p$"
      )
    )

    # Standardized regression coefficients
    trt <- rep(trt, 2)
    ctl <- rep(ctl, 2)
    lm_fit <- lm(scale(trt) ~ scale(ctl))
    lm_fit_output <- apa_print(lm_fit, standardized = TRUE)

    expect_identical(lm_fit_output$full$Intercept, "$b^* = .00$, 95\\% CI $[-.43, .43]$, $t(18) = 0.00$, $p > .999$")
    expect_identical(lm_fit_output$full$z_ctl, "$b^* = -.46$, 95\\% CI $[-.90, -.02]$, $t(18) = -2.18$, $p = .042$")
    expect_apa_results(
      lm_fit_output
      , labels = list(
        term        = "Predictor"
        , estimate  = "$b^*$"
        , conf.int  = "95\\% CI"
        , statistic = "$t$"
        , df        = "$\\mathit{df}$"
        , p.value   = "$p$"
      )
      , term_names = c("Intercept", "z_ctl", "modelfit")
      , table_terms = c("Intercept", "Ctl")
    )

    # No CI information
    expect_error(apa_print(lm_fit, conf.int = NULL), "The parameter 'conf.int' is NULL.")

    # deprecated argument 'ci'
    expect_warning(
      apa_print(lm_fit, ci = .95)
      , "Using argument 'ci' in calls to 'apa_print()' is deprecated. Please use 'conf.int' instead."
      , fixed = TRUE
    )

  }
)


context("apa_print.summary.lm()")

test_that(
  "Linear regression: summary(lm())"
  , {
    ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
    trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
    group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
    weight <- c(ctl, trt)

    lm_fit <- lm(weight ~ group)
    lm_fit_output <- apa_print(lm_fit)

    lm_summary <- summary(lm_fit)
    lm_summary_output <- apa_print(lm_summary)

    expect_identical(lm_summary_output, lm_fit_output)

    lm_fit_output <- apa_print(lm_fit, digits = 0)
    expect_identical(lm_fit_output$est$Intercept, "$b = 5$, 95\\% CI $[5, 5]$")
  }
)

context("apa_print.glm()")

test_that(
  "Linear regression: glm()-fit"
  , {
    counts <- c(18,17,15,20,10,20,25,13,12)
    outcome <- gl(3,1,9)
    glm_fit <- glm(counts ~ outcome, family = poisson())

    glm_fit_output <- apa_print(glm_fit)

    expect_apa_results(
      glm_fit_output
      , labels = list(
        term        = "Predictor"
        , estimate  = "$b$"
        , conf.int  = "95\\% CI"
        , statistic = "$z$"
        , p.value   = "$p$"
      )
    )

    # stat
    expect_identical(length(glm_fit_output$stat), 3L)
    expect_identical(names(glm_fit_output$stat), c("Intercept", "outcome2", "outcome3"))

    expect_identical(glm_fit_output$stat$Intercept, "$z = 24.17$, $p < .001$")
    expect_identical(glm_fit_output$stat$outcome2, "$z = -2.25$, $p = .025$")
    expect_identical(glm_fit_output$stat$outcome3, "$z = -1.52$, $p = .128$")

    # est
    expect_identical(names(glm_fit_output$est), c("Intercept", "outcome2", "outcome3", "modelfit"))
    expect_identical(length(glm_fit_output$est$modelfit), 2L)
    expect_identical(names(glm_fit_output$est$modelfit), c("aic", "bic"))

    expect_identical(glm_fit_output$est$Intercept, "$b = 3.04$, 95\\% CI $[2.79, 3.28]$")
    expect_identical(glm_fit_output$est$outcome2, "$b = -0.45$, 95\\% CI $[-0.86, -0.06]$")
    expect_identical(glm_fit_output$est$outcome3, "$b = -0.29$, 95\\% CI $[-0.68, 0.08]$")
    expect_identical(glm_fit_output$est$modelfit$aic, "$\\mathrm{AIC} = 52.76$")
    expect_identical(glm_fit_output$est$modelfit$bic, "$\\mathrm{BIC} = 53.35$")

    # full
    expect_identical(names(glm_fit_output$full), c("Intercept", "outcome2", "outcome3"))

    expect_identical(glm_fit_output$full$Intercept, "$b = 3.04$, 95\\% CI $[2.79, 3.28]$, $z = 24.17$, $p < .001$")
    expect_identical(glm_fit_output$full$outcome2, "$b = -0.45$, 95\\% CI $[-0.86, -0.06]$, $z = -2.25$, $p = .025$")
    expect_identical(glm_fit_output$full$outcome3, "$b = -0.29$, 95\\% CI $[-0.68, 0.08]$, $z = -1.52$, $p = .128$")

    # table
    expect_identical(nrow(glm_fit_output$table), 3L)

    # Manual CI
    glm_fit_output <- apa_print(glm_fit, conf.int = matrix(c(1, 2), ncol = 2, nrow = 3, byrow = TRUE, dimnames = list(names(glm_fit$coefficients), c("2.5 \\%", "97.5 \\%"))))
    expect_apa_results(
      glm_fit_output
      , labels = list(
        term        = "Predictor"
        , estimate  = "$b$"
        , conf.int  = "95\\% CI"
        , statistic = "$z$"
        , p.value   = "$p$"
      )
    )
    expect_identical(glm_fit_output$full$Intercept, "$b = 3.04$, 95\\% CI $[1.00, 2.00]$, $z = 24.17$, $p < .001$")
    expect_identical(glm_fit_output$full$outcome2, "$b = -0.45$, 95\\% CI $[1.00, 2.00]$, $z = -2.25$, $p = .025$")
    expect_identical(glm_fit_output$full$outcome3, "$b = -0.29$, 95\\% CI $[1.00, 2.00]$, $z = -1.52$, $p = .128$")

    # Set name of estimate
    glm_fit_output <- apa_print(glm_fit, est_name = "\\beta")
    expect_apa_results(
      glm_fit_output
      , labels = list(
        term        = "Predictor"
        , estimate  = "$\\beta$"
        , conf.int  = "95\\% CI"
        , statistic = "$z$"
        , p.value   = "$p$"
      )
    )
    expect_identical(glm_fit_output$est$Intercept, "$\\beta = 3.04$, 95\\% CI $[2.79, 3.28]$")
    expect_identical(glm_fit_output$est$outcome2, "$\\beta = -0.45$, 95\\% CI $[-0.86, -0.06]$")
    expect_identical(glm_fit_output$est$outcome3, "$\\beta = -0.29$, 95\\% CI $[-0.68, 0.08]$")
  }
)


context("apa_print.summary.glm()")

test_that(
  "Linear regression: summary(glm())"
  , {
    counts <- c(18,17,15,20,10,20,25,13,12)
    outcome <- gl(3,1,9)
    treatment <- gl(3,3)
    glm_fit <- glm(counts ~ outcome + treatment, family = poisson())

    glm_fit_output <- apa_print(glm_fit)

    glm_summary <- summary(glm_fit)
    glm_summary_output <- apa_print(glm_summary)

    expect_identical(glm_summary_output, glm_fit_output)

    glm_fit_output <- apa_print(glm_fit, digits = 0)
    expect_identical(glm_fit_output$est$Intercept, "$b = 3$, 95\\% CI $[3, 3]$")
  }
)

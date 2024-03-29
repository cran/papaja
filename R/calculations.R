# Modified percentile bootstrap
# Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confidence Intervals for an Effect Size Measure in Multiple Linear Regression.
# Educational and Psychological Measurement, 67(2), 207-218. https://doi.org/10.1177/0013164406292030

# x: apa_model_comp object
# models: list of lm-objects

#' @keywords internal

delta_r2_ci <- function(x, models, conf.int = 0.90, R = 100, progress_bar = FALSE, ...) {
  if(!package_available("boot")) stop("Please install the package 'boot' to calculate bootstrap confidence intervals.")

  validate(x, check_class = "data.frame")
  validate(length(models), "length(models)", check_range = c(2, Inf))
  validate(conf.int, check_range = c(0, 1))

  model_summaries <- lapply(models, summary)
  r2s <- sapply(model_summaries, function(x) x$r.squared)
  delta_r2s <- diff(r2s)

  if(progress_bar) {
    boot_env <- environment()

    cat("Calculating confidence intervals for differences in R-squared based on", R, "bootstrap samples.\n")
    pb <- utils::txtProgressBar(min = 0, max = R * length(delta_r2s), style = 3, width = min(getOption("width") - 10L, 100L))
    boot_env$count <- -length(delta_r2s) # seems to be evaluated once more than R
  }

  percent_cis <- lapply(seq_along(delta_r2s), function(y) {

    delta_r2_samples <- boot::boot(
      get(as.character(model_summaries[[y]]$call$data))
      , function(data, i, calls) {
        bdata <- data[i, ]

        calls[[1]]$data <- bdata
        mod1 <- eval(calls[[1]])

        calls[[2]]$data <- bdata
        mod2 <- eval(calls[[2]])

        if(progress_bar) {
          boot_env$count <- boot_env$count + 1L
          black_hole <- utils::setTxtProgressBar(pb, value = boot_env$count)
          # print(count)
          # print(i)
        }
        summary(mod2)$r.squared - summary(mod1)$r.squared
      }
      , calls = list(model_summaries[[y]]$call, model_summaries[[y + 1]]$call)
      , R = R
      , ...
    )


    boot_r2_ci <- boot::boot.ci(delta_r2_samples, conf = conf.int, type = "perc")

    # If difference is not significant set lower bound (closest to zero) == 0 (p. 210, Algina, Keselman & Penfield, 2007)
    if(x[y, "p.value"] >= (1 - conf.int) / 2) {
      lower_bound <- which(boot_r2_ci$percent[1, 4:5] == min(abs(boot_r2_ci$percent[1, 4:5])))
      boot_r2_ci$percent[1, 3 + lower_bound] <- 0
    }

    boot_r2_ci
  })
  if(progress_bar) close(pb) # generic is from base R

  percent_cis <- t(cbind(sapply(percent_cis, function(x) x$percent))) # Reformat to one CI per line
  percent_cis <- percent_cis[, 4:5, drop = FALSE] # Preserv matrix structure
  ci_levels <- c((1 - conf.int) / 2, (1 - conf.int) / 2 + conf.int) * 100
  colnames(percent_cis) <- paste(ci_levels, "%")
  attr(percent_cis, "conf.level") <- conf.int

  percent_cis
}



#' Within-Subjects Confidence Intervals
#'
#' Calculate Cousineau-Morey within-subjects confidence intervals.
#'
#' @param data A \code{data.frame} that contains the data.
#' @param id Character. Variable name that identifies subjects.
#' @param factors Character. A vector of variable names that is used to stratify the data.
#' @param dv Character. The name of the dependent variable.
#' @param level Numeric. Defines the width of the interval. Defaults to 0.95
#'    for 95% confidence intervals.
#' @param method Character. The method that is used to calculate CIs. Currently,
#'          "Morey" and "Cousineau" are supported. Defaults to "Morey".
#' @references
#'    Morey, R. D. (2008). Confidence Intervals from Normalized Data: A correction to Cousineau (2005).
#'    *Tutorials in Quantitative Methods for Psychology*, *4*(2), 61--64.
#'
#'    Cousineau, D. (2005). Confidence intervals in within-subjects designs:
#'    A simpler solution to Loftus and Masson's method.
#'    *Tutorials in Quantitative Methods for Psychology*, *1*(1), 42--45.
#'
#' @return
#'   A `data.frame` with additional class `papaja_wsci`.
#'   The `summary()` method for this class returns a `data.frame` with
#'   means along lower and upper limit for each cell of the design.
#'
#'
#'
#'
#' @examples
#' wsci(
#'    data = npk
#'    , id = "block"
#'    , dv = "yield"
#'    , factors = c("N", "P")
#' )
#' @rdname wsci
#' @export

wsci <- function(data, id, factors, dv, level = .95, method = "Morey") {
  # comment out again!
  # data <- fast_aggregate(data = data, factors = c(id, factors), dv = dv, fun = mean)

  # `split()` (below) needs standard factors, because it does not apply `as.factor`
  # by default
  for(i in c(id, factors)){
    data[[i]] <- as.factor(data[[i]])
  }

  # for (i in 1:length(factors)) {
  #   if (all(rowSums(table(data[[id]], data[[factors[i]]])>0)==1)) {
  #     between <- c(between, factors[i])
  #   } else {
  #     within <- c(within, factors[i])
  #   }
  # }

  tmp <- determine_within_between(data = data, id = id, factors = factors)

  between <- tmp$between
  within <- tmp$within

#   print(sapply(X = factors, FUN = function(x){
#     ifelse(all(rowSums(table(data[[id]], data[[x]]))==1), "between", "within")
#   }))

  test <- tapply(data[[dv]], as.list(data[, c(id, within), drop = FALSE]), FUN = function(x){sum(!is.na(x))})

  if(any(test > 1, na.rm = TRUE)){
    stop("More than one observation per cell. Ensure you aggregated multiple observations per participant/within-subjects condition combination.")
  }

  # ----------------------------------------------------------------------------
  # Handling of missing values

  data <- complete_observations(data = data, id = id, dv = dv, within = within)

  # print warnings ----
  if("removed_cases_explicit_NA" %in% names(attributes(data))) {
    warning(
      "Because of NAs in the dependent variable, the following cases were removed from calculation of within-subjects confidence intervals:\n"
      , id
      , ": "
      , paste(attr(data, "removed_cases_explicit_NA"), collapse = ", ")
    )
  }
  if("removed_cases_implicit_NA" %in% names(attributes(data))) {
    warning(
      "Because of incomplete data, the following cases were removed from calculation of within-subjects confidence intervals:\n"
      , id
      , ": "
      , paste(attr(data, "removed_cases_implicit_NA"), collapse = ", ")
    )
  }


  # split by between-subjects factors ----
  if( length(between) > 0L) {
    splitted <- split(data, f = data[, between, drop = FALSE], sep = ":")
  } else {
    splitted <- list(data)
  }


  if(!is.null(within)) {

    if(method != "Cousineau") {
      if(method != "Morey") {
        warning("Method ", encodeString(method, quote = "'"), " not supported. Defaulting to 'Morey'.")
        method <- "Morey"
      }
    }

    Morey_CI <- lapply(X = splitted, FUN = function(x) {
      y <- tapply(x[[dv]], as.list(x[, c(id, within), drop = FALSE]), FUN = as.numeric) # transform to matrix
      z <- y - rowMeans(y, na.rm = TRUE) + mean(y, na.rm = TRUE) # normalise
      CI <- apply(z, MARGIN = seq_along(within) + 1L, FUN = conf_int, level) # calculate CIs for each condition

      # Morey correction
      if(method == "Morey") {
        M <- prod(apply(X = as.matrix(x[, within, drop = FALSE]), MARGIN = 2, FUN = function(x) { nlevels(as.factor(x)) }))
        Morey_CI <- CI * sqrt(M/(M-1))
      } else {
        Morey_CI <- CI
      }

      # reshape to data.frame
      Morey_CI <- as.data.frame(as.table(Morey_CI))
      if(length(within) == 1) {
        colnames(Morey_CI)[colnames(Morey_CI)=="Var1"] <- within
      }
      colnames(Morey_CI)[colnames(Morey_CI)=="Freq"] <- dv
      # return
      Morey_CI
    })

    if(is.null(between)) {
      ee <- data.frame(unlist(Morey_CI, recursive=FALSE))
    } else {
      names <- strsplit(names(Morey_CI), split = ":")
      for (i in 1:length(Morey_CI)) {
        for (j in 1:length(between)) {
          Morey_CI[[i]][[between[j]]] <- names[[i]][j]
        }
      }
    }

    if(package_available("dplyr")) {
      ee <- fast_aggregate(data = dplyr::bind_rows(Morey_CI), factors = factors, dv = dv, fun = mean)
    } else {
      tmpdat <- do.call("rbind", Morey_CI)

      ee <- stats::aggregate(
        x = tmpdat[, dv, drop = FALSE]
        , by = tmpdat[, factors, drop = FALSE]
        , FUN = mean
      )
    }

  } else {
    stop("No within-subjects factors specified.")
  }
  values <- ee
  means <- stats::aggregate(x = data[[dv]], by = data[, factors, drop = FALSE], FUN = mean)
  colnames(means)[ncol(means)] <- dv

  attr(values, "Between-subjects factors") <- if(is.null(between)){"none"} else {between}
  attr(values, "Within-subjects factors") <- within
  attr(values, "Dependent variable") <- dv
  attr(values, "Subject identifier") <- id
  attr(values, "Confidence level") <- level
  attr(values, "Method") <- method
  attr(values, "means") <- means
  class(values) <- c("papaja_wsci", "data.frame")
  return(values)
}



#' @rdname wsci
#' @export

within_subjects_conf_int <- wsci


#' Summarize Within-Subjects Confidence Intervals
#'
#' Calculate upper and lower limits of within-subjects confidence intervals calculated
#' with [wsci()] and return them along their respective means.
#'
#' @param object An object of class `papaja_wsci`, generated with function [wsci()].
#' @param ... Further arguments that may be passed, currently ignored.
#' @return A `data.frame` containing means as well as lower and upper confidence
#'   bounds for each cell of the design.
#' @export

summary.papaja_wsci <- function(object, ...) {

  means <- attr(object, "means")
  colnames(means)[ncol(means)] <- "mean"
  colnames(object)[ncol(object)] <- "ci_diff"

  y <- merge(x = object, y = means, sort = FALSE)
  y$lower_limit <- y$mean - y$ci_diff
  y$upper_limit <- y$mean + y$ci_diff
  y$ci_diff <- NULL
  variable_labels(y) <- Filter(Negate(is.null), variable_labels(means))
  y
}

#' Between-Subjects Confidence Intervals
#'
#' Calculates the deviation that is needed to construct confidence intervals for a vector of observations.
#'
#' @param x Numeric. A vector of observations from your dependent variable.
#' @param level Numeric. Defines the width of the interval if confidence intervals are plotted. Defaults to 0.95
#'    for 95% confidence intervals.
#' @param na.rm Logical. Specifies if missing values should be removed.
#' @return Returns a single numeric value, the deviation of the symmetric
#'   confidence bounds from the mean based on the t distribution.
#' @export

conf_int <- function(x, level = 0.95, na.rm = TRUE) {
  validate(x, check_class = "numeric", check_NA = FALSE)
  validate(level, check_class = "numeric", check_length = 1, check_range = c(0, 1))

  a <- (1 - level)/2
  n <- sum(!is.na(x))
  fac <- -suppressWarnings(stats::qt(a, df = n-1))

  if(n < 2){
    message("Less than two non-missing values in at least one cell of your design: Thus, no confidence interval can be computed.")
  }

  ee <- (stats::sd(x, na.rm = na.rm) * fac) / sqrt(n)
  return(ee)
}

#' @rdname conf_int
#' @export

conf.int <- conf_int

#' @rdname conf_int
#' @export

ci <- conf_int


#' Standard Error of the Mean
#'
#' Calculates the standard error of the mean.
#'
#' @param x Numeric. A vector of observations.
#' @param na.rm Logical. Specifies if missing values should be removed.
#' @return The standard error of the mean as numeric vector of length 1.
#' @export

se <- function(x, na.rm = TRUE) {
  n <- sum(!is.na(x))
  stats::sd(x, na.rm = na.rm) / sqrt(n)
}


#' Highest-Density Intervals
#'
#' Calculates the highest-density interval of a vector of values.
#'
#' @param x Numeric. A vector of observations.
#' @param level Numeric. Defines the width of the interval. Defaults to 95% highest-density intervals.

hd_int <- function(x, level = 0.95) {
  validate(x, check_class = "numeric")
  validate(level, check_class = "numeric", check_length = 1, check_range = c(0, 1))

  sorted_estimate_posterior <- sort(x)
  n_samples <- length(sorted_estimate_posterior)
  gap <- max(1, min(n_samples - 1, round(n_samples * level)))
  init <- 1:(n_samples - gap)
  lower_index <- which.min(sorted_estimate_posterior[init + gap] - sorted_estimate_posterior[init])
  hdinterval <- cbind(sorted_estimate_posterior[lower_index], sorted_estimate_posterior[lower_index + gap])
  colnames(hdinterval) <- c(paste((1 - level) / 2 * 100, "%"), paste(((1 - level) / 2 + level) * 100, "%"))
  attr(hdinterval, "conf.level") <- level
  hdinterval
}


#' Effect Sizes for Analysis of Variance
#'
#' Calculates effect-size measures for Analysis of Variance output objects.
#' *This function is not exported and will soon be deprecated.*
#'
#' @param x An object of class \code{apa_variance_table}.
#' @param es Character. A vector naming all to-be-computed effect-size measures.
#'   Currently, partial eta-squared (\code{"pes"}), generalized eta-squared
#'   (\code{"ges"}), and eta-squared (\code{"es"}) are supported.
#' @param observed Character. A vector naming all factors that are observed
#'   (i.e., \emph{not} manipulated).
#' @param mse Logical. Should means-squared errors be computed?
#' @param intercept Logical. Should the sum of squares of the intercept (i.e., the
#'   deviation of the grand mean from 0) be included in the calculation of eta-squared?
#'
#' @keywords internal

add_effect_sizes <- function(x, es = "ges", observed = NULL, mse = TRUE, intercept = FALSE) {
  # ----------------------------------------------------------------------------
  # We don't validate here because this function is intended to be used
  # internally, validation should have happened earlier in the processing chain.

  # validate(x, check_class = "apa_variance_table", check_NA = FALSE)
  # validate(es, check_class = "character", check_NA = FALSE)

  if(!is.null(es)) {
    # Stop if the user requires a non-supported effect-size measure ----
    if(!all(es %in% c("pes", "ges", "es"))) {
      stop("Requested effect size measure(s) currently not supported: ", paste(es, collapse = ", "), ".")
    }

    # --------------------------------------------------------------------------
    # Calculate generalized eta-squared
    #
    # This code is a copy from the afex package by Henrik Singmann et al.
    # In the package's source code, it is stated that the code is basically a copy
    # from ezANOVA by Mike Lawrence
    if("ges" %in% es) {
      if(!is.null(observed)) {
        obs <- rep(FALSE, nrow(x))
        for(i in observed) {
          if (!any(grepl(paste0("\\<", i, "\\>", collapse = "|"), x$term))) {
            stop(paste0("Observed variable not in data: ", i, collapse = " "))
          }
          obs <- obs | grepl(paste0("\\<", i, "\\>", collapse = "|"), x$term)
        }
        obs_SSn1 <- sum(x$sumsq*obs, na.rm = TRUE)
        obs_SSn2 <- x$sumsq*obs
      } else {
        obs_SSn1 <- 0
        obs_SSn2 <- 0
      }
      x$estimate <- x$sumsq / (x$sumsq + sum(unique(x$sumsq_err)) + obs_SSn1 - obs_SSn2)
      tinylabels::variable_label(x$estimate) <- "$\\hat{\\eta}^2_G$"
    }

    # --------------------------------------------------------------------------
    # Calculate eta-squared
    #
    # In it's current implementation, correct calculation of eta-squared relies
    # on the fact that the design is balanced (otherwise, the summation below)
    # is simply false. Replacing this term by the sum of squared deviations of
    # individual observations from the grand mean (the general specification)
    # would be highly desirable. However, most ANOVA outputs do not provide
    # the necessary information, so we have to go with this hack.
    if("es" %in% es) {
      index <- rep(TRUE, nrow(x))
      if(!intercept){
        index <- x$term!="(Intercept)"
      }
      x$estimate <- x$sumsq / sum(x$sumsq[index], unique(x$sumsq_err))
      tinylabels::variable_label(x$estimate) <- "$\\hat{\\eta}^2$"
      message("Note that eta-squared is calculated correctly if and only if the design is balanced.")
    }

    # --------------------------------------------------------------------------
    # Calculate partial eta-squared
    #
    # This one should be unproblematic and work in all cases.
    if("pes" %in% es) {
      x$estimate <- x$sumsq / (x$sumsq + x$sumsq_err)
      tinylabels::variable_label(x$estimate) <- "$\\hat{\\eta}^2_p$"
    }
  }

  # ----------------------------------------------------------------------------
  # Only calculate MSE if required (otherwise, Levene tests give an error).
  if(mse) {
    df_col <- intersect("df.residual", colnames(x))
    if(!is.null(x$sumsq_err) & !is.null(x[[df_col]])) {
      x$mse <- x$sumsq_err / x[[df_col]]
      tinylabels::variable_label(x$mse) <- "$\\mathit{MSE}$"
    } else {
      warning("Mean-squared errors requested, but necessary information not available.")
    }
  }

  x
}


# Description -----

#' Detection Range Probability Model
#'
#' Uses a third-order polynomial, logit, or, probit model to
#' determine detection range based on a expected percentage of detections
#' for acoustic telemetry receivers. This function is to be used after a
#' preliminary range test which uses multiple distances away from
#' representative receiver. Preliminary detection efficiency is to be
#' determined in Vemco's Range Testing software and exported as a csv.
#
#' @param formula an object of class \code{formula} or one that can be coerced
#' to that class): a symbolic description of the model to be fitted.
#' The details of model specification are given under Details.
#'
#' @param data an optional data frame, list or environment (or object coercible
#' by as.data.frame to a data frame) containing the variables in the model.
#' If not found in data, the variables are taken from environment(formula),
#' typically the environment from which \code{detection_range_model()} is called.
#'
#' @param percentage a given percentage of expected detections to be heard
#' by representative receiver. For example a value of 50 will calculate the
#' distance at which 50% of a range tag is expected to be heard by a
#' representative receiver. If more than one percentage value is wanted,
#' specify by creating a vector. Percentages can be calculated down to the
#' 1e-16 of a percentage (e.g. 99.9). However, the closer you approach 0 or 100
#' the less accurate the model becomes. The resulting dataframe may round the
#' display, if it does just call the specific column desired.
#'
#' @param link default is \code{"polynomial"} which  will  use and return a
#' third order polynomial model. However, if a logit or probit model is
#' desired the argument can be supplied with  \code{"logit"} or
#'  \code{"probit"} and will return a logit or probit model. See Details about
#'  y variable format depending on which link is chosen.
#'
#' @param subset allows for the data to be subsetted if desired. Default set
#' to `NULL` and does not need to be supplied.
#'
#' @param summary_stats default is set to \code{TRUE} resulting in all summary
#' statistics of the model to be returned. If set to \code{FALSE}, summary
#' statistics of the model will not be displayed. Summary statistics should
#' always be evaluated, however the argument exists solely to shorten the
#' dataframe in the need to quickly look at predicted distances. Use with caution.
#'
#' @param model_frame  argument call is only required when link is set to
#' \code{"polynomial"} default is \code{"data_frame"}. See details about
#' polynomial \code{formula} call and when it is appropriate to use
#' \code{"data_frame"} or \code{"matrix"}.
#' @details If a third order polynomial model is selected, the formula call
#' can be in two different formats. The preferred and default format is
#' \code{y ~ -1 + x + I(x ^ 2) + I(x ^ 3) + offset(y-intercept)}. \code{model_frame}
#' needs to be set \code{"data_frame"}to properly extract
#' parameters and determine distances away from a receiver given a percentage
#' of interest. If using the \code{base::poly()} within the formula as such
#' \code{y ~ -1 + poly(x, 3, raw = TRUE) + offset(y-intercept)}, then
#' \code{model_frame} argument needs to be set to \code{"matrix"}.
#' Both formula formats have \code{offset()} which sets the  y-intercept.
#' The y-intercept needs to be set to 100, as x = 0 m away from a receiver
#' you expect to hear a tag 100\% of the time.
#'
#' A third order polynomial will handle preliminary detection efficiency
#' percentages (y variable) as whole numbers as the model is not bound by
#' 0 and 1. While both logit and probit models have to use percentages as
#' decimals as the models are bound by 0 and 1.
#'
#' Additionally, its been noticed that with fewer data points a third order
#' polynomial often fits the data better however this does not mean that neither
#' a logit or probit model should not be assessed as well.
#'
#'
#' @return A tibble object that consists of the percentage of interest, the predicted
#' distance from the model, and model summary statistics:
#'
#' If a third order polynomial is selected the following summary statistics are displayed:
#' degrees of freedom, chi-square test, person's goodness of fit test,
#' slopes, slopes' standard error, slopes' p-value, residual standard error,
#' r squared and adjusted r squared values, and aic value.
#'
#' If a logit or probit model is selected the following summary statistics are displayed:
#' degrees of freedom, chi-square (deviance), person's goodness of fit test,
#' slope, slope's standard error, slope's p-value, z-value, null deviance, and aic value.

#' @references This function was developed for an ongoing study which followed
#' detection range efficiency methods similar to:
#'
#' Brownscombe, J.W., L.P. Griffin, J.M. Chapman, D. Morley, A.
#' Acosta, G.T. Crossin, S.J. Iverson, A.J. Adams, S.J. Cooke, and A.J.
#' Danylchuk. 2020. A practical method to account for variation in detection
#' range in acoustic telemetry arrays to accurately quantify the spatial ecology
#' of aquatic animals. Methods in Ecology and Evolution 11(1):82–94.
#'
#' @author Benjamin L. Hlina
#' @examples
#' sample_detection_efficiency
#'
#' # third order polynomial: # ave_percent is a whole number
#'
#' m <- detection_range_model(
#'   avg_percent ~ -1 + distance_m + I(distance_m^2) +
#'     I(distance_m^3) + offset(intercept),
#'   data = sample_detection_efficiency,
#'   percentage = c(10, 50, 90),
#'   link = "polynomial",
#'   model_frame = "data_frame"
#' )
#'
#' # logit model: aver percent is in decimal form
#'
#' m1 <- detection_range_model(avg_percent_d ~ distance_m,
#'   data = sample_detection_efficiency,
#'   percentage = c(10, 50, 90),
#'   link = "logit",
#'   summary_stats = TRUE
#' )
#'
#' # probit model: aver percent is in decimal form
#'
#' m2 <- detection_range_model(avg_percent_d ~ distance_m,
#'   data = sample_detection_efficiency,
#'   percentage = c(10, 50, 90),
#'   link = "probit",
#'   summary_stats = TRUE
#' )
#'
#' m
#' m1
#' m2
#'
#' # for further instruction see vignettes
#'
#' @export

# function ------
detection_range_model <- function(formula,
                                  data,
                                  percentage = NULL,
                                  link = NULL,
                                  subset = NULL,
                                  summary_stats = TRUE,
                                  model_frame = NULL) {
  # set null for link to logit however probit and polynomail can be used if supplied -----
  if (is.null(link)) {
    link <- c("polynomial")
  }
  if (link == "logit") {
    link <- c("logit")
  }
  if (link == "probit") {
    link <- c("probit")
  }

  # create model for logit ----
  if (link == "logit") {
    model <- do.call("glm", list(
      formula = formula,
      family = stats::binomial(link = "logit"),
      data = data,
      subset = substitute(subset)
    ))
  }

  # create model for probit -----
  if (link == "probit") {
    model <- do.call("glm", list(
      formula = formula,
      family = stats::binomial(link = "probit"),
      data = data,
      subset = substitute(subset)
    ))
  }

  # create model for third order polynomial ------
  if (link == "polynomial") {
    model <- do.call("lm", list(
      formula = formula,
      data = data,
      subset = substitute(subset)
    ))
  }

  # make percentage a null object and create warning message if percentage isn't supplied -----
  if (is.null(percentage)) {
    percentage <- seq(1, 99, 1)
    warning("`percentage`argument has to be supplied otherwise detection percentage values for 1-99 will be displayed", call. = FALSE)
  }


  if (link %in% "logit" | link %in% "probit") {
    # # test goodness of fit for model -----
    chi_square <- sum(stats::residuals(model, link = "pearson")^2)

    df <- stats::df.residual(model)

    pgof <- stats::pchisq(chi_square, df, lower.tail = FALSE)

    # create summary which you are going to extract coefficientsts from -----
    summary <- summary(model)

    # Intercept (b0)

    b0 <- summary$coefficients[1]

    # Slope (b1)

    b1 <- summary$coefficients[2]

    # intercept info

    intercept_se <- summary$coefficients[3]
    intercept_sig <- summary$coefficients[7]

    # slope info

    slope_se <- summary$coefficients[4]
    slope_sig <- summary$coefficients[8]

    # z value

    z_value <- summary$coefficients[6]

    # deviance
    deviance <- summary$deviance

    # null deviance
    null_deviance <- summary$null.deviance

    # AIC
    aic <- summary$aic
  } else {
    # test goodness of fit for model -----
    chi_square <- sum(stats::residuals(model, link = "pearson")^2)

    df <- stats::df.residual(model)

    pgof <- stats::pchisq(chi_square, df, lower.tail = FALSE)

    # create summary which you are going to extract coefficients from -----
    summary <- summary(model)
    # poly looks like such ax^3 + bx^2 + cx + d = 0  therefore the following ----
    # However because of naming conventions in R we will use the following letters
    # ax^3 + bx^2 + dx + f = 0

    # Intercept (b0) or f set to 100 usually as at 0 m away from rec you would hear a tag 100 %

    y1 <- unique(model$offset)


    # a  -----
    a <- summary$coefficients[1]

    # standard error of a
    a_se <- summary$coefficients[4]

    # p value of a
    a_sig <- summary$coefficients[10]

    # now b -----

    b <- summary$coefficients[2]
    # standard error of b
    b_se <- summary$coefficients[5]

    # p value of a
    b_sig <- summary$coefficients[11]

    # this is c ------
    # but because of r using c, but it's bad practice to name obj c so not sure atm
    d <- summary$coefficients[3]

    # standard error of b
    d_se <- summary$coefficients[6]

    # p value of a
    d_sig <- summary$coefficients[12]

    # Residual standard error

    resid_se <- summary$sigma
    # r^2
    r2 <- summary$r.squared

    # adjusted r^2
    adj_r2 <- summary$adj.r.squared

    # AIC
    aic <- stats::AIC(model)
  }

  # logit ------
  if (link == "logit") {
    # Calculate m for all percentage levels based on  logits -----
    # in est
    est <- log((percentage / 100) / (1 - (percentage / 100)))
    m <- (est - b0) / b1
  }

  if (link == "probit") {
    # Calculate m for all percentage levels based on probits -----
    # in est


    est <- stats::qnorm(percentage / 100)
    m <- (est - b0) / b1
  }


  # third order polynomial ------
  if (link == "polynomial") {
    # it spit out x from 3rd order poly ------
    # x variables to be able to use approx correctly ----
    # tried to not hard code this as much as its flexible with forumual
    if (is.null(model_frame)) {
      model_frame <- c("data_frame")
    }
    if (model_frame == "matrix") {
      model_frame <- c("matrix")
    }

    form <- formula(model)

    # create vector of distance value used in approx function
    if (model_frame == "data_frame") {
      # if(any(form == "y ~ -1 + poly(x, 3, raw = TRUE) + offset(y-intercept"))
      # stopifnot(form == "y ~ -1 + x + I(x ^ 2) + I(x ^ 3) + offset(y-intercept)")
      warning("Check if your formula is correct for the model_frame argument", call. = FALSE)
      dist <- stats::model.frame(model)[[2]]
    }

    if (model_frame == "matrix") {
      # model_frame <- c("matrix")
      warning("Check if your formula is correct for the model_frame argument", call. = FALSE)
      matr <- stats::model.frame(model)[[2]]
      dist <- matr[, 1]
    }


    fit <- model$fitted.values

    # determine x which is distance for polynomial using
    m <- round(approx(x = fit, y = dist, xout = percentage)$y, 0)
  }


  if (link %in% "logit" | link %in% "probit") {
    if (summary_stats == FALSE) {
      table <- dplyr::tibble(
        p = percentage,
        distance = m
      )
    }

    if (summary_stats == TRUE) {
      table <- dplyr::tibble(
        p = percentage,
        distance = m,
        df = df,
        chi_square = chi_square,
        pgof = pgof,
        slope = b1,
        slope_se = slope_se,
        slope_sig = slope_sig,
        intercept = b0,
        intercept_se = intercept_se,
        intercept_sig = intercept_sig,
        z_value = z_value,
        null_deviance = null_deviance,
        aic = aic
      )
    }
  } else {
    if (summary_stats == FALSE) {
      table <- dplyr::tibble(
        p = percentage,
        distance = m
      )
    }

    if (summary_stats == TRUE) {
      table <- dplyr::tibble(
        p = percentage,
        distance = m,
        df = df,
        chi_square = chi_square,
        pgof = pgof,
        a = a,
        a_se = a_se,
        a_sig = a_sig,
        b = b,
        b_se = b_se,
        b_sig = b_sig,
        d = d,
        d_se = d_se,
        d_sig = d_sig,
        offset = y1,
        resid_se,
        r2 = r2,
        adj_r2 = adj_r2,
        aic = aic
      )
    }
  }



  return(table)
}

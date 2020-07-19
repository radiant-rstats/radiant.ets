#' ARIMA model
#'
#' @details See \url{https://radiant-rstats.github.io/docs/ets/arima.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable in the model
#' @param order Order ...
#' @param seasonal Seasonal order ...
#' @param evar Additional explanatory variables in the model
#' @param mean Mean ...
#' @param drift Drift ...
#' @param model Model ...
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#' @param ... other parameters to pass to forecast::Arima
#'
#' @return A list of all inputs and results as an object of class Arima
#'
#' @examples
#' Arima(diamonds, "price", order = c(1, 0, 1))
#' @seealso \code{\link{summary.Arima}} to summarize results
#' @seealso \code{\link{plot.Arima}} to plot results
#' @seealso \code{\link{forecast.Arima}} to generate predictions
#'
#' @importFrom forecast Arima checkresiduals
#'
#' @export
Arima <- function(
  dataset, rvar, order = c(1, 0, 0), seasonal = NULL,
  evar = NULL, mean = TRUE, drift = FALSE, model = NULL,
  data_filter = "", envir = parent.frame(), ...
) {

    if (length(model) > 0) {
      if (missing(rvar)) rvar <- attr(model, "rvar")
      if (missing(evar)) evar <- attr(model, "evar")
      # if (missing(data_filter)) data_filter <- attr(model, "data_filter")
    }

    df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
    dataset <- get_data(dataset, c(rvar, evar), filt = data_filter, envir = envir)

    if (any(summarise_all(dataset, does_vary) == FALSE)) {
        return("One or more selected variables show no variation. Please select other variables." %>%
            add_class("regress"))
    }

    y <- dataset[[rvar]]
    if (!inherits(y, "ts")) {
      freq <- ifelse(length(model) > 0, attr(model$x, "tsp")[3], 4)
      y <- ts(y, frequency = freq)
    }

    if (length(model) > 0) {
      mod_list <- list(y = y, model = model)
    } else {
      mod_list <- list(y = y, order = order, include.mean = mean, include.drift = drift)
    }

    if (length(evar) > 0) {
        mod_list[["xreg"]] <- paste0(rvar, " ~ -1 + ", paste0(evar, collapse = " + ")) %>%
            as.formula() %>%
            model.matrix(dataset)
    }

    if (length(seasonal) > 0) {
        mod_list[["seasonal"]] <- seasonal
        if (length(seasonal) == 2) {
          mod_list[["y"]] <- ts(y, frequency = seasonal[[2]])
        }
    }

    if (length(model) > 0) {
      mod_list <- mod_list["y"]
      mod_list[["model"]] <- model
    }

    object <- try(do.call(forecast::Arima, c(mod_list, ...)), silent = TRUE)
    if (inherits(object, "try-error")) {
      print("** Unable to estimate model parameters **")
      print("** Please try simplifying the model specification and try again **")
    }

    object$series <- paste0(df_name, "$", rvar)

    attr(object, "rvar") <- rvar
    attr(object, "evar") <- evar
    dataset[[rvar]] <- y
    attr(object, "dataset") <- dataset
    attr(object, "data_filter") <- data_filter
    object
}

#' Store residuals from a model
#'
#' @details The store method for objects of class "model". Adds model residuals to the dataset while handling missing values and filters. See \url{https://radiant-rstats.github.io/docs/model/Arima.html} for an example in Radiant
#'
#' @param dataset Dataset to append residuals to
#' @param object Return value from a model function
#' @param name Variable name(s) assigned to model residuals
#' @param envir Environment to evaluate filter expressions in
#' @param ... Additional arguments
#'
#' @examples
#' Arima(diamonds, "price", order = c(1, 0, 1)) %>%
#'  store(diamonds, ., name = "resid") %>%
#'  head()
#'
#' @export
store.Arima <- function(dataset, object, name = "residuals", envir = parent.frame(), ...) {
  indr <- indexr(dataset, vars = c(attr(object, "rvar"), attr(object, "evar")), filt = attr(object, "data_filter"))
  res <- rep(NA, indr$nr)
  res[indr$ind] <- object$residuals
  dataset[[name]] <- res
  dataset
}

##' Store predicted values from an Arima model
#'
#' @details See \url{https://radiant-rstats.github.io/docs/ets/arima.html} for an example in Radiant
#'
#' @param dataset Dataset to add predictions to
#' @param object Return value from model predict function
#' @param name Variable name(s) assigned to predicted values
#' @param ... Additional arguments
#'
#' @examples
#' Arima(diamonds, rvar = "price") %>%
#'   Arima(diamonds, model = .) %>%
#'   store(diamonds, ., name = "pred_pref")
#'
#' @export
store.arima.predict <- function(dataset, object, name = "prediction", ...) {
  radiant.model:::store.model.predict(dataset, object, name = name, ...)
}

#' Predict method for the Arima function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/Arima.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{Arima}}
#' @param pred_data Provide the dataframe to generate predictions (e.g., diamonds). The dataset must contain all columns used in the estimation
#' @param pred_cmd Command used to generate data for prediction
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param interval Type of interval calculation ("confidence" or "prediction"). Set to "none" if se is FALSE
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom radiant.model predict_model
#'
#' @seealso \code{\link{Arima}} to generate the result
#' @seealso \code{\link{summary.Arima}} to summarize results
#' @seealso \code{\link{plot.Arima}} to plot results
#'
#' @export
predict.Arima <- function(
  object, pred_data = NULL, pred_cmd = "", conf_lev = 0.95,
  se = TRUE, interval = "confidence", dec = 3, ...
) {

  pred <- forecast::forecast(object)

  if (is.character(object)) return(object)
  if (isTRUE(se)) {
    if (isTRUE(interval == "none")) {
      se <- FALSE
    } else if ("center" %in% object$check || "standardize" %in% object$check) {
      message("Standard error calculations not supported when coefficients are centered or standardized")
      se <- FALSE; interval <- "none"
    }
  } else {
    interval <- "none"
  }

  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <-
      try(
        sshhr(
          predict(
            model, pred,
            interval = ifelse(se, interval, "none"),
            level = conf_lev
          )
        ),
        silent = TRUE
      )

    if (!inherits(pred_val, "try-error")) {
      if (se) {
        pred_val %<>% data.frame(stringsAsFactors = FALSE) %>% mutate(diff = .[, 3] - .[, 1])
        ci_perc <- ci_label(cl = conf_lev)
        colnames(pred_val) <- c("Prediction", ci_perc[1], ci_perc[2], "+/-")
      } else {
        pred_val %<>% data.frame(stringsAsFactors = FALSE) %>% select(1)
        colnames(pred_val) <- "Prediction"
      }
    }

    pred_val
  }

  predict_model(object, pfun, "regress.predict", pred_data, pred_cmd, conf_lev, se, dec) %>%
    set_attr("radiant_interval", interval) %>%
    set_attr("radiant_pred_data", df_name)
}


#' Plot method for model.predict functions
#'
#' @param x Return value from predict functions (e.g., predict.Arima)
#' @param xvar Variable to display along the X-axis of the plot
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different color
#' @param conf_lev Confidence level to use for prediction intervals (.95 is the default)
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom ggplot2 aes_string
#'
#' @seealso \code{\link{predict.Arima}} to generate predictions
#'
#' @export
plot.arima.predict <- function(
  x, xvar = "", facet_row = ".",
  facet_col = ".", color = "none",
  conf_lev = .95, ...
) {

  if (is.character(x)) return(x)
  if (is_empty(xvar)) return(invisible())
  if (facet_col != "." && facet_row == facet_col) {
    return("The same variable cannot be used for both Facet row and Facet column")
  }

  cn <- colnames(x)
  pvars <- "Prediction"
  cnpred <- which(cn == pvars)
  if (length(cn) > cnpred) {
    pvars <- c(pvars, "ymin", "ymax")
    cn[cnpred + 1] <- pvars[2]
    cn[cnpred + 2] <- pvars[3]
    colnames(x) <- cn
  }

  byvar <- NULL
  if (color != "none") byvar <- color
  if (facet_row != ".") {
    byvar <- if (is.null(byvar)) facet_row else unique(c(byvar, facet_row))
  }

  if (facet_col != ".") {
    byvar <- if (is.null(byvar)) facet_col else unique(c(byvar, facet_col))
  }

  tbv <- if (is.null(byvar)) xvar else c(xvar, byvar)

  if (any(!tbv %in% colnames(x))) {
    return("Some specified plotting variables are not in the model.\nPress the Estimate button to update results.")
  }

  tmp <- x %>%
    select_at(.vars = c(tbv, pvars)) %>%
    group_by_at(.vars = tbv) %>%
    summarise_all(mean)

  if (color == "none") {
    p <- ggplot(tmp, aes_string(x = xvar, y = "Prediction"))
  } else {
    p <- ggplot(tmp, aes_string(x = xvar, y = "Prediction", color = color, group = color))
  }

  if (length(pvars) >= 3) {
    if (is.factor(tmp[[xvar]]) || length(unique(tmp[[xvar]])) < 11) {
      p <- p + geom_pointrange(aes_string(ymin = "ymin", ymax = "ymax"), size = .3)
    } else {
      p <- p + geom_ribbon(aes_string(ymin = "ymin", ymax = "ymax"), fill = "grey70", color = NA, alpha = 0.5)
    }
  }

  ## needed now that geom_smooth no longer accepts ymin and ymax as arguments
  ## can't see line properly using geom_ribbon
  if (color == "none") {
    p <- p + geom_line(aes(group = 1))
  } else {
    p <- p + geom_line()
  }

  if (facet_row != "." || facet_col != ".") {
    facets <- ifelse(facet_row == ".", paste("~", facet_col), paste(facet_row, "~", facet_col))
    facet_fun <- ifelse(facet_row == ".", facet_wrap, facet_grid)
    p <- p + facet_fun(as.formula(facets))
  }

  sshhr(p)
}

# to avoid 'no visible binding for global variable' NOTE
globalVariables(c("."))

#' radiant.ets
#'
#' @name radiant.ets
#' @docType package
#' @import radiant.data shiny dplyr ggplot2
#' @importFrom dplyr %>%
#' @importFrom magrittr %<>%
#' @importFrom forecast ggAcf ggPacf
#' @importFrom stats as.formula model.matrix predict ts

NULL

#' @importFrom forecast Acf
#' @export
forecast::Acf

#' Monthly advertising and sales data
#' @details Description provided in attr(adv_sales, "description")
#' @docType data
#' @keywords datasets
#' @name adv_sales
#' @usage data(adv_sales)
#' @format A data frame with 36 rows and 3 variables
NULL

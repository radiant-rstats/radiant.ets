#' Launch radiant.ets in the default browser
#'
#' @description Launch radiant.ets in the default web browser
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.ets()
#' }
#' @export
radiant.ets <- function(state, ...) radiant.data::launch(package = "radiant.ets", run = "browser", state, ...)

#' Launch radiant.ets in an Rstudio window
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.ets_window()
#' }
#' @export
radiant.ets_window <- function(state, ...) radiant.data::launch(package = "radiant.ets", run = "window", state, ...)

#' Launch radiant.ets in the Rstudio viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.ets_viewer()
#' }
#' @export
radiant.ets_viewer <- function(state, ...) radiant.data::launch(package = "radiant.ets", run = "viewer", state, ...)


import_fs("radiant.ets", libs = "forecast", incl = "Acf")

## urls for menu
r_url_list <- getOption("radiant.url.list")
r_url_list[["ARIMA"]] <- "ets/arima/"
r_url_list[["ARIMA"]] <-
  list("tabs_arima" = list(
    "Summary" = "ets/arima/",
    "Predict" = "ets/arima/predict/",
    "Plot" = "ets/arima/plot/"
  ))
options(radiant.url.list = r_url_list)
rm(r_url_list)

## time-series menu
options(
  radiant.ets_ui =
    tagList(
      navbarMenu(
        "ETS",
        tags$head(
          tags$script(src = "www_ets/js/run_return.js")
        ),
        "Single variable",
        tabPanel("ARIMA", uiOutput("arima"))
      )
    )
)

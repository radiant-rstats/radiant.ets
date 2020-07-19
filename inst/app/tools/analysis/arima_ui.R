################################################################
# ARIMA - UI
################################################################
arima_predict <- c(
  "None" = "none",
  "Data" = "data",
  "Command" = "cmd",
  "Data & Command" = "datacmd"
)
arima_check <- c("Mean" = "mean", "Drift" = "drift")
arima_plots <- c(
  "None" = "none", "TS plot" = "tsplot",
  "ACF" = "acf", "PACF" = "pacf",
  "Diagnostics" = "diag"
)

arima_args <- as.list(formals(radiant.ets::Arima))

## list of function inputs selected by user
arima_inputs <- reactive({
  arima_args$data_filter <- if (isTRUE(input$show_filter)) input$data_filter else ""
  arima_args$dataset <- input$dataset
  arima_args$order <- c(input$arima_order_p, input$arima_order_d, input$arima_order_q)
  if (isTRUE(input$arima_show_seasonal)) {
    seasonal <- list(order = c(input$arima_seasonal_p, input$arima_seasonal_d, input$arima_seasonal_q), period = input$arima_frequency)
    if (!any(seasonal$order > 0)) seasonal <- NULL
  } else {
    seasonal <- NULL
  }
  arima_args$seasonal <- seasonal
  for (i in r_drop(names(arima_args), drop = c("dataset", "data_filter", "order", "seasonal"))) {
    arima_args[[i]] <- input[[paste0("arima_", i)]]
  }
  arima_args
})

# arima_pred_args <- as.list(if (exists("predict.arima")) {
#   formals(predict.arima)
# } else {
#   formals(radiant.model:::predict.arima)
# } )

## list of function inputs selected by user
arima_pred_inputs <- reactive({
  # for (i in names(arima_pred_args))
  #   arima_pred_args[[i]] <- input[[paste0("arima_", i)]]
  # arima_pred_args <- list(model)
  arima_pred_args <- list()
  arima_pred_args$pred_cmd <- arima_pred_args$pred_data <- ""
  if (input$arima_predict == "cmd") {
    arima_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$arima_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$arima_predict == "data") {
    arima_pred_args$pred_data <- input$arima_pred_data
  } else if (input$arima_predict == "datacmd") {
    arima_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$arima_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    arima_pred_args$pred_data <- input$arima_pred_data
  }

  ## setting value for prediction interval type
  # arima_pred_args$interval <- "confidence"

  arima_pred_args
})

# arima_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
#   formals(plot.model.predict)
# } else {
#   formals(radiant.model:::plot.model.predict)
# })

## list of function inputs selected by user
arima_pred_plot_inputs <- reactive({
  arima_pred_plot_args <- list()

  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(arima_pred_plot_args))
    arima_pred_plot_args[[i]] <- input[[paste0("arima_", i)]]
  arima_pred_plot_args
})

output$ui_arima_rvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    isNum <- .get_class() %in% c("integer", "numeric", "ts")
    vars <- varnames()[isNum]
  })
  selectInput(
    inputId = "arima_rvar", label = "Response variable:", choices = vars,
    selected = state_single("arima_rvar", vars), multiple = FALSE
  )
})

output$ui_arima_order <- renderUI({
  tagList(
    tags$table(
      tags$td(numericInput("arima_order_p", label = "p", min = 0, value = state_init("arima_order_p", 1))),
      tags$td(numericInput("arima_order_d", label = "d", min = 0, value = state_init("arima_order_d", 0))),
      tags$td(numericInput("arima_order_q", label = "q", min = 0, value = state_init("arima_order_q", 0)))
    )
  )
})

output$ui_arima_seasonal <- renderUI({
  req(input$arima_rvar)
  tsp <- .get_data()[[input$arima_rvar]] %>%
    attr("tsp")
  freq <- ifelse(length(tsp) == 0, 4, tsp[3])

  tags$table(
    tags$td(numericInput("arima_seasonal_p", label = "P", min = 0, value = state_init("arima_seasonal_p", 1))),
    tags$td(numericInput("arima_seasonal_d", label = "D", min = 0, value = state_init("arima_seasonal_d", 0))),
    tags$td(numericInput("arima_seasonal_q", label = "Q", min = 0, value = state_init("arima_seasonal_q", 0))),
    tags$td(numericInput("arima_frequency", label = "Freq", min = 4, value = state_init("arima_frequency", freq)))
  )
})

output$ui_arima_xvar <- renderUI({
  req(available(input$arima_rvar))
  vars <- varnames()
  ## don't use setdiff, removes names
  if (length(vars) > 0 && input$arima_rvar %in% vars) {
    vars <- vars[-which(vars == input$arima_rvar)]
  }

  selectizeInput(
    inputId = "arima_evar", label = "Exogenous variables:", choices = vars,
    selected = state_multiple("arima_evar", vars, isolate(input$arima_evar)),
    multiple = TRUE,
    options = list(
      placeholder = "Select x-variable(s)",
      plugins = list("remove_button", "drag_drop")
    )
  )
})

## reset prediction and plot settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "arima_predict", selected = "none")
  updateSelectInput(session = session, inputId = "arima_plots", selected = "none")
})

output$ui_arima_predict_plot <- renderUI({
  predict_plot_controls("arima")
})

output$ui_arima_store_res_name <- renderUI({
  req(input$dataset)
  textInput("arima_store_res_name", "Store residuals:", "", placeholder = "Provide variable name")
})

arima_run_rval <- reactiveVal(-1)
observeEvent(input$arima_run, {
  arima_run_rval(1)
  updateActionButton(session, "arima_run", "Estimate model", icon = icon("play"))
})

observe({
  ## notify user when the model needs to be updated
  input$data_filter
  input$show_filter
  input$arima_show_seasonal
  input$arima_order_p
  input$arima_order_p
  input$arima_order_q
  input$arima_seasonal_p
  input$arima_seasonal_d
  input$arima_seasonal_q
  input$arima_frequency

  sapply(r_drop(names(arima_args)), function(x) input[[paste0("arima_", x)]])

  if (isolate(arima_run_rval()) == 1) {
    updateActionButton(session, "arima_run", "Re-estimate model", icon = icon("refresh", class = "fa-spin"))
    isolate(arima_run_rval(0))
  }
})

## data ui and tabs
output$ui_arima <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("arima_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_arima == 'Summary'",
        uiOutput("ui_arima_rvar"),
        uiOutput("ui_arima_order"),
        checkboxInput("arima_show_seasonal", "Seasonal", state_init("arima_show_seasonal", FALSE)),
        conditionalPanel(
          condition = "input.arima_show_seasonal === true",
          uiOutput("ui_arima_seasonal")
        ),
        uiOutput("ui_arima_xvar"),
        conditionalPanel(
          condition = "input.arima_rvar != null",
          tags$table(
            tags$td(checkboxInput("arima_mean", "Mean", value = state_init("arima_mean", TRUE)), style = "padding-right:10px;"),
            tags$td(checkboxInput("arima_drift", "Drift", value = state_init("arima_drift", FALSE)), style = "padding-right:10px;"),
            tags$td(checkboxInput("arima_diag", "Diagnostics", value = state_init("arima_diag", FALSE)), style = "padding-right:10px;")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_arima == 'Predict'",
        selectInput(
          "arima_predict", label = "Prediction input type:", arima_predict,
          selected = state_single("arima_predict", arima_predict, "none")
        ),
        conditionalPanel(
          "input.arima_predict == 'data' | input.arima_predict == 'datacmd'",
          selectizeInput(
            inputId = "arima_pred_data", label = "Prediction data:",
            choices = c("None" = "", r_info[["datasetlist"]]),
            selected = state_single("arima_pred_data", c("None" = "", r_info[["datasetlist"]])),
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.arima_predict == 'cmd' | input.arima_predict == 'datacmd'",
          returnTextAreaInput(
            "arima_pred_cmd", "Prediction command:",
            value = state_init("arima_pred_cmd", ""),
            rows = 3,
            placeholder = "Type a formula to set values for model variables (e.g., carat = 1; cut = 'Ideal') and press return"
          )
        ),
        conditionalPanel(
          condition = "input.arima_predict != 'none'",
          checkboxInput("arima_pred_plot", "Plot predictions", state_init("arima_pred_plot", FALSE)),
          conditionalPanel(
            "input.arima_pred_plot == true",
            uiOutput("ui_arima_predict_plot")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel(
          "input.arima_predict == 'data' | input.arima_predict == 'datacmd'",
          tags$table(
            tags$td(textInput("arima_store_pred_name", "Store predictions:", state_init("arima_store_pred_name", "pred_arima"))),
            tags$td(actionButton("arima_store_pred", "Store", icon = icon("plus")), style = "padding-top:30px;")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_arima == 'Plot'",
        selectInput(
          "arima_plots", "Plots:", choices = arima_plots,
          selected = state_single("arima_plots", arima_plots)
        )
      ),
      conditionalPanel(
        condition = "input.tabs_arima == 'Summary'",
        tags$table(
          tags$td(uiOutput("ui_arima_store_res_name")),
          tags$td(actionButton("arima_store_res", "Store", icon = icon("plus")), style = "padding-top:30px;")
        )
      )
    ),
    help_and_report(
      modal_title = "ARIMA", fun_name = "arima",
      help_file = inclRmd(file.path(getOption("radiant.path.ets"), "app/tools/help/arima.Rmd"))
    )
  )
})

arima_plot_width <- function() 650
arima_plot_height <- function() 500
arima_pred_plot_height <- function()
  if (input$arima_pred_plot) 500 else 1

# output is called from the main radiant ui.R
output$arima <- renderUI({
  register_print_output("summary_arima", ".summary_arima")
  register_print_output("predict_arima", ".predict_print_arima")
  register_plot_output(
    "predict_plot_arima", ".predict_plot_arima",
    height_fun = "arima_pred_plot_height"
  )
  register_plot_output(
    "plot_arima", ".plot_arima",
    height_fun = "arima_plot_height",
    width_fun = "arima_plot_width"
  )

  ## two separate tabs
  arima_output_panels <- tabsetPanel(
    id = "tabs_arima",
    tabPanel(
      "Summary",
      verbatimTextOutput("summary_arima")
    ),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.arima_pred_plot == true",
        download_link("dlp_arima_pred"),
        plotOutput("predict_plot_arima", width = "100%", height = "100%")
      ),
      download_link("dl_arima_pred"), br(),
      verbatimTextOutput("predict_arima")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_arima"),
      plotOutput("plot_arima", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "ETS",
    tool = "ARIMA",
    tool_ui = "ui_arima",
    output_panels = arima_output_panels
  )
})

arima_available <- eventReactive(input$arima_run, {
  if (not_available(input$arima_rvar)) {
    "This analysis requires a response variable of type integer\nor numeric.\nIf such a variable is not available please select another dataset.\n\n" %>%
      suggest_data("diamonds")
  } else {
    "available"
  }
})

.arima <- eventReactive(input$arima_run, {
  ari <- arima_inputs()
  ari$envir <- r_data
  withProgress(message = "Estimating model", value = 1, {
    do.call(Arima, ari)
  })
})

.summary_arima <- reactive({
  if (not_pressed(input$arima_run)) return("** Press the Estimate button to estimate the model **")
  if (arima_available() != "available") return(arima_available())
    mod <- .arima()
    do.call(summary, c(list(object = mod)))
    if (input$arima_diag) forecast::checkresiduals(mod, plot = FALSE)
})

.predict_arima <- reactive({
  if (not_pressed(input$arima_run)) return("** Press the Estimate button to estimate the model **")
  if (arima_available() != "available") return(arima_available())
  if (is_empty(input$arima_predict, "none")) return("** Select prediction input **")
  if ((input$arima_predict == "data" || input$arima_predict == "datacmd") && is_empty(input$arima_pred_data)) {
    return("** Select data for prediction **")
  }
  if (input$arima_predict == "cmd" && is_empty(input$arima_pred_cmd)) {
    return("** Enter prediction commands **")
  }
  withProgress(message = "Generating predictions", value = 1, {
    pred <- do.call(Arima, list(dataset = input$arima_pred_data, model = .arima(), envir = r_data))
    data.frame(attr(pred, "dataset"), Prediction = pred$fitted) %>% add_class("arima.predict")
  })
})

.predict_print_arima <- reactive({
  result <- .predict_arima()
  if (is.character(result)) {
    cat(result, "\n")
  } else {
    print(head(result, n = 20), row.names = FALSE)
  }
})

.predict_plot_arima <- reactive({
  req(
    pressed(input$arima_run), input$arima_pred_plot,
    !is_empty(input$arima_predict, "none")
  )

.predict_arima()
  withProgress(message = "Generating prediction plot", value = 1, {
    do.call(plot, c(list(x = .predict_arima()), arima_pred_plot_inputs()))
  })
})

.plot_arima <- reactive({
  if (not_pressed(input$arima_run)) {
    return("** Press the Estimate button to estimate the model **")
  } else if (is_empty(input$arima_plots, "none")) {
    return("Please select an arima plot from the drop-down menu")
  } else if (arima_available() != "available") {
    return(arima_available())
  }

  result <- .arima()

  withProgress(message = "Generating plots", value = 1, {
    if (input$arima_plots == "tsplot") {
      forecast::autoplot(result$x) + ylab(input$arima_rvar)
    } else if (input$arima_plots == "diag") {
      forecast::checkresiduals(result)
    } else if (input$arima_plots == "acf") {
      forecast::ggAcf(result$residuals)
    } else if (input$arima_plots == "pacf") {
      forecast::ggPacf(result$residuals)
    }
  })
})

observeEvent(input$arima_report, {
  if (!pressed(input$arima_run)) return(invisible())
  outputs <- c("summary")
  inp_out <- list("", "")
  figs <- FALSE

  if (isTRUE(input$arima_diag)) {
    xcmd <- "forecast::checkresiduals(result, plot = FALSE)"
  } else {
    xcmd <- c()
  }

  if (!is_empty(input$arima_store_res_name)) {
    fixed <- fix_names(input$arima_store_res_name)
    updateTextInput(session, "arima_store_res_name", value = fixed)
    xcmd <- c(xcmd, paste0(input$dataset, " <- store(", input$dataset, ", result, name = \"", fixed, "\")"))
  }

  if (!is_empty(input$arima_plots, "none")) {
    figs <- TRUE
    if (input$arima_plots == "diag") {
      if (input$arima_diag) {
        xcmd <- "forecast::checkresiduals(result)"
      } else {
        xcmd <- "forecast::checkresiduals(result, test = FALSE)"
      }
    } else if (input$arima_plots == "tsplot") {
      xcmd <- c(xcmd, paste0("forecast::autoplot(result$x) + ylab(\"", input$arima_rvar, "\")"))
    } else if (input$arima_plots == "acf") {
      xcmd <- c(xcmd, paste0("forecast::ggAcf(result$residuals)"))
    } else if (input$arima_plots == "pacf") {
      xcmd <- c(xcmd, paste0("forecast::ggPacf(result$residuals)"))
    }
  }

#   if (!is_empty(input$arima_predict, "none") &&
#      (!is_empty(input$arima_pred_data) || !is_empty(input$arima_pred_cmd))) {
#     pred_args <- clean_args(arima_pred_inputs(), arima_pred_args[-1])

#     if (!is_empty(pred_args$pred_cmd)) {
#       pred_args$pred_cmd <- strsplit(pred_args$pred_cmd, ";")[[1]]
#     } else {
#       pred_args$pred_cmd <- NULL
#     }
#     if (!is_empty(pred_args$pred_data)) {
#       pred_args$pred_data <- as.symbol(pred_args$pred_data)
#     } else {
#       pred_args$pred_data <- NULL
#     }

#     inp_out[[2 + figs]] <- pred_args
#     outputs <- c(outputs, "pred <- predict")
#     xcmd <- paste0(xcmd, "print(pred, n = 10)")
#     if (input$arima_predict %in% c("data", "datacmd")) {
#       fixed <- unlist(strsplit(input$arima_store_pred_name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
#         fix_names() %>%
#         deparse(., control = getOption("dctrl"), width.cutoff = 500L)
#       xcmd <- paste0(xcmd, "\n", input$arima_pred_data , " <- store(",
#         input$arima_pred_data, ", pred, name = ", fixed, ")"
#       )
#     }

#     if (input$arima_pred_plot && !is_empty(input$arima_xvar)) {
#       inp_out[[3 + figs]] <- clean_args(arima_pred_plot_inputs(), arima_pred_plot_args[-1])
#       inp_out[[3 + figs]]$result <- "pred"
#       outputs <- c(outputs, "plot")
#       figs <- TRUE
#     }
#   }

  update_report(
    inp_main = clean_args(arima_inputs(), arima_args),
    fun_name = "radiant.ets::Arima",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = arima_plot_width(),
    fig.height = arima_plot_height(),
    xcmd = paste0(xcmd, collapse = "\n")
  )
})

observeEvent(input$arima_store_res, {
  req(pressed(input$arima_run))
  robj <- .arima()
  if (is_empty(robj) || is.character(robj) || is_empty(input$arima_store_res_name)) return()
  fixed <- fix_names(input$arima_store_res_name)
  updateTextInput(session, "arima_store_res_name", value = fixed)
  withProgress(
    message = "Storing residuals", value = 1,
    r_data[[input$dataset]] <- store(r_data[[input$dataset]], robj, name = fixed)
  )
})



observeEvent(input$arima_store_pred, {
  req(!is_empty(input$arima_pred_data), pressed(input$arima_run))
  pred <- .predict_arima()
  if (is.null(pred)) return()
  fixed <- unlist(strsplit(input$arima_store_pred_name, "(\\s*,\\s*|\\s*;\\s*)")) %>%
    fix_names() %>%
    paste0(collapse = ", ")
  updateTextInput(session, "arima_store_pred_name", value = fixed)
  withProgress(
    message = "storing predictions", value = 1,
    r_data[[input$arima_pred_data]] <- store(
      r_data[[input$arima_pred_data]], pred,
      name = fixed
    )
  )
})

dl_arima_pred <- function(path) {
  if (pressed(input$arima_run)) {
    write.csv(.predict_arima(), file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_arima_pred",
  fun = dl_arima_pred,
  fn = function() paste0(input$dataset, "_arima_pred"),
  type = "csv",
  caption = "Save ARIMA predictions"
)

download_handler(
  id = "dlp_arima_pred",
  fun = download_handler_plot,
  fn = paste0(input$dataset, "_arima_pred"),
  type = "png",
  caption = "Save ARIMA prediction plot",
  plot = .predict_plot_arima,
  width = plot_width,
  height = arima_pred_plot_height
)

download_handler(
  id = "dlp_arima",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_", input$arima_plots, "_arima"),
  type = "png",
  caption = "Save ARIMA plot",
  plot = .plot_arima,
  width = arima_plot_width,
  height = arima_plot_height
)

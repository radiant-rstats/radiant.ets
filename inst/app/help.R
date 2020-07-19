help_ets <- c(
  "ARIMA" = "arima.md"
)
output$help_ets <- reactive(append_help("help_ets", file.path(getOption("radiant.path.ets"), "app/tools/help"), Rmd = TRUE))

observeEvent(input$help_ets_all, {
  help_switch(input$help_ets_all, "help_ets")
})

observeEvent(input$help_ets_none, {
  help_switch(input$help_ets_none, "help_ets", help_on = FALSE)
})

help_ts_panel <- tagList(
  wellPanel(
    HTML("<label>ETS menu: <i id='help_ts_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_ets_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput(
      "help_ets", NULL, help_ets,
      selected = state_group("help_ets"), inline = TRUE
    )
  )
)

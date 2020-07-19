shinyServer(function(input, output, session) {

  ## source shared functions
  source(file.path(getOption("radiant.path.data"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  source(file.path(getOption("radiant.path.data"), "app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)

  ## source data & app tools from radiant.data
  for (file in list.files(
    c(
      file.path(getOption("radiant.path.data"), "app/tools/app"),
      file.path(getOption("radiant.path.data"), "app/tools/data")
    ),
    pattern = "\\.(r|R)$",
    full.names = TRUE
  )) {
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)
  }

  ## setting up help
  source("help.R", encoding = getOption("radiant.encoding"), local = TRUE)

  ## help ui
  output$help_ets_ui <- renderUI({
    sidebarLayout(
      sidebarPanel(
        help_data_panel,
        help_ets_panel,
        uiOutput("help_text"),
        width = 3
      ),
      mainPanel(
        HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
        htmlOutput("help_data"),
        htmlOutput("help_ets")
      )
    )
  })

  ## packages to use for example data
  options(radiant.example.data = c("radiant.data", "radiant.ets"))

  ## 'sourcing' radiant's package functions in the server.R environment
  if (!"package:radiant.ets" %in% search() && getOption("radiant.path.ets") == "..") {
    ## for shiny-server and development
    for (file in list.files("../../R", pattern = "\\.(r|R)$", full.names = TRUE)) {
      source(file, encoding = getOption("radiant.encoding"), local = TRUE)
    }
  } else {
    ## for use with launcher
    radiant.data::copy_all(radiant.ets)
  }

  copy_from(radiant.model, predict_model, print_predict_model)
  source(file.path(getOption("radiant.path.model"), "app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)

  ## source analysis tools for ets app
  for (file in list.files(c("tools/analysis"), pattern = "\\.(r|R)$", full.names = TRUE)) {
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)
  }

  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})

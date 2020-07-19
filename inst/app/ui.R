## ui for time-series menu in radiant
navbar_proj(
  do.call(
    navbarPage,
    c(
      "Radiant",
      getOption("radiant.nav_ui"),
      getOption("radiant.ets_ui"),
      getOption("radiant.shared_ui"),
      help_menu("help_ets_ui")
    )
  )
)

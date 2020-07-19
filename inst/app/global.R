

## sourcing from radiant.data
options(radiant.path.data = system.file(package = "radiant.data"))
source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = getOption("radiant.encoding", default = "UTF-8"), local = TRUE)

ifelse(grepl("radiant.ets", getwd()) && file.exists("../../inst"), "..", system.file(package = "radiant.ets")) %>%
  options(radiant.path.ets = .)

## setting path for figures in help files
addResourcePath("figures_ets", "tools/help/figures/")

## setting path for www resources
addResourcePath("www_ets", file.path(getOption("radiant.path.ets"), "app/www/"))

if (is.null(getOption("radiant.path.model"))) options(radiant.path.model = system.file(package = "radiant.model"))

## loading urls and ui
source("init.R", encoding = getOption("radiant.encoding", "UTF-8"), local = TRUE)
options(radiant.url.patterns = make_url_patterns())

#init

required_packages = c("shiny","leaflet","sf","dplyr","tidyr","purrr","stringr","tibble",
  "lubridate","jsonlite","data.table","gtfstools","DT","osrm")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(required_packages, install_if_missing))
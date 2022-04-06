#' Access files in the current app
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "diades.atlas")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config R_CONFIG_ACTIVE value.
#' @param use_parent Logical, scan the parent directory for config file.
#'
#' @noRd
get_golem_config <- function(value,
                             config = Sys.getenv(
                               "GOLEM_CONFIG_ACTIVE",
                               Sys.getenv(
                                 "R_CONFIG_ACTIVE",
                                 "default"
                               )
                             ),
                             use_parent = TRUE,
                             # Modify this if your config file is somewhere else:
                             file = app_sys("golem-config.yml")) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

source("renv/activate.R")
renv::activate()

# cache ----
if (dir.exists("/opt/local/renv/cache")) {
  # Cache inside the docker container with persistent drive
  Sys.setenv(RENV_PATHS_CACHE = "/opt/local/renv/cache")
  renv::settings$use.cache(TRUE)
} else {
  # No cache
  renv::settings$use.cache(FALSE)
}

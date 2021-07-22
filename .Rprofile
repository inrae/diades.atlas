# If needed
# source("~/.Rprofile")

if (Sys.getenv("RENV_PATHS_LIBRARY_ROOT") != "") {
  Sys.setenv(RENV_PATHS_LIBRARY_ROOT = "/home/rstudio/.renv/library")
}

source("renv/activate.R")
renv::activate()

# cache ----
if (Sys.getenv("RENV_PATHS_CACHE") != "") {
  renv::settings$use.cache(TRUE)
} else if (dir.exists("/opt/local/renv/cache")) {
  # Cache inside the docker container with persistent drive
  # shared on host
  Sys.setenv(RENV_PATHS_CACHE = "/opt/local/renv/cache")
  renv::settings$use.cache(TRUE)
} else {
  # No cache
  renv::settings$use.cache(FALSE)
}

# file.copy(".Rprofile", to = "~/.Rprofile")

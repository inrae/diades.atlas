source("renv/activate.R")
renv::activate()

# cache ----
if (dir.exists("/opt/local/renv/cache")) {
  # Cache inside the docker container with persistent drive
  Sys.setenv(RENV_PATHS_CACHE = "/opt/local/renv/cache")
  renv::settings$use.cache(TRUE)
} else if (dir.exists("/mnt/Data/renv_cache")) {
  # Your local path on linux/unix if project used out of Docker
  Sys.setenv(RENV_PATHS_CACHE = "/mnt/Data/renv_cache")
  renv::settings$use.cache(TRUE)
} else if (dir.exists("D:/renv_cache_windows/")) {
  # Your local path on Windows if project used out of Docker
  Sys.setenv(RENV_PATHS_CACHE = "D:/renv_cache_windows")
  renv::settings$use.cache(TRUE)
} else {
  # No cache
  renv::settings$use.cache(FALSE)
}

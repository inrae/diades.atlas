if (file.exists("~/.Rprofile")){
  source("~/.Rprofile")
}


if (Sys.getenv("RENV_PATHS_LIBRARY_ROOT") != "") {
  Sys.setenv(RENV_PATHS_LIBRARY_ROOT = "/home/rstudio/.renv/library")
}

source("renv/activate.R")

lock_ <- renv:::lockfile(file = "renv.lock")

# # Fix CRAN version
if (Sys.info()["sysname"] == "Linux") {
  lock_$repos(CRAN = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")
} else {
  lock_$repos(CRAN = "https://cran.rstudio.com")
}

lock_$write(file = "renv.lock")
rm(lock_)

renv::settings$use.cache(TRUE)

renv::activate()

# file.copy(".Rprofile", to = "~/.Rprofile")

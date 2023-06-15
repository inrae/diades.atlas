if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile")
}
if (Sys.info()["user"] != "rstudio-connect") {
  # Fix CRAN version
  
  if (grepl("ubuntu 18.04|debian 8", tolower(utils::osVersion))) {
    cat("[renv] Setting repo to RSPM bionic ----\n")
    repos <- c("RSPM" = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest",
               # repos <- c("RSPM" = "https://cran.rstudio.com",
               # "thinkropen" = "https://thinkr-open.r-universe.dev",
               "CRAN" = "https://cran.rstudio.com")
  } else if (grepl("ubuntu 20.04|debian 9", tolower(utils::osVersion))) {
    cat("[renv] Setting repo to RSPM focal ----\n")
    repos <- c("RSPM" = "https://packagemanager.rstudio.com/all/__linux__/focal/latest",
               # repos <- c("RSPM" = "https://cran.rstudio.com",
               # "thinkropen" = "https://thinkr-open.r-universe.dev",
               "CRAN" = "https://cran.rstudio.com")
  } else if (grepl("ubuntu 22.04|debian 10", tolower(utils::osVersion))) {
    cat("[renv] Setting repo to RSPM jammy ----\n")
    repos <- c("RSPM" = "https://packagemanager.rstudio.com/all/__linux__/jammy/latest",
               # repos <- c("RSPM" = "https://cran.rstudio.com",
               # "thinkropen" = "https://thinkr-open.r-universe.dev",
               "CRAN" = "https://cran.rstudio.com")
  } else if (grepl("centos", tolower(utils::osVersion))) {
    cat("[renv] Setting repo to RSPM centos ----\n")
    # Important for MacOS users in particular
    repos <- c("RSPM" = "https://packagemanager.rstudio.com/all/__linux__/centos7/latest",
               # "thinkropen" = "https://thinkr-open.r-universe.dev",
               "CRAN" = "https://cran.rstudio.com")
  } else {
    # Important for MacOS users in particular
    repos <- c("RSPM" = "https://cran.rstudio.com",
               # "thinkropen" = "https://thinkr-open.r-universe.dev",
               "CRAN" = "https://cran.rstudio.com")
  }
  
  a <- try(lock_ <- renv:::lockfile(file = "renv.lock"), silent = TRUE)
  if (!inherits(a, "try-error")) {
    lock_$repos(.repos = repos)
    lock_$write(file = "renv.lock")
    rm(lock_)
  } else {
    # renv::lockfile_modify(repos = c(repos))
    lock_ <- readLines("renv.lock")
    which_url <- grep("URL", lock_)
    lock_[which_url] <- sprintf("        \"URL\": \"%s\"", repos[seq_along(which_url)])
    cat(lock_, sep = "\n", file = "renv.lock")
    rm(lock_)
  }
  options(repos = repos)
  
  
  
  rm(repos)
}

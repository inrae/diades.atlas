if (file.exists("~/.Rprofile")) {
    source("~/.Rprofile")
}
if (Sys.info()["user"] != "rstudio-connect") {
    lock_ <- renv:::lockfile(file = "renv.lock")
    # # Fix CRAN version
    if (Sys.info()["sysname"] == "Linux") {
        cat("[renv] Setting repo to RSPM ----\n")
        lock_$repos(CRAN = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")
    } else {
        cat("[renv] Setting repo to cran.rstudio.com ----\n")
        lock_$repos(CRAN = "https://cran.rstudio.com")
    }
    lock_$write(file = "renv.lock")
}
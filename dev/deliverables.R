# Deliverables ====
dir.create("deliverables")
usethis::use_build_ignore("deliverables")
usethis::use_git_ignore("deliverables")

## _pkgdown ----
# _Compile manual vignette
# See readme
rstudioapi::navigateToFile("README.Rmd")

## -- Release --
## Upgrade version Number in DESCRIPTION
## Update NEWS
rstudioapi::navigateToFile("NEWS.md")
## Create a commit for the version
## Add tag on version commit

## _covr ----
Sys.setenv(
  "R_CONFIG_ACTIVE" = "dev"
)
x <- covr::package_coverage()
# Change {my-project}
remotes::install_version('DT', version = "0.19")
# restart, relaunch Mongo
covr::report(x, file = "deliverables/codecoverage/diades-codecoverage-full-report.html")
renv::restore()

## Update description files in app
chameleon::create_pkg_biblio_file(
  to = "html",
  out.dir = "inst/app/www/about", edit = FALSE)
chameleon::create_pkg_desc_file(
  source = c("archive"),
  out.dir = "inst/app/www/about", to = "html", edit = FALSE)

## _add covrpage ----
# remotes::install_github("metrumresearchgroup/covrpage")
covrpage::covrpage(vignette = TRUE)
file.remove("tests/README.md")

# remotes::install_github("ThinkR-open/chameleon")
# remotes::install_github("ThinkR-open/thinkrtemplate")
# remotes::install_github("r-lib/pkgdown", ref = "v1.3.0")
# chameleon::open_pkgdown_function(path = "docs")
devtools::install(upgrade = "never")
chameleon::build_pkgdown(
  lazy = TRUE,
  yml = system.file("pkgdown/_pkgdown.yml", package = "thinkridentity"),
  favicon = system.file("pkgdown/favicon.ico", package = "thinkridentity"),
  move = TRUE, clean_before = TRUE, preview = FALSE
)
down_dir <- "deliverables/pkgdown"
unlink(down_dir, recursive = TRUE)
file.copy("inst/docs", "deliverables", recursive = TRUE)
file.rename("deliverables/docs", down_dir)
unlink("inst/docs", recursive = TRUE)

# pkgdown::build_site()
# pkgdown::build_site(override = list(development = list(mode = "devel")))
# debugonce(pkgdown:::build_site_local)
# pkgdown::deploy_to_branch(new_process = FALSE, override = list(development = list(mode = "devel")))
# aa <- pkgdown::as_pkgdown()
# aa$version
# aa$meta
# pkgdown:::meta_development(aa$meta, aa$version)
# aa$meta$destination

# Deploy {pkgdown} on rsconnect ----
usethis::use_git_ignore("docs/rsconnect")
usethis::use_git_ignore("inst/docs/rsconnect")
usethis::use_git_ignore("deliverables/pkgdown")
usethis::use_git_ignore("rsconnect")
usethis::use_build_ignore("rsconnect")

rsconnect::accounts()
account_name <- rstudioapi::showPrompt("Rsconnect account", "Please enter your username:", "name")
account_server <- rstudioapi::showPrompt("Rsconnect server", "Please enter your server name:", "1.1.1.1")
# origwd <- setwd("inst/docs")
origwd <- setwd("deliverables/pkgdown")
rsconnect::deployApp(
  ".",                       # the directory containing the content
  appFiles = list.files(".", recursive = TRUE), # the list of files to include as dependencies (all of them)
  appPrimaryDoc = "index.html",                 # the primary file
  appId = 350,
  appName = "pkgdown-diades.atlas",                   # name of the endpoint (unique to your account on Connect)
  appTitle = "pkgdown.diades.atlas",                  # display name for the content
  account = account_name,                # your Connect username
  server = account_server#,                    # the Connect server, see rsconnect::accounts()
  # forceUpdate = TRUE
)
setwd(origwd)


# Deploy App on rsconnect ----
# Deploy app on connect
appFiles <- list.files(".", recursive = TRUE)
appFiles <- appFiles[!grepl(".Rprofile|renv|rstudio_|deliverables|dev|data-raw|docker", appFiles)]

rsconnect::writeManifest(appFiles = appFiles)

# Deploy App on rsconnect manually
rsconnect::accounts()
account_name <- rstudioapi::showPrompt("Rsconnect account", "Please enter your username:", "name")
account_server <- rstudioapi::showPrompt("Rsconnect server", "Please enter your server name:", "1.1.1.1")

appFiles <- list.files(".", recursive = TRUE)
appFiles <- appFiles[!grepl(".Rprofile|renv|rstudio_|deliverables|dev|data-raw|docker", appFiles)]
rsconnect::deployApp(
  ".",                       # the directory containing the content
  appFiles = appFiles, # the list of files to include as dependencies (all of them)
  # appId = 478,
  appName = "app-diades.atlas",                   # name of the endpoint (unique to your account on Connect)
  appTitle = "app-diades.atlas",                  # display name for the content
  account = account_name,                # your Connect username
  server = account_server                    # the Connect server, see rsconnect::accounts()
)


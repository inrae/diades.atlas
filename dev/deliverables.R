# Deliverables ====
dir.create("deliverables")
usethis::use_build_ignore("deliverables")

## _pkgdown ----
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
  appName = "pkgdown-azti-diades",                   # name of the endpoint (unique to your account on Connect)
  appTitle = "pkgdown.azti.diades",                  # display name for the content
  account = account_name,                # your Connect username
  server = account_server#,                    # the Connect server, see rsconnect::accounts()
  # forceUpdate = TRUE
)
setwd(origwd)

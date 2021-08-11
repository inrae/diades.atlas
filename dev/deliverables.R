# Deliverables ====
dir.create("deliverables")
usethis::use_build_ignore("deliverables")

## _pkgdown ----
# _Compile manual vignette
rmarkdown::render(input = here::here("data-raw/aa-data-exploration-and-preparation.Rmd"),
                  output_format = "rmarkdown::html_vignette",
                  output_options = list(toc = TRUE),
                  output_file = here::here("vignettes/explo-manual.html"))

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
pkgdown::build_site(override = list(development = list(mode = "devel")))
debugonce(pkgdown:::build_site_local)
pkgdown::deploy_to_branch(new_process = FALSE, override = list(development = list(mode = "devel")))
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


# Clone on Irstea GitLab
# Sourcing user .Rprofile if it exists
home_profile <- file.path(
  Sys.getenv("HOME"),
  ".Rprofile"
)
if (file.exists(home_profile)) {
  source(home_profile)
}

options(renv.config.pak.enabled = TRUE)
if (dir.exists("renv")) {
  source("renv/activate.R")
}

if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile")
}

source("dev/switch_renv_repo.R")


renv::activate()
# renv::settings$snapshot.type("explicit")

# cache ----
# usethis::edit_r_environ(scope = "project")

renv::settings$use.cache(TRUE)

# if (Sys.getenv("RENV_PATHS_CACHE") != "") {
#   renv::settings$use.cache(TRUE)
# } else if (dir.exists(Sys.getenv("LOCAL_RENV_CACHE", unset = "~/renv_cache"))) {
#   # Cache on your own computer
#   # shared between projects
#   Sys.setenv(RENV_PATHS_CACHE = Sys.getenv("LOCAL_RENV_CACHE", unset = "~/renv_cache"))
#   renv::settings$use.cache(TRUE)
# } else if (dir.exists("/opt/local/renv/cache")) {
#   # Cache inside the docker container with persistent drive with {devindocker}
#   # shared on host
#   Sys.setenv(RENV_PATHS_CACHE = "/opt/local/renv/cache")
#   renv::settings$use.cache(TRUE)
# } else {
#   # No cache
#   renv::settings$use.cache(FALSE)
# }

# if (requireNamespace("prompt", quietly = TRUE)) {
#   cli::cat_rule("[.Rprofile] Setting up prompt")
#   options("continue" = " ")
#   prompt::set_prompt(
#     function(expr, value, ok, visible) {
#       paste0(
#         "📁 ",
#         crayon::blue(
#           crayon::bold(
#             basename(getwd())
#           )
#         ),
#         {
#           if (file.exists(".git")){
#             paste0(
#               " 🔨 ",
#               crayon::green(
#                 crayon::bold(
#                   system("git branch --show-current", intern = TRUE)
#                 )
#               )
#             )
#           } else {
#             ""
#           }
#         },
#         " > "
#       )
#     }
#   )
# }



# Setting shiny.autoload.r to FALSE
options(shiny.autoload.r = FALSE)

#' species UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_species_ui <- function(id, multiple = FALSE) {
  if (multiple) {
    f <- shiny::checkboxGroupInput
  } else {
    f <- shiny::radioButtons
  }
  ns <- NS(id)

  choiceValues <- unique(golem::get_golem_options("species_list")$local_name)
  choiceNames <- lapply(
    unique(golem::get_golem_options("species_list")$local_name),
    function(x) {
      with_i18(x, x)
    }
  )

  tagList(
    w3_hover_button(
      "Select a Species" %>% with_i18("select-species"),
      content = tagList(
        if (multiple) {
          container(
            w3css::w3_actionButton(
              class = "w3-border",
              ns("undo"),
              "Undo all selection" %>% with_i18("button-unselectall")
            )
          )
        },
        container(
          f(
            ns("species"),
            NULL,
            choiceValues = choiceValues,
            choiceNames = choiceNames
          )
        )
      ),
      content_style = "width:25em",
      button_id = ns("species_hover")
    )
  )
}

#' species Server Functions
#'
#' @noRd
mod_species_server <- function(id, r, entry = "species") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$species,
      {
        golem::invoke_js(
          "changeinnerhtmlwithid",
          list(
            id = ns("species_hover"),
            content = {
              if (length(input$species) == 0 | length(input$species) > 1) {
                paste(
                  "Select a Species" %>% with_i18("select-species"),
                  "(",
                  length(input$species),
                  "selected" %>% with_i18("selected"),
                  ")"
                )
              } else if (length(input$species) == 1) {
                paste(
                  "Select a Species" %>% with_i18("select-species"),
                  "(",
                  "selected" %>% with_i18("selecteed"),
                  ":",
                  input$species,
                  ")"
                )
              }
            }
          )
        )
        req(input$species)
        r[[entry]] <- input$species
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$undo, {
      updateSelectInput(
        session,
        inputId = "species",
        selected = ""
      )
    })
  })
}

## To be copied in the UI
# mod_species_ui("species_ui_1")

## To be copied in the server
# mod_species_server("species_ui_1")
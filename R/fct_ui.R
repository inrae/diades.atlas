#' ui function
#'
#' @description A shiny function
#'
#' @param Internal parameters for the function.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
menu <- function(...){
  div(
    class = "w3-row w3-large w3-light-grey", 
    tagList(
      ...
    )
  )
}

menuItem <- function(
  id, 
  name, 
  i18n
){
  tags$div(
    class = "w3-col s3",
    tags$a(
      href = "javascript:void(0)",
      class = "w3-button w3-block w3navbar",
      `data-i18n` = i18n,
      onclick = glue::glue('$( ".bloc" ).hide(); $( "#{id}" ).show();$( "#{id}" ).trigger("show");$( "#{id}" ).trigger("shown");$(".w3navbar").removeClass("focused");$(this).addClass("focused");'),
      name
    )
  )
}

tabItem <- function(
  id, 
  ...
){
  tags$div(
    id = id, 
    class = "bloc", 
    style = "display:none",
    tagList(...)
  )
}

tabItems <- function(...){
  tagList(...)
}

modal <- function(
  inputId,
  title = "",
  body = "",
  footer = "",
  color = "",
  display = "none"
){
  htmlTemplate(
    app_sys("app/www/modal.html"),
    id = inputId,
    title = title,
    body = body,
    footer = footer,
    color = color,
    display = display
  )
}

container <- function(...){
  tags$div(
    class = "w3-row-padding w3-padding-16 w3-center",
    ...
  )
}

with_i18 <- function(
  tag,
  i18n
){
  if (class(tag)[1] == "character"){
    tag <- tags$span(tag)
  }
  tag %>%
    tagAppendAttributes(
      `data-i18n` = i18n
    )
}

w3_hover_button <- function(
  button_text = "Hover Over Me!",
  content = list(), 
  button_id = NULL,
  button_class = NULL,
  content_style = NULL
){
  tags$div(
    class = "w3-dropdown-hover w3-border",
    class = button_class,
    tags$button(
      class = "w3-button",
      button_text,
      id = button_id
    ),
    tags$div(
      class = "w3-dropdown-content w3-bar-block w3-border",
      style = content_style,
      content
    )
  )
}

ecosystem_hover_content <- function(basin){
  if (length(basin) == 0 | length(basin) > 1 ){
    paste(
      "Select an Ecosystem Service" %>% with_i18("select-ecosystem"), 
      "(", 
      length(basin),
      "selected" %>% with_i18("selecteeed"),
      ")"
    )
  } else if (length(basin) == 1){
    paste(
      "Select an Ecosystem Service" %>% with_i18("select-ecosystem"), 
      "(", 
      "selected" %>% with_i18("selecteeeed"),
      ":",
      basin,
      ")"
    )
  } 
}

date_hover_content <- function(date){
  paste(
    "Select a Date" %>% with_i18("select-daterange"),
    "(",
    "selected" %>% with_i18("selecteed"),
    ":",
    date[1], 
    "/",
    date[2],
    ")"
  )
}

scenario_hover_content <- function(scenario){
  paste(
    "Select a scenario" %>% with_i18("select-scenario"),
    "(",
    "selected" %>% with_i18("selecteed"),
    ":",
    scenario, 
    ")"
  )
}

case_study_hover_content <- function(case_study){
  if (length(case_study) == 0 | length(case_study) > 1 ){
    paste(
      "Select a Case Study" %>% with_i18("select-case_study"), 
      "(", 
      length(case_study),
      "selected" %>% with_i18("selecteeed"),
      ")"
    )
  } else if (length(case_study) == 1){
    paste(
      "Select a Case Study" %>% with_i18("select-case_study"), 
      "(", 
      "selected" %>% with_i18("selecteeeed"),
      ":",
      case_study,
      ")"
    )
  } 
}

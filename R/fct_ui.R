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

half <- function(...){
  tags$div(
    class = "w3-half",
    ...
  )
}

third <- function(...){
  tags$div(
    class = "w3-third",
    ...
  )
}

quarter <- function(...){
  tags$div(
    class = "w3-quarter  w3-left",
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
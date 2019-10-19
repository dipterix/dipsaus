# Action Button but styled
actionButtonStyled <- function(
  inputId, label, icon = NULL, width = NULL, type = "default",
  btn_type = "button", class = "", ...
){
  value <- shiny::restoreInput(id = inputId, default = NULL)
  args = list(...)
  style = c(args[["style"]], "")[[1]]
  width = c(width, "auto")[[1]]
  style = paste0("width: ", shiny::validateCssUnit(width), ";", style)
  args[["style"]] = style
  args[["id"]] = inputId
  args[["type"]] = btn_type
  args[["class"]] = sprintf("btn btn-%s action-button %s", type, class)
  args[["data-val"]] = value
  args[["id"]] = inputId
  do.call(shiny::tags$button, c(list(list(icon, label)), args))
}

updateActionButtonStyled <- function(session, inputId, label = NULL, icon = NULL){

  session$sendCustomMessage(
    type = 'shiny.actionButtonInput',
    message = list(
      inputId = session$ns( inputId )
    )
  )

  shiny::updateActionButton(session, inputId, label, icon)
}
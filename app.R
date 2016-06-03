
library(shiny)

ui <- basicPage(
  fluidRow(
    actionButton(inputId = "add_button", label = "Add Button")
  ),
  uiOutput("more_buttons")
)

random_id <- function() {
  paste(
    sample(c(0:9, letters, LETTERS), 8, replace = TRUE),
    collapse = ""
  )
}

server <- function(input, output){

  rvs <- reactiveValues(
    buttons = list()
  )

  observeEvent(
    input$add_button,
    {
      id <- random_id()
      rvs$buttons[[id]] <- div(
        actionButton(inputId = paste0("button", id), label = id),
        actionButton(
          inputId = paste0("del_button", id),
          label = paste0("Delete")
        )
      )

      observeEvent(
        input[[paste0("button", id)]],
        { print(id) }
      )

      observeEvent(
        input[[paste0("del_button", id)]],
        { rvs$buttons[[id]] <- NULL }
      )
    }
  )

  output$more_buttons <- renderUI({
    print(rvs$buttons)
    do.call(fluidRow, unname(rvs$buttons))
  })

}

shinyApp(ui, server)

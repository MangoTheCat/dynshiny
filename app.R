
library(shiny)

ui <- basicPage(
  fluidRow(
    selectInput("inputFile", "File", choices = c("x.txt", "y.txt")),
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
    input$inputFile,
    {
      rvs$buttons <- list()
      btn <- readLines(input$inputFile)
      for (str in btn) {
        id <- random_id()
        rvs$buttons[[id]] <- div(
          actionButton(inputId = paste0("button", id), label = str),
          actionButton(
            inputId = paste0("del_button", id),
            label = paste0("Delete")
          )
        )

        local({
          id2 <- id
          observeEvent(
            input[[paste0("button", id2)]],
            { print(id2) }
          )

          observeEvent(
            input[[paste0("del_button", id2)]],
            { rvs$buttons[[id2]] <- NULL }
          )
        })
      }
    }
  )

  observeEvent(
    input$add_button,
    {
      id <- random_id()
      rvs$buttons[[id]] <- div(
        actionButton(inputId = paste0("button", id), label = "xxx"),
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

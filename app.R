
library(shiny)

ui <- basicPage(
  fluidPage(
    fluidRow(
      selectInput("inputFile", "File", choices = c("x.txt", "y.txt")),
      actionButton(inputId = "add_button", label = "Add Button")
    ),
    uiOutput("more_buttons")
  )
)

random_id <- function() {
  paste(
    sample(c(0:9, letters, LETTERS), 8, replace = TRUE),
    collapse = ""
  )
}

server <- function(input, output) {

  rvs <- reactiveValues(
    buttons = list()
  )

  create_button <- function(id, label) {

    rvs$buttons[[id]] <- wellPanel(
      textInput(
        paste0("input-", id),
        label = label
      ),
      actionButton(inputId = paste0("button", id), label = label),
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

  observeEvent(
    input$inputFile,
    {
      rvs$buttons <- list()
      btn <- readLines(input$inputFile)
      for (str in btn) {
        create_button(str, str)
      }
    }
  )

  observeEvent(
    input$add_button,
    {
      create_button(random_id(), "xxx")
    }
  )

  output$more_buttons <- renderUI({
    do.call(fluidRow, unname(rvs$buttons))
  })

}

shinyApp(ui, server)

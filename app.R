
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

server <- function(input, output, session) {

  data <- list()

  updateDB <- reactive({
    d <- read.table(input$inputFile, stringsAsFactors = FALSE)
    data <<- structure(as.list(d[,2]), names = d[,1])
  })

  create_button <- function(id, label, value) {

    w <- wellPanel(
      textInput(
        paste0("input-", id),
        label = label,
        value = value
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
        input[[paste0("input-", id2)]],
        { data[[id]] <<- input[[paste0("input-", id2)]] }
      )

      observeEvent(
        input[[paste0("button", id2)]],
        { print(id2) }
      )

      observeEvent(
        input[[paste0("del_button", id2)]],
        {
          data[[id2]] <<- NULL
        }
      )
    })

    w
  }

  updateAdd <- reactive({
    if (input$add_button) data[[random_id()]] <<- "xxx"
  })

  output$more_buttons <- renderUI({
    updateDB()
    updateAdd()
    w <- lapply(seq_along(data), function(i) {
      create_button(names(data)[i], names(data)[i], data[[i]])
    })
    do.call(fluidRow, w)
  })

}

shinyApp(ui, server)

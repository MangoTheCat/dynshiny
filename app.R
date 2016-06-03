
library(shiny)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Dynamic UI with database backend"),
  sidebarPanel(
    selectInput("inputFile", "File", choices = c("x.txt", "y.txt")),
    actionButton(inputId = "add_button", label = "Add Button")
  ),
  mainPanel(
    uiOutput("more_buttons")
  )
))

random_id <- function() {
  paste(
    sample(c(0:9, letters, LETTERS), 8, replace = TRUE),
    collapse = ""
  )
}

server <- function(input, output, session) {

  data <- list()
  delete <- reactiveValues(notify = 0)

  updateDB <- reactive({
    d <- readLines(input$inputFile)
    n <- vapply(seq_along(d), function(x) random_id(), "")
    data <<- structure(as.list(d), names = n)
  })

  create_button <- function(id, label, value) {

    w <- div(wellPanel(
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
    ))

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
          delete$notify <- isolate(delete$notify) + 1
        }
      )
    })

    w
  }

  updateAdd <- reactive({
    if (input$add_button) data[[random_id()]] <<- "xxx"
  })

  updateDelete <- reactive({
    delete$notify
  })

  output$more_buttons <- renderUI({
    updateAdd()
    updateDB()
    updateDelete()
    w <- lapply(seq_along(data), function(i) {
      create_button(names(data)[i], names(data)[i], data[[i]])
    })
    do.call(fluidRow, w)
  })
}

shinyApp(ui, server)

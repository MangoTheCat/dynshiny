
library(shiny)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Dynamic UI with database backend"),
  sidebarPanel(
    selectInput("inputFile", "File", choices = c("x.txt", "y.txt")),
    actionButton(inputId = "add_button", label = "Add"),
    actionButton(inputId = "cancel_button", label = "Cancel"),
    actionButton(inputId = "save_button", label = "Save", class = "btn-primary")
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
  dbdata <- list()
  delete <- reactiveValues(notify = 0)

  updateDB <- reactive({
    load_db(input$inputFile)
  })

  load_db <- function(db) {
    d <- readLines(db)
    n <- vapply(seq_along(d), function(x) random_id(), "")
    dbdata <<- data <<- structure(as.list(d), names = n)
  }

  create_button <- function(id, label, value) {

    w <- div(wellPanel(
      textInput(
        paste0("input-", id),
        label = label,
        value = value
      ),
      actionButton(
        inputId = paste0("del_button", id),
        label = paste0("Delete"),
        class = "btn-danger"
      )
    ))

    local({
      id2 <- id

      observeEvent(
        input[[paste0("input-", id2)]],
        {
          data[[id]] <<- input[[paste0("input-", id2)]]
        }
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
    if (input$add_button) {
      data[[random_id()]] <<- ""
    }
  })

  updateDelete <- reactive({
    delete$notify
  })

  observeEvent(
    input$save_button,
    {
      if (identical(data, dbdata)) {
        message("Nothing to write")
      } else {
        writeLines(unlist(data), con = input$inputFile)
      }
    }
  )

  updateCancel <- reactive({
    if (input$cancel_button) load_db(input$inputFile)
  })

  output$more_buttons <- renderUI({

    updateAdd()
    updateDB()
    updateDelete()
    updateCancel()

    w <- lapply(seq_along(data), function(i) {
      create_button(names(data)[i], "Description", data[[i]])
    })
    do.call(fluidRow, w)

  })
}

shinyApp(ui, server)

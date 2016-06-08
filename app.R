
library(shiny)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Dynamic UI with database backend"),
  sidebarPanel(
    selectInput("inputFile", "File", choices = c("x.txt", "y.txt")),
    uiOutput("buttons")
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
  rvs <- reactiveValues(deleted = 0, changed = FALSE)

  ## All buttons are dynamic, otherwise they wouldn't (easily)
  ## be in the same row.
  output$buttons <- renderUI({
    div(
      actionButton(inputId = "add_button", label = "Add"),
      actionButton(inputId = "cancel_button", label = "Cancel"),
      if (rvs$changed) {
        actionButton(inputId = "save_button", label = "Save", class = "btn-primary")
      }
    )
  })

  updateDB <- reactive({
    load_db(input$inputFile)
  })

  load_db <- function(db) {
    d <- readLines(db)
    n <- vapply(seq_along(d), function(x) random_id(), "")
    dbdata <<- data <<- structure(as.list(d), names = n)
    rvs$changed <- FALSE
  }

  different_data <- function(d1, d2) {
    !identical(unname(d1), unname(d2))
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
          rvs$changed <- different_data(data, dbdata)
        }
      )

      observeEvent(
        input[[paste0("del_button", id2)]],
        {
          data[[id2]] <<- NULL
          rvs$deleted <- isolate(rvs$deleted) + 1
          rvs$changed <- different_data(data, dbdata)
        }
      )
    })

    w
  }

  updateAdd <- reactive({
    if (!is.null(input$add_button) && input$add_button) {
      data[[random_id()]] <<- ""
      rvs$changed <- different_data(data, dbdata)
    }
  })

  updateDelete <- reactive({
    rvs$deleted
  })

  observeEvent(
    input$save_button,
    {
      if (!rvs$changed) {
        message("Nothing to write")
      } else {
        writeLines(unlist(data), con = input$inputFile)
        dbdata <<- data
        rvs$changed <- FALSE
      }
    }
  )

  updateCancel <- reactive({
    if (!is.null(input$cancel_button) && input$cancel_button) {
      data <<- dbdata
      rvs$changed <- FALSE
    }
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

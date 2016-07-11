
library(shiny)

ui <- shinyUI(pageWithSidebar(
  headerPanel("Dynamic UI with database backend"),
  sidebarPanel(
    selectInput("file", "File", choices = c("x.txt", "y.txt")),
    div(
      actionButton(inputId = "add", label = "Add"),
      actionButton(inputId = "cancel", label = "Cancel"),
      actionButton(inputId = "save", label = "Save", class = "btn-primary")
    )
  ),
  mainPanel(
    uiOutput("records")
  )
))

server <- function(input, output, session) {

  rvs <- reactiveValues(
    data = list(),
    dbdata = list(),
    recordState = 1
  )

  observeEvent(input$file, {
    d <- read.csv(input$file, stringsAsFactors = FALSE)
    rvs$data <- rvs$dbdata <- d
    rvs$recordState <- rvs$recordState + 1
  })

  observeEvent(input$add, {
    newid <- if (nrow(rvs$data) == 0) {
      1
    } else {
      max(as.numeric(rvs$data$id)) + 1
    }
    rvs$data <- rbind(rvs$data, list(id = newid, description = ""))
    rvs$recordState <- rvs$recordState + 1
  })

  observeEvent(input$cancel, {
    rvs$data <- rvs$dbdata
    rvs$recordState <- rvs$recordState + 1
  })

  observeEvent(input$save, {
    write.csv(rvs$data, input$file, quote = FALSE, row.names = FALSE)
    rvs$dbdata <- rvs$data
  })

  output$records <- renderUI({
    rvs$recordState
    mydata <- isolate(rvs$data)
    w <- lapply(seq_len(nrow(mydata)), function(i) {
      create_record(i, mydata[i,])
    })
    do.call(fluidRow, w)
  })

  create_record <- (function() {

    inited <- 0

    function(wid, record) {
      w <- div(wellPanel(
        textInput(
          paste0("inp-", wid),
          label = record$id,
          value = record$description
        ),
        actionButton(
          paste0("del-", wid),
          label = "Delete",
          class = "btn-danger"
        )
      ))

      if (wid > inited) {

        local({
          wid2 <- wid

          observeEvent(input[[paste0("inp-", wid2)]], {
            rvs$data[wid2, "description"] <- input[[paste0("inp-", wid2)]]
          })

          observeEvent(input[[paste0("del-", wid2)]], {
            rvs$data <- rvs$data[-wid2, , drop = FALSE]
            rvs$recordState <- rvs$recordState + 1
          })
        })

        inited <<- wid
      }

      w
    }
  })()
}

shinyApp(ui, server)

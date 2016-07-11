
library(shiny)

## Buttons are dynamically generated, because the Cancel and the
## Save button are only shown if something has changed.

ui <- shinyUI(pageWithSidebar(
  headerPanel("Dynamic UI with database backend"),
  sidebarPanel(
    selectInput("file", "File", choices = c("x.csv", "y.csv")),
    uiOutput("buttons")
  ),
  mainPanel(
    uiOutput("records")
  )
))

server <- function(input, output, session) {

  ## `data` is the actual value of the data being edited. This is updated
  ## whenever the input widgets change. We assume that the data is a
  ## data frame, and each record is a row. For this simple app we assume
  ## that the data frame has columns 'id' and 'description'.
  ##
  ## `dbdata` holds the data that was read from the database. We keep this
  ## to be able to tell if something has changed.
  ##
  ## `recordState` is a dummy variable that we change every time we
  ## need to rebuild the widgets that hold the records:
  ## - when another file is selected, and loaded
  ## - when Cancel is pressed
  ## - when a record is deleted
  ## - when a record is added
  ## We cannot make the UI rebuild simply depend on the data, because
  ## not all changes in the data require a UI rebuild.
  ##
  ## `dataSame` declares whether `data` and `dbdata` are the same.

  rvs <- reactiveValues(
    data = list(),
    dbdata = list(),
    recordState = 1,
    dataSame = TRUE
  )

  ## The dynamic buttons, 'Cancel' and 'Save' are only shown if sg has
  ## changed.

  output$buttons <- renderUI({
    div(
      actionButton(inputId = "add", label = "Add"),
      if (! rvs$dataSame) {
        span(
          actionButton(inputId = "cancel", label = "Cancel"),
          actionButton(inputId = "save", label = "Save",
                       class = "btn-primary")
        )
      } else {
        span()
      }
    )
  })

  ## This app is different than usual apps, because it is event-driven.
  ## Many (most?) shiny apps are reactive, i.e. they only contain
  ## recipes for how the different output values can be updated, and then
  ## it is up to Shiny to make sure that they are updated whenever they
  ## need to.
  ##
  ## We found it hard to write this app the traditional way, mainly because
  ## the UI contains many action buttons that trigger dynamic UI changes,
  ## and also because the internal representation of the data must be
  ## changed without any output change. (More about the latter later.)
  ##
  ## So again, this app is event-driven. We specify what should happen
  ## whenever the user presses the various action buttons, or edits
  ## the records.

  ## The first event is a change in the selected file. Should this
  ## happen, we
  ## (1) set data and dbdata to the contents of the file, and
  ## (2) trigger a UI rebuild

  observeEvent(input$file, {
    cat("i Reading input file", input$file, "\n")
    d <- read.csv(input$file, stringsAsFactors = FALSE)
    rvs$data <- rvs$dbdata <- d
    rvs$recordState <- rvs$recordState + 1
    rvs$dataSame <- TRUE
  })

  ## Adding a new record. We create a new id for it first, then
  ## just add it to the bottom of the data frame that holds the data.
  ## Then we trigger a UI rebuild.
  ##
  ## After adding a new empty record, it is highly unlikely that
  ## data and dbdata would be the same, but we check for it, nevertheless.

  observeEvent(input$add, {
    cat("i Adding new record\n")
    newid <- if (nrow(rvs$data) == 0) {
      1
    } else {
      max(as.numeric(rvs$data$id)) + 1
    }
    rvs$data <- rbind(rvs$data, list(id = newid, description = ""))
    rvs$recordState <- rvs$recordState + 1
    rvs$dataSame <- identical(rvs$data, rvs$dbdata)
  })

  ## Cancel button is pressed. We need to restore the data from the
  ## saved dbdata, and trigger a UI rebuild. This is not always needed,
  ## but it is also safe to do it.

  observeEvent(input$cancel, {
    cat("i Cancelling all modifications\n")
    rvs$data <- rvs$dbdata
    rvs$recordState <- rvs$recordState + 1
    rvs$dataSame <- TRUE
  })

  ## Save button was pressed. Save the file and set dbdata to data.

  observeEvent(input$save, {
    cat("i Saving", input$file)
    write.csv(rvs$data, input$file, quote = FALSE, row.names = FALSE)
    rvs$dbdata <- rvs$data
    rvs$dataSame <- TRUE
  })

  ## Recipe to build the UI. The dummy `rvs$recordState` expression
  ## makes sure that we always rebuild the UI, whenever a rebuild was
  ## triggered by changing the `rvs$recordState` value.
  ##
  ## Note the use of `isolate`. We do not want `output$records` to
  ## depend on `rvs$data` directly, because we only want to rebuild the UI
  ## after selected events, but not all data changes.
  ##
  ## We use `create_record` to create the UI and the event wiring
  ## for each record. Its first argument is the widget id, a number
  ## between 1 and `n`, where `n` is the number of records on the screen.

  output$records <- renderUI({
    cat("i Updating record display\n")
    rvs$recordState
    mydata <- isolate(rvs$data)
    w <- lapply(seq_len(nrow(mydata)), function(i) {
      create_record(i, mydata[i,])
    })
    do.call(fluidRow, w)
  })

  ## `create_record` is a closure: a function
  ## with an environment to store data. We need the environment
  ## to store the maximum number of widgets that were wired up with
  ## edit and delete events. We need this, because (as far as I know),
  ## one cannot delete an existing `observeEvent` connection. So every time
  ## we rebuild a widget with a given widget id (`wid`) number, we check
  ## if the widget's events were already wired up, and only do the wiring
  ## if they were not.
  ##
  ## In other words, the newly built UI will reuse as many of the existing
  ## wired widgets as possible. `inited` stores the number of wired widgets
  ## and the ids of their inputs and delete buttons are `inp-x` and
  ## `del-x`, where `x` is between 1 and `inited`.

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

        ## Whenever the input changes, we update `data`. Note that we do
        ## not trigger a UI rebuild in this case. That would result
        ## deleting and re-adding all UI widgets as the user edits the
        ## input field

        observeEvent(input[[paste0("inp-", wid)]], {
          rvs$data[wid, "description"] <- input[[paste0("inp-", wid)]]
          rvs$dataSame <- identical(rvs$data, rvs$dbdata)
        })

        ## Deleting a record. Quite simple.

        observeEvent(input[[paste0("del-", wid)]], {
          rvs$data <- rvs$data[-wid, , drop = FALSE]
          rvs$recordState <- rvs$recordState + 1
          rvs$dataSame <- identical(rvs$data, rvs$dbdata)
        })

        ## We need to update the number of wired widgets in the closure

        inited <<- wid
      }

      w
    }
  })()
}

shinyApp(ui, server)

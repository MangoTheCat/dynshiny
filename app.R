#' # Dynamically generated Shiny UI
#' 
#' <br>
#' <img width="400" src="./screen1.png" alt="">
#' <img width="400" src="./screen2.png" alt="">
#' <br>
#' 
#' ## Introduction
#' 
#' It often happens that the user interface of a Shiny application needs to be
#' generated dynamically, based on data or other variables. One typical use
#' case is when the UI lets the user edit variable number of records from a
#' database.
#' 
#' Imagine that you have an employee database, where each employee can be
#' assigned multiple roles. Each assignment also has additional data, for
#' example the proportion of work time the employee is expected to exercise
#' that role, a comment field, etc. In the relational database, you would
#' store this information in a `roles` table, and then have (potentially)
#' multiple roles for each employee. When you write a web app to edit the
#' database, it makes sense to edit all roles of an employee on the same page:
#' add or delete roles, or modify existing roles. This requires generating the
#' user interface (UI) of the app dynamically, from the database.
#' 
#' ## Requirements
#' 
#' It particular we want our app to satisfy the following requirements:
#' * It must handle multiple employees, i.e. when a new employee is selected
#'   from a list, it should read in and show all current roles of that
#'   employee.
#' * It must be able to edit existing roles of an employee, and then update
#'   the database.
#' * It must be able to add new roles to an employee, and write the updated
#'   data to the database.
#' * It must be able to delete roles from an employee, and write the updated
#'   data to the database.
#' * It must be able to handle arbitrary number of roles for an employee,
#'   including no roles at all.
#' * It must only modify the database once the user clicks on the `Save`
#'   button.
#' * It must have a `Cancel` button that discards all edits, and shows the
#'   employee roles as last read from the database.
#' * The `Save` and `Cancel` buttons must be hidden if the employee data has
#'   not been changed.
#' 
#' While these requirements are quite straightforward, they are not completely
#' trivial to implement in Shiny. In the following we will build a Shiny app
#' that implements them, step by step.
#' 
#' ## The app
#' 
library(shiny)

#' 
#' ### The UI part of the app
#' 
#' The UI definition of the app it quite straightforward, as most of the
#' content will be dynamically generated. We will have the employee selection
#' box on a side panel, and the roles of the selected employee on the main
#' panel.
#' 
ui <- shinyUI(pageWithSidebar(
  headerPanel("Employee role database"),
  sidebarPanel(
    selectInput(
      "employee",
      "Employee",
      choices = c("Jo Gee", "John Doe")
    ),
    uiOutput("buttons")
  ),
  mainPanel(
    uiOutput("roles")
  )
))

#' 
#' For this simple example, we just list all employees here. In practice the
#' employee names come from the database, of course.
#' 
#' `buttons` will contain the `Add`, `Save` and `Cancel` buttons. The last
#' two are dynamic, as they are only shown if the roles have changed. So we
#' generate all three buttons dynamically.
#' 
#' ### The `server` function
#' 
#' We are ready to write the more complicated `server` function.
#' 
#' First of all we need to store some reactive values that help us compare
#' the current state of the roles to the state in the database, for the
#' current employee.
#' 
#' * `data` is the actual value of the roles being edited. This is updated
#'   whenever the input widgets change. We assume that `data` is a data
#'   frame and each role is a row in it. For this simple app we assume that
#'   the data frame have columns `id`, `role`. The `id` field is a simple
#'   numeric id for the role of the employee.
#' 
#' * `dbdata` holds the data that was read from the database. We keep this to
#'   be able to tell if something has changed.
#' 
#' * `recordState` is a dummy variable that we change every time we need to
#'   rebuild the widgets that hold the roles:
#'   - when another employee is selected
#'   - when `Cancel` is pressed
#'   - when a role is deleted
#'   - when a role is added
#' 
#'   Note that we don't rebuild the UI when existing roles are updated. So we
#'   cannot make the UI rebuild simply depend on `data`, because not all
#'   changes to `data` require a UI rebuild.
#' 
#' * `dataSame` declares whether `data` and `dbdata` are the same. Technically
#'   it is not needed, but it speeds up the decision whether or not the
#'   `Cancel` and `Save` buttons should be shown.
#' 
server <- function(input, output, session) {

  rvs <- reactiveValues(
    data = list(),
    dbdata = list(),
    recordState = 1,
    dataSame = TRUE
  )

#' 
#' ### Dynamic `Cancel` and `Save` buttons
#' 
#' The `Add` button is always shown. The `Cancel` and `Save` buttons are only
#' shown if `data` and `dbdata` are not the same.
#' 
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

#' 
#' ### Add reactivity
#' 
#' This app is different than usual apps, because it is event-driven. Many
#' (most?) shiny apps are reactive, i.e. they only contain recipes for how the
#' different output values can be updated, and then it is up to Shiny to make
#' sure that they are updated whenever they need to.
#' 
#' I found it hard to write this app the traditional way, mainly because the
#' UI contains many action buttons that trigger dynamic UI changes, and also
#' because the internal representation of the data must be changed without any
#' output change. (More about the latter later.)
#' 
#' So again, this app is event-driven. We specify what should happen whenever
#' the user presses the various action buttons, or edits the roles.
#' 
#' The first event is a change in the selected employee. Should this happen,
#' we
#' 
#' 1. set `data` and `dbdata` to the roles of the selected employee, and
#' 2. trigger a UI rebuild.
#' 
#' For simplicity, we assume that each employee's data is stored in a CSV file
#' that is named according to the employee. It is easy to change this to a
#' proper database query.
#' 
  observeEvent(input$employee, {
    filename <- paste0(input$employee, ".csv")
    d <- read.csv(filename, stringsAsFactors = FALSE)
    rvs$data <- rvs$dbdata <- d
    rvs$dataSame <- TRUE
    rvs$recordState <- rvs$recordState + 1
  })

#' 
#' Next event is adding a new role. We create a new id for it first, then just
#' add it to the bottom of the data frame that holds the data. Then we trigger
#' a UI rebuild. After adding a new role, it is highly unlikely that `data`
#' and `dbdata` would be the same, but we check for it, nevertheless.
#' 
  observeEvent(input$add, {
    newid <- if (nrow(rvs$data) == 0) {
      1
    } else {
      max(as.numeric(rvs$data$id)) + 1
    }
    rvs$data <- rbind(
      rvs$data,
      list(id = newid, role = "")
    )
    rvs$recordState <- rvs$recordState + 1
    rvs$dataSame <- identical(rvs$data, rvs$dbdata)
  })

#' 
#' When the `Cancel` button is hit, we need to restore the data from the saved
#' `dbdata`. It is possible that somebody else modified the roles of this
#' employee since we last read it from the database, so reading the data in
#' again is an alternative to this.
#' 
#' Then we trigger a UI rebuild. This is not always needed, but it is the
#' simplest way to make sure that the UI shows the current data.
#' 
  observeEvent(input$cancel, {
    rvs$data <- rvs$dbdata
    rvs$dataSame <- TRUE
    rvs$recordState <- rvs$recordState + 1
  })

#' 
#' The `Save` button is also simple. We write out the file, and set `dbdata`
#' to `data`. No UI rebuild is needed in this case.
#' 
  observeEvent(input$save, {
    filename <- paste0(input$employee, ".csv")
    write.csv(rvs$data, filename, quote = FALSE, row.names = FALSE)
    rvs$dbdata <- rvs$data
    rvs$dataSame <- TRUE
  })

#' 
#' ### The main dynamic UI
#' 
#' The next part is rebuilding the main UI, that contains the employee
#' roles. The dummy `rvs$recordState` expression makes sure that we always
#' rebuild the UI, whenever a rebuild was triggered by changing the
#' `rvs$recordState` value.
#' 
#' Note the use of `isolate`. We do not want `output$roles` to depend on
#' `rvs$data` directly, because we only want to rebuild the UI after selected
#' events, but not all data changes.
#' 
#' We use `create_role` to create the UI and (possibly) the event wiring for
#' each role. Its first argument is the widget id, a number between `1` and
#' `n`, where `n` is the number of roles on the screen.
#' 
  output$roles <- renderUI({
    rvs$recordState
    mydata <- isolate(rvs$data)
    w <- lapply(seq_len(nrow(mydata)), function(i) {
      create_role(i, mydata[i, ])
    })
    do.call(fluidRow, w)
  })

#' 
#' ### Creating the UI for a role
#' 
#' This is the key of the whole app, and it is also the part that is easy to
#' write incorrectly. `create_role` is a closure, a function that creates both
#' a function and an environment to store data.
#' 
#' We need the environment to store the maximum number of widgets that were
#' wired up with edit and delete events. We need this because in Shiny it is
#' not (easily) possible to remove bindings (i.e. `observeEvent` triggers). So
#' even if we rebuild the UI, and remove some elements, the previously created
#' triggers will be still alive, and recreating them will trigger duplicate
#' events.
#' 
  create_role <- (function() {

    inited <- 0

    function(wid, record) {
      w <- div(wellPanel(
        textInput(
          paste0("inp-", wid),
          label = record$id,
          value = record$role
        ),
        actionButton(
          paste0("del-", wid),
          label = "Delete",
          class = "btn-danger"
        )
      ))

#' 
#' So every time we build a widget with a given id (`wid`) number, we check if
#' the widget's events were already wired up, and create new `observeEvent`
#' triggers if they were not.
#' 
#' In other words, the newly built UI will reuse as many of the existing wired
#' widgets as possible. `inited` stores the number of `wired` widgets and the
#' ids of their inputs and delete buttons are `inp-x` and `del-x`, where `x`
#' is a number between 1 and `inited`.
#' 
#' Note that editing the text input field does not trigger a UI rebuild, and
#' this is intentional. We don't want rebuilds just because the user has typed
#' in something new in the input field.
#' 
      if (wid > inited) {
        observeEvent(input[[paste0("inp-", wid)]], {
          rvs$data[wid, "role"] <- input[[paste0("inp-", wid)]]
          rvs$dataSame <- identical(rvs$data, rvs$dbdata)
        })

        observeEvent(input[[paste0("del-", wid)]], {
          rvs$data <- rvs$data[-wid, , drop = FALSE]
          rvs$recordState <- rvs$recordState + 1
          rvs$dataSame <- identical(rvs$data, rvs$dbdata)
        })

#' 
#' We need to update `inited` if we created wiring for a new widget.
#' 
	    inited <<- wid
      }

      w
    }
  })()
}

shinyApp(ui, server)

#' 
#' This construct create a new function, that we assign to
#' `create_role`. `create_role` has access to `inited`, the number of widgets
#' wired up already, and it updates `inited` as needed.
#' 
#' ### Request for comments
#' 
#' It took me a couple of days writing this small Shiny app. I don't claim
#' that this is the simplest or best way to handle this use case, but it is
#' the only approach that worked for us so far, in three larger Shiny apps.
#' 
#' This said, I would be very excited to hear about alternative
#' solutions. Should you have one, please open an issue in the
#' https://github.com/MangoTheCat/dynshiny repository. Thank you!
#' 
#' ### Try the app
#' 
#' TODO

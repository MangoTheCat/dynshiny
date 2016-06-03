
library(shiny)

ui <- basicPage(
  fluidRow(
    actionButton(inputId = "add_button", label = "Add Button")
  ),
  uiOutput("more_buttons")
)

server <- function(input, output){

  rvs <- reactiveValues(
    buttons = list()
  )

  observeEvent(
    input$add_button,
    {
      len <- length(rvs$buttons) + 1
      rvs$buttons[[len]] <- div(
        actionButton(inputId = paste0("button", len), label = len),
        actionButton(
          inputId = paste0("del_button", len),
          label = paste0("D", len)
        )
      )
    }
  )

  output$more_buttons <- renderUI({
    do.call(fluidRow, rvs$buttons)
  })

  for (ii in 1:10){
    local({
      i <- ii
      observeEvent(input[[paste0("button", i)]], { print(i) })
      observeEvent(
        input[[paste0("del_button", i)]],
        { rvs$buttons[[i]] <- NULL }
      )
    })

  }

}

shinyApp(ui, server)

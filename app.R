
library(shiny)

ui <- basicPage(
  fluidRow(
    actionButton(inputId = "add_button", label = "Add Button"),
    actionButton(inputId = "del_button", label = "Delete Button")
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
      rvs$buttons[[len]] <- actionButton(inputId = paste0("button",len),
                                         label = len)
    }
  )

  observeEvent(
    input$del_button,
    {
      len <- length(rvs$buttons)
      rvs$buttons[[len]] <- NULL
    }
  )
  
  output$more_buttons <- renderUI({
    do.call(fluidRow, rvs$buttons)
  })

  for(ii in 1:10){
    local({
      i <- ii
      observeEvent(eventExpr = input[[paste0("button",i)]],
                   handlerExpr = {print(i)})
    })

  }

}

shinyApp(ui, server)

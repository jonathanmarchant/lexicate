library(shiny)
library(shinyjs)

ui <- fluidPage(
  tags$head(
      tags$style(
        "body{
    max-width: 500px;
    margin: auto;
        }"
      )
    ),
  shinyjs::useShinyjs(),
  wellPanel(
    h2(textOutput("instruction")),
    textInput("attempt", "Type the word here", ""),
    actionButton("doneButton", "Done"),
    actionButton("nextButton", "Next", disabled=TRUE),
    hidden(textOutput("outcome"))
  )

)

server <- function(input, output, session) {

  target_word <- reactive({ 
    input$nextButton
    sample(wordlist, 1)
  })
  output$instruction <- renderText({ c("Spell ", target_word()) })

  outcome_text <- eventReactive(input$doneButton, {
    dplyr::if_else(input$attempt == target_word(), "Correct", "Incorrect")
})
  
  output$outcome <- renderText({
    outcome_text()
})
  
  observeEvent(input$doneButton, {
    show("outcome")
    if(input$attempt == target_word()) {
      enable("nextButton")
    }
  })

  observeEvent(input$nextButton, {
    disable("nextButton")
    updateTextInput(session, "attempt", value = "")
    hide("outcome")
  })


}

shinyApp(ui, server)

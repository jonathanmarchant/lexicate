library(shiny)
library(shinyjs)
library(htmltools)
library(bslib)

jscode_enter <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

ui <- fluidPage(
  theme = bs_theme(
    bg = "#fffc36",
    fg = "#ff0000",
    primary = "#ffd501",
    secondary = "#000000",
    success = "#009E73",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),

  tags$head(tags$script(HTML(jscode_enter))),
  tags$head(
      tags$style(
        "body{
    max-width: 500px;
    margin: auto;
        }"
      )
    ),
  shinyjs::useShinyjs(),
  includeScript("www/howler.js"),
  wellPanel(
    hidden(h2(textOutput("instruction"), id="h_instruction")),
    tagQuery(
      textInput("attempt", "Type the word here", "")
    )$find("input")$addAttrs("autocomplete" = "off", "autocapitalize" = "none", "spellcheck" = "false", "data-proxy-click" = "doneButton")$allTags(),
    actionButton("doneButton", "Done"),
    actionButton("nextButton", "Next", disabled=TRUE),
    hidden(textOutput("outcome")),

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
    } else {
      show("h_instruction")
    }
  })

  observeEvent(input$nextButton, {
    disable("nextButton")
    updateTextInput(session, "attempt", value = "")
    hide("outcome")
    hide("h_instruction")
  })

  observeEvent(target_word(), {
    new_js <- stringr::str_c("var music = new Howl({src: ['", target_word(), ".m4a']});  music.play();")
    shinyjs::runjs(new_js)
  })


}

shinyApp(ui, server)

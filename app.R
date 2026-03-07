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
    code_font = font_google("JetBrains Mono"),
    "min-contrast-ratio" = 1.1
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
    hidden(textOutput("outcome"))
  ),
  wellPanel(
    textOutput("ticks"),
    textOutput("crosses")
  )

)

server <- function(input, output, session) {
  counters <- reactiveValues(correct = 0, incorrect = 0, assistance = 0)
  state <- reactiveValues(user="jrm")

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
  
  observeEvent(counters$assistance, {
    if (counters$assistance == 0) {
      hide("h_instruction")
    } else {
      show("h_instruction")
    }
  })
  
  observeEvent(input$doneButton, {
    show("outcome")
    if(input$attempt == target_word()) {
      write_word_log(target_word(), counters$assistance, state$user, 1)
      counters$correct <- counters$correct + 1
      enable("nextButton")
      disable("doneButton")
    } else {
      write_word_log(target_word(), counters$assistance, state$user, 0)
      counters$incorrect <- counters$incorrect + 1
      counters$assistance = counters$assistance + 1
    }
  })

  observeEvent(input$nextButton, {
    counters$assistance <- 0
    disable("nextButton")
    updateTextInput(session, "attempt", value = "")
    hide("outcome")
    enable("doneButton")
  })

  observeEvent(target_word(), {
    new_js <- stringr::str_c("var music = new Howl({src: ['", target_word(), ".m4a']});  music.play();")
    shinyjs::runjs(new_js)
  })

  output$ticks <- renderText(
    stringr::str_c("Correct: ", strrep("🚂", counters$correct))
  )

  output$crosses <- renderText(
    stringr::str_c("Incorrect: ", strrep("🚓", counters$incorrect))
  )


}

shinyApp(ui, server)

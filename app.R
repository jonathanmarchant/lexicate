library(shiny)
library(shinyjs)
library(htmltools)
library(bslib)
library(gt)

options(bslib.color_contrast_warnings = FALSE)

loaded_user <- dplyr::if_else(Sys.getenv("LEXICATE_USER") == "", "jrm", Sys.getenv("LEXICATE_USER"))
wordlist <- create_wordlist(con)

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
    tagQuery(
      textInput("attempt", "Type the word here", "")
    )$find("input")$addAttrs("autocomplete" = "off", "autocapitalize" = "none",
      "spellcheck" = "false", "data-proxy-click" = "doneButton", "class" = "form-control-lg")$allTags(),
    actionButton("doneButton", "Done", class="btn-lg btn-danger"),
    actionButton("nextButton", "Next", disabled=TRUE, class="btn-lg btn-danger"),
    actionButton("replayButton", "🗣️", class="btn-lg"),
    actionButton("sentenceButton", "Sentence", class="btn-lg"),
    p(),
    hidden(h3(textOutput("sentence"), id="h_sentence", style="color:black")),
    h2(textOutput("instruction"), id="h_instruction")
  ),
  wellPanel(
    textOutput("ticks"),
    textOutput("crosses")
  ) #,
  #wellPanel(
  #  gt_output("progress_table")
  #)

)

server <- function(input, output, session) {
  counters <- reactiveValues(correct = 0, incorrect = 0)
  session_state <- reactiveValues(user = loaded_user, local_word_log = get_word_log(con, wordlist, loaded_user))
  task_state <- reactiveValues(assistance = 0, answer_found = 0)

  target_word <- reactive({ 
    input$nextButton
    isolate(session_state$local_word_log) |> 
      summarise_word_log(wordlist) |> 
      choose_word()
  })

  jumbled_word <- reactive({
    stringi::stri_rand_shuffle(target_word())
  })

  outcome_text <- eventReactive(input$doneButton, {
    dplyr::if_else(input$attempt == target_word(), "Correct", "Incorrect")
})

  output$instruction <- renderText({
    dplyr::case_when(    
      task_state$answer_found == 1 ~ "Correct",
      task_state$assistance >= 2 ~ stringr::str_c("Answer: ", target_word()),
      task_state$assistance == 1 ~ stringr::str_c("Unjumble: ", jumbled_word()),
      .default = "Spell the word"
  )
  })

  output$sentence <- renderText({ context_sentence(target_word(), wordlist) })

  observeEvent(task_state$answer_found, {
    if (task_state$answer_found == 0) {
      enable("doneButton")
      disable("nextButton")
    } else {
      disable("doneButton")
      enable("nextButton")
    }
  })
  
  observeEvent(input$doneButton, {
    if (task_state$answer_found == 1) {
      # Do nothing, this happened due to Javascript lag
    } else if(input$attempt == target_word()) {
      session_state$local_word_log <- write_word_log(con, target_word(), task_state$assistance, session_state$user, 1, session_state$local_word_log)
      counters$correct <- counters$correct + 1
      task_state$answer_found <- 1
    } else {
      session_state$local_word_log <- write_word_log(con, target_word(), task_state$assistance, session_state$user, 0, session_state$local_word_log)
      counters$incorrect <- counters$incorrect + 1
      task_state$assistance = task_state$assistance + 1
    }
  })

  observeEvent(input$nextButton, {
    task_state$assistance <- 0
    task_state$answer_found <- 0
    updateTextInput(session, "attempt", value = "")
  })

  observeEvent(input$replayButton, {
    replay_js <- "window.wordsound.play();"
    shinyjs::runjs(replay_js)
  })

  observeEvent(input$sentenceButton, {
    toggle("h_sentence")
  })

  observeEvent(target_word(), {
    if(is_homophone(target_word(), wordlist)) {
      show("h_sentence")
    } else {
      hide("h_sentence")
    }

    new_js <- stringr::str_c("window.wordsound = new Howl({src: ['", stringr::str_to_lower(target_word()) , ".m4a']});  window.wordsound.play();")
    shinyjs::runjs(new_js)
  })

  output$ticks <- renderText(
    stringr::str_c("Correct: ", strrep("🚂", counters$correct))
  )

  output$crosses <- renderText(
    stringr::str_c("Incorrect: ", strrep("🚓", counters$incorrect))
  )

  #output$progress_table <- render_gt({
  #  target_word()
  #  user_summary(con, session_state$user, session_state$local_word_log)
  #})
  }

shinyApp(ui, server)

# app.R
library(shiny)
library(bslib)
library(shinyvalidate)
library(glue)

generate_story <- function(noun, verb, adjective, adverb) {
  glue(
    "Once upon a time, there was a {adjective} {noun} who loved to ",
    "{verb} {adverb}. It was the funniest thing ever!"
  )
}

ui <- page_sidebar(
  title = "Mad Libs (bslib + live + validation + stderr logging)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    h4("Fill in the blanks"),
    textInput("noun1",     "Noun",      placeholder = "e.g., cat"),
    textInput("verb",      "Verb",      placeholder = "e.g., dance"),
    textInput("adjective", "Adjective", placeholder = "e.g., goofy"),
    textInput("adverb",    "Adverb",    placeholder = "e.g., loudly"),
    helpText("Tip: letters, spaces, dashes, and apostrophes are allowed.")
  ),
  card(
    card_header("Your Mad Libs Story"),
    card_body(
      textOutput("story")
    )
  )
)

server <- function(input, output, session) {
  ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Log helpers -> send to STDERR
  log_input <- function(id, value) {
    cat(sprintf("[%s] INPUT %s: '%s'\n", ts(), id, value), file = stderr())
  }
  log_msg <- function(msg) {
    cat(sprintf("[%s] %s\n", ts(), msg), file = stderr())
  }

  log_msg("Session started.")

  # --- Validation setup ---
  iv <- InputValidator$new()
  iv$add_rule("noun1",     sv_required(message = "Please enter a noun."))
  iv$add_rule("verb",      sv_required(message = "Please enter a verb."))
  iv$add_rule("adjective", sv_required(message = "Please enter an adjective."))
  iv$add_rule("adverb",    sv_required(message = "Please enter an adverb."))

  # Base R/TRE-safe pattern (no perl=TRUE)
  letters_spaces_rule <- function(value) {
    if (!nzchar(value)) return(NULL) # required handles empties
    ok <- grepl("^[[:alpha:]][[:alpha:][:space:]'-]*$", value)
    if (!ok) "Use letters, spaces, dashes (-), or apostrophes (')."
  }
  for (id in c("noun1", "verb", "adjective", "adverb")) {
    iv$add_rule(id, letters_spaces_rule)
  }
  iv$enable()

  # --- Logging: log every input change ---
  ids <- c("noun1", "verb", "adjective", "adverb")
  lapply(ids, function(id) {
    observeEvent(input[[id]], {
      log_input(id, input[[id]])
    }, ignoreInit = TRUE)
  })

  # Optional: log validation status whenever inputs change
  observe({
    log_msg(sprintf("Validation status: %s", if (iv$is_valid()) "VALID" else "INVALID"))
  })

  # --- Live story generation (no button) ---
  story <- reactive({
    req(iv$is_valid())
    generate_story(input$noun1, input$verb, input$adjective, input$adverb)
  })

  # Log when a new valid story is produced
  observeEvent(story(), {
    log_msg(sprintf("STORY UPDATED -> %s", story()))
  }, ignoreInit = TRUE)

  output$story <- renderText({
    validate(need(iv$is_valid(), "Fill in all fields with valid text to see your story."))
    story()
  })

  session$onSessionEnded(function() {
    log_msg("Session ended.")
  })
}

shinyApp(ui, server)

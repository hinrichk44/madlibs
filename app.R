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
  title = "Mad Libs (bslib + live + validation)",
  theme = bs_theme(version = 5),  # pick a bootswatch if you like, e.g., bootswatch = "minty"
  sidebar = sidebar(
    h4("Fill in the blanks"),
    textInput("noun1",     "Noun",      placeholder = "e.g., cat"),
    textInput("verb",      "Verb",      placeholder = "e.g., dance"),
    textInput("adjective", "Adjective", placeholder = "e.g., goofy"),
    textInput("adverb",    "Adverb",    placeholder = "e.g., loudly"),
    # No button — updates live
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
  # --- Validation setup ---
  iv <- InputValidator$new()

  # Required fields
  iv$add_rule("noun1",     sv_required(message = "Please enter a noun."))
  iv$add_rule("verb",      sv_required(message = "Please enter a verb."))
  iv$add_rule("adjective", sv_required(message = "Please enter an adjective."))
  iv$add_rule("adverb",    sv_required(message = "Please enter an adverb."))

  # Allow letters, spaces, dashes, apostrophes (simple, friendly guardrails)
  letters_spaces_rule <- function(value) {
    if (!nzchar(value)) return(NULL)
    # Start with a letter; then letters, spaces, apostrophes, or dashes
    ok <- grepl("^[[:alpha:]][[:alpha:][:space:]'-]*$", value)
    if (!ok) "Use letters, spaces, dashes (-), or apostrophes (')."
  }

  for (id in c("noun1", "verb", "adjective", "adverb")) {
    iv$add_rule(id, letters_spaces_rule)
  }

  # Turn on real-time validation UI
  iv$enable()

  # --- Live story generation (no button) ---
  story <- reactive({
    req(iv$is_valid())  # only proceed when inputs are valid
    generate_story(input$noun1, input$verb, input$adjective, input$adverb)
  })

  output$story <- renderText({
    validate(need(iv$is_valid(), "Fill in all fields with valid text to see your story."))
    story()
  })
}

shinyApp(ui, server)

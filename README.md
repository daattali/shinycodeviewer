# shinycodeviewer

View and edit a series of code chunks with syntax highlighting in Shiny

### Example 1: Allow editing all lines, skip first 2 lines, use automatic actions

```r
library(shiny)
library(shinycodeviewer)

init_code <- list(
  "library(somepackage)",
  "library(anotherpkg)",
  "",
  "# a comment",
  "x <- 5",
  "y <- x %>%\n  sum()",
  "z <- function(num) {\n  num*2\n}",
  "x - y / z"
)

ui <- fluidPage(
  fluidRow(
    column(
      6,
      h1("Editable code chunks"),
      code_viewer_ui("code")
    ),
    column(
      6,
      h1("Result:"),
      verbatimTextOutput("out")
    )
  )
)

server <- function(input, output, session) {
  chunks <- code_viewer_server("code", chunks = init_code,
    editable = TRUE,skip = 2, auto_actions = TRUE)
  
  output$out <- renderText({
    paste(
      paste(seq_along(chunks()), ":", chunks()),
      collapse = "\n"
    )
  })
}

shinyApp(ui, server)
```

### Example 2: Let user choose which lines are editable, how many lines to skip, which line shows an error, and use custom actions

```r
library(shiny)
library(shinycodeviewer)

init_code <- list(
  "library(somepackage)",
  "library(anotherpkg)",
  "",
  "# a comment",
  "x <- 5",
  "y <- x %>%\n  sum()",
  "z <- function(num) {\n  num*2\n}",
  "x - y / z"
)

ui <- fluidPage(
  selectInput("editable", "Chunks to edit", multiple = TRUE,
    choices = seq_along(init_code), selected = seq_along(init_code)),
  sliderInput("skip", "Number of chunks to skip", min = 0, 
    max = length(init_code), value = 0, step = 1),
  sliderInput("error", "Chunk number that shows an error", min = 0,
    max = length(init_code), value = 0, step = 1),
  code_viewer_ui("code")
)

server <- function(input, output, session) {
  code <- code_viewer_server("code", chunks = init_code, auto_actions = FALSE,
    editable = reactive(input$editable), skip = reactive(input$skip), error_line = reactive(input$error))
  
  observeEvent(code$insert(), {
    shinyalert::shinyalert(paste("Insert before chunk", code$insert()), closeOnClickOutside = TRUE)
  })
  observeEvent(code$modify(), {
    shinyalert::shinyalert(paste("Modify chunk", code$modify()), closeOnClickOutside = TRUE)
  })
  observeEvent(code$delete(), {
    shinyalert::shinyalert(paste("Delete chunk", code$delete()), closeOnClickOutside = TRUE)
  })
}

shinyApp(ui, server)
```


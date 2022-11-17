#' Code viewer display area
#'
#' Display a series of code chunks with syntax highlighting. Each chunk can optionally
#' have Insert/Modify/Delete buttons. One chunk can be shown as an error/bug.
#' @param id Unique ID for the module
#' @examples
#' if (interactive()) {
#'
#' ## Example 1: Allow editing all lines, skip first 2 lines, use automatic actions
#' library(shiny)
#' library(shinycodeviewer)
#'
#' init_code <- list(
#'   "library(somepackage)",
#'   "library(anotherpkg)",
#'   "",
#'   "# a comment",
#'   "x <- 5",
#'   "y <- x %>%\n  sum()",
#'   "z <- function(num) {\n  num*2\n}",
#'   "x - y / z"
#' )
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(
#'       6,
#'       h1("Editable code chunks"),
#'       code_viewer_ui("code")
#'     ),
#'     column(
#'       6,
#'       h1("Result:"),
#'       verbatimTextOutput("out")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   chunks <- code_viewer_server("code", chunks = init_code,
#'                                editable = TRUE,skip = 2, auto_actions = TRUE)
#'
#'   output$out <- renderText({
#'     paste(
#'       paste(seq_along(chunks()), ":", chunks()),
#'       collapse = "\n"
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' ## -----------------------------------
#'
#' ## Example 2: Let user choose which lines are editable, how many lines to skip, which line shows an error, and use custom actions
#' library(shiny)
#' library(shinycodeviewer)
#'
#' init_code <- list(
#'   "library(somepackage)",
#'   "library(anotherpkg)",
#'   "",
#'   "# a comment",
#'   "x <- 5",
#'   "y <- x %>%\n  sum()",
#'   "z <- function(num) {\n  num*2\n}",
#'   "x - y / z"
#' )
#'
#' ui <- fluidPage(
#'   selectInput("editable", "Chunks to edit", multiple = TRUE,
#'     choices = seq_along(init_code), selected = seq_along(init_code)),
#'   sliderInput("skip", "Number of chunks to skip", min = 0,
#'     max = length(init_code), value = 0, step = 1),
#'   sliderInput("error", "Chunk number that shows an error", min = 0,
#'     max = length(init_code), value = 0, step = 1),
#'   code_viewer_ui("code")
#' )
#'
#' server <- function(input, output, session) {
#'   code <- code_viewer_server("code", chunks = init_code, auto_actions = FALSE,
#'     editable = reactive(input$editable), skip = reactive(input$skip), error_line = reactive(input$error))
#'
#'   observeEvent(code$insert(), {
#'     shinyalert::shinyalert(paste("Insert before chunk", code$insert()), closeOnClickOutside = TRUE)
#'   })
#'   observeEvent(code$modify(), {
#'     shinyalert::shinyalert(paste("Modify chunk", code$modify()), closeOnClickOutside = TRUE)
#'   })
#'   observeEvent(code$delete(), {
#'     shinyalert::shinyalert(paste("Delete chunk", code$delete()), closeOnClickOutside = TRUE)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @name code_viewer
NULL

#' @rdname code_viewer
#' @param height The height of the code viewer. Must be a valid CSS unit (like `"100%"`,
#' `"400px"`, `"auto"`) or a number which will be the number of pixels.
#' @param width The width of the code viewer. Must be a valid CSS unit (like `"100%"`,
#' `"400px"`, `"auto"`) or a number which will be the number of pixels.
#' @export
code_viewer_ui <- function(id, height = NULL, width = "100%") {
  ns <- shiny::NS(id)

  width_css <- paste0("width: ", htmltools::validateCssUnit(width), ";")
  height_css <- if (!is.null(height)) paste0("height: ", htmltools::validateCssUnit(height)) else ""
  style <- paste(width_css, height_css)

  shiny::tagList(
    shinyalert::useShinyalert(force = TRUE),
    fontawesome::fa_html_dependency(),

    htmltools::htmlDependency(
      "highlight.js",
      "6.2",
      src = "assets/highlight",
      package = "shinycodeviewer",
      script = "highlight.pack.js",
      stylesheet = "highlight.css"
    ),

    htmltools::htmlDependency(
      name = "shinycodeviewer-binding",
      version = as.character(utils::packageVersion("shinycodeviewer")),
      package = "shinycodeviewer",
      src = "assets/shinycodeviewer",
      stylesheet = "shinycodeviewer.css"
    ),

    shiny::div(
      style = style,
      class = "shiny-code-viewer",
      shiny::uiOutput(ns("code_section"))
    )
  )
}

#' @rdname code_viewer
#' @param chunks (reactive or static) List of code chunks, where each code chunk is a string.
#' A single chunk is also acceptable. A code chunk can consist of multiple lines.
#' @param editable (reactive or static) Vector of chunk numbers that are editable, or TRUE to make everything editable.
#' @param error_line (reactive or static) Chunk number that should be shown as the error.
#' @param skip (reactive or static) Number of lines to skip in the numbering system, essentially ignoring those lines
#' for interaction purposes but still showing them. For example, if there are 10 lines in total and `skip = 3` and a user
#' clicks on insert/modify/delete on the 5th line, then the module will report it as the second line (5 - 3 = 2).
#' Similarly, if `skip = 3` and `error_line = 5`, then it will appear as if the 8th line is the error (because the
#' first 3 don't count).
#' @param auto_actions (boolean) If `TRUE`, clicking on an action (insert/modify/delete) will be handled
#' automatically by {shinycodeviewer}. If `FALSE`, clicking these actions will not trigger
#' any action, and you will need to implement a custom action by listening to the module's return
#' values.
#' @return If `auto_actions == TRUE`, the return value is a reactive list of the current code chunks displayed.\cr\cr
#' If `auto_actions == FALSE`, a list with reactive elements corresponding to user interactions is returned
#' to allow you to respond to these actions:
#'   - insert: The chunk number the user wants to insert before
#'   - modify: The chunk number the user wants to modify
#'   - delete: The chunk number the user wants to delete
#' @export
code_viewer_server <- function(id, chunks = NULL, editable = NULL, error_line = NULL, skip = NULL, auto_actions = TRUE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      chunks_r <- make_reactive(chunks)
      error_line_r <- make_reactive(error_line)
      editable_r <- make_reactive(editable)
      skip_r <- make_reactive(skip)

      chunks_current <- shiny::reactiveVal(NULL)
      shiny::observeEvent(chunks_r(), {
        chunks_current(chunks_r())
      })

      skip_num <- shiny::reactive({
        if (is.null(skip_r())) {
          0
        } else {
          skip_r()
        }
      })

      output$code_section <- shiny::renderUI({
        if (length(chunks_current()) == 0 || (length(chunks_current()) == 1 && chunks_current() == "")) {
          return()
        }

        chunks_html <- lapply(seq_along(chunks_current()), function(chunk_idx) {
          chunk <- chunks_current()[[chunk_idx]]

          chunk_idx <- chunk_idx - skip_num()

          error <- (!is.null(error_line_r()) && error_line_r() == chunk_idx)

          if (chunk_idx < 1) {
            edit <- FALSE
          } else if (!is.null(editable_r()) && isTRUE(editable_r())) {
            edit <- TRUE
          } else {
            edit <- chunk_idx %in% editable_r()
          }

          if (is.null(chunk) || chunk == "") {
            chunk <- " "
          }

          if (error) {
            chunk <- paste0(
              chunk,
              "<i class='fa fa-bug chunk-error-icon'></i>"
            )
          }
          chunk_html <- shiny::tags$pre(shiny::HTML(as.character(shiny::tags$div(HTML(chunk), class = "language-r hl-me"))))

          onclick_tpl <- function(action) {
            glue::glue(
              "Shiny.setInputValue('{{ session$ns(action) }}', `${event.target.closest('.chunk-btns').dataset.chunkNum}`, {priority: 'event'});",
              .open = "{{", .close = "}}"
            )
          }

          shiny::tags$div(
            chunk_html,
            if (edit) shiny::tags$div(
              class = "chunk-btns",
              `data-chunk-num` = chunk_idx,
              shiny::tags$i(
                title = "Insert before this line",
                class = "fa fa-share chunk-btn fa-flip-horizontal fa-fw",
                onclick = onclick_tpl("insert")
              ),
              shiny::tags$i(
                title = "Modify",
                class = "fa fa-pen chunk-btn fa-fw",
                onclick = onclick_tpl("modify")
              ),
              shiny::tags$i(
                title = "Delete",
                class = "fa fa-trash-alt chunk-btn fa-fw",
                onclick = onclick_tpl("delete")
              )
            ),
            class = "shiny-code-chunk",
            class = if (edit) "chunk-editable",
            class = if (error) "chunk-error"
          )
        })

        shiny::tagList(
          shiny::tags$div(class = "shiny-code-chunks", chunks_html),
          shiny::HTML(as.character(shiny::tags$script(
            glue::glue(
              'document.querySelectorAll("#{{ session$ns("code_section") }} .shiny-code-chunks .hl-me").forEach(function(el) { hljs.highlightBlock(el); })',
              .open = "{{", .close = "}}"
            )
          )))
        )
      })

      if (auto_actions) {
        shiny::observeEvent(input$insert, {
          shinyalert::shinyalert(
            title = "Insert code",
            text = shiny::textAreaInput(
              session$ns("shinycodechunk_insert_code"),
              NULL,
              "",
              rows = 3
            ),
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = TRUE,
            showCancelButton = TRUE,
            inputId = 'shinycodechunk_insert_click',
            confirmButtonText = "Insert"
          )
        })

        shiny::observeEvent(input$shinycodechunk_insert_click, {
          chunk_idx <- as.integer(input$insert) + skip_num()
          old_chunks <- chunks_current()
          new_chunks <- append(old_chunks, input$shinycodechunk_insert_code, after = chunk_idx - 1)
          chunks_current(new_chunks)
        })

        shiny::observeEvent(input$modify, {
          chunk_idx <- as.integer(input$modify) + skip_num()
          shinyalert::shinyalert(
            title = "Modify code",
            text = shiny::textAreaInput(
              session$ns("shinycodechunk_modify_code"),
              NULL,
              chunks_current()[chunk_idx],
              rows = 3
            ),
            closeOnEsc = TRUE,
            closeOnClickOutside = TRUE,
            html = TRUE,
            showCancelButton = TRUE,
            inputId = 'shinycodechunk_modify_click',
            confirmButtonText = "Modify"
          )
        })

        shiny::observeEvent(input$shinycodechunk_modify_click, {
          chunk_idx <- as.integer(input$modify) + skip_num()
          new_chunks <- chunks_current()
          new_chunks[chunk_idx] <- input$shinycodechunk_modify_code
          chunks_current(new_chunks)
        })


        shiny::observeEvent(input$delete, {
          chunk_idx <- as.integer(input$delete) + skip_num()
          old_chunks <- chunks_current()
          new_chunks <- old_chunks[-chunk_idx]
          chunks_current(new_chunks)
        })
      }

      if (auto_actions) {
        return(shiny::reactive(chunks_current()))
      } else {
        return(list(
          insert = shiny::reactive(shiny::req(as.integer(input$insert))),
          modify = shiny::reactive(shiny::req(as.integer(input$modify))),
          delete = shiny::reactive(shiny::req(as.integer(input$delete)))
        ))
      }
    }
  )
}

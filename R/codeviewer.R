#' Code chunk display area
#'
#' Display a series of code chunks with syntax highlighting. Each chunk can optionally
#' have Insert/Modify/Delete buttons. One chunk can be shown as an error/bug.
#' @param id Unique ID for the module
#' @import shiny
#' @name code_chunk
NULL

#' @rdname code_chunk
code_chunk_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyalert::useShinyalert(force = TRUE),
    fontawesome::fa_html_dependency(),

    singleton(tags$head(tags$style(HTML(
      ".lca-code-chunks { padding: 0.8rem; background: #fafafa; border: 1px solid #ececec; }
       .lca-code-chunks pre { background: inherit; border: 0; padding: 0; margin: 0; }
       .lca-code-chunk { position: relative; padding: 0.2rem; border-radius: 4px; transition: background 0.25s; }
       .lca-code-chunk:hover { background: #eee; }
       .lca-code-chunk .chunk-btns { position: absolute; top: 0; right: 0.2rem; color: #444; opacity: 0; transition: opacity 0.25s; font-size: 1.2em; }
       .lca-code-chunk:hover .chunk-btns { opacity: 1; }
       .lca-code-chunk .chunk-btn { opacity: 0.5; cursor: pointer; transition: opacity 0.25s; margin: 0 .3rem; }
       .lca-code-chunk .chunk-btn:hover { opacity: 1; }
       .lca-code-chunk.chunk-error { background: #f2dede; }
       .lca-code-chunk.chunk-error:hover { background: #f1d0d0; }
       .lca-code-chunk .chunk-error-icon { color: #d2322d; margin-left: 4px; }"
    )))),

    htmltools::htmlDependency(
      "highlight.js",
      "6.2",
      src = "assets/highlight",
      package = PACKAGE_NAME,
      script = "highlight.pack.js",
      stylesheet = "highlight.css"
    ),

    uiOutput(ns("code_section"))
  )
}

#' @rdname code_chunk
#' @param chunks (reactive or static) List of code chunks, where each code chunk is a string.
#' A single chunk is also acceptable. A code chunk can consist of multiple lines.
#' @param editable (reactive or static) Vector of chunk numbers that are editable, or TRUE to make everything editable
#' @param error_line (reactive or static) Chunk number that should be shown as the error.
#' @param skip (reactive or static) Number of lines to skip in the numbering system, essentially ignoring those lines
#' for all purposes but still showing them. For example, if there are 10 lines in total and `skip = 3` and a user
#' clicks on insert/modify/delete on the 5th line, then the module will report it as the second line (5 - 3 = 2).
#' Similarly, if `skip = 3` and `error_line = 5`, then it will appear as if the 8th line is the error (because the
#' first 3 don't count).
#' @param actions_default (boolean) If `TRUE`, the editable actions (insert/modify/delete) will be
#' be handled automatically by {shinycodechunk}. If `FALSE`, clicking these actions will not trigger
#' any action, and you will need to implement a custom action by listening to the module's return
#' values.
#' @return List with reactive elements corresponding to user interactions:
#'   - insert: The chunk number the user wants to insert before
#'   - modify: The chunk number the user wants to modify
#'   - delete: The chunk number the user wants to delete
#'   - chunks: The current code chunks shown
code_chunk_server <- function(id, chunks = NULL, editable = NULL, error_line = NULL, skip = NULL, actions_default = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {

      chunks_r <- make_reactive(chunks)
      error_line_r <- make_reactive(error_line)
      editable_r <- make_reactive(editable)
      skip_r <- make_reactive(skip)

      chunks_current <- reactiveVal(NULL)
      observeEvent(chunks_r(), {
        chunks_current(chunks_r())
      })

      skip_num <- reactive({
        if (is.null(skip_r())) {
          0
        } else {
          skip_r()
        }
      })

      output$code_section <- renderUI({
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
          chunk_html <- tags$pre(HTML(as.character(tags$div(HTML(chunk), class = "language-r hl-me"))))

          onclick_tpl <- function(action) {
            glue::glue(
              "Shiny.setInputValue('{{ session$ns(action) }}', `${event.target.closest('.chunk-btns').dataset.chunkNum}`, {priority: 'event'});",
              .open = "{{", .close = "}}"
            )
          }

          tags$div(
            chunk_html,
            if (edit) tags$div(
              class = "chunk-btns",
              `data-chunk-num` = chunk_idx,
              tags$i(
                title = "Insert before this line",
                class = "fa fa-share chunk-btn fa-flip-horizontal fa-fw",
                onclick = onclick_tpl("insert")
              ),
              tags$i(
                title = "Modify",
                class = "fa fa-pen chunk-btn fa-fw",
                onclick = onclick_tpl("modify")
              ),
              tags$i(
                title = "Delete",
                class = "fa fa-trash-alt chunk-btn fa-fw",
                onclick = onclick_tpl("delete")
              )
            ),
            class = "lca-code-chunk",
            class = if (edit) "chunk-editable",
            class = if (error) "chunk-error"
          )
        })

        tagList(
          tags$div(class = "lca-code-chunks", chunks_html),
          HTML(as.character(tags$script(
            glue::glue(
              'document.querySelectorAll("#{{ session$ns("code_section") }} .lca-code-chunks .hl-me").forEach(function(el) { hljs.highlightBlock(el); })',
              .open = "{{", .close = "}}"
            )
          )))
        )
      })

      if (actions_default) {
        observeEvent(input$insert, {
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

        observeEvent(input$shinycodechunk_insert_click, {
          chunk_idx <- as.integer(input$insert) + skip_num()
          old_chunks <- chunks_current()
          new_chunks <- append(old_chunks, input$shinycodechunk_insert_code, after = chunk_idx - 1)
          chunks_current(new_chunks)
        })

        observeEvent(input$modify, {
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

        observeEvent(input$shinycodechunk_modify_click, {
          chunk_idx <- as.integer(input$modify) + skip_num()
          new_chunks <- chunks_current()
          new_chunks[chunk_idx] <- input$shinycodechunk_modify_code
          chunks_current(new_chunks)
        })


        observeEvent(input$delete, {
          chunk_idx <- as.integer(input$delete) + skip_num()
          old_chunks <- chunks_current()
          new_chunks <- old_chunks[-chunk_idx]
          chunks_current(new_chunks)
        })
      }

      return(list(
        insert = reactive(req(as.integer(input$insert))),
        modify = reactive(req(as.integer(input$modify))),
        delete = reactive(req(as.integer(input$delete))),
        chunks = reactive(chunks_current())
      ))

    }
  )
}

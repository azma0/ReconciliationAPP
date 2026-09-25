# Distribution Reconciliation Validator (demo)
#
# Partners upload the results of a cash distribution (who was paid, how much,
# when, transaction references). The app checks the file against the
# programme's rules, shows every problem at once, and only writes clean,
# not-yet-reconciled records back to the database. A second tool handles
# partner corrections to household details, with a similarity check on names.
#
# Shiny automatically sources every file in R/ before this file runs:
#   config.R          business rules
#   validate.R        upload validation
#   reconcile.R       matching and write-back
#   name_changes.R    household corrections and Arabic name comparison
#   data_store.R      local CSV stand-in for the ActivityInfo API
#   synthetic_data.R  demo data

library(shiny)
library(DT)

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  warning("Run R in a UTF-8 locale so Arabic names are read correctly.")
}

read_upload <- function(path) {
  read.csv(path, colClasses = "character", na.strings = "",
           check.names = FALSE, encoding = "UTF-8")
}

small_table <- function(df) {
  datatable(df, rownames = FALSE,
            options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
}

ui <- fluidPage(
  titlePanel("Distribution Reconciliation Validator (demo)"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Reconciliation"),
      fileInput("recon_file", "Upload distribution results (.csv)", accept = ".csv"),
      uiOutput("recon_button"),
      hr(),
      h4("2. Household corrections"),
      fileInput("changes_file", "Upload corrections (.csv)", accept = ".csv"),
      uiOutput("changes_button"),
      hr(),
      h4("Sample files"),
      p("All data is synthetic. Download a sample, then upload it above."),
      downloadButton("dl_valid",   "Clean reconciliation file"), br(), br(),
      downloadButton("dl_errors",  "File with errors"), br(), br(),
      downloadButton("dl_changes", "Household corrections"), br(), br(),
      actionButton("reset", "Reset demo data")
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Reconciliation",
                 br(), uiOutput("recon_message"),
                 h4("Problems found"), DTOutput("recon_issues"),
                 h4("Batch status changes"), DTOutput("batch_status")),
        tabPanel("Household corrections",
                 br(), uiOutput("changes_message"),
                 h4("Requested changes"), DTOutput("changes_table")),
        tabPanel("Database",
                 br(),
                 selectInput("form", "Form",
                             c("distributions", "households", "batches", "change_log")),
                 DTOutput("form_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  config <- recon_config()

  # Each session gets its own copy of the synthetic database
  store_dir <- tempfile("recon_demo_")
  dir.create(store_dir)
  generate_synthetic_data(store_dir)

  state <- reactiveValues(
    recon_issues = NULL, recon_ready = NULL, recon_msg = NULL,
    batch_status = NULL, changes = NULL, changes_msg = NULL,
    refresh = 0          # bumped after every write so tables re-read the store
  )

  # ---- Reconciliation ----------------------------------------------------
  observeEvent(input$recon_file, {
    upload <- read_upload(input$recon_file$datapath)
    batches <- store_read(store_dir, "batches")
    issues <- validate_recon_upload(upload, batches, config)

    state$recon_issues <- issues
    state$batch_status <- NULL
    state$recon_ready  <- NULL

    if (nrow(issues) > 0) {
      state$recon_msg <- paste(nrow(issues),
                               "problem(s) found. Fix the file and upload it again.")
      return()
    }

    prep <- prepare_reconciliation(upload, store_read(store_dir, "distributions"))
    notes <- c(
      sprintf("%d new record(s) ready to reconcile.", nrow(prep$new)),
      if (nrow(prep$already))
        sprintf("%d record(s) already reconciled and will not be changed.",
                nrow(prep$already)),
      if (nrow(prep$not_found))
        sprintf("%d record(s) not found for that QA code and round.",
                nrow(prep$not_found))
    )
    state$recon_msg <- paste(c("No problems found.", notes), collapse = " ")
    if (nrow(prep$new) > 0) state$recon_ready <- prep$new
  })

  output$recon_button <- renderUI({
    req(state$recon_ready)
    actionButton("reconcile", "Reconcile", class = "btn-primary")
  })

  observeEvent(input$reconcile, {
    req(state$recon_ready)
    state$batch_status <- apply_reconciliation(store_dir, state$recon_ready)
    state$recon_msg <- sprintf("%d record(s) reconciled and saved.",
                               nrow(state$recon_ready))
    state$recon_ready <- NULL
    state$refresh <- state$refresh + 1
  })

  output$recon_message <- renderUI({
    req(state$recon_msg)
    tags$div(class = "alert alert-info", state$recon_msg)
  })
  output$recon_issues <- renderDT({ req(state$recon_issues); small_table(state$recon_issues) })
  output$batch_status <- renderDT({ req(state$batch_status); small_table(state$batch_status) })

  # ---- Household corrections -------------------------------------------
  observeEvent(input$changes_file, {
    upload <- read_upload(input$changes_file$datapath)
    needed <- c("qa_code_sn", config$editable_fields)
    missing <- setdiff(needed, names(upload))
    if (length(missing) > 0) {
      state$changes <- NULL
      state$changes_msg <- paste("Missing columns:", paste(missing, collapse = ", "))
      return()
    }

    changes <- compare_changes(upload, store_read(store_dir, "households"), config)
    not_found <- attr(changes, "not_found")
    state$changes <- changes
    state$changes_msg <- paste(
      sprintf("%d change(s) found: %d approved, %d need review.",
              nrow(changes), sum(changes$approved), sum(!changes$approved)),
      if (length(not_found))
        paste("QA codes not found:", paste(not_found, collapse = ", "))
    )
  })

  output$changes_button <- renderUI({
    req(state$changes, any(state$changes$approved))
    actionButton("apply_changes", "Apply approved changes", class = "btn-primary")
  })

  observeEvent(input$apply_changes, {
    req(state$changes)
    n <- apply_changes(store_dir, state$changes)
    state$changes_msg <- sprintf(
      "%d change(s) applied. Rejected changes were not written.", n)
    state$changes <- NULL
    state$refresh <- state$refresh + 1
  })

  output$changes_message <- renderUI({
    req(state$changes_msg)
    tags$div(class = "alert alert-info", state$changes_msg)
  })
  output$changes_table <- renderDT({
    req(state$changes)
    small_table(state$changes[, c("qa_code_sn", "field", "old_value", "new_value",
                                  "similarity", "approved", "reason")])
  })

  # ---- Database view, samples, reset --------------------------------------
  output$form_table <- renderDT({
    req(input$form)
    state$refresh
    small_table(store_read(store_dir, input$form))
  })

  sample_download <- function(file_name) {
    downloadHandler(
      filename = file_name,
      content = function(file) file.copy(file.path(store_dir, "uploads", file_name), file)
    )
  }
  output$dl_valid   <- sample_download("recon_valid.csv")
  output$dl_errors  <- sample_download("recon_with_errors.csv")
  output$dl_changes <- sample_download("data_changes.csv")

  observeEvent(input$reset, {
    generate_synthetic_data(store_dir)
    state$recon_issues <- state$recon_ready <- state$recon_msg <- NULL
    state$batch_status <- state$changes <- state$changes_msg <- NULL
    state$refresh <- state$refresh + 1
    showNotification("Demo data reset.")
  })

  session$onSessionEnded(function() unlink(store_dir, recursive = TRUE))
}

shinyApp(ui, server)

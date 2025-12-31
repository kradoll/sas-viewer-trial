library(shiny)
library(DT)
library(tidyverse)
library(haven)
library(readxl)
library(tools)

# ========= 設定 =========
MAX_ROWS <- 1000

# ========= UI =========
ui <- fluidPage(
  titlePanel("Data Viewer (Trial Version)"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(
        "target_dir",
        "Target folder",
        value = ""
      ),
      actionButton("load_files", "Load files"),
      
      hr(),
      
      uiOutput("file_selector"),
      
      hr(),
      
      downloadButton(
        "download_csv",
        "Download CSV (displayed rows only)"
      )
    ),
    
    mainPanel(
      verbatimTextOutput("info"),
      DTOutput("table")
    )
  )
)

# ========= Server =========
server <- function(input, output, session) {
  
  # --- ファイル一覧 ---
  files <- eventReactive(input$load_files, {
    req(input$target_dir)
    
    list.files(
      input$target_dir,
      full.names = TRUE
    ) |>
      keep(~ file_ext(.x) %in% c("sas7bdat", "csv", "xls", "xlsx"))
  })
  
  # --- ファイル選択UI ---
  output$file_selector <- renderUI({
    req(files())
    
    selectInput(
      "selected_file",
      "Select file",
      choices = files()
    )
  })
  
  # --- データ読み込み ---
  raw_data <- reactive({
    req(input$selected_file)
    
    ext <- file_ext(input$selected_file)
    
    df <- switch(
      ext,
      "sas7bdat" = read_sas(input$selected_file),
      "csv"      = read_csv(input$selected_file, show_col_types = FALSE),
      "xls"      = read_excel(input$selected_file),
      "xlsx"     = read_excel(input$selected_file),
      NULL
    )
    
    as_tibble(df)
  })
  
  # --- 表示用（1000行制限） ---
  display_data <- reactive({
    req(raw_data())
    
    raw_data() |>
      slice_head(n = MAX_ROWS)
  })
  
  # --- 情報表示 ---
  output$info <- renderText({
    req(raw_data())
    
    paste0(
      "File: ", basename(input$selected_file), "\n",
      "Rows: ", nrow(raw_data()),
      if (nrow(raw_data()) > MAX_ROWS) {
        paste0(" (showing first ", MAX_ROWS, " rows)")
      } else {
        ""
      },
      "\n",
      "Columns: ", ncol(raw_data())
    )
  })
  
  # --- テーブル表示 ---
  output$table <- renderDT({
    req(display_data())
    
    datatable(
      display_data(),
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  # --- CSVダウンロード（体験版） ---
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(
        tools::file_path_sans_ext(basename(input$selected_file)),
        "_preview.csv"
      )
    },
    content = function(file) {
      write_csv(display_data(), file)
    }
  )
}

shinyApp(ui, server)

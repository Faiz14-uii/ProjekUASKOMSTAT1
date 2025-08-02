# Memuat pustaka yang diperlukan
library(shiny)
library(bslib)         # Untuk tema dan komponen UI modern
library(thematic)      # Agar plot sesuai dengan tema Shiny
library(readxl)        # Untuk membaca file Excel
library(simpleOLSkel1) # PERUBAHAN: Kembali menggunakan library Anda

# --- FUNGSI DARI PAKET ANDA TIDAK LAGI DIDEFINISIKAN DI SINI ---
# Fungsi simple_ols() dan plot_ols() sekarang akan dipanggil dari library simpleOLSkel1


# --- Antarmuka Pengguna (UI) yang Ditingkatkan ---
ui <- page_sidebar(
  title = "Analisis Regresi OLS",
  theme = bs_theme(
    version = 5,
    bootswatch = "sandstone",
    base_font = font_google("Inter"),
    heading_font = font_google("Montserrat"),
    "primary" = "#325d88",
    "sidebar_bg" = "#f5f5f5"
  ),
  sidebar = sidebar(
    title = p(strong("Panel Kontrol")),

    # Langkah 1: Unggah Data
    fileInput("file", "1. Unggah File Data",
              accept = c(".csv", ".xlsx", ".xls"),
              buttonLabel = "Cari File...",
              placeholder = "Belum ada file dipilih"
    ),
    checkboxInput("header", "File memiliki header", TRUE),
    hr(),

    # Langkah 2: Pilih Variabel
    selectInput("y_var", "2. Pilih Variabel Y (Dependen)", choices = NULL),
    selectInput("x_var", "3. Pilih Variabel X (Independen)", choices = NULL),
    hr(),

    # Langkah 3: Jalankan Analisis
    actionButton("run", "Jalankan Analisis", icon = icon("play"), class = "btn-primary w-100"),
    downloadButton("download", "Unduh Hasil", icon = icon("download"), class = "btn-outline-secondary w-100 mt-2")
  ),

  # Tata letak panel utama menggunakan Tab
  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      title = "Ringkasan & Data",
      icon = icon("table-list"),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Hasil Estimasi"),
          verbatimTextOutput("results"),
          h5("Ringkasan Statistik"),
          tableOutput("summary")
        ),
        card(
          card_header("Tinjauan Data"),
          dataTableOutput("data_table")
        )
      )
    ),
    nav_panel(
      title = "Visualisasi Grafik",
      icon = icon("chart-line"),
      # PERUBAHAN: Menggunakan layout_columns untuk membuat grafik berdampingan
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Grafik Regresi OLS"),
          plotOutput("plot", height = "400px")
        ),
        card(
          card_header("Grafik Residual vs. Fitted Values"),
          plotOutput("resid_plot", height = "400px")
        )
      )
    )
  )
)

# --- Logika Server ---
server <- function(input, output, session) {

  # Mengaktifkan tema untuk plot agar sesuai dengan UI
  thematic::thematic_shiny()

  # Reaktif: data dari file yang diunggah
  data <- reactive({
    req(input$file) # Proses hanya berjalan jika file sudah diunggah

    inFile <- input$file
    ext <- tools::file_ext(inFile$name)

    df <- switch(ext,
                 csv = read.csv(inFile$datapath, header = input$header),
                 xls = readxl::read_excel(inFile$datapath, col_names = input$header),
                 xlsx = readxl::read_excel(inFile$datapath, col_names = input$header),
                 validate(paste("Format file tidak didukung:", ext))
    )
    return(df)
  })

  # Perbarui pilihan variabel secara dinamis saat data baru dimuat
  observeEvent(data(), {
    var_names <- names(data())
    updateSelectInput(session, "y_var", choices = var_names, selected = var_names[2])
    updateSelectInput(session, "x_var", choices = var_names, selected = var_names[1])
  })

  # Jalankan model OLS ketika tombol ditekan
  model <- eventReactive(input$run, {
    req(data(), input$y_var, input$x_var)
    # MENGGUNAKAN FUNGSI DARI LIBRARY ANDA
    simple_ols(
      x = data()[[input$x_var]],
      y = data()[[input$y_var]]
    )
  })

  # Output: Tabel data
  output$data_table <- renderDataTable({
    req(data())
    data()
  }, options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE))

  # Output: Hasil estimasi model
  output$results <- renderPrint({
    req(model())
    cat("=== HASIL ESTIMASI MODEL OLS ===\n\n")
    cat("Formula Model:\n", input$y_var, "~", input$x_var, "\n\n")
    cat("Koefisien:\n")
    print(model()$coefficients)
    cat("\nR-squared:", round(model()$r.squared, 4))
  })

  # Output: Ringkasan statistik
  output$summary <- renderTable({
    req(data(), input$y_var, input$x_var)
    data.frame(
      Variabel = c(input$x_var, input$y_var),
      Rata_Rata = c(mean(data()[[input$x_var]]), mean(data()[[input$y_var]])),
      Standar_Deviasi = c(sd(data()[[input$x_var]]), sd(data()[[input$y_var]]))
    )
  })

  # Output: Grafik regresi
  output$plot <- renderPlot({
    req(model(), data())
    # MENGGUNAKAN FUNGSI DARI LIBRARY ANDA
    plot_ols(
      data = data.frame(x = data()[[input$x_var]], y = data()[[input$y_var]]),
      model = model()
    )
  }, res = 96)

  # Output: Grafik residual dengan warna yang disesuaikan
  output$resid_plot <- renderPlot({
    req(model())
    plot(
      model()$fitted.values,
      model()$residuals,
      xlab = "Fitted Values",
      ylab = "Residuals",
      main = "Grafik Residual vs. Fitted Values",
      pch = 19,
      col = "#325d88"
    )
    abline(h = 0, col = "#c9302c", lty = 2, lwd = 1.5)
  }, res = 96)

  # Fungsi untuk mengunduh hasil
  output$download <- downloadHandler(
    filename = function() {
      paste("hasil-analisis-ols-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(model())
      model_results <- model()
      results_df <- data.frame(
        Term = names(model_results$coefficients),
        Estimate = model_results$coefficients
      )
      r_squared_row <- data.frame(Term = "R-squared", Estimate = model_results$r.squared)
      final_df <- rbind(results_df, r_squared_row)
      write.csv(final_df, file, row.names = FALSE)
    }
  )
}

# Menjalankan aplikasi
shinyApp(ui, server)

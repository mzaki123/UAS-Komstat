library(shiny)
library(shinydashboard)
library(DT)
library(haven)
library(sf)
library(plotly)
library(corrplot)
library(ggplot2)
library(car) 
library(broom) 
library(dplyr)
library(tidyr)
library(leaflet)
library(rmarkdown)
library(knitr)
library(lmtest) 
library(car)
library(officer)
library(flextable)

# =====================================
# DATA
# =====================================
sovi_data <- read_sav("data_sovi.sav")
kabkota <- st_read("indonesia511.geojson")
sovi_data <- sovi_data[, !names(sovi_data) %in% "DISTRICTCODE"]

# =====================================
# CSS STYLES
# =====================================
custom_css <- "
  body {
    font-family: 'Arial', sans-serif;
    background-color: #f8f9fa;
  }
  
  .main-title {
    color: #1e3a8a;
    font-size: 28px;
    font-weight: bold;
    text-align: center;
    margin-bottom: 30px;
    padding: 20px;
    background: linear-gradient(135deg, #3b82f6, #1e40af);
    color: white;
    border-radius: 10px;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  }
  
  .section-title {
    color: #1e40af;
    font-size: 22px;
    font-weight: 600;
    margin: 25px 0 15px 0;
    border-bottom: 2px solid #3b82f6;
    padding-bottom: 5px;
  }
  
  .stats-box {
    background: linear-gradient(135deg, #dbeafe, #bfdbfe);
    border: 1px solid #93c5fd;
    border-radius: 8px;
    padding: 20px;
    text-align: center;
    margin: 10px 0;
    box-shadow: 0 2px 4px rgba(59, 130, 246, 0.1);
    transition: transform 0.2s ease;
  }
  
  .stats-box:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 8px rgba(59, 130, 246, 0.2);
  }
  
  .stats-number {
    display: block;
    font-size: 32px;
    font-weight: bold;
    color: #1e40af;
    margin-bottom: 5px;
  }
  
  .stats-label {
    display: block;
    font-size: 14px;
    color: #374151;
    font-weight: 500;
  }
  
  .about-section {
    background: white;
    border-radius: 10px;
    padding: 25px;
    margin-top: 30px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    border-left: 4px solid #3b82f6;
  }
  
  .about-content {
    color: #374151;
    line-height: 1.6;
  }
  
  .about-content p {
    margin-bottom: 15px;
    text-align: justify;
  }
  
  .about-content ul {
    margin-left: 20px;
    margin-bottom: 15px;
  }
  
  .about-content li {
    margin-bottom: 8px;
    padding-left: 5px;
  }
  
  .about-content strong {
    color: #1e40af;
    font-weight: 600;
  }
  
  .metadata-section {
    background: #f1f5f9;
    border-radius: 10px;
    padding: 25px;
    margin-top: 25px;
    border: 1px solid #cbd5e1;
  }
  
  .metadata-table {
    width: 100%;
    border-collapse: collapse;
    margin-top: 15px;
  }
  
  .metadata-table th,
  .metadata-table td {
    padding: 10px;
    text-align: left;
    border-bottom: 1px solid #e2e8f0;
  }
  
  .metadata-table th {
    background-color: #3b82f6;
    color: white;
    font-weight: 600;
  }
  
  .metadata-table tr:hover {
    background-color: #f8fafc;
  }
  
  .box.box-primary .box-header {
    background: linear-gradient(135deg, #3b82f6, #1e40af)!important;
    border-radius: 7px 7px 0 0;
  }

  .box.box-success .box-header {
    background: linear-gradient(135deg, #3b82f6, #1e40af)!important;
    border-radius: 7px 7px 0 0;
  }
  
  .box.box-info .box-header {
    background: linear-gradient(135deg, #0ea5e9, #0284c7)!important;
    color: white !important;
  }
  
  .btn-primary {
    background: linear-gradient(135deg, #1e3a8a, #1e40af); 
    border: none;
    border-radius: 6px;
    padding: 10px 20px;
    font-weight: 600;
    transition: all 0.3s ease;
    color: white;
  }

  .btn-primary:hover {
    background: linear-gradient(135deg, #1e40af, #1d4ed8); 
    transform: translateY(-1px);
    box-shadow: 0 4px 8px rgba(30, 58, 138, 0.3); 
  }

  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover,
  .nav-tabs > li.active > a:focus {
    background: linear-gradient(135deg, #1e3a8a, #1e40af); 
    color: white;
    border: none;
    font-weight: 600;
  }

  .table > thead > tr > th {
    background: linear-gradient(135deg, #1e3a8a, #1e40af); 
    color: white;
    font-weight: 600;
    border: none;
    padding: 12px;
  }

  .metadata-table th {
    background-color: #1e3a8a; 
    color: white;
    font-weight: 600;
  }

  .form-control:focus {
    border-color: #1e3a8a; 
    box-shadow: 0 0 0 3px rgba(30, 58, 138, 0.1); 
    outline: none;
  }
  
  .dataTables_wrapper .dataTables_filter input:focus {
    border-color: #1e3a8a; 
    outline: none;
  }
  
  .box {
    border: none !important;
    .btn-info {
    background: linear-gradient(135deg, #475569, #334155)!important;
    border: none;
    border-radius: 6px;
    padding: 10px 20px;
    font-weight: 600;
    transition: all 0.3s ease;
    color: white;
    box-shadow: 0 2px 4px rgba(71, 85, 105, 0.2);
  }
  
  .btn-info:hover {
    background: linear-gradient(135deg, #334155, #1e293b);
    transform: translateY(-1px);
    box-shadow: 0 4px 8px rgba(71, 85, 105, 0.3);
    color: white;
  }
  
  .btn-info:active,
  .btn-info:focus {
    background: linear-gradient(135deg, #334155, #1e293b);
    box-shadow: 0 0 0 3px rgba(71, 85, 105, 0.2);
    color: white;
    outline: none;
  }
  }
"

# =====================================
# FUNGSI PEMBUAT UI (Modular)
# =====================================
menu_beranda <- function() {
  tabItem(tabName = "beranda",
          div(style = "padding: 20px;",
              h1("Selamat Datang di (DashSovInd) Dashboard SOVI Indonesia", class = "main-title"),
              
              h2("Ringkasan Statistik Data", class = "section-title"),
              
              fluidRow(
                column(3, div(class = "stats-box", 
                              span(class = "stats-number", "511"), 
                              span(class = "stats-label", "Kabupaten/Kota"))),
                column(3, div(class = "stats-box", 
                              span(class = "stats-number", "17"), 
                              span(class = "stats-label", "Variabel Data"))),
                column(3, div(class = "stats-box", 
                              span(class = "stats-number", "2017"), 
                              span(class = "stats-label", "Tahun Data"))),
                column(3, div(class = "stats-box", 
                              span(class = "stats-number", "BPS"), 
                              span(class = "stats-label", "Sumber Data")))
              ),
              
              div(class = "about-section",
                  h2("Tentang Dashboard", class = "section-title"),
                  div(class = "about-content",
                      p("Dashboard interaktif ini dirancang sebagai alat untuk melakukan eksplorasi dan analisis statistik terhadap data Kerentanan Sosial di 511 kabupaten/kota di Indonesia. Proyek ini disusun untuk memenuhi tugas Ujian Akhir Semester (UAS) mata kuliah Komputasi Statistik. Tujuan utama dashboard ini adalah menyediakan antarmuka yang ramah pengguna untuk menganalisis data multi-dimensi, mulai dari statistik deskriptif hingga analisis inferensial yang lebih kompleks."),
                      
                      p(strong("Fitur Utama Dashboard:")),
                      tags$ul(
                        tags$li("Manajemen Data: Menyediakan fasilitas untuk mengelola data, termasuk kemampuan untuk mengubah variabel kontinyu menjadi variabel kategorik untuk analisis lebih lanjut"),
                        tags$li("Eksplorasi Data: Menampilkan statistik deskriptif, tabel, serta beragam visualisasi data seperti grafik dan peta tematik untuk memahami karakteristik data secara mendalam"),
                        tags$li("Analisis Inferensial: uji beda rata-rata, uji proporsi, ANOVA"),
                        tags$li("Analisis regresi berganda beserta informasi asumsinya"),
                        tags$li("Fungsi unduh data dalam format Excel dan unduh laporan dalam word")
                      ),
                      
                      p(style = "font-style: italic; margin-top: 20px;",
                        strong("Sumber Data:"),
                        br(),
                        "1. https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv",
                        br(),
                        "2. https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
                      )
                  )
              ),
              
              div(class = "metadata-section",
                  h2("Metadata Variabel", class = "section-title"),
                  div(class = "about-content",
                      p(strong("Deskripsi Variabel Data SOVI:")),
                      tags$table(class = "metadata-table",
                                 tags$tr(tags$th("Variabel"), tags$th("Nama"), tags$th("Deskripsi")),
                                 tags$tr(tags$td("CHILDREN"), tags$td("Anak-anak"), tags$td("Persentase penduduk usia di bawah lima tahun")),
                                 tags$tr(tags$td("WOMEN"), tags$td("Perempuan"), tags$td("Persentase penduduk perempuan")),
                                 tags$tr(tags$td("ELDERLY"), tags$td("Lansia"), tags$td("Persentase penduduk usia 65 tahun ke atas")),
                                 tags$tr(tags$td("FEMALE_HEAD"), tags$td("Kepala RT Perempuan"), tags$td("Persentase rumah tangga dengan kepala rumah tangga perempuan")),
                                 tags$tr(tags$td("FAMILY_SIZE"), tags$td("Ukuran Keluarga"), tags$td("Rata-rata jumlah anggota rumah tangga")),
                                 tags$tr(tags$td("NO_ELECTRICITY"), tags$td("Tanpa Listrik"), tags$td("Persentase rumah tangga yang tidak menggunakan listrik")),
                                 tags$tr(tags$td("LOW_EDU"), tags$td("Pendidikan Rendah"), tags$td("Persentase penduduk usia 15+ dengan pendidikan rendah")),
                                 tags$tr(tags$td("GROWTH"), tags$td("Pertumbuhan"), tags$td("Persentase perubahan populasi")),
                                 tags$tr(tags$td("POVERTY"), tags$td("Kemiskinan"), tags$td("Persentase penduduk miskin")),
                                 tags$tr(tags$td("ILLITERATE"), tags$td("Buta Huruf"), tags$td("Persentase penduduk yang tidak bisa membaca dan menulis")),
                                 tags$tr(tags$td("NO_TRAINING"), tags$td("Tanpa Pelatihan"), tags$td("Persentase rumah tangga tanpa pelatihan bencana")),
                                 tags$tr(tags$td("DISASTER_PRONE"), tags$td("Rawan Bencana"), tags$td("Persentase rumah tangga di daerah rawan bencana")),
                                 tags$tr(tags$td("RENT"), tags$td("Sewa Rumah"), tags$td("Persentase rumah tangga yang menyewa rumah")),
                                 tags$tr(tags$td("NO_DRAINAGE"), tags$td("Tanpa Drainase"), tags$td("Persentase rumah tangga tanpa sistem drainase")),
                                 tags$tr(tags$td("TAP_WATER"), tags$td("Air Keran"), tags$td("Persentase rumah tangga yang menggunakan air keran")),
                                 tags$tr(tags$td("POPULATION"), tags$td("Populasi"), tags$td("Jumlah penduduk"))
                      )
                  )
              )
          )
  )
}

menu_manajemen_data <- function() {
  tabItem(
    tabName = "manajemen_data",
    h2("Manajemen Data"),
    
    fluidRow(
      box(
        title = "⚙️ Pengaturan Kategorisasi",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        
        fluidRow(
          column(3, 
                 selectInput("var_cat", "Pilih Variabel:", choices = NULL)
          ),
          column(3, 
                 radioButtons("metode_cat", "Metode Kategorisasi:",
                              choices = c("Quantile", "Equal Width", "Manual Cuts"),
                              selected = "Quantile"
                 )
          ),
          column(2, 
                 numericInput("jumlah_cat", "Jumlah Kategori:", 
                              value = 2, min = 2, max = 5
                 )
          ),
          column(4, 
                 conditionalPanel(
                   condition = "input.metode_cat == 'Manual Cuts'",
                   textInput("manual_cat", "Cut Points (perhatikan min dan max):", 
                             placeholder = "10, 20, 30"
                   )
                 )
          )
        ),
        
        fluidRow(
          column(5, 
                 textInput("label_cat", "Label Kategori (pisahkan dengan koma):", 
                           placeholder = "Rendah, Sedang, Tinggi"
                 )
          ),
          column(4, 
                 textInput("judul_cat", "Judul Kolom Kategori:", 
                           value = "Kategori", placeholder = "Kategori"
                 )
          ),
          column(3, 
                 br(), 
                 actionButton("btn_cat", "Kategorikan Data", 
                              class = "btn-primary btn-block"
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 br(),
                 downloadButton("download_categorization_report", 
                                "Unduh Laporan Kategorisasi (Word)", 
                                class = "btn-info"
                 )
          )
        ),
        
        fluidRow(
          column(12, 
                 div(class = "info-box", 
                     h5("Informasi Variabel:"), 
                     verbatimTextOutput("var_info_cat", placeholder = TRUE)
                 )
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = "📄 Hasil Kategorisasi",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        br(),
        
        tabsetPanel(
          id = "result_tabs",
          tabPanel("📊 Tabel Data", 
                   br(), 
                   DT::dataTableOutput("output_tabel_cat")
          ),
          tabPanel("📈 Distribusi", 
                   br(), 
                   plotOutput("output_plot_cat")
          ),
          tabPanel("📋 Summary", 
                   br(), 
                   verbatimTextOutput("output_summary_cat")
          ),
          tabPanel("💡 Interpretasi", 
                   br(),
                   div(class = "interpretation-container",
                       div(class = "interpretation-box",
                           verbatimTextOutput("output_interpretasi_cat")
                       )
                   )
          )
        )
      )
    )
  )
}

menu_eksplorasi <- function() {
  tabItem(tabName = "eksplorasi",
          h2("Eksplorasi Data SOVI"),
          
          tabsetPanel(
            id = "exploration_tabs",
            type = "tabs",
            
            tabPanel(
              title = "Peta Kerentanan", 
              icon = icon("map-marker-alt"),
              value = "peta",
              
              br(),
              fluidRow(
                box(
                  title = "Kontrol Peta", 
                  status = "success",
                  solidHeader = TRUE, 
                  width = 4,
                  
                  selectizeInput(
                    inputId = "sovi_variable",
                    label = "Pilih Variabel:",
                    choices = c("",
                                "CHILDREN" = "CHILDREN",
                                "FEMALE" = "FEMALE",
                                "ELDERLY" = "ELDERLY",
                                "FHEAD" = "FHEAD",
                                "FAMILYSIZE" = "FAMILYSIZE",
                                "NOELECTRIC" = "NOELECTRIC",
                                "LOWEDU" = "LOWEDU",
                                "GROWTH" = "GROWTH",
                                "POVERTY" = "POVERTY",
                                "ILLITERATE" = "ILLITERATE",
                                "NOTRAINING" = "NOTRAINING",
                                "DPRONE" = "DPRONE",
                                "RENTED" = "RENTED",
                                "NOSEWER" = "NOSEWER",
                                "TAPWATER" = "TAPWATER",
                                "POPULATION" = "POPULATION"),
                    selected = "",
                    options = list(placeholder = "Pilih Variabel")
                  ),
                  
                  hr(),
                  
                  h4("Legenda SOVI"),
                  tags$div(
                    tags$p(icon("square", style = "color: #bdc3c7;"), " Abu-abu: Tidak ada data"),
                    tags$p(icon("square", style = "color: #d73027;"), " Merah Gelap: Tinggi"),
                    tags$p(icon("square", style = "color: #fee08b;"), " Merah Cerah: Rendah")
                  )
                ),
                
                box(
                  title = "Peta", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 8,
                  leafletOutput("sovi_map", width = "100%", height = "500px")
                )
              )
            ),
            
            tabPanel(
              title = "Grafik", 
              icon = icon("chart-bar"),
              value = "grafik",
              
              br(),
              fluidRow(
                box(
                  title = "Pengaturan Grafik", 
                  status = "success",
                  solidHeader = TRUE, 
                  width = 12,
                  
                  fluidRow(
                    column(4, 
                           selectInput("plot_type", "Jenis Grafik:",
                                       choices = c("Histogram" = "histogram", 
                                                   "Scatter Plot" = "scatterplot"))
                    ),
                    column(4, 
                           selectInput("x_var_plot", "Variabel X:", choices = NULL)
                    ),
                    column(4, 
                           conditionalPanel(
                             condition = "input.plot_type == 'scatterplot'",
                             selectInput("y_var_plot", "Variabel Y:", choices = NULL)
                           )
                    )
                  ),
                  
                  fluidRow(
                    column(4, 
                           conditionalPanel(
                             condition = "input.plot_type == 'scatterplot'",
                             checkboxInput("add_smooth_scatterplot", 
                                           "Tambahkan Garis Tren", value = FALSE)
                           )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Hasil Visualisasi", 
                  status = "info", 
                  solidHeader = TRUE, 
                  width = 12,
                  plotlyOutput("dynamic_plot", height = "500px")
                )
              )
            ),
            
            tabPanel(
              title = "Tabel Data", 
              icon = icon("table"),
              value = "tabel",
              
              br(),
              fluidRow(
                box(
                  title = "Data SOVI Lengkap", 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  p("Tabel berikut menampilkan seluruh data Social Vulnerability Index (SOVI) 
                    yang dapat difilter dan diurutkan sesuai kebutuhan analisis."),
                  
                  DT::dataTableOutput("sovi_data_table")
                )
              )
            )
          )
  )
}


menu_uji_asumsi <- function() {
  tabItem(tabName = "uji_asumsi",
          h2("Uji Asumsi Statistik"),
          
          fluidRow(
            column(12,
                   tabsetPanel(
                     id = "asumsi_tabs",
                     type = "tabs",
                     
                     tabPanel("Uji Normalitas",
                              fluidRow(
                                box(
                                  title = "Input Variabel untuk Uji Normalitas",
                                  status = "success", 
                                  solidHeader = TRUE,
                                  width = 12,
                                  fluidRow(
                                    column(6,
                                           selectInput("var_normalitas", "Pilih Variabel untuk Uji Normalitas:",
                                                       choices = NULL, selected = NULL)
                                    ),
                                    column(6,
                                           div(style = "margin-top: 25px;",
                                               actionButton("run_normality_test", "Jalankan Uji Normalitas",
                                                            class = "btn-primary")
                                           )
                                    )
                                  )
                                )
                              ),
                              fluidRow(
                                box(
                                  title = "Hasil Uji Normalitas",
                                  status = "info", 
                                  solidHeader = TRUE,
                                  width = 12,
                                  fluidRow(
                                    column(6,
                                           h4("Shapiro-Wilk Test"),
                                           verbatimTextOutput("shapiro_result"),
                                           h4("Interpretasi"),
                                           verbatimTextOutput("normality_interpretation")
                                    ),
                                    column(6,
                                           h4("Visualisasi Normalitas"),
                                           plotlyOutput("normality_plot", height = "400px")
                                    )
                                  )
                                )
                              )
                     ),
                     
                     tabPanel("Uji Homogenitas",
                              fluidRow(
                                box(
                                  title = "Input Variabel untuk Uji Homogenitas",
                                  status = "success", 
                                  solidHeader = TRUE,
                                  width = 12,
                                  fluidRow(
                                    column(4,
                                           selectInput("var_homogenitas", "Pilih Variabel untuk Uji Homogenitas:",
                                                       choices = NULL, selected = NULL)
                                    ),
                                    column(4,
                                           selectInput("group_var_asumsi", "Pilih Variabel Grouping:",
                                                       choices = NULL, selected = NULL)
                                    ),
                                    column(4,
                                           div(style = "margin-top: 25px;",
                                               actionButton("run_homogeneity_test", "Jalankan Uji Homogenitas",
                                                            class = "btn-primary")
                                           )
                                    )
                                  )
                                )
                              ),
                              fluidRow(
                                box(
                                  title = "Hasil Uji Homogenitas",
                                  status = "info",
                                  solidHeader = TRUE,
                                  width = 12,
                                  fluidRow(
                                    column(6,
                                           h4("Levene's Test"),
                                           verbatimTextOutput("levene_result"),
                                           h4("Bartlett's Test"),
                                           verbatimTextOutput("bartlett_result"),
                                           h4("Interpretasi"),
                                           verbatimTextOutput("homogeneity_interpretation")
                                    ),
                                    column(6,
                                           h4("Boxplot per Group"),
                                           plotlyOutput("homogeneity_plot", height = "400px")
                                    )
                                  )
                                )
                              )
                     ),
                     tabPanel(
                       title = "download", 
                       icon = icon("download"),
                       value = "download",
                       div(style = "text-align: center; padding: 20px;",
                           downloadButton("download_word_asumsi", "Unduh Laporan Eksplorisasi (Word)", class = "btn-primary")
                       )
                     )
                   )
            )
          )
  )
}

menu_uji_rerata <- function() {
  tabItem(tabName = "uji_rata_rata",
          h2("Uji Beda Rata-rata"),
          fluidRow(
            box(
              title = "Pengaturan Uji Rata-rata", status = "success", solidHeader = TRUE, width = 12,
              selectInput("metode_rata", "Pilih Metode Uji:",
                          choices = list(
                            "Uji Beda Rata-rata 1 Kelompok (One Sample t-test)" = "one_sample",
                            "Uji Beda Rata-rata 2 Kelompok (Two Sample t-test)" = "two_sample"
                          )),
              conditionalPanel(
                condition = "input.metode_rata == 'one_sample'",
                fluidRow(
                  column(6, 
                         selectInput("var_one", "Pilih Variabel:", choices = NULL), 
                         numericInput("mu0", "Nilai Hipotesis (μ₀):", value = 0, step = 0.1)
                  ),
                  column(6, 
                         selectInput("alt_one", "Alternatif Hipotesis:", 
                                     choices = list("Tidak sama dengan (≠)" = "two.sided", 
                                                    "Lebih besar dari (>)" = "greater", 
                                                    "Lebih kecil dari (<)" = "less")), 
                         numericInput("conf_one", "Confidence Level (%):", value = 95, min = 90, max = 99)
                  )
                )
              ),
              conditionalPanel(
                condition = "input.metode_rata == 'two_sample'",
                fluidRow(
                  column(6, 
                         selectInput("var_two", "Pilih Variabel Numerik:", choices = NULL), 
                         selectInput("group_var_two_sample", "Pilih Variabel Kelompok:", choices = NULL)
                  ),
                  column(6, 
                         selectInput("alt_two", "Alternatif Hipotesis:", 
                                     choices = list("Tidak sama dengan (≠)" = "two.sided", 
                                                    "Kelompok 1 > Kelompok 2" = "greater", 
                                                    "Kelompok 1 < Kelompok 2" = "less")), 
                         numericInput("conf_two", "Confidence Level (%):", value = 95, min = 90, max = 99), 
                         checkboxInput("equal_var", "Asumsi Varians Sama", value = TRUE)
                  )
                )
              ),
              br(),
              fluidRow(
                column(6, 
                       actionButton("run_test_rata", "Jalankan Uji", class = "btn-primary")
                ),
                column(6, 
                       downloadButton("download_rata_report", "Download Laporan (Word)", 
                                      class = "btn-info", 
                                      style = "float: right;")
                )
              )
            )
          ),
          fluidRow(
            box(title = "Hasil Uji Statistik", status = "info", solidHeader = TRUE, width = 12,
                verbatimTextOutput("test_result_rata"),
                br(),
                h4("Interpretasi Hasil:"),
                div(id = "interpretation_rata", style = "background-color: #f4f4f4; padding: 15px; border-radius: 5px;",
                    verbatimTextOutput("interpretation_text_rata"))
            )
          )
  )
}

menu_uji_proporsi_varians <- function() {
  tabItem(tabName = "uji_proporsi_variance",
          h2("Uji Proporsi dan Varians"),
          tabsetPanel(
            tabPanel("Uji Proporsi", icon = icon("percent"),
                     fluidRow(
                       box(
                         title = "Pengaturan Uji Proporsi", status = "success", solidHeader = TRUE,
                         width = 4,
                         selectInput("prop_variable", "Pilih Variabel:", choices = NULL),
                         radioButtons("prop_test_type", "Jenis Uji:", 
                                      choices = list("1 Kelompok" = "1_sample", "2 Kelompok" = "2_sample"), 
                                      selected = "1_sample"),
                         conditionalPanel(
                           condition = "input.prop_test_type == '1_sample'",
                           numericInput("prop_null", "Nilai Proporsi Null (H0):", value = 0.5, min = 0, max = 1, step = 0.01)
                         ),
                         conditionalPanel(
                           condition = "input.prop_test_type == '2_sample'",
                           selectInput("prop_group_var", "Pilih Variabel Kelompok:", choices = NULL)
                         ),
                         numericInput("prop_conf_level", "Tingkat Kepercayaan:", value = 0.95, min = 0.8, max = 0.99, step = 0.01),
                         actionButton("run_prop_test", "Jalankan Uji", class = "btn-primary")
                       ),
                       box(
                         title = "Hasil Uji Proporsi", status = "info", solidHeader = TRUE,
                         width = 8,
                         verbatimTextOutput("prop_test_result"),
                         br(),
                         h4("Interpretasi:"),
                         verbatimTextOutput("prop_interpretation")
                       )
                     )
            ),
            tabPanel("Uji Varians", icon = icon("sigma"),
                     fluidRow(
                       box(
                         title = "Pengaturan Uji Varians", status = "success", solidHeader = TRUE,
                         width = 4,
                         selectInput("var_variable", "Pilih Variabel (Numerik):", choices = NULL),
                         radioButtons("var_test_type", "Jenis Uji:", 
                                      choices = list("1 Kelompok" = "1_sample", "2 Kelompok" = "2_sample"), 
                                      selected = "1_sample"),
                         conditionalPanel(
                           condition = "input.var_test_type == '1_sample'",
                           numericInput("var_null", "Nilai Varians Null (H0):", value = 1, min = 0.001, step = 0.01)
                         ),
                         conditionalPanel(
                           condition = "input.var_test_type == '2_sample'",
                           selectInput("var_group_var", "Pilih Variabel Kelompok:", choices = NULL)
                         ),
                         numericInput("var_conf_level", "Tingkat Kepercayaan:", value = 0.95, min = 0.8, max = 0.99, step = 0.01),
                         actionButton("run_var_test", "Jalankan Uji", class = "btn-primary")
                       ),
                       box(
                         title = "Hasil Uji Varians", status = "info", solidHeader = TRUE,
                         width = 8,
                         verbatimTextOutput("var_test_result"),
                         br(),
                         h4("Interpretasi:"),
                         verbatimTextOutput("var_interpretation")
                       )
                     )
            ),
            tabPanel(
              title = "download", 
              icon = icon("download"),
              value = "download",
              div(style = "text-align: center; padding: 20px;",
                  downloadButton("download_word_prop_var", "Unduh Laporan Eksplorisasi (Word)", class = "btn-primary")
              )
            )
          )
  )
}

menu_anova <- function() {
  tabItem(tabName = "anova",
          h2("Analisis Varians (ANOVA)"),
          fluidRow(
            box(
              title = "Pengaturan Analisis ANOVA",
              status = "success",
              solidHeader = TRUE,
              width = 4,
              radioButtons("anova_type", "Jenis ANOVA:", 
                           choices = list("One-Way ANOVA" = "oneway", "Two-Way ANOVA" = "twoway"), 
                           selected = "oneway"),
              selectInput("dep_var", "Variabel Dependen (Numerik):", choices = NULL),
              selectInput("factor1", "Faktor 1 (Kategorikal):", choices = NULL),
              conditionalPanel(
                condition = "input.anova_type == 'twoway'",
                selectInput("factor2", "Faktor 2 (Kategorikal):", choices = NULL)
              ),
              conditionalPanel(
                condition = "input.anova_type == 'twoway'",
                checkboxInput("include_interaction", "Sertakan Interaksi", value = TRUE)
              ),
              downloadButton("download_anova_report", "Download Laporan ANOVA (.docx)", 
                             class = "btn-info", style = "margin-top: 10px;"),
              br(),
              br(),
              actionButton("run_anova", "Jalankan Analisis", class = "btn-primary", style = "width: 100%;")
            ),
            box(title = "Statistik Deskriptif per Grup", status = "info", solidHeader = TRUE, width = 8,
                DT::dataTableOutput("descriptive_stats"))
          ),
          fluidRow(
            box(title = "Hasil ANOVA", status = "info", solidHeader = TRUE, width = 12,
                verbatimTextOutput("anova_results"),
                h4("Interpretasi:"),
                verbatimTextOutput("interpretation_anova")
            )
          ),
          fluidRow(
            box(title = "Post-Hoc Test (Tukey HSD)", status = "info", solidHeader = TRUE, width = 12,
                conditionalPanel(
                  condition = "input.run_anova > 0",
                  verbatimTextOutput("posthoc_results")
                )
            )
          )
  )
}

menu_regresi <- function() {
  tabItem(tabName = "regresi",
          h2("Analisis Regresi Linear Berganda"),
          fluidRow(
            box(
              title = "Pengaturan Model Regresi", status = "success", solidHeader = TRUE, width = 4,
              selectInput("dependent_var", "Variabel Dependen (Y):", choices = NULL, selected = NULL),
              checkboxGroupInput("independent_vars", "Variabel Independen (X):", choices = NULL, selected = NULL),
              br(),
              actionButton("run_regression", "Jalankan Regresi", class = "btn-primary", width = "100%"),
              br(), br(),
              downloadButton("download_regression_results", "Download Hasil Regresi", class = "btn-info")
            )
          ),
          fluidRow(
            box(title = "Hasil Regresi Linear Berganda & Interpretasi Koefisien", status = "info", solidHeader = TRUE, width = 12,
                verbatimTextOutput("regression_results"))
          ),
          fluidRow(
            box(title = "Matriks Korelasi", status = "info", solidHeader = TRUE, width = 6,
                plotOutput("correlation_matrix")),
            box(title = "Statistik Kualitas Model", status = "info", solidHeader = TRUE, width = 6,
                verbatimTextOutput("model_stats"))
          ),
          fluidRow(
            box(title = "Kesimpulan & Rekomendasi Model", status = "info", solidHeader = TRUE, width = 12,
                verbatimTextOutput("model_conclusion"))
          ),
          fluidRow(
            box(title = "Uji Normalitas Residual", status = "info", solidHeader = TRUE, width = 4,
                plotOutput("normality_test"),
                verbatimTextOutput("shapiro_test")),
            
            box(title = "Uji Homoskedastisitas", status = "info", solidHeader = TRUE, width = 4,
                plotOutput("homoscedasticity_test"),
                verbatimTextOutput("bp_test")),
            
            box(title = "Uji Autokorelasi", status = "info", solidHeader = TRUE, width = 4,
                verbatimTextOutput("durbin_watson_test"))
          ),
          
          fluidRow(
            box(title = "Uji Multikolinearitas (VIF)", status = "info", solidHeader = TRUE, width = 6,
                verbatimTextOutput("vif_test")),
            
            box(title = "Ringkasan Uji Asumsi", status = "info", solidHeader = TRUE, width = 6,
                verbatimTextOutput("assumptions_summary"))
          )
  )
}

menu_download <- function() {
  tabItem(tabName = "download",
          h2("Unduh Data dan Hasil Analisis"),
          fluidRow(
            box(title = "Opsi Unduh", status = "success", solidHeader = TRUE, width = 12,
                h4("Unduh Data Mentah"),
                downloadButton("download_data_mentah", "Unduh Data SOVI (CSV)", class = "btn-info"),
                br(), br(),
                h4("Unduh Output Gabungan "),
                downloadButton("download_report_umum", "Unduh Laporan Gabungan", class = "btn-info"),
                br(), br(),
                h4("Unduh Laporan Analisis"),
                tags$a(
                  href = "2KS3_222313244_Muhammad Imaddudin Zaki_UAS.pdf", 
                  target = "_blank",
                  class = "btn btn-info",
                  "Unduh Laporan Analisis"
                )
            )
          )
  )
}

# =====================================
# UI
# =====================================
ui <- dashboardPage(
  dashboardHeader(title = "DashSovInd"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      
      menuItem("Beranda Utama", tabName = "beranda", icon = icon("home")),
      
      menuItem("Manajemen Data", tabName = "manajemen_data", icon = icon("database")),
      
      menuItem("Eksplorasi Data",icon = icon("chart-line"),tabName = "eksplorasi"),
      
      menuItem("Uji Asumsi", tabName = "uji_asumsi", icon = icon("check-circle")),
      
      menuItem("Statistik Inferensia", icon = icon("flask"),
               menuSubItem("Uji Rata-rata", tabName = "uji_rata_rata", icon = icon("chart-pie")),
               menuSubItem("Uji Proporsi dan Varians", tabName = "uji_proporsi_variance", icon = icon("percent")),
               menuSubItem("ANOVA", tabName = "anova", icon = icon("calculator"))
      ),
      
      menuItem("Regresi", tabName = "regresi", icon = icon("arrow-right")),
      menuItem("Download", tabName = "download", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(
      menu_beranda(),
      menu_manajemen_data(),
      menu_eksplorasi(),
      menu_uji_asumsi(),
      menu_uji_rerata(),
      menu_uji_proporsi_varians(),
      menu_anova(),
      menu_regresi(),
      menu_download()
    )
  )
)

# =====================================
# SERVER
# =====================================
server <- function(input, output, session) {
  
  # Reactive value untuk menyimpan data utama
  cangih_data <- reactiveValues(
    sovi = sovi_data
  )
  
  # Reactive value untuk menyimpan data kategorisasi sementara
  categorized_data_storage <- reactiveVal(NULL)
  
  # =====================================
  # FUNGSI BANTUAN UNTUK SERVER
  # =====================================
  
  get_numeric_vars <- function(data) {
    names(data)[sapply(data, is.numeric)]
  }
  
  get_categorical_vars <- function(data, min_levels = 2, max_levels = 10) {
    names(data)[sapply(data, function(x) {
      if (is.numeric(x)) {
        num_unique <- length(unique(x[!is.na(x)]))
        return(num_unique >= min_levels && num_unique <= max_levels)
      } else if (is.factor(x) || is.character(x)) {
        num_unique <- length(unique(as.character(x[!is.na(x)])))
        return(num_unique >= min_levels && num_unique <= max_levels)
      }
      return(FALSE)
    })]
  }
  
  # =====================================
  # 1. DATA MANAGEMENT - SERVER LOGIC
  # =====================================
  MIN_CATEGORIES <- 2
  COLORS <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7")
  
  create_cut_points <- function(method, var_data_clean, num_categories, manual_cuts) {
    if (method == "Quantile") {
      probs <- seq(0, 1, length.out = num_categories + 1)
      cut_points <- quantile(var_data_clean, probs = probs, na.rm = TRUE, type = 7)
      cut_points <- unique(cut_points)
      
    } else if (method == "Equal Width") {
      min_val <- min(var_data_clean, na.rm = TRUE)
      max_val <- max(var_data_clean, na.rm = TRUE)
      cut_points <- seq(min_val, max_val, length.out = num_categories + 1)
      cut_points <- unique(cut_points)
      
    } else if (method == "Manual Cuts") {
      cut_points_parsed <- tryCatch({
        vals <- as.numeric(strsplit(manual_cuts, ",")[[1]])
        if (any(is.na(vals))) stop("Cut points harus berupa angka yang dipisahkan koma!")
        vals
      }, error = function(e) stop(e$message))
      
      min_val <- min(var_data_clean, na.rm = TRUE)
      max_val <- max(var_data_clean, na.rm = TRUE)
      cut_points <- sort(unique(c(min_val, cut_points_parsed, max_val)))
      
      if (length(cut_points) < 2) {
        stop("Minimal diperlukan 2 cut points untuk membuat kategori!")
      }
    }
    
    cut_points[length(cut_points)] <- cut_points[length(cut_points)] + .Machine$double.eps^0.5
    return(cut_points)
  }
  
  observe({
    numeric_vars <- get_numeric_vars(cangih_data$sovi)
    updateSelectInput(session, "var_cat",
                      choices = numeric_vars,
                      selected = if(length(numeric_vars) > 0) numeric_vars[1] else NULL)
  })
  
  output$var_info_cat <- renderText({
    req(input$var_cat)
    var_data <- cangih_data$sovi[[input$var_cat]]
    var_data <- var_data[!is.na(var_data)]
    
    if (length(var_data) == 0) {
      return(paste("Variabel:", input$var_cat, "| Tidak ada data valid."))
    }
    paste(
      "Variabel:", input$var_cat,
      "| Min:", round(min(var_data), 2),
      "| Max:", round(max(var_data), 2),
      "| Mean:", round(mean(var_data), 2),
      "| Median:", round(median(var_data), 2),
      "| Missing:", sum(is.na(cangih_data$sovi[[input$var_cat]]))
    )
  })
  
  observeEvent(input$btn_cat, {
    req(input$var_cat)
    
    if (input$metode_cat %in% c("Quantile", "Equal Width") && input$jumlah_cat < MIN_CATEGORIES) {
      showNotification("Jumlah kategori harus minimal 2.", type = "error")
      return(NULL)
    }
    
    if (input$metode_cat == "Manual Cuts" && input$manual_cat == "") {
      showNotification("Harap masukkan cut points untuk metode manual!", type = "error")
      return(NULL)
    }
    
    original_var_name <- input$var_cat
    var_data <- cangih_data$sovi[[original_var_name]]
    var_data_clean <- var_data[!is.na(var_data)]
    
    tryCatch({
      cut_points <- create_cut_points(input$metode_cat, var_data_clean, input$jumlah_cat, input$manual_cat)
      num_created_categories <- length(cut_points) - 1
      
      if (input$label_cat != "") {
        labels <- trimws(strsplit(input$label_cat, ",")[[1]])
        if (length(labels) != num_created_categories) {
          showNotification(paste0("Jumlah label (", length(labels), ") tidak sesuai dengan jumlah kategori yang dibuat (", num_created_categories, "). Menggunakan label default."), type = "warning", duration = 5)
          labels <- paste0("Cat_", 1:num_created_categories)
        }
      } else {
        labels <- paste0("Cat_", 1:num_created_categories)
      }
      
      categorized_var <- cut(var_data, breaks = cut_points, labels = labels, include.lowest = TRUE, right = TRUE)
      
      result_data <- cangih_data$sovi
      column_title <- if(input$judul_cat != "") input$judul_cat else "Kategori"
      new_col_name <- paste0(original_var_name, "_", column_title)
      result_data[[new_col_name]] <- categorized_var
      
      cangih_data$sovi <- result_data
      
      categorized_data_storage(list(
        data = result_data,
        cut_points = cut_points,
        original_var = original_var_name,
        categorized_var = categorized_var,
        column_title = column_title,
        metode_cat = input$metode_cat,
        jumlah_cat = num_created_categories,
        new_col_name = new_col_name
      ))
      
      showNotification("Kategorisasi berhasil!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
    })
  })
  
  output$output_tabel_cat <- DT::renderDataTable({
    req(categorized_data_storage())
    data_to_show <- categorized_data_storage()$data
    DT::datatable(data_to_show,
                  options = list(
                    scrollX = TRUE, pageLength = 15, dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel'),
                    language = list(search = "Cari:", lengthMenu = "Tampilkan _MENU_ data per halaman", info = "Menampilkan _START_ sampai _END_ dari _TOTAL_ data")
                  ),
                  caption = paste("Data dengan Kolom", categorized_data_storage()$column_title))
  })
  
  output$output_plot_cat <- renderPlot({
    req(categorized_data_storage())
    cat_data <- categorized_data_storage()$categorized_var
    category_counts <- table(cat_data)
    validate(need(length(category_counts) > 0, "Tidak ada data kategori untuk diplot."))
    
    colors <- COLORS[1:length(category_counts)]
    barplot(category_counts,
            main = paste("Distribusi", categorized_data_storage()$column_title, "-", categorized_data_storage()$original_var),
            xlab = "Kategori", ylab = "Frekuensi", col = colors, las = 2, border = "white", cex.main = 1.2)
    text(x = 1:length(category_counts), y = category_counts + max(category_counts) * 0.02, labels = category_counts, pos = 3, cex = 0.9)
  })
  
  output$output_summary_cat <- renderText({
    req(categorized_data_storage())
    summary_info <- categorized_data_storage()
    
    summary_text <- paste("SUMMARY KATEGORISASI\n")
    summary_text <- paste0(summary_text, strrep("=", 50), "\n")
    summary_text <- paste0(summary_text, "Variabel Original      : ", summary_info$original_var, "\n")
    summary_text <- paste0(summary_text, "Kolom Kategori         : ", summary_info$new_col_name, "\n")
    summary_text <- paste0(summary_text, "Metode                 : ", summary_info$metode_cat, "\n")
    summary_text <- paste0(summary_text, "Jumlah Kategori Dibuat : ", summary_info$jumlah_cat, "\n\n")
    
    summary_text <- paste0(summary_text, "CUT POINTS (termasuk min/max data):\n")
    for (i in 1:length(summary_info$cut_points)) {
      summary_text <- paste0(summary_text, sprintf("  [%d] %.2f\n", i, summary_info$cut_points[i]))
    }
    
    summary_text <- paste0(summary_text, "\nDISTRIBUSI KATEGORI:\n")
    category_counts <- table(summary_info$categorized_var)
    if (length(category_counts) == 0) {
      summary_text <- paste0(summary_text, "  Tidak ada data dalam kategori yang dihasilkan.\n")
    } else {
      for (i in 1:length(category_counts)) {
        percentage <- round(category_counts[i]/sum(category_counts)*100, 1)
        summary_text <- paste0(summary_text, sprintf("  %-15s: %3d wilayah (%5.1f%%)\n",
                                                     names(category_counts)[i], category_counts[i], percentage))
      }
    }
    return(summary_text)
  })
  
  output$output_interpretasi_cat <- renderText({
    req(categorized_data_storage())
    summary_info <- categorized_data_storage()
    cat_data <- summary_info$categorized_var
    cut_points <- summary_info$cut_points
    var_name <- summary_info$original_var
    
    category_counts <- table(cat_data)
    total_obs <- sum(category_counts)
    
    if (total_obs == 0) {
      return("Tidak ada data untuk diinterpretasikan setelah kategorisasi.")
    }
    
    interpretation <- paste("INTERPRETASI KATEGORISASI VARIABEL", toupper(var_name), "\n")
    interpretation <- paste0(interpretation, strrep("=", 60), "\n\n")
    
    for (i in 1:length(category_counts)) {
      cat_name <- names(category_counts)[i]
      count <- category_counts[i]
      percentage <- round(count/total_obs*100, 1)
      
      range_start <- round(cut_points[i], 2)
      range_end <- round(cut_points[i+1], 2)
      
      if (i == length(category_counts)) {
        actual_max_val <- max(cangih_data$sovi[[var_name]], na.rm = TRUE)
        if (range_end < actual_max_val) {
          range_end <- round(actual_max_val, 2)
        }
      }
      
      range_text <- paste0(
        ifelse(i == 1, "[", "("), range_start, " - ", range_end, ifelse(i == length(category_counts), "]", "]")
      )
      
      interp_desc <- if (percentage < 20) {
        "Kategori ini memiliki proporsi yang relatif kecil dalam dataset."
      } else if (percentage > 40) {
        "Kategori ini mendominasi distribusi data."
      } else {
        "Kategori ini memiliki proporsi yang seimbang."
      }
      
      interpretation <- paste0(interpretation, "KATEGORI: ", cat_name, "\n")
      interpretation <- paste0(interpretation, "Range      : ", range_text, "\n")
      interpretation <- paste0(interpretation, "Jumlah     : ", count, " wilayah (", percentage, "%)\n")
      interpretation <- paste0(interpretation, "Analisis   : ", interp_desc, "\n")
      interpretation <- paste0(interpretation, strrep("-", 45), "\n\n")
    }
    return(interpretation)
  })
  
  ## DOWNLOAD MANAJEMEN TABEL
  output$download_categorization_report <- downloadHandler(
    filename = function() {
      req(categorized_data_storage())
      var_name <- categorized_data_storage()$original_var
      paste0("Laporan_Kategorisasi_", var_name, "_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      req(categorized_data_storage())
      
      summary_info <- categorized_data_storage()
      original_data <- cangih_data$sovi[[summary_info$original_var]]
      
      mean_val <- round(mean(original_data, na.rm = TRUE), 2)
      min_val <- round(min(original_data, na.rm = TRUE), 2)
      max_val <- round(max(original_data, na.rm = TRUE), 2)
      missing_count <- sum(is.na(original_data))
      
      cat_table <- table(summary_info$categorized_var, useNA = "ifany")
      
      rmd_content <- paste0(
        '---\n',
        'title: "Laporan Kategorisasi Variabel"\n',
        'date: "', format(Sys.Date(), "%d %B %Y"), '"\n',
        'output: word_document\n',
        '---\n\n',
        
        '# Informasi Umum\n\n',
        '**Variabel Asli:** ', summary_info$original_var, '\n\n',
        '**Kolom Kategori Baru:** ', summary_info$new_col_name, '\n\n',
        '**Metode Kategorisasi:** ', summary_info$metode_cat, '\n\n',
        '**Jumlah Kategori:** ', summary_info$jumlah_cat, '\n\n',
        
        '# Statistik Data Asli\n\n',
        '- **Mean:** ', mean_val, '\n',
        '- **Minimum:** ', min_val, '\n', 
        '- **Maksimum:** ', max_val, '\n',
        '- **Data Missing:** ', missing_count, '\n\n',
        
        '# Cut Points\n\n'
      )
      
      # Tambah cut points
      for (i in 1:length(summary_info$cut_points)) {
        rmd_content <- paste0(rmd_content, 
                              i, '. ', round(summary_info$cut_points[i], 3), '\n')
      }
      
      # Tambah distribusi kategori
      rmd_content <- paste0(rmd_content, '\n# Distribusi Kategori\n\n')
      
      for(i in 1:length(cat_table)) {
        if(is.na(names(cat_table)[i])) next
        persen <- round(cat_table[i]/sum(cat_table, na.rm = TRUE)*100, 1)
        
        range_start <- round(summary_info$cut_points[i], 2)
        range_end <- round(summary_info$cut_points[i+1], 2)
        range_text <- paste0("[", range_start, " - ", range_end, "]")
        
        rmd_content <- paste0(rmd_content, 
                              '**', names(cat_table)[i], '** ', range_text, ': ', 
                              cat_table[i], ' wilayah (', persen, '%)\n\n')
      }
      
      # Interpretasi
      rmd_content <- paste0(rmd_content, '# Interpretasi Hasil\n\n')
      
      max_cat <- names(cat_table)[which.max(cat_table)]
      min_cat <- names(cat_table)[which.min(cat_table)]
      max_pct <- round(max(cat_table)/sum(cat_table)*100, 1)
      min_pct <- round(min(cat_table)/sum(cat_table)*100, 1)
      
      rmd_content <- paste0(rmd_content,
                            '**Kategori Dominan:** ', max_cat, ' dengan ', max_pct, '% dari total data\n\n',
                            '**Kategori Terendah:** ', min_cat, ' dengan ', min_pct, '% dari total data\n\n'
      )
      
      temp_rmd <- tempfile(fileext = ".Rmd")
      writeLines(rmd_content, temp_rmd)
      
      tryCatch({
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      }, error = function(e) {
        simple_content <- paste0(
          "LAPORAN KATEGORISASI\n",
          "===================\n\n",
          "Variabel: ", summary_info$original_var, "\n",
          "Metode: ", summary_info$metode_cat, "\n",
          "Jumlah Kategori: ", summary_info$jumlah_cat, "\n\n",
          "DISTRIBUSI:\n",
          paste(names(cat_table), ": ", cat_table, " (", round(cat_table/sum(cat_table)*100, 1), "%)", collapse = "\n"),
          "\n\nDibuat: ", Sys.time()
        )
        writeLines(simple_content, file)
      })
      
      unlink(temp_rmd)
    }
  )
  # =====================================
  # 2. EXPLORATION - MAP
  # =====================================
  output$sovi_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  observe({
    req(input$sovi_variable)
    
    variable <- input$sovi_variable
    
    map_data <- kabkota
    
    if (!variable %in% colnames(kabkota)) return()
    
    data_values <- map_data[[variable]]
    
    color_pal <- colorNumeric(
      palette = "YlOrRd", 
      domain = data_values,
      na.color = "#bdc3c7"
    )
    
    popup_text <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      map_data$nmkab, 
      variable,
      ifelse(is.na(data_values), "Tidak Ada Data", format(round(data_values, 2), nsmall = 2, big.mark = ","))
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("sovi_map", data = map_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~color_pal(data_values),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = popup_text,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#e74c3c", 
          fillOpacity = 1,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = color_pal,
        values = data_values,
        opacity = 0.7,
        title = variable,
        position = "bottomright",
        na.label = "Tidak Ada Data"
      )
  })
  
  # =====================================
  # 2. EXPLORATION - GRAPH - SERVER
  # =====================================
  observe({
    numeric_vars <- get_numeric_vars(cangih_data$sovi)
    
    if (input$plot_type == "histogram") {
      updateSelectInput(session, "x_var_plot", 
                        label = "Variabel (Numerik):",
                        choices = numeric_vars)
    } else if (input$plot_type == "scatterplot") {
      updateSelectInput(session, "x_var_plot", 
                        label = "Variabel X (Numerik):",
                        choices = numeric_vars)
      updateSelectInput(session, "y_var_plot", 
                        label = "Variabel Y (Numerik):",
                        choices = numeric_vars)
    }
  })
  
  output$dynamic_plot <- renderPlotly({
    req(input$x_var_plot)
    plot_data <- cangih_data$sovi
    
    p <- ggplot(plot_data) + theme_minimal()
    
    if (input$plot_type == "histogram") {
      req(input$x_var_plot %in% get_numeric_vars(plot_data))
      p <- p + geom_histogram(aes_string(x = input$x_var_plot, text = paste0("round(", input$x_var_plot, ", 2)")), 
                              bins = 30, fill = "#007bff", color = "black", alpha = 0.7) +
        labs(title = paste("Histogram dari", input$x_var_plot), x = input$x_var_plot, y = "Frekuensi")
    } else if (input$plot_type == "scatterplot") {
      req(input$x_var_plot, input$y_var_plot)
      req(input$x_var_plot %in% get_numeric_vars(plot_data) && input$y_var_plot %in% get_numeric_vars(plot_data))
      plot_data_clean <- plot_data[!is.na(plot_data[[input$x_var_plot]]) & !is.na(plot_data[[input$y_var_plot]]),]
      if(nrow(plot_data_clean) == 0) return(plotly_empty() %>% layout(title = "Tidak ada data valid untuk Scatter Plot."))
      
      p <- ggplot(plot_data_clean, aes_string(x = input$x_var_plot, y = input$y_var_plot)) +
        geom_point(aes_string(text = paste0("paste('X: ', round(", input$x_var_plot, ",2), '<br>Y: ', round(", input$y_var_plot, ",2))")),
                   color = "#FF6B6B", alpha = 0.6) +
        labs(title = paste("Scatter Plot:", input$y_var_plot, "vs", input$x_var_plot), 
             x = input$x_var_plot, y = input$y_var_plot)
      if (input$add_smooth_scatterplot) {
        p <- p + geom_smooth(method = "lm", color = "#007bff", se = FALSE)
      }
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(title = list(text = paste('<b>', p$labels$title, '</b>'), x = 0.5))
  })
  
  
  # =====================================
  # 2. EXPLORATION - TABLE - SERVER
  # =====================================
  output$sovi_data_table <- DT::renderDataTable({
    DT::datatable(cangih_data$sovi, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # =====================================
  # 3. UJI ASUMSI - SERVER LOGIC
  # =====================================
  uji_asumsi_results <- reactiveValues(
    shapiro = NULL,
    levene = NULL,
    bartlett = NULL,
    data_subset_normality = NULL,
    data_subset_homogeneity = NULL
  )
  
  observe({
    req(cangih_data$sovi)
    
    numeric_vars <- get_numeric_vars(cangih_data$sovi)
    categorical_for_grouping <- get_categorical_vars(cangih_data$sovi, min_levels = 2, max_levels = 10)
    
    updateSelectInput(session, "var_normalitas",
                      choices = numeric_vars,
                      selected = if(length(numeric_vars) > 0) numeric_vars[1] else NULL)
    
    updateSelectInput(session, "var_homogenitas",
                      choices = numeric_vars,
                      selected = if(length(numeric_vars) > 0) numeric_vars[1] else NULL)
    updateSelectInput(session, "group_var_asumsi",
                      choices = categorical_for_grouping,
                      selected = if(length(categorical_for_grouping) > 0) categorical_for_grouping[1] else NULL)
  })
  
  observeEvent(input$run_normality_test, {
    req(input$var_normalitas)
    
    if (!(input$var_normalitas %in% get_numeric_vars(cangih_data$sovi))) {
      showNotification("Variabel untuk Uji Normalitas harus numerik.", type = "error")
      return()
    }
    
    test_data <- cangih_data$sovi[, input$var_normalitas, drop = FALSE]
    test_data <- na.omit(test_data)
    
    if (nrow(test_data) == 0) {
      showNotification("Tidak ada observasi lengkap setelah menghilangkan nilai hilang untuk uji normalitas.", type = "error", duration = 5)
      uji_asumsi_results$shapiro <- NULL
      uji_asumsi_results$data_subset_normality <- NULL
      return()
    }
    
    if (nrow(test_data) > 5000) {
      uji_asumsi_results$shapiro <- list(statistic = NA, p.value = NA, method = "Ukuran sampel terlalu besar untuk uji Shapiro-Wilk (n > 5000). Pertimbangkan plot visual.")
    } else {
      tryCatch({
        uji_asumsi_results$shapiro <- shapiro.test(test_data[[input$var_normalitas]])
      }, error = function(e) {
        uji_asumsi_results$shapiro <- list(statistic = NA, p.value = NA, method = paste("Error Shapiro-Wilk:", e$message))
      })
    }
    
    uji_asumsi_results$data_subset_normality <- test_data
  })
  
  observeEvent(input$run_homogeneity_test, {
    req(input$var_homogenitas, input$group_var_asumsi)
    
    if (!(input$var_homogenitas %in% get_numeric_vars(cangih_data$sovi))) {
      showNotification("Variabel untuk Uji Homogenitas harus numerik.", type = "error")
      return()
    }
    
    available_categorical <- get_categorical_vars(cangih_data$sovi, min_levels = 2, max_levels = Inf)
    if (!(input$group_var_asumsi %in% available_categorical)) {
      showNotification("Variabel grouping harus kategorikal dengan minimal 2 kategori.", type = "error")
      return()
    }
    
    test_data <- cangih_data$sovi[, c(input$var_homogenitas, input$group_var_asumsi)]
    test_data <- na.omit(test_data)
    
    if (nrow(test_data) == 0) {
      showNotification("Tidak ada observasi lengkap setelah menghilangkan nilai hilang untuk uji homogenitas.", type = "error", duration = 5)
      uji_asumsi_results$levene <- NULL
      uji_asumsi_results$bartlett <- NULL
      uji_asumsi_results$data_subset_homogeneity <- NULL
      return()
    }
    
    test_data[[input$group_var_asumsi]] <- as.factor(test_data[[input$group_var_asumsi]])
    if (length(unique(test_data[[input$group_var_asumsi]])) < 2) {
      showNotification("Variabel grouping untuk uji homogenitas harus memiliki minimal 2 kategori unik.", type = "error", duration = 5)
      uji_asumsi_results$levene <- list(error = "Variabel grouping tidak memiliki cukup kategori.")
      uji_asumsi_results$bartlett <- list(error = "Variabel grouping tidak memiliki cukup kategori.")
    } else {
      tryCatch({
        formula_str <- paste(input$var_homogenitas, "~", input$group_var_asumsi)
        uji_asumsi_results$levene <- car::leveneTest(as.formula(formula_str), data = test_data)
      }, error = function(e) {
        uji_asumsi_results$levene <- list(error = paste("Error Levene's Test:", e$message))
      })
      
      tryCatch({
        formula_str <- paste(input$var_homogenitas, "~", input$group_var_asumsi)
        uji_asumsi_results$bartlett <- bartlett.test(as.formula(formula_str), data = test_data)
      }, error = function(e) {
        uji_asumsi_results$bartlett <- list(error = paste("Error Bartlett's Test:", e$message))
      })
    }
    
    uji_asumsi_results$data_subset_homogeneity <- test_data
  })
  
  output$shapiro_result <- renderText({
    if(is.null(uji_asumsi_results$shapiro)) return("Pilih variabel dan klik 'Jalankan Uji Normalitas' untuk melihat hasil.")
    if(is.na(uji_asumsi_results$shapiro$statistic)) return(uji_asumsi_results$shapiro$method)
    paste0("Shapiro-Wilk Normality Test\n",
           "W = ", round(uji_asumsi_results$shapiro$statistic, 4), "\n",
           "p-value = ", format(uji_asumsi_results$shapiro$p.value, scientific = TRUE, digits = 4), "\n")
  })
  
  output$normality_interpretation <- renderText({
    if(is.null(uji_asumsi_results$shapiro)) return("")
    if(is.na(uji_asumsi_results$shapiro$statistic)) return("Tidak dapat melakukan uji normalitas karena ukuran sampel terlalu besar.")
    alpha <- 0.05
    p_val <- uji_asumsi_results$shapiro$p.value
    
    interpretation_text <- paste0("INTERPRETASI UJI NORMALITAS (Variabel: ", input$var_normalitas, "):\n",
                                  "H0: Data berdistribusi normal\n",
                                  "H1: Data tidak berdistribusi normal\n\n",
                                  "Nilai p-value Shapiro-Wilk: ", format(p_val, scientific = TRUE, digits = 4), "\n",
                                  "Tingkat Signifikansi (α): ", alpha, "\n\n")
    
    if(p_val > alpha) {
      interpretation_text <- paste0(interpretation_text,
                                    "Kesimpulan: GAGAL TOLAK H0.\n",
                                    "Dengan tingkat signifikasi yang ditentukan dan jumlah sample yang diketahui \n ",
                                    "dapat diketahui bahwa p-value < alpha yang artinya gagal tolak H0 atau .\n",
                                    "Dengan kata lain, asumsi normalitas terpenuhi untuk variabel ", input$var_normalitas, ".")
    } else {
      interpretation_text <- paste0(interpretation_text,
                                    "Kesimpulan: TOLAK H0.\n",
                                    "Terdapat bukti yang **cukup** untuk menyatakan bahwa data tidak mengikuti distribusi normal.\n",
                                    "Dengan kata lain, asumsi normalitas **tidak terpenuhi** untuk variabel ", input$var_normalitas, ".")
    }
    return(interpretation_text)
  })
  
  output$normality_plot <- renderPlotly({
    req(uji_asumsi_results$data_subset_normality, input$var_normalitas)
    
    if(is.null(uji_asumsi_results$data_subset_normality) ||
       (is.list(uji_asumsi_results$shapiro) && is.na(uji_asumsi_results$shapiro$statistic))) {
      return(plotly_empty() %>% layout(title = "Plot tidak tersedia atau data terlalu besar."))
    }
    
    data <- uji_asumsi_results$data_subset_normality
    var_name <- input$var_normalitas
    
    if(!(var_name %in% names(data))) {
      return(plotly_empty() %>% layout(title = "Variabel tidak ditemukan dalam data."))
    }
    
    p1 <- ggplot(data, aes_string(x = var_name)) + 
      geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.7,
                     fill = "skyblue", color = "black") +
      geom_density(color = "red", linetype = "dashed", linewidth = 1) +
      theme_minimal() + 
      labs(title = paste("Histogram & Density -", var_name),
           x = var_name, y = "Density")
    
    p2 <- ggplot(data, aes_string(sample = var_name)) + 
      stat_qq(color = "blue", alpha = 0.7) + 
      stat_qq_line(color = "red", linewidth = 1) + 
      theme_minimal() + 
      labs(title = paste("Q-Q Plot -", var_name),
           x = "Theoretical Quantiles", y = "Sample Quantiles")
    
    subplot(ggplotly(p1), ggplotly(p2), nrows = 1) %>%
      layout(title = list(text = paste('<b>Analisis Normalitas:', var_name, '</b>'), x = 0.5))
  })
  
  output$levene_result <- renderText({
    if(is.null(uji_asumsi_results$levene)) return("Pilih variabel dan klik 'Jalankan Uji Homogenitas' untuk melihat hasil.")
    if(!is.null(uji_asumsi_results$levene$error)) return(paste("Levene's Test:", uji_asumsi_results$levene$error))
    paste0("Levene's Test for Homogeneity of Variances\n",
           "F = ", round(uji_asumsi_results$levene$`F value`[1], 4), "\n",
           "p-value = ", format(uji_asumsi_results$levene$`Pr(>F)`[1], scientific = TRUE, digits = 4), "\n")
  })
  
  output$bartlett_result <- renderText({
    if(is.null(uji_asumsi_results$bartlett)) return("Pilih variabel dan klik 'Jalankan Uji Homogenitas' untuk melihat hasil.")
    if(!is.null(uji_asumsi_results$bartlett$error)) return(paste("Bartlett's Test:", uji_asumsi_results$bartlett$error))
    paste0("Bartlett's Test for Homogeneity of Variances\n",
           "K-squared = ", round(uji_asumsi_results$bartlett$statistic, 4), "\n",
           "p-value = ", format(uji_asumsi_results$bartlett$p.value, scientific = TRUE, digits = 4), "\n")
  })
  
  output$homogeneity_interpretation <- renderText({
    if(is.null(uji_asumsi_results$levene) && is.null(uji_asumsi_results$bartlett)) return("")
    
    alpha <- 0.05
    interpretations <- c(paste0("INTERPRETASI UJI HOMOGENITAS (Variabel: ", input$var_homogenitas, ", Grup: ", input$group_var_asumsi, "):\n"))
    
    if(!is.null(uji_asumsi_results$levene) && is.null(uji_asumsi_results$levene$error)) {
      p_val_levene <- uji_asumsi_results$levene$`Pr(>F)`[1]
      interpretations <- c(interpretations,
                           paste0("Levene's Test:\n",
                                  "H0: Varians antar kelompok homogen\n",
                                  "H1: Varians antar kelompok tidak homogen\n",
                                  "p-value: ", format(p_val_levene, scientific = TRUE, digits = 4), "\n",
                                  "Kesimpulan: ", ifelse(p_val_levene > alpha,
                                                         "GAGAL TOLAK H0 - Varians antar grup **HOMOGEN** (Asumsi terpenuhi).",
                                                         "TOLAK H0 - Varians antar grup **TIDAK HOMOGEN** (Asumsi tidak terpenuhi)."), "\n"))
    } else if (!is.null(uji_asumsi_results$levene$error)) {
      interpretations <- c(interpretations, paste0("Levene's Test: ", uji_asumsi_results$levene$error, "\n"))
    }
    
    if(!is.null(uji_asumsi_results$bartlett) && is.null(uji_asumsi_results$bartlett$error)) {
      p_val_bartlett <- uji_asumsi_results$bartlett$p.value
      interpretations <- c(interpretations,
                           paste0("Bartlett's Test:\n",
                                  "H0: Varians antar kelompok homogen\n",
                                  "H1: Varians antar kelompok tidak homogen\n",
                                  "p-value: ", format(p_val_bartlett, scientific = TRUE, digits = 4), "\n",
                                  "Kesimpulan: ", ifelse(p_val_bartlett > alpha,
                                                         "GAGAL TOLAK H0 - Varians antar grup **HOMOGEN** (Asumsi terpenuhi).",
                                                         "TOLAK H0 - Varians antar grup **TIDAK HOMOGEN** (Asumsi tidak terpenuhi)."), "\n",
                                  "(Catatan: Uji Bartlett sensitif terhadap non-normalitas. Jika data tidak normal, hasil Levene's lebih dapat diandalkan.)"))
    } else if (!is.null(uji_asumsi_results$bartlett$error)) {
      interpretations <- c(interpretations, paste0("Bartlett's Test: ", uji_asumsi_results$bartlett$error, "\n"))
    }
    
    paste(interpretations, collapse = "\n")
  }) 
  
  ## DOWNLOAD WORD 
  output$download_word_asumsi <- downloadHandler(
    filename = function() {
      paste0("Laporan_Uji_Asumsi_", Sys.Date(), ".docx")
    },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      rmd_content <- generate_assumption_report()
      
      writeLines(rmd_content, temp_rmd)
      
      rmarkdown::render(temp_rmd, 
                        output_format = "word_document",
                        output_file = file,
                        quiet = TRUE)
    }
  )
  
  
  generate_assumption_report <- function() {
    
    normality_run <- !is.null(uji_asumsi_results$shapiro)
    homogeneity_run <- !is.null(uji_asumsi_results$levene) || !is.null(uji_asumsi_results$bartlett)
    
    content <- c(
      "---",
      "title: 'Laporan Hasil Uji Asumsi'",
      "date: '`r Sys.Date()`'",
      "output: word_document",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
      "```",
      "",
      "# Laporan Hasil Uji Asumsi Statistik",
      "",
      paste("Tanggal Analisis:", Sys.Date()),
      "",
      "## Ringkasan Dataset",
      paste("- Jumlah observasi:", ifelse(exists("cangih_data") && !is.null(cangih_data$sovi), nrow(cangih_data$sovi), "Data tidak tersedia")),
      paste("- Jumlah variabel:", ifelse(exists("cangih_data") && !is.null(cangih_data$sovi), ncol(cangih_data$sovi), "Data tidak tersedia")),
      ""
    )
    
    if(normality_run) {
      content <- c(content,
                   "## Hasil Uji Normalitas",
                   "",
                   paste("**Variabel yang diuji:** ", input$var_normalitas),
                   "",
                   "### Hasil Shapiro-Wilk Test",
                   ""
      )
      
      if(!is.na(uji_asumsi_results$shapiro$statistic)) {
        content <- c(content,
                     paste("- Statistik W:", round(uji_asumsi_results$shapiro$statistic, 4)),
                     paste("- p-value:", format(uji_asumsi_results$shapiro$p.value, scientific = TRUE, digits = 4)),
                     "",
                     "### Interpretasi Uji Normalitas",
                     "",
                     "**Hipotesis:**",
                     "- H₀: Data berdistribusi normal",  
                     "- H₁: Data tidak berdistribusi normal",
                     ""
        )
        
        alpha <- 0.05
        p_val <- uji_asumsi_results$shapiro$p.value
        
        if(p_val > alpha) {
          content <- c(content,
                       paste("**Kesimpulan:** GAGAL TOLAK H₀ (p-value =", format(p_val, scientific = TRUE, digits = 4), "> α = 0.05)"),
                       "",
                       paste("Dengan tingkat signifikansi α = 0.05, dapat disimpulkan bahwa **asumsi normalitas terpenuhi** untuk variabel", input$var_normalitas, ".")
          )
        } else {
          content <- c(content,
                       paste("**Kesimpulan:** TOLAK H₀ (p-value =", format(p_val, scientific = TRUE, digits = 4), "< α = 0.05)"),
                       "",
                       paste("Dengan tingkat signifikansi α = 0.05, dapat disimpulkan bahwa **asumsi normalitas tidak terpenuhi** untuk variabel", input$var_normalitas, ".")
          )
        }
      } else {
        content <- c(content, 
                     uji_asumsi_results$shapiro$method,
                     ""
        )
      }
      
      content <- c(content, "", "---", "")
    }
    
    if(homogeneity_run) {
      content <- c(content,
                   "## Hasil Uji Homogenitas Varians",
                   "",
                   paste("**Variabel yang diuji:** ", input$var_homogenitas),
                   paste("**Variabel pengelompokan:** ", input$group_var_asumsi),
                   ""
      )
      
      if(!is.null(uji_asumsi_results$levene) && is.null(uji_asumsi_results$levene$error)) {
        content <- c(content,
                     "### Hasil Levene's Test",
                     "",
                     paste("- Statistik F:", round(uji_asumsi_results$levene$`F value`[1], 4)),
                     paste("- p-value:", format(uji_asumsi_results$levene$`Pr(>F)`[1], scientific = TRUE, digits = 4)),
                     ""
        )
        
        p_val_levene <- uji_asumsi_results$levene$`Pr(>F)`[1]
        alpha <- 0.05
        
        content <- c(content,
                     "**Interpretasi Levene's Test:**",
                     "- H₀: Varians antar kelompok homogen",
                     "- H₁: Varians antar kelompok tidak homogen",
                     "",
                     paste("**Kesimpulan:**", 
                           ifelse(p_val_levene > alpha,
                                  "GAGAL TOLAK H₀ - Varians antar grup **HOMOGEN** (Asumsi terpenuhi).",
                                  "TOLAK H₀ - Varians antar grup **TIDAK HOMOGEN** (Asumsi tidak terpenuhi).")),
                     ""
        )
      }
      
      if(!is.null(uji_asumsi_results$bartlett) && is.null(uji_asumsi_results$bartlett$error)) {
        content <- c(content,
                     "### Hasil Bartlett's Test",
                     "",
                     paste("- Statistik K-squared:", round(uji_asumsi_results$bartlett$statistic, 4)),
                     paste("- p-value:", format(uji_asumsi_results$bartlett$p.value, scientific = TRUE, digits = 4)),
                     ""
        )
        
        p_val_bartlett <- uji_asumsi_results$bartlett$p.value
        alpha <- 0.05
        
        content <- c(content,
                     "**Interpretasi Bartlett's Test:**",
                     "- H₀: Varians antar kelompok homogen",
                     "- H₁: Varians antar kelompok tidak homogen",
                     "",
                     paste("**Kesimpulan:**", 
                           ifelse(p_val_bartlett > alpha,
                                  "GAGAL TOLAK H₀ - Varians antar grup **HOMOGEN** (Asumsi terpenuhi).",
                                  "TOLAK H₀ - Varians antar grup **TIDAK HOMOGEN** (Asumsi tidak terpenuhi).")),
                     "",
                     "*Catatan: Uji Bartlett sensitif terhadap non-normalitas. Jika data tidak normal, hasil Levene's Test lebih dapat diandalkan.*"
        )
      }
    }
    
    content <- c(content,
                 "",
                 "---",
                 "",
                 "## Catatan",
                 "",
                 "- Tingkat signifikansi yang digunakan: α = 0.05"
    )
    
    return(content)
  }
  
  # =====================================
  # 4. UJI BEDA RATA-RATA - SERVER LOGIC
  # =====================================
  uji_rerata_results <- reactiveValues(
    test = NULL,
    interpretation = NULL,
    desc_stats = NULL
  )
  
  observe({
    numeric_vars <- get_numeric_vars(cangih_data$sovi)
    categorical_for_grouping <- get_categorical_vars(cangih_data$sovi, min_levels = 2, max_levels = 2)
    
    updateSelectInput(session, "var_one", choices = numeric_vars)
    updateSelectInput(session, "var_two", choices = numeric_vars)
    updateSelectInput(session, "group_var_two_sample", choices = categorical_for_grouping)
  })
  
  observeEvent(input$run_test_rata, {
    req(input$metode_rata)
    
    uji_rerata_results$test <- NULL
    uji_rerata_results$interpretation <- NULL
    uji_rerata_results$desc_stats <- NULL
    
    if (input$metode_rata == "one_sample") {
      req(input$var_one, input$mu0)
      
      if (!(input$var_one %in% get_numeric_vars(cangih_data$sovi))) {
        showNotification("Variabel untuk uji satu sampel harus numerik.", type = "error")
        return(NULL)
      }
      
      data_var <- cangih_data$sovi[[input$var_one]]
      data_var <- data_var[!is.na(data_var)]
      
      if (length(data_var) < 1) { 
        showNotification("Tidak ada data valid untuk uji satu sampel.", type = "error")
        return(NULL)
      }
      
      test_result <- t.test(data_var, mu = input$mu0, alternative = input$alt_one, conf.level = input$conf_one/100)
      
      alpha <- 1 - input$conf_one/100
      
      p_value <- test_result$p.value 
      t_statistic <- test_result$statistic
      df <- test_result$parameter
      confidence_interval <- test_result$conf.int
      h0_text <- paste0("H₀: μ = ", input$mu0)
      h1_text <- paste0("H₁: μ ", switch(input$alt_one, "two.sided" = "≠", "greater" = ">", "less" = "<"), " ", input$mu0)
      
      interpretation_text <- paste(
        "Hipotesis:", h0_text, "vs", h1_text, "\n",
        "Nilai p-value:", format(p_value, scientific = TRUE, digits = 4), "\n",
        "Tingkat signifikansi (α):", alpha, "\n\n",
        ifelse(p_value < alpha,
               paste0("Kesimpulan: TOLAK H₀.\nTerdapat bukti yang cukup bahwa rata-rata populasi dari ", input$var_one,
                      " **", switch(input$alt_one, "two.sided" = "tidak sama dengan", "greater" = "lebih besar dari", "less" = "lebih kecil dari"),
                      " ", input$mu0, "** pada tingkat signifikansi ", paste0(alpha*100, "%", ".")),
               paste0("Kesimpulan: GAGAL TOLAK H₀.\nTidak terdapat bukti yang cukup bahwa rata-rata populasi dari ", input$var_one,
                      " **", switch(input$alt_one, "two.sided" = "berbeda dari", "greater" = "lebih besar dari", "less" = "lebih kecil dari"),
                      " ", input$mu0, "** pada tingkat signifikansi ", paste0(alpha*100, "%", ".")))
      )
      
      desc_stats <- data.frame(
        Statistik = c("Jumlah Data", "Rata-rata Sampel", "Std. Deviasi Sampel", "Minimum", "Maksimum"),
        Nilai = c(length(data_var), round(mean(data_var), 4), round(sd(data_var), 4), round(min(data_var), 4), round(max(data_var), 4))
      )
      uji_rerata_results$params <- list(
        method = "one_sample",
        var_name = input$var_one,
        mu0 = input$mu0,
        alternative = input$alt_one,
        conf_level = input$conf_one
      )
      uji_rerata_results$params <- list(
        method = "two_sample",
        var_name = input$var_two,
        group_var = input$group_var_two_sample,
        alternative = input$alt_two,
        conf_level = input$conf_two,
        equal_var = input$equal_var
      )
      uji_rerata_results$test <- test_result
      uji_rerata_results$interpretation <- interpretation_text
      uji_rerata_results$desc_stats <- desc_stats
      
    } else if (input$metode_rata == "two_sample") {
      req(input$var_two, input$group_var_two_sample)
      
      if (!(input$var_two %in% get_numeric_vars(cangih_data$sovi))) {
        showNotification("Variabel numerik untuk uji dua sampel harus numerik.", type = "error")
        return(NULL)
      }
      
      available_categorical <- get_categorical_vars(cangih_data$sovi, min_levels = 2, max_levels = 2)
      if (!(input$group_var_two_sample %in% available_categorical)) {
        showNotification("Variabel kelompok untuk uji dua sampel harus kategorikal dengan tepat 2 kategori.", type = "error")
        return(NULL)
      }
      
      data_clean <- cangih_data$sovi[!is.na(cangih_data$sovi[[input$var_two]]) & !is.na(cangih_data$sovi[[input$group_var_two_sample]]), ]
      
      if (nrow(data_clean) == 0) { 
        showNotification("Tidak ada observasi lengkap untuk uji dua sampel. Periksa data Anda.", type = "error")
        return(NULL)
      }
      
      data_clean[[input$group_var_two_sample]] <- as.factor(data_clean[[input$group_var_two_sample]])
      groups <- levels(data_clean[[input$group_var_two_sample]])
      
      if (length(groups) != 2) {
        showNotification("Variabel kelompok harus memiliki TEPAT 2 kategori unik untuk uji dua sampel. Silakan kategorikan ulang jika perlu.", type = "error", duration = 8)
        return(NULL)
      }
      
      group1_data <- data_clean[[input$var_two]][data_clean[[input$group_var_two_sample]] == groups[1]]
      group2_data <- data_clean[[input$var_two]][data_clean[[input$group_var_two_sample]] == groups[2]]
      
      test_result <- t.test(group1_data, group2_data, alternative = input$alt_two, conf.level = input$conf_two/100, var.equal = input$equal_var)
      
      alpha <- 1 - input$conf_two/100
      p_value <- test_result$p.value
      t_statistic <- test_result$statistic
      df <- test_result$parameter
      confidence_interval <- test_result$conf.int
      
      h0_text <- "H₀: μ₁ = μ₂"
      h1_text <- switch(input$alt_two,
                        "two.sided" = "H₁: μ₁ ≠ μ₂",
                        "greater" = paste0("H₁: μ₁ > μ₂ (", groups[1], " > ", groups[2], ")"),
                        "less" = paste0("H₁: μ₁ < μ₂ (", groups[1], " < ", groups[2], ")"))
      
      interpretation_text <- paste(
        "=== HASIL UJI BEDA RATA-RATA DUA KELOMPOK ===\n",
        "Hipotesis:", h0_text, "vs", h1_text, "\n",
        "Variabel Numerik:", input$var_two, "\n",
        "Variabel Kelompok:", input$group_var_two_sample, "\n",
        "Kelompok 1:", groups[1], "(n =", length(group1_data), ")\n",
        "Kelompok 2:", groups[2], "(n =", length(group2_data), ")\n",
        "Asumsi Varians Sama:", ifelse(input$equal_var, "YA", "TIDAK (Welch's t-test)"), "\n\n",
        "=== STATISTIK UJI ===\n",
        "t-statistik:", round(t_statistic, 4), "\n",
        "Derajat bebas:", round(df, 2), "\n",
        "Nilai p-value:", format(p_value, scientific = TRUE, digits = 4), "\n",
        "Tingkat signifikansi (α):", alpha, "\n",
        "Confidence Interval (", input$conf_two, "%):", 
        paste0("[", round(confidence_interval[1], 4), ", ", round(confidence_interval[2], 4), "]"), "\n\n",
        "=== KESIMPULAN ===\n",
        ifelse(p_value < alpha,
               paste0("TOLAK H₀.\nTerdapat perbedaan yang signifikan antara rata-rata ", input$var_two,
                      " untuk kelompok **", groups[1], "** dan kelompok **", groups[2], "** pada tingkat signifikansi ", paste0(alpha*100, "%", ".")) ,
               paste0("GAGAL TOLAK H₀.\nTidak terdapat perbedaan yang signifikan antara rata-rata ", input$var_two,
                      " untuk kelompok **", groups[1], "** dan kelompok **", groups[2], "** pada tingkat signifikansi ", paste0(alpha*100, "%", ".")))
      )
      
      desc_stats <- data.frame(
        Kelompok = c(groups[1], groups[2]),
        `Jumlah Data` = c(length(group1_data), length(group2_data)),
        `Rata-rata` = c(round(mean(group1_data), 4), round(mean(group2_data), 4)),
        `Std. Deviasi` = c(round(sd(group1_data), 4), round(sd(group2_data), 4)),
        `Std. Error` = c(round(sd(group1_data)/sqrt(length(group1_data)), 4),
                         round(sd(group2_data)/sqrt(length(group2_data)), 4)),
        `Minimum` = c(round(min(group1_data), 4), round(min(group2_data), 4)),
        `Maksimum` = c(round(max(group1_data), 4), round(max(group2_data), 4))
      )
      uji_rerata_results$test <- test_result
      uji_rerata_results$interpretation <- interpretation_text
      uji_rerata_results$desc_stats <- desc_stats
    }
  })
  
  output$test_result_rata <- renderPrint({ 
    req(uji_rerata_results$test)
    uji_rerata_results$test 
  })
  
  output$interpretation_text_rata <- renderText({ 
    req(uji_rerata_results$interpretation)
    uji_rerata_results$interpretation 
  })
  
  output$download_rata_report <- downloadHandler(
    filename = function() {
      paste0("Laporan_Uji_Rata_rata_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(uji_rerata_results$test)
      req(uji_rerata_results$params) 
      
      test_obj <- uji_rerata_results$test
      desc_stats <- uji_rerata_results$desc_stats
      interpretation <- uji_rerata_results$interpretation
      params <- uji_rerata_results$params
      
      if (params$method == "one_sample") {
        method_name <- "Uji t Satu Sampel"
        param_text <- paste0("- Variabel: ", params$var_name, "\\n",
                             "- Nilai Hipotesis (μ₀): ", params$mu0, "\\n",
                             "- Alternatif: ", params$alternative, "\\n",
                             "- Confidence Level: ", params$conf_level, "%")
      } else {
        method_name <- "Uji t Dua Sampel"
        param_text <- paste0("- Variabel Numerik: ", params$var_name, "\\n",
                             "- Variabel Kelompok: ", params$group_var, "\\n",
                             "- Alternatif: ", params$alternative, "\\n",
                             "- Confidence Level: ", params$conf_level, "%\\n",
                             "- Asumsi Varians Sama: ", ifelse(params$equal_var, "Ya", "Tidak"))
      }
      
      if (params$method == "one_sample") {
        desc_table_code <- paste0('
          desc_table <- data.frame(
            Statistik = c("', paste(desc_stats$Statistik, collapse = '", "'), '"),
            Nilai = c("', paste(desc_stats$Nilai, collapse = '", "'), '")
          )')
                } else {
                  desc_table_code <- paste0('
          desc_table <- data.frame(
            Kelompok = c("', paste(desc_stats$Kelompok, collapse = '", "'), '"),
            Jumlah.Data = c("', paste(desc_stats$Jumlah.Data, collapse = '", "'), '"),
            Rata.rata = c("', paste(desc_stats$Rata.rata, collapse = '", "'), '"),
            Std.Deviasi = c("', paste(desc_stats$Std.Deviasi, collapse = '", "'), '"),
            Std.Error = c("', paste(desc_stats$Std.Error, collapse = '", "'), '"),
            Minimum = c("', paste(desc_stats$Minimum, collapse = '", "'), '"),
            Maksimum = c("', paste(desc_stats$Maksimum, collapse = '", "'), '")
          )')
                }
                
                rmd_content <- paste0('---
          title: "Laporan Hasil Uji Beda Rata-rata"
          output: 
            word_document:
              reference_docx: NULL
          date: "', Sys.Date(), '"
          ---
          
          ```{r setup, include=FALSE}
          knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
          library(knitr)
          ```
          
          ## Ringkasan Uji
          
          **Metode Uji:** ', method_name, '
          
          **Parameter Uji:**
          
          ', gsub("\\\\n", "\n", param_text), '
          
          ## Statistik Deskriptif
          
          ```{r desc-stats}', desc_table_code, '
          kable(desc_table, align = "c", caption = "Statistik Deskriptif")
          ```
          
          ## Hasil Uji Statistik
          
          ```{r test-results}
          test_summary <- data.frame(
            Statistik = c("t-statistik", "Derajat Bebas", "p-value", "Confidence Interval"),
            Nilai = c(
              "', round(test_obj$statistic, 4), '",
              "', round(test_obj$parameter, 4), '",
              "', format(test_obj$p.value, scientific = TRUE, digits = 4), '",
              "[', round(test_obj$conf.int[1], 4), ', ', round(test_obj$conf.int[2], 4), ']"
            )
          )
          kable(test_summary, align = "c", caption = "Hasil Uji Statistik")
          ```
          
          ## Interpretasi dan Kesimpulan
          
          ', gsub("\n", "\n\n", interpretation), '
          
          ---
          *Laporan dibuat secara otomatis pada ', Sys.time(), '*
          ')
            
            # Buat file temporary
            temp_dir <- tempdir()
            rmd_file <- file.path(temp_dir, paste0("report_rata_", Sys.getpid(), ".Rmd"))
            
            # Tulis file RMD
            writeLines(rmd_content, rmd_file)
            
            # Render ke Word
            output_file <- file.path(temp_dir, paste0("Laporan_Uji_Rata_rata_", Sys.getpid(), ".docx"))
            tryCatch({
              rmarkdown::render(rmd_file, 
                                output_file = output_file, 
                                quiet = TRUE)
              
              # Copy ke file tujuan
              if (file.exists(output_file)) {
                file.copy(output_file, file)
              } else {
                stop("File output tidak berhasil dibuat")
              }
              
            }, error = function(e) {
              showNotification(paste("Error membuat laporan:", e$message), type = "error", duration = 10)
              # Buat file kosong agar tidak error
              writeLines("Error: Gagal membuat laporan", file)
            })    
    },
    
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )

# =====================================
# 5. UJI PROPORSI DAN VARIANS - SERVER LOGIC 
# =====================================
  uji_prop_var_results <- reactiveValues(
    prop_test = NULL,
    prop_interpretation = NULL,
    var_test = NULL,
    var_interpretation = NULL
  )
  
  observe({
    numeric_vars <- get_numeric_vars(cangih_data$sovi)
    all_vars <- names(cangih_data$sovi)
    categorical_for_grouping <- get_categorical_vars(cangih_data$sovi, min_levels = 2, max_levels = 2)
    
    updateSelectInput(session, "prop_variable", choices = all_vars)
    updateSelectInput(session, "prop_group_var", choices = categorical_for_grouping)
    updateSelectInput(session, "var_variable", choices = numeric_vars)
    updateSelectInput(session, "var_group_var", choices = categorical_for_grouping)
  })
  
  observeEvent(input$run_prop_test, {
    req(input$prop_variable)
    
    uji_prop_var_results$prop_test <- NULL
    uji_prop_var_results$prop_interpretation <- NULL
    
    data_col <- cangih_data$sovi[[input$prop_variable]]
    
    if(input$prop_test_type == "1_sample") {
      tryCatch({
        data_clean <- data_col[!is.na(data_col)]
        
        if(is.logical(data_clean)) {
          binary_data <- as.numeric(data_clean)
        } else if(is.numeric(data_clean)) {
          binary_data <- ifelse(data_clean > 0, 1, 0)
        } else {
          binary_data <- ifelse(data_clean %in% c("TRUE", "True", "true", "1", "Ya", "yes", "Y"), 1, 0)
        }
        successes <- sum(binary_data == 1)
        total <- length(binary_data)
        
        test_result <- prop.test(successes, total, p = input$prop_null, conf.level = input$prop_conf_level)
        
        interpretation_text <- paste(
          "Uji Proporsi 1 Kelompok\n", strrep("=", 25), "\n",
          "Jumlah sukses (nilai 1):", successes, "\n",
          "Total observasi valid:", total, "\n",
          "Proporsi sampel:", round(successes/total, 4), "\n",
          "Proporsi null (H0):", input$prop_null, "\n",
          "Chi-squared:", round(test_result$statistic, 4), "\n",
          "p-value:", format(test_result$p.value, scientific = TRUE, digits = 4), "\n",
          "Confidence Interval:", round(test_result$conf.int[1], 4), "sampai", round(test_result$conf.int[2], 4)
        )
        
        alpha <- 1 - input$prop_conf_level
        conclusion_text <- ifelse(test_result$p.value < alpha,
                                  paste("Kesimpulan: Dengan tingkat signifikansi", alpha, ", kita TOLAK H0. Terdapat bukti yang cukup kuat bahwa proporsi populasi dari", input$prop_variable, "berbeda signifikan dari", input$prop_null, "."),
                                  paste("Kesimpulan: Dengan tingkat signifikansi", alpha, ", kita GAGAL MENOLAK H0. Tidak terdapat bukti yang cukup kuat bahwa proporsi populasi dari", input$prop_variable, "berbeda dari", input$prop_null, "."))
        
        uji_prop_var_results$prop_test <- test_result
        uji_prop_var_results$prop_interpretation <- paste(interpretation_text, conclusion_text, sep = "\n\n")
        
      }, error = function(e) {
        showNotification(paste("Error Uji Proporsi 1 Kelompok:", e$message), type = "error")
        return(NULL)
      })
      
    } else {
      req(input$prop_group_var)
      group_col <- cangih_data$sovi[[input$prop_group_var]]
      
      tryCatch({
        complete_cases <- !is.na(data_col) & !is.na(group_col)
        data_var_clean <- data_col[complete_cases]
        group_var_clean <- as.factor(group_col[complete_cases])
        
        unique_groups <- levels(group_var_clean)
        
        if (length(unique_groups) > 2) {
          freq_table <- table(group_var_clean)
          top_2_groups <- names(sort(freq_table, decreasing = TRUE))[1:2]
          keep_indices <- group_var_clean %in% top_2_groups
          data_var_clean <- data_var_clean[keep_indices]
          group_var_clean <- droplevels(group_var_clean[keep_indices])
          unique_groups <- levels(group_var_clean)
        }
        
        group1_name <- unique_groups[1]
        group2_name <- unique_groups[2]
        
        if(is.logical(data_var_clean)) {
          binary_data_all <- as.numeric(data_var_clean)
        } else if(is.numeric(data_var_clean)) {
          binary_data_all <- ifelse(data_var_clean > 0, 1, 0)
        } else {
          binary_data_all <- ifelse(data_var_clean %in% c("TRUE", "True", "true", "1", "Ya", "yes", "Y"), 1, 0)
        }
        
        group1_indices <- which(group_var_clean == group1_name)
        group2_indices <- which(group_var_clean == group2_name)
        binary_group1 <- binary_data_all[group1_indices]
        binary_group2 <- binary_data_all[group2_indices]
        
        successes1 <- sum(binary_group1 == 1)
        total1 <- length(binary_group1)
        successes2 <- sum(binary_group2 == 1)
        total2 <- length(binary_group2)
        
        test_result <- prop.test(c(successes1, successes2), c(total1, total2), conf.level = input$prop_conf_level)
        
        interpretation_text <- paste(
          "Uji Proporsi 2 Kelompok\n", strrep("=", 25), "\n",
          "Kelompok 1 (", group1_name, "): Sukses=", successes1, ", Total=", total1, " (Proporsi:", round(successes1/total1, 4), ")\n",
          "Kelompok 2 (", group2_name, "): Sukses=", successes2, ", Total=", total2, " (Proporsi:", round(successes2/total2, 4), ")\n",
          "Chi-squared:", round(test_result$statistic, 4), "\n",
          "p-value:", format(test_result$p.value, scientific = TRUE, digits = 4), "\n",
          "Confidence Interval selisih:", round(test_result$conf.int[1], 4), "sampai", round(test_result$conf.int[2], 4)
        )
        
        alpha <- 1 - input$prop_conf_level
        conclusion_text <- ifelse(test_result$p.value < alpha,
                                  paste("Kesimpulan: Dengan tingkat signifikansi", alpha, ", kita TOLAK H0. Terdapat perbedaan yang signifikan antara proporsi", input$prop_variable, "pada kelompok", group1_name, "dan kelompok", group2_name, "."),
                                  paste("Kesimpulan: Dengan tingkat signifikansi", alpha, ", kita GAGAL MENOLAK H0. Tidak terdapat perbedaan yang signifikan antara proporsi", input$prop_variable, "pada kelompok", group1_name, "dan kelompok", group2_name, "."))
        
        uji_prop_var_results$prop_test <- test_result
        uji_prop_var_results$prop_interpretation <- paste(interpretation_text, conclusion_text, sep = "\n\n")
        
      }, error = function(e) {
        showNotification(paste("Error Uji Proporsi 2 Kelompok:", e$message), type = "error")
        return(NULL)
      })
    }
  })
  
  output$prop_test_result <- renderPrint({ 
    req(uji_prop_var_results$prop_test)
    uji_prop_var_results$prop_test 
  })
  
  output$prop_interpretation <- renderText({ 
    req(uji_prop_var_results$prop_interpretation)
    uji_prop_var_results$prop_interpretation 
  })
  
  observeEvent(input$run_var_test, {
    req(input$var_variable)
    
    uji_prop_var_results$var_test <- NULL
    uji_prop_var_results$var_interpretation <- NULL
    
    data_var <- cangih_data$sovi[[input$var_variable]]
    if (!is.numeric(data_var)) {
      showNotification("Variabel untuk Uji Varians harus numerik.", type = "error")
      return(NULL)
    }
    data_var <- data_var[!is.na(data_var)]
    
    if (length(data_var) < 2) { 
      showNotification("Minimal 2 observasi diperlukan untuk menghitung varians.", type = "error")
      return(NULL)
    }
    
    if(input$var_test_type == "1_sample") {
      if (input$var_null <= 0) { 
        showNotification("Nilai varians null harus positif.", type = "error")
        return(NULL) 
      }
      
      n <- length(data_var)
      sample_var <- var(data_var)
      
      chi_stat <- (n - 1) * sample_var / input$var_null
      
      p_value <- 2 * min(pchisq(chi_stat, df = n - 1), 1 - pchisq(chi_stat, df = n - 1))
      
      alpha <- 1 - input$var_conf_level
      ci_lower <- (n - 1) * sample_var / qchisq(1 - alpha/2, df = n - 1)
      ci_upper <- (n - 1) * sample_var / qchisq(alpha/2, df = n - 1)
      
      test_result_text <- paste(
        "Uji Varians 1 Kelompok (Chi-squared Test)\n", strrep("=", 35), "\n",
        "Variabel:", input$var_variable, "\n",
        "Jumlah observasi:", n, "\n",
        "Varians sampel:", round(sample_var, 4), "\n",
        "Varians null (H0):", input$var_null, "\n",
        "Chi-squared statistic:", round(chi_stat, 4), "\n",
        "Degrees of freedom (df):", n - 1, "\n",
        "p-value:", format(p_value, scientific = TRUE, digits = 4), "\n",
        "Confidence Interval (", input$var_conf_level*100, "%): [", round(ci_lower, 4), ", ", round(ci_upper, 4), "]"
      )
      
      conclusion_text <- ifelse(p_value < alpha,
                                paste0("Kesimpulan: Dengan tingkat signifikansi ", alpha, ", kita TOLAK H0. Terdapat bukti yang cukup kuat bahwa varians populasi dari ", input$var_variable, " berbeda signifikan dari ", input$var_null, "."),
                                paste0("Kesimpulan: Dengan tingkat signifikansi ", alpha, ", kita GAGAL MENOLAK H0. Tidak terdapat bukti yang cukup kuat bahwa varians populasi dari ", input$var_variable, " berbeda dari ", input$var_null, "."))
      
      uji_prop_var_results$var_test <- test_result_text
      uji_prop_var_results$var_interpretation <- conclusion_text
      
    } else {
      req(input$var_group_var)
      group_col <- cangih_data$sovi[[input$var_group_var]]
      
      available_categorical <- get_categorical_vars(cangih_data$sovi, min_levels = 2, max_levels = Inf)
      if (!(input$var_group_var %in% available_categorical)) {
        showNotification("Variabel kelompok untuk uji varians harus kategorikal dengan minimal 2 kategori.", type = "error")
        return(NULL)
      }
      
      complete_cases <- !is.na(data_var) & !is.na(group_col)
      data_var_clean <- data_var[complete_cases]
      group_var_clean <- as.factor(group_col[complete_cases])
      
      unique_groups <- levels(group_var_clean)
      if (length(unique_groups) < 2) {
        showNotification("Variabel kelompok harus memiliki minimal 2 kategori unik untuk uji varians 2 sampel.", type = "error", duration = 8)
        return(NULL)
      }
      
      
      if (length(unique_groups) > 2) {
        freq_table <- table(group_var_clean)
        top_2_groups <- names(sort(freq_table, decreasing = TRUE))[1:2]
        keep_indices <- group_var_clean %in% top_2_groups
        data_var_clean <- data_var_clean[keep_indices]
        group_var_clean <- droplevels(group_var_clean[keep_indices])
        unique_groups <- levels(group_var_clean)
        showNotification(paste0("Variabel kelompok memiliki >2 kategori. Menggunakan 2 kategori terbanyak: '", unique_groups[1], "' dan '", unique_groups[2], "'."), type = "message", duration = 8)
      }
      
      group1_name <- unique_groups[1]
      group2_name <- unique_groups[2]
      data_group1 <- data_var_clean[group_var_clean == group1_name]
      data_group2 <- data_var_clean[group_var_clean == group2_name]
      
      if (length(data_group1) < 2 || length(data_group2) < 2) {
        showNotification("Minimal 2 observasi per kelompok diperlukan untuk uji varians 2 sampel.", type = "error")
        return(NULL)
      }
      
      f_test_result <- var.test(data_group1, data_group2, conf.level = input$var_conf_level)
      
      test_result_text <- paste(
        "Uji Varians 2 Kelompok (F-test)\n", strrep("=", 33), "\n",
        "Variabel Numerik:", input$var_variable, "\n",
        "Variabel Kelompok:", input$var_group_var, "\n",
        "Kelompok 1 (", group1_name, "): n =", length(data_group1), ", varians =", round(var(data_group1), 4), "\n",
        "Kelompok 2 (", group2_name, "): n =", length(data_group2), ", varians =", round(var(data_group2), 4), "\n",
        "F-statistic:", round(f_test_result$statistic, 4), "\n",
        "p-value:", format(f_test_result$p.value, scientific = TRUE, digits = 4), "\n",
        "Confidence Interval rasio (", input$var_conf_level*100, "%): [", round(f_test_result$conf.int[1], 4), ", ", round(f_test_result$conf.int[2], 4), "]"
      )
      
      alpha <- 1 - input$var_conf_level
      conclusion_text <- ifelse(f_test_result$p.value < alpha,
                                paste0("Kesimpulan: Dengan tingkat signifikansi ", alpha, ", kita TOLAK H0. Terdapat perbedaan yang signifikan antara varians ", input$var_variable, " pada kelompok ", group1_name, " dan kelompok ", group2_name, "."),
                                paste0("Kesimpulan: Dengan tingkat signifikansi ", alpha, ", kita GAGAL MENOLAK H0. Tidak terdapat perbedaan yang signifikan antara varians ", input$var_variable, " pada kelompok ", group1_name, " dan kelompok ", group2_name, "."))
      
      uji_prop_var_results$var_test <- test_result_text
      uji_prop_var_results$var_interpretation <- conclusion_text
    }
  })
  
  output$var_test_result <- renderText({ 
    req(uji_prop_var_results$var_test)
    uji_prop_var_results$var_test 
  })
  
  output$var_interpretation <- renderText({ 
    req(uji_prop_var_results$var_interpretation)
    uji_prop_var_results$var_interpretation 
  })
  
  output$download_word_prop_var <- downloadHandler(
    filename = function() {
      paste0("Laporan_Uji_Proporsi_Varians_", Sys.Date(), ".docx")
    },
    content = function(file) {
      content_lines <- c(
        "# LAPORAN HASIL UJI PROPORSI DAN VARIANS",
        "",
        paste("**Tanggal:** ", format(Sys.Date(), "%d %B %Y")),
        "",
        "## Ringkasan Analisis",
        "",
        "Laporan ini berisi hasil analisis uji proporsi dan uji varians yang telah dilakukan pada dataset.",
        ""
      )
      
      if(!is.null(uji_prop_var_results$prop_test) && !is.null(uji_prop_var_results$prop_interpretation)) {
        content_lines <- c(content_lines,
                           "## UJI PROPORSI",
                           "",
                           "### Hasil Statistik",
                           "",
                           capture.output(uji_prop_var_results$prop_test),
                           "",
                           "### Interpretasi",
                           "",
                           strsplit(uji_prop_var_results$prop_interpretation, "\n")[[1]],
                           ""
        )
      } else {
        content_lines <- c(content_lines,
                           "## UJI PROPORSI", 
                           "",
                           "*Belum ada hasil uji proporsi yang dijalankan.*",
                           ""
        )
      }
      
      if(!is.null(uji_prop_var_results$var_test) && !is.null(uji_prop_var_results$var_interpretation)) {
        content_lines <- c(content_lines,
                           "## UJI VARIANS",
                           "",
                           "### Hasil Statistik", 
                           "",
                           strsplit(uji_prop_var_results$var_test, "\n")[[1]],
                           "",
                           "### Interpretasi",
                           "",
                           strsplit(uji_prop_var_results$var_interpretation, "\n")[[1]],
                           ""
        )
      } else {
        content_lines <- c(content_lines,
                           "## UJI VARIANS",
                           "",
                           "*Belum ada hasil uji varians yang dijalankan.*",
                           ""
        )
      }
      
      content_lines <- c(content_lines,
                         "## Catatan Metodologi",
                         "",
                         "### Uji Proporsi",
                         "- **1 Kelompok**: Menggunakan chi-squared test untuk menguji apakah proporsi sampel berbeda dari nilai hipotesis null",
                         "- **2 Kelompok**: Menggunakan two-proportion z-test untuk membandingkan proporsi antar kelompok",
                         "",
                         "### Uji Varians",
                         "- **1 Kelompok**: Menggunakan chi-squared test untuk menguji apakah varians sampel berbeda dari nilai hipotesis null", 
                         "- **2 Kelompok**: Menggunakan F-test untuk membandingkan varians antar kelompok",
                         "",
                         "---",
                         "",
                         paste("*Laporan dibuat otomatis pada", format(Sys.time(), "%d %B %Y pukul %H:%M:%S"), "*")
      )
      
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      rmd_content <- c(
        "---",
        "output: word_document",
        "---",
        "",
        content_lines
      )
      
      writeLines(rmd_content, temp_rmd)
      
      tryCatch({
        rmarkdown::render(
          input = temp_rmd,
          output_file = file,
          quiet = TRUE
        )
      }, error = function(e) {
        showNotification(paste("Error saat membuat laporan:", e$message), type = "error")
      })
      
      unlink(temp_rmd)
    }
  )
  
  
  # =====================================
  # 6. ANOVA - SERVER LOGIC 
  # =====================================
  anova_results_reactive <- reactiveValues(
    filtered_data = NULL,
    model = NULL,
    anova_summary = NULL,
    anova_table = NULL,
    p_values = NULL,
    tukey_test = NULL
  )
  
  observe({
    numeric_vars <- get_numeric_vars(cangih_data$sovi)
    categorical_vars_anova <- get_categorical_vars(cangih_data$sovi, min_levels = 2)
    
    updateSelectInput(session, "dep_var", choices = numeric_vars)
    updateSelectInput(session, "factor1", choices = categorical_vars_anova)
    updateSelectInput(session, "factor2", choices = categorical_vars_anova)
  })
  
  observeEvent(input$run_anova, {
    req(input$dep_var, input$factor1)
    
    uji_rerata_results$params <- NULL 
    anova_results_reactive$filtered_data <- NULL
    anova_results_reactive$model <- NULL
    anova_results_reactive$anova_summary <- NULL
    anova_results_reactive$anova_table <- NULL
    anova_results_reactive$p_values <- NULL
    anova_results_reactive$tukey_test <- NULL
    
    data_anova <- cangih_data$sovi
    
    if (!(input$dep_var %in% get_numeric_vars(data_anova))) {
      showNotification("Variabel Dependen harus numerik.", type = "error")
      return(NULL)
    }
    if (!(input$factor1 %in% get_categorical_vars(data_anova, min_levels = 2))) {
      showNotification("Faktor 1 harus kategorikal dengan minimal 2 kategori unik.", type = "error")
      return(NULL)
    }
    
    if (input$anova_type == "oneway") {
      data_anova <- data_anova[!is.na(data_anova[[input$dep_var]]) & !is.na(data_anova[[input$factor1]]), ]
      data_anova[[input$factor1]] <- as.factor(data_anova[[input$factor1]])
      
      if (length(unique(data_anova[[input$factor1]])) < 2) {
        showNotification("Faktor 1 harus memiliki minimal 2 kategori unik dengan data valid setelah menghilangkan NA.", type = "error", duration = 5)
        return(NULL)
      }
      
      model <- aov(as.formula(paste(input$dep_var, "~", input$factor1)), data = data_anova)
      
    } else {
      req(input$factor2)
      if (!(input$factor2 %in% get_categorical_vars(data_anova, min_levels = 2))) {
        showNotification("Faktor 2 harus kategorikal dengan minimal 2 kategori unik.", type = "error")
        return(NULL)
      }
      
      data_anova <- data_anova[!is.na(data_anova[[input$dep_var]]) & 
                                 !is.na(data_anova[[input$factor1]]) & 
                                 !is.na(data_anova[[input$factor2]]), ]
      data_anova[[input$factor1]] <- as.factor(data_anova[[input$factor1]])
      data_anova[[input$factor2]] <- as.factor(data_anova[[input$factor2]])
      
      if (length(unique(data_anova[[input$factor1]])) < 2 || length(unique(data_anova[[input$factor2]])) < 2) {
        showNotification("Kedua faktor harus memiliki minimal 2 kategori unik dengan data valid setelah menghilangkan NA.", type = "error", duration = 5)
        return(NULL)
      }
      
      if (input$include_interaction) {
        model <- aov(as.formula(paste(input$dep_var, "~", input$factor1, "*", input$factor2)), data = data_anova)
      } else {
        model <- aov(as.formula(paste(input$dep_var, "~", input$factor1, "+", input$factor2)), data = data_anova)
      }
    }
    
    if (nrow(data_anova) == 0) {
      showNotification("Tidak ada observasi lengkap setelah menghilangkan nilai hilang untuk ANOVA. Periksa data Anda.", type = "error")
      return(NULL)
    }
    
    anova_results_reactive$filtered_data <- data_anova
    anova_results_reactive$model <- model
    anova_results_reactive$anova_summary <- summary(model)
    
    tryCatch({
      anova_table <- broom::tidy(model)
      anova_results_reactive$anova_table <- anova_table
      anova_results_reactive$p_values <- setNames(anova_table$p.value, anova_table$term)
    }, error = function(e) {
      showNotification(paste("Error dalam mengekstrak hasil ANOVA:", e$message), type = "error")
      return(NULL)
    })
    
    anova_results_reactive$tukey_test <- perform_tukey_test(model, anova_results_reactive$p_values, input)
    
    showNotification("Analisis ANOVA berhasil dijalankan!", type = "message", duration = 3)
  })
  
  perform_tukey_test <- function(model, p_values, input) {
    if (is.null(p_values) || length(p_values) == 0) {
      return("Error: Tidak dapat mengekstrak p-values untuk uji Tukey.")
    }
    
    alpha <- 0.05
    tukey_results <- list()
    
    if (input$anova_type == "oneway") {
      p_val_factor1 <- p_values[input$factor1]
      
      if (!is.na(p_val_factor1) && p_val_factor1 < alpha) {
        tryCatch({
          tukey_results[[input$factor1]] <- TukeyHSD(model, which = input$factor1)
        }, error = function(e) {
          tukey_results[["error"]] <- paste("Error saat melakukan TukeyHSD untuk", input$factor1, ":", e$message)
        })
      }
      
    } else {
      factors_to_check <- c(input$factor1, input$factor2)
      
      for (factor_name in factors_to_check) {
        p_val <- p_values[factor_name]
        
        if (!is.na(p_val) && p_val < alpha) {
          tryCatch({
            tukey_results[[factor_name]] <- TukeyHSD(model, which = factor_name)
          }, error = function(e) {
            tukey_results[[paste0("error_", factor_name)]] <- paste("Error saat melakukan TukeyHSD untuk", factor_name, ":", e$message)
          })
        }
      }
    }
    
    if (length(tukey_results) == 0) {
      return("Tidak ada efek signifikan yang ditemukan. Uji Post-Hoc Tukey HSD tidak diperlukan.")
    }
    
    return(tukey_results)
  }
  
  output$descriptive_stats <- DT::renderDataTable({
    req(anova_results_reactive$filtered_data)
    data <- anova_results_reactive$filtered_data
    
    if (input$anova_type == "oneway") {
      desc_stats <- data %>%
        group_by(!!sym(input$factor1)) %>%
        summarise(
          N = n(),
          Mean = mean(!!sym(input$dep_var), na.rm = TRUE),
          SD = sd(!!sym(input$dep_var), na.rm = TRUE),
          Min = min(!!sym(input$dep_var), na.rm = TRUE),
          Max = max(!!sym(input$dep_var), na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(across(c(Mean, SD, Min, Max), ~round(., 3)))
    } else {
      req(input$factor2)
      desc_stats <- data %>%
        group_by(!!sym(input$factor1), !!sym(input$factor2)) %>%
        summarise(
          N = n(),
          Mean = mean(!!sym(input$dep_var), na.rm = TRUE),
          SD = sd(!!sym(input$dep_var), na.rm = TRUE),
          Min = min(!!sym(input$dep_var), na.rm = TRUE),
          Max = max(!!sym(input$dep_var), na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(across(c(Mean, SD, Min, Max), ~round(., 3)))
    }
    DT::datatable(desc_stats, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$anova_results <- renderText({
    req(anova_results_reactive$anova_table)
    
    formula_text <- if (input$anova_type == "oneway") {
      paste(input$dep_var, "~", input$factor1)
    } else {
      if (input$include_interaction) { 
        paste(input$dep_var, "~", input$factor1, "*", input$factor2) 
      } else { 
        paste(input$dep_var, "~", input$factor1, "+", input$factor2) 
      }
    }
    
    anova_table <- anova_results_reactive$anova_table
    
    result_text <- paste0("=== ", toupper(input$anova_type), " ANOVA RESULTS ===\n",
                          "Formula: ", formula_text, "\n\n",
                          "ANOVA Table:\n",
                          sprintf("%-15s %8s %10s %10s %10s %10s\n", 
                                  "Source", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)"),
                          paste(rep("-", 75), collapse = ""), "\n")
    
    for (i in 1:nrow(anova_table)) {
      result_text <- paste0(result_text,
                            sprintf("%-15s %8.0f %10.3f %10.3f %10.3f %10.6f\n",
                                    anova_table$term[i],
                                    anova_table$df[i],
                                    anova_table$sumsq[i],
                                    anova_table$meansq[i],
                                    anova_table$statistic[i],
                                    anova_table$p.value[i]))
    }
    
    return(result_text)
  })
  
  
  output$interpretation_anova <- renderText({
    req(anova_results_reactive$p_values)
    
    validate(
      need(!is.null(anova_results_reactive$p_values), "Error: Tidak dapat mengekstrak p-values."),
      need(length(anova_results_reactive$p_values) > 0, "Error: P-values kosong.")
    )
    
    p_values_vec <- anova_results_reactive$p_values
    alpha <- 0.05
    
    if (input$anova_type == "oneway") {
      p_value_factor1 <- p_values_vec[input$factor1]
      
      validate(
        need(!is.na(p_value_factor1), paste("Error: Tidak dapat mengekstrak p-value untuk", input$factor1))
      )
      
      interpretation_text <- paste0("INTERPRETASI ONE-WAY ANOVA:\n\n",
                                    "H0: Rata-rata ", input$dep_var, " semua kelompok ", input$factor1, " sama\n",
                                    "H1: Minimal ada satu kelompok ", input$factor1, " yang rata-ratanya berbeda\n\n",
                                    "p-value untuk ", input$factor1, " = ", format.pval(p_value_factor1, digits = 4), "\n\n",
                                    "Kesimpulan: ",
                                    if (p_value_factor1 < alpha) {
                                      paste0("TOLAK H0 (p < ", alpha, ")\nTerdapat perbedaan signifikan rata-rata ", input$dep_var, " antar kelompok ", input$factor1, ".\nLanjutkan dengan uji post-hoc (Tukey HSD) untuk mengetahui kelompok mana yang berbeda.")
                                    } else {
                                      paste0("GAGAL TOLAK H0 (p ≥ ", alpha, ")\nTIDAK terdapat perbedaan signifikan rata-rata ", input$dep_var, " antar kelompok ", input$factor1, ".")
                                    })
    } else {
      p_value_factor1 <- p_values_vec[input$factor1]
      p_value_factor2 <- p_values_vec[input$factor2]
      
      validate(
        need(!is.na(p_value_factor1), paste("Error: Tidak dapat mengekstrak p-value untuk", input$factor1)),
        need(!is.na(p_value_factor2), paste("Error: Tidak dapat mengekstrak p-value untuk", input$factor2))
      )
      
      interpretation_text <- paste0("INTERPRETASI TWO-WAY ANOVA:\n\n",
                                    "Tingkat Signifikansi (α): ", alpha, "\n\n",
                                    "**1. Efek Utama (Main Effect) ", input$factor1, ":**\n",
                                    "   H0: Tidak ada perbedaan rata-rata ", input$dep_var, " antar level ", input$factor1, "\n",
                                    "   H1: Ada perbedaan rata-rata ", input$dep_var, " antar level ", input$factor1, "\n",
                                    "   p-value = ", format.pval(p_value_factor1, digits = 4), "\n",
                                    "   Kesimpulan: ", if (p_value_factor1 < alpha) { "SIGNIFIKAN - Ada perbedaan rata-rata antar kelompok." } else { "TIDAK SIGNIFIKAN - Tidak ada perbedaan rata-rata antar kelompok." }, "\n\n",
                                    
                                    "**2. Efek Utama (Main Effect) ", input$factor2, ":**\n",
                                    "   H0: Tidak ada perbedaan rata-rata ", input$dep_var, " antar level ", input$factor2, "\n",
                                    "   H1: Ada perbedaan rata-rata ", input$dep_var, " antar level ", input$factor2, "\n",
                                    "   p-value = ", format.pval(p_value_factor2, digits = 4), "\n",
                                    "   Kesimpulan: ", if (p_value_factor2 < alpha) { "SIGNIFIKAN - Ada perbedaan rata-rata antar kelompok." } else { "TIDAK SIGNIFIKAN - Tidak ada perbedaan rata-rata antar kelompok." }, "\n\n")
      
      if (input$include_interaction) {
        interaction_terms <- names(p_values_vec)[grepl(":", names(p_values_vec))]
        
        if (length(interaction_terms) > 0) {
          interaction_term <- interaction_terms[1]  
          p_value_interaction <- p_values_vec[interaction_term]
          
          if (!is.na(p_value_interaction)) {
            interpretation_text <- paste0(interpretation_text,
                                          "**3. Efek Interaksi (Interaction Effect) ", input$factor1, " × ", input$factor2, ":**\n",
                                          "   H0: Tidak ada interaksi antara ", input$factor1, " dan ", input$factor2, " terhadap ", input$dep_var, "\n",
                                          "   H1: Ada interaksi antara ", input$factor1, " dan ", input$factor2, " terhadap ", input$dep_var, "\n",
                                          "   p-value = ", format.pval(p_value_interaction, digits = 4), "\n",
                                          "   Kesimpulan: ", if (p_value_interaction < alpha) { "SIGNIFIKAN - Ada interaksi antara faktor-faktor. Efek satu faktor bergantung pada level faktor lain." } else { "TIDAK SIGNIFIKAN - Tidak ada interaksi antara faktor-faktor. Efek faktor-faktor bersifat aditif." }, "\n")
          }
        }
      }
    }
    return(interpretation_text)
  })
  
  
  output$posthoc_results <- renderPrint({
    req(input$run_anova)
    if (is.null(anova_results_reactive$tukey_test)) return(NULL)
    
    tukey_results <- anova_results_reactive$tukey_test
    
    if (is.character(tukey_results)) {
      cat(tukey_results)
      return()
    }
    
    if (is.list(tukey_results)) {
      cat("=== TUKEY HSD POST-HOC TEST ===\n\n")
      cat("Karena ANOVA menunjukkan adanya efek signifikan, dilakukan uji post-hoc untuk perbandingan berpasangan:\n\n")
      
      for (factor_name in names(tukey_results)) {
        if (grepl("^error", factor_name)) {
          cat("ERROR:", tukey_results[[factor_name]], "\n\n")
        } else {
          cat("--- Tukey HSD untuk", factor_name, "---\n")
          print(tukey_results[[factor_name]])
          cat("\n")
        }
      }
      
      cat("Interpretasi:\n")
      cat("- Pasangan kelompok dengan p adj < 0.05 memiliki perbedaan rata-rata yang signifikan (pada α = 0.05).\n")
      cat("- Pasangan kelompok dengan p adj ≥ 0.05 tidak memiliki perbedaan rata-rata yang signifikan.\n")
    }
  })
  
  # Download handler untuk laporan ANOVA
  output$download_anova_report <- downloadHandler(
    filename = function() {
      paste0("Laporan_ANOVA_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(anova_results_reactive$anova_table)
      
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      rmd_content <- paste0('
        ---
        title: "Laporan Analisis ANOVA"
        author: "Generated by Shiny App"
        date: "', format(Sys.Date(), "%d %B %Y"), '"
        output: 
          word_document:
            reference_docx: null
        ---
        
        ```{r setup, include=FALSE}
        knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
        library(knitr)
        library(dplyr)
        ```
        
        # Ringkasan Analisis
        
        **Jenis Analisis:** ', toupper(input$anova_type), ' ANOVA  
        **Variabel Dependen:** ', input$dep_var, '  
        **Faktor 1:** ', input$factor1, '  
        ', ifelse(input$anova_type == "twoway", paste0("**Faktor 2:** ", input$factor2, "  \n"), ""), '
        ', ifelse(input$anova_type == "twoway" && input$include_interaction, "**Interaksi:** Ya  \n", ""), '
        **Tanggal Analisis:** ', format(Sys.Date(), "%d %B %Y"), '
        
        ---
        
        ## 1. Statistik Deskriptif
        
        ```{r descriptive_table}
        # Recreate descriptive statistics
        data_for_desc <- anova_results_reactive$filtered_data
        
        if (input$anova_type == "oneway") {
          desc_stats <- data_for_desc %>%
            group_by(!!sym(input$factor1)) %>%
            summarise(
              N = n(),
              Mean = round(mean(!!sym(input$dep_var), na.rm = TRUE), 3),
              SD = round(sd(!!sym(input$dep_var), na.rm = TRUE), 3),
              Min = round(min(!!sym(input$dep_var), na.rm = TRUE), 3),
              Max = round(max(!!sym(input$dep_var), na.rm = TRUE), 3),
              .groups = "drop"
            )
        } else {
          desc_stats <- data_for_desc %>%
            group_by(!!sym(input$factor1), !!sym(input$factor2)) %>%
            summarise(
              N = n(),
              Mean = round(mean(!!sym(input$dep_var), na.rm = TRUE), 3),
              SD = round(sd(!!sym(input$dep_var), na.rm = TRUE), 3),
              Min = round(min(!!sym(input$dep_var), na.rm = TRUE), 3),
              Max = round(max(!!sym(input$dep_var), na.rm = TRUE), 3),
              .groups = "drop"
            )
        }
        
        kable(desc_stats, caption = "Statistik Deskriptif per Grup")
        ```
        
        ## 2. Hasil ANOVA
        
        **Formula:** ', 
          ifelse(input$anova_type == "oneway", 
                 paste(input$dep_var, "~", input$factor1),
                 ifelse(input$include_interaction,
                        paste(input$dep_var, "~", input$factor1, "*", input$factor2),
                        paste(input$dep_var, "~", input$factor1, "+", input$factor2)
                 )
          ), '
        
        ```{r anova_table}
        # Format ANOVA table
        anova_table <- anova_results_reactive$anova_table
        anova_table$p.value <- ifelse(anova_table$p.value < 0.001, "< 0.001", 
                                      sprintf("%.6f", anova_table$p.value))
        
        # Rename columns for better presentation
        colnames(anova_table) <- c("Sumber Variasi", "Df", "Sum of Squares", 
                                  "Mean Square", "F-value", "P-value")
        
        kable(anova_table, caption = "Tabel ANOVA", digits = 3)
        ```
        
        ## 3. Interpretasi
        
        ', 
          # Generate interpretation text
          paste0(
            ifelse(input$anova_type == "oneway",
                   paste0("**Hipotesis:**\n",
                          "- H₀: Rata-rata ", input$dep_var, " semua kelompok ", input$factor1, " sama\n",
                          "- H₁: Minimal ada satu kelompok ", input$factor1, " yang rata-ratanya berbeda\n\n",
                          "**Hasil:**\n",
                          "- P-value untuk ", input$factor1, " = ", format.pval(anova_results_reactive$p_values[input$factor1], digits = 4), "\n",
                          "- Tingkat signifikansi (α) = 0.05\n\n",
                          "**Kesimpulan:**\n",
                          ifelse(anova_results_reactive$p_values[input$factor1] < 0.05,
                                 paste0("Tolak H₀ (p < 0.05). Terdapat perbedaan signifikan rata-rata ", input$dep_var, " antar kelompok ", input$factor1, "."),
                                 paste0("Gagal tolak H₀ (p ≥ 0.05). Tidak terdapat perbedaan signifikan rata-rata ", input$dep_var, " antar kelompok ", input$factor1, ".")
                          )
                   ),
                   paste0("**Efek Utama ", input$factor1, ":**\n",
                          "- P-value = ", format.pval(anova_results_reactive$p_values[input$factor1], digits = 4), "\n",
                          "- Hasil: ", ifelse(anova_results_reactive$p_values[input$factor1] < 0.05, "Signifikan", "Tidak Signifikan"), "\n\n",
                          "**Efek Utama ", input$factor2, ":**\n",
                          "- P-value = ", format.pval(anova_results_reactive$p_values[input$factor2], digits = 4), "\n",
                          "- Hasil: ", ifelse(anova_results_reactive$p_values[input$factor2] < 0.05, "Signifikan", "Tidak Signifikan"), "\n\n",
                          ifelse(input$include_interaction && any(grepl(":", names(anova_results_reactive$p_values))),
                                 paste0("**Efek Interaksi:**\n",
                                        "- P-value = ", format.pval(anova_results_reactive$p_values[grep(":", names(anova_results_reactive$p_values))[1]], digits = 4), "\n",
                                        "- Hasil: ", ifelse(anova_results_reactive$p_values[grep(":", names(anova_results_reactive$p_values))[1]] < 0.05, "Signifikan", "Tidak Signifikan")), "")
                   )
            )
          ), '
        
        ', 
        ifelse(!is.null(anova_results_reactive$tukey_test) && is.list(anova_results_reactive$tukey_test),
               '\n\n## 4. Post-Hoc Test (Tukey HSD)\n\nKarena ditemukan efek signifikan, dilakukan uji post-hoc untuk perbandingan berpasangan:\n\n```{r tukey_results}\n# Display Tukey results\ntukey_results <- anova_results_reactive$tukey_test\nfor (factor_name in names(tukey_results)) {\n  if (!grepl("^error", factor_name)) {\n    cat("\\n### Tukey HSD untuk", factor_name, "\\n")\n    print(tukey_results[[factor_name]])\n  }\n}\n```\n\n**Interpretasi Post-Hoc:**\n- Pasangan kelompok dengan p adj < 0.05 memiliki perbedaan rata-rata yang signifikan.\n- Pasangan kelompok dengan p adj ≥ 0.05 tidak memiliki perbedaan rata-rata yang signifikan.',
               ''
        ), '
        
        ---
        *Laporan ini dibuat secara otomatis oleh aplikasi Shiny pada ', format(Sys.time(), "%d %B %Y pukul %H:%M"), '*
        ')
            
            # Write R Markdown content to temporary file
            writeLines(rmd_content, temp_rmd)
            
            # Render to Word document
            tryCatch({
              rmarkdown::render(temp_rmd, 
                                output_file = file,
                                quiet = TRUE)
            }, error = function(e) {
              showNotification(paste("Error saat membuat laporan:", e$message), type = "error")
            })
          }
        )
  
  
  # =====================================
  # 7. REGRESSION ANALYSIS - SERVER LOGIC
  # =====================================
  regression_results_reactive <- reactiveValues(
    model = NULL,
    numeric_vars_for_reg = NULL
  )
  
  observe({
    numeric_vars_reg <- get_numeric_vars(cangih_data$sovi)
    regression_results_reactive$numeric_vars_for_reg <- numeric_vars_reg
    
    updateSelectInput(session, "dependent_var", choices = numeric_vars_reg)
    updateCheckboxGroupInput(session, "independent_vars", choices = numeric_vars_reg)
  })
  
  observeEvent(input$run_regression, {
    req(input$dependent_var, input$independent_vars)
    
    regression_results_reactive$model <- NULL
    
    if (length(input$independent_vars) == 0) {
      showNotification("Pilih setidaknya satu variabel independen.", type = "error")
      return(NULL)
    }
    if (input$dependent_var %in% input$independent_vars) {
      showNotification("Variabel dependen tidak boleh termasuk dalam variabel independen.", type = "error")
      return(NULL)
    }
    
    all_selected_vars <- c(input$dependent_var, input$independent_vars)
    if (!all(all_selected_vars %in% get_numeric_vars(cangih_data$sovi))) {
      showNotification("Semua variabel yang dipilih (dependen dan independen) harus numerik.", type = "error")
      return(NULL)
    }
    
    reg_data <- cangih_data$sovi[all_selected_vars]
    reg_data <- na.omit(reg_data)
    
    if (nrow(reg_data) == 0) {
      showNotification("Tidak ada observasi lengkap setelah menghilangkan nilai hilang. Periksa data Anda.", type = "error")
      return(NULL)
    }
    
    constant_vars <- sapply(reg_data, function(x) length(unique(x)) == 1)
    if (any(constant_vars)) {
      showNotification(paste0("Variabel-variabel berikut bersifat konstan setelah menghilangkan NA dan tidak dapat digunakan dalam regresi: ",
                              paste(names(reg_data)[constant_vars], collapse = ", ")), type = "error", duration = 10)
      return(NULL)
    }
    
    formula_str <- paste(input$dependent_var, "~", paste(input$independent_vars, collapse = " + "))
    
    tryCatch({
      regression_results_reactive$model <- lm(as.formula(formula_str), data = reg_data)
      showNotification("Analisis regresi berhasil dijalankan!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error saat menjalankan regresi:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$regression_results <- renderPrint({
    req(regression_results_reactive$model)
    model_summary <- summary(regression_results_reactive$model)
    
    cat("=== HASIL REGRESI LINEAR BERGANDA ===\n")
    print(model_summary)
    
    cat("\n\n=== INTERPRETASI KOEFISIEN REGRESI ===\n\n")
    coef_summary <- model_summary$coefficients
    
    cat("Persamaan Regresi:\n")
    eq_str <- paste0(input$dependent_var, " = ", round(coef_summary[1,1], 4))
    for(i in 2:nrow(coef_summary)) {
      coef_val <- coef_summary[i,1]
      var_name <- rownames(coef_summary)[i]
      sign_char <- ifelse(coef_val >= 0, " + ", " - ")
      eq_str <- paste0(eq_str, sign_char, abs(round(coef_val, 4)), "*", var_name)
    }
    cat(eq_str, "\n\n")
    
    cat("Interpretasi Koefisien:\n")
    cat("Intercept (", round(coef_summary[1,1], 4), "):\n")
    cat("  Ketika semua variabel independen bernilai 0, nilai rata-rata dari", input$dependent_var, "diperkirakan adalah", round(coef_summary[1,1], 4), ".\n\n")
    
    for(i in 2:nrow(coef_summary)) {
      var_name <- rownames(coef_summary)[i]
      coef_val <- coef_summary[i,1]
      std_err <- coef_summary[i,2]
      t_val <- coef_summary[i,3]
      p_val <- coef_summary[i,4]
      significance <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", ifelse(p_val < 0.1, ".", ""))))
      
      cat(var_name, ": Koefisien = ", round(coef_val, 4), " (Std. Error = ", round(std_err, 4), ", t-value = ", round(t_val, 4), ", p-value = ", format.pval(p_val, digits = 4), significance, ")\n", sep = "")
      cat("  Artinya: Dengan asumsi variabel independen lainnya konstan, setiap kenaikan 1 unit pada **", var_name, "** akan ",
          ifelse(coef_val > 0, "meningkatkan", "menurunkan"), " rata-rata **", input$dependent_var, "** sebesar ", abs(round(coef_val, 4)), " unit.\n")
      cat("  Signifikansi: Koefisien ini secara statistik ", ifelse(p_val < 0.05, "SIGNIFIKAN", "TIDAK SIGNIFIKAN"), " pada tingkat kepercayaan 95%.\n\n")
    }
  })
  
  output$correlation_matrix <- renderPlot({
    req(input$dependent_var, input$independent_vars)
    
    all_selected_vars <- c(input$dependent_var, input$independent_vars)
    all_selected_vars <- all_selected_vars[all_selected_vars %in% get_numeric_vars(cangih_data$sovi)]
    
    if (length(all_selected_vars) < 2) {
      plot(NA, xlim=c(0,1), ylim=c(0,1), main="Tidak Cukup Variabel untuk Matriks Korelasi", xaxt="n", yaxt="n", xlab="", ylab="")
      text(0.5, 0.5, "Pilih setidaknya dua variabel numerik\nuntuk menampilkan matriks korelasi.", cex=1.2)
      return(NULL)
    }
    
    cor_data <- cangih_data$sovi[all_selected_vars]
    cor_data <- na.omit(cor_data)
    
    if (nrow(cor_data) < 2) {
      plot(NA, xlim=c(0,1), ylim=c(0,1), main="Tidak Cukup Data Lengkap untuk Matriks Korelasi", xaxt="n", yaxt="n", xlab="", ylab="")
      text(0.5, 0.5, "Tidak cukup data lengkap untuk variabel yang dipilih\nsetelah menghilangkan nilai hilang.", cex=1.2)
      return(NULL)
    }
    
    cor_matrix <- cor(cor_data)
    par(mar = c(0, 0, 2, 0) + 0.1)
    corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7,
             main = "Matriks Korelasi Variabel (Numerik)")
  })
  
  output$model_stats <- renderPrint({
    req(regression_results_reactive$model)
    model_summary <- summary(regression_results_reactive$model)
    
    cat("=== STATISTIK KUALITAS MODEL ===\n")
    cat("R-squared (Koefisien Determinasi):", round(model_summary$r.squared, 4), "\n")
    cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
    cat("F-statistic:", round(model_summary$fstatistic[1], 4), "\n")
    cat("Degrees of Freedom (DF1, DF2):", model_summary$fstatistic[2], ", ", model_summary$fstatistic[3], "\n")
    cat("p-value F-test:", format.pval(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE), digits = 4), "\n")
    cat("AIC (Akaike Information Criterion):", round(AIC(regression_results_reactive$model), 4), "\n")
    cat("BIC (Bayesian Information Criterion):", round(BIC(regression_results_reactive$model), 4), "\n")
    cat("Residual Standard Error (RMSE):", round(model_summary$sigma, 4), "(df:", model_summary$df[2], ")\n")
    cat("Jumlah Observasi Valid:", nobs(regression_results_reactive$model), "\n")
  })
  
  output$model_conclusion <- renderPrint({
    req(regression_results_reactive$model)
    model_summary <- summary(regression_results_reactive$model)
    
    cat("=== KESIMPULAN & REKOMENDASI MODEL REGRESI ===\n\n")
    
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_pvalue <- pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    alpha <- 0.05
    
    cat("KUALITAS MODEL KESELURUHAN:\n")
    cat("- Koefisien Determinasi (R-squared):", round(r_squared, 4), " (", round(r_squared*100, 2), "% variasi variabel dependen dijelaskan oleh model)\n")
    cat("- Adjusted R-squared:", round(adj_r_squared, 4), "(penyesuaian untuk jumlah prediktor)\n")
    cat("- Uji F-statistik model keseluruhan:\n")
    cat("  H0: Semua koefisien regresi (selain intercept) adalah nol (model tidak signifikan)\n")
    cat("  H1: Setidaknya ada satu koefisien regresi yang tidak nol (model signifikan)\n")
    cat("  p-value F-test:", format.pval(f_pvalue, digits = 4), "\n")
    cat("  Kesimpulan: Model secara keseluruhan", ifelse(f_pvalue < alpha, "**SIGNIFIKAN**", "**TIDAK SIGNIFIKAN**"), "dalam memprediksi", input$dependent_var, ".\n\n")
    
    if(r_squared > 0.7) {
      cat("Model ini memiliki daya prediksi yang **sangat baik** (menjelaskan >70% variasi).\n")
    } else if(r_squared > 0.5) {
      cat("Model ini memiliki daya prediksi yang **cukup baik** (menjelaskan >50% variasi).\n")
    } else if (r_squared > 0.2) {
      cat("Model ini memiliki daya prediksi yang **moderat** (menjelaskan 20%-50% variasi).\n")
    } else {
      cat("Model ini memiliki daya prediksi yang **rendah** (menjelaskan <20% variasi).\n")
    }
    
    coef_summary <- model_summary$coefficients
    sig_vars <- rownames(coef_summary)[coef_summary[,4] < alpha]
    sig_vars <- sig_vars[sig_vars != "(Intercept)"]
    
    cat("\nVARIABEL INDEPENDEN YANG SIGNIFIKAN MEMPENGARUHI", input$dependent_var, "(pada α =", alpha, "):\n")
    if(length(sig_vars) > 0) {
      for(var in sig_vars) {
        coef_val <- coef_summary[var, 1]
        p_val <- coef_summary[var, 4]
        cat("- **", var, "**: Berpengaruh ", ifelse(coef_val > 0, "positif", "negatif"), " secara signifikan (p-value: ", format.pval(p_val, digits = 4), ").\n", sep = "")
      }
    } else {
      cat("Tidak ada variabel independen yang signifikan memprediksi", input$dependent_var, "pada tingkat signifikansi", alpha, ".\n")
    }
    
    cat("\nREKOMENDASI UNTUK PENGEMBANGAN MODEL:\n")
    if(f_pvalue < alpha && r_squared > 0.5) {
      cat("Model ini menunjukkan kinerja yang baik dan dapat dipertimbangkan untuk digunakan dalam prediksi dan analisis lebih lanjut, asalkan asumsi regresi terpenuhi.\n")
      cat("Disarankan untuk melakukan uji asumsi regresi (normalitas residual, homoskedastisitas, non-multikolinearitas, dll.) untuk memvalidasi lebih lanjut model ini.\n")
    } else {
      cat("Model ini mungkin perlu perbaikan. Pertimbangkan langkah-langkah berikut:\n")
      cat("- Periksa kembali variabel-variabel yang dipilih: Apakah ada variabel independen lain yang relevan secara teori?\n")
      cat("- Tangani nilai-nilai ekstrim (outlier) atau observasi berpengaruh.\n")
      cat("- Pertimbangkan transformasi variabel (misalnya, logaritmik, akar kuadrat) jika asumsi linearitas atau normalitas residual tidak terpenuhi.\n")
      cat("- Eksplorasi model regresi alternatif (misalnya, regresi non-linear, regresi robust, model dengan interaksi).\n")
      cat("- Evaluasi multikolinearitas antar variabel independen (VIF).\n")
    }
  })
  # Uji Normalitas Residual - Plot Q-Q
  output$normality_test <- renderPlot({
    req(regression_results_reactive$model)
    qqnorm(residuals(regression_results_reactive$model), main = "Q-Q Plot Residual")
    qqline(residuals(regression_results_reactive$model), col = "red")
  })
  
  # Uji Normalitas - Shapiro-Wilk Test
  output$shapiro_test <- renderPrint({
    req(regression_results_reactive$model)
    residuals <- residuals(regression_results_reactive$model)
    shapiro_result <- shapiro.test(residuals)
    cat("=== UJI NORMALITAS RESIDUAL ===\n")
    cat("Shapiro-Wilk Test\n")
    cat("W-statistic:", round(shapiro_result$statistic, 4), "\n")
    cat("p-value:", format.pval(shapiro_result$p.value, digits = 4), "\n")
    cat("Kesimpulan: Residual", ifelse(shapiro_result$p.value > 0.05, "NORMAL", "TIDAK NORMAL"), "\n")
  })
  
  # Uji Homoskedastisitas - Scatter Plot
  output$homoscedasticity_test <- renderPlot({
    req(regression_results_reactive$model)
    fitted_values <- fitted(regression_results_reactive$model)
    residuals <- residuals(regression_results_reactive$model)
    plot(fitted_values, residuals, 
         main = "Residuals vs Fitted Values",
         xlab = "Fitted Values", ylab = "Residuals")
    abline(h = 0, col = "red")
  })
  
  # Uji Homoskedastisitas - Breusch-Pagan Test
  output$bp_test <- renderPrint({
    req(regression_results_reactive$model)
    library(lmtest)
    bp_result <- bptest(regression_results_reactive$model)
    cat("=== UJI HOMOSKEDASTISITAS ===\n")
    cat("Breusch-Pagan Test\n")
    cat("BP-statistic:", round(bp_result$statistic, 4), "\n")
    cat("p-value:", format.pval(bp_result$p.value, digits = 4), "\n")
    cat("Kesimpulan:", ifelse(bp_result$p.value > 0.05, "HOMOSKEDASTIS", "HETEROSKEDASTIS"), "\n")
  })
  
  # Uji Autokorelasi - Durbin-Watson Test
  output$durbin_watson_test <- renderPrint({
    req(regression_results_reactive$model)
    library(lmtest)
    dw_result <- dwtest(regression_results_reactive$model)
    cat("=== UJI AUTOKORELASI ===\n")
    cat("Durbin-Watson Test\n")
    cat("DW-statistic:", round(dw_result$statistic, 4), "\n")
    cat("p-value:", format.pval(dw_result$p.value, digits = 4), "\n")
    cat("Kesimpulan:", ifelse(dw_result$p.value > 0.05, "TIDAK ADA AUTOKORELASI", "ADA AUTOKORELASI"), "\n")
  })
  
  # Uji Multikolinearitas - VIF Test
  output$vif_test <- renderPrint({
    req(regression_results_reactive$model)
    library(car)
    if(length(input$independent_vars) > 1) {
      vif_values <- vif(regression_results_reactive$model)
      cat("=== UJI MULTIKOLINEARITAS ===\n")
      cat("Variance Inflation Factor (VIF)\n")
      for(i in 1:length(vif_values)) {
        cat(names(vif_values)[i], ":", round(vif_values[i], 4), "\n")
      }
      cat("\nKesimpulan:\n")
      high_vif <- names(vif_values)[vif_values > 10]
      if(length(high_vif) > 0) {
        cat("MULTIKOLINEARITAS TERDETEKSI pada:", paste(high_vif, collapse = ", "), "\n")
      } else {
        cat("TIDAK ADA MULTIKOLINEARITAS (semua VIF < 10)\n")
      }
    } else {
      cat("=== UJI MULTIKOLINEARITAS ===\n")
      cat("Hanya ada satu variabel independen.\n")
      cat("Uji multikolinearitas tidak diperlukan.\n")
    }
  })
  
  # Ringkasan Semua Uji Asumsi
  output$assumptions_summary <- renderPrint({
    req(regression_results_reactive$model)
    library(lmtest)
    library(car)
    
    cat("=== RINGKASAN UJI ASUMSI ===\n\n")
    
    # Normalitas
    residuals <- residuals(regression_results_reactive$model)
    shapiro_result <- shapiro.test(residuals)
    cat("1. NORMALITAS RESIDUAL:", ifelse(shapiro_result$p.value > 0.05, "✓ TERPENUHI", "✗ TIDAK TERPENUHI"), "\n")
    
    # Homoskedastisitas
    bp_result <- bptest(regression_results_reactive$model)
    cat("2. HOMOSKEDASTISITAS:", ifelse(bp_result$p.value > 0.05, "✓ TERPENUHI", "✗ TIDAK TERPENUHI"), "\n")
    
    # Autokorelasi
    dw_result <- dwtest(regression_results_reactive$model)
    cat("3. NON-AUTOKORELASI:", ifelse(dw_result$p.value > 0.05, "✓ TERPENUHI", "✗ TIDAK TERPENUHI"), "\n")
    
    # Multikolinearitas
    if(length(input$independent_vars) > 1) {
      vif_values <- vif(regression_results_reactive$model)
      high_vif <- any(vif_values > 10)
      cat("4. NON-MULTIKOLINEARITAS:", ifelse(!high_vif, "✓ TERPENUHI", "✗ TIDAK TERPENUHI"), "\n")
    } else {
      cat("4. NON-MULTIKOLINEARITAS: N/A (satu variabel)\n")
    }
    
    cat("\n=== STATUS KESELURUHAN ===\n")
    assumptions_met <- shapiro_result$p.value > 0.05 & bp_result$p.value > 0.05 & dw_result$p.value > 0.05
    if(length(input$independent_vars) > 1) {
      vif_values <- vif(regression_results_reactive$model)
      assumptions_met <- assumptions_met & all(vif_values <= 10)
    }
    cat("Model regresi:", ifelse(assumptions_met, "VALID (semua asumsi terpenuhi)", "PERLU PERBAIKAN (ada asumsi yang tidak terpenuhi)"), "\n")
  })
  
  ## DOWNLOAD REGRESI 
  output$download_regression_results <- downloadHandler(
    filename = function() { paste("hasil_regresi_", Sys.Date(), ".docx", sep = "") },
    content = function(file) {
      req(regression_results_reactive$model)
      model_summary_dl <- summary(regression_results_reactive$model)
      coef_summary_dl <- model_summary_dl$coefficients
      
      doc <- read_docx()
      
      doc <- doc %>%
        body_add_par("LAPORAN HASIL ANALISIS REGRESI LINEAR BERGANDA", style = "heading 1") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par(paste("Tanggal Analisis:", format(Sys.Date(), "%d %B %Y")), style = "Normal") %>%
        body_add_par(paste("Variabel Dependen (Y):", input$dependent_var), style = "Normal") %>%
        body_add_par(paste("Variabel Independen (X):", paste(input$independent_vars, collapse = ", ")), style = "Normal") %>%
        body_add_par("", style = "Normal")
      
      doc <- doc %>%
        body_add_par("1. PERSAMAAN REGRESI", style = "heading 2")
      
      eq_str <- paste0(input$dependent_var, " = ", round(coef_summary_dl[1,1], 4))
      for(i in 2:nrow(coef_summary_dl)) {
        coef_val <- coef_summary_dl[i,1]
        var_name <- rownames(coef_summary_dl)[i]
        sign_char <- ifelse(coef_val >= 0, " + ", " - ")
        eq_str <- paste0(eq_str, sign_char, abs(round(coef_val, 4)), " × ", var_name)
      }
      
      doc <- doc %>%
        body_add_par(eq_str, style = "Normal") %>%
        body_add_par("", style = "Normal")
      
      doc <- doc %>%
        body_add_par("2. TABEL KOEFISIEN REGRESI", style = "heading 2")
      
      coef_df <- data.frame(
        "Variabel" = rownames(coef_summary_dl),
        "Koefisien" = round(coef_summary_dl[,1], 4),
        "Std Error" = round(coef_summary_dl[,2], 4),
        "t-value" = round(coef_summary_dl[,3], 4),
        "p-value" = round(coef_summary_dl[,4], 4),
        "Signifikansi" = ifelse(coef_summary_dl[,4] < 0.001, "***", 
                                ifelse(coef_summary_dl[,4] < 0.01, "**", 
                                       ifelse(coef_summary_dl[,4] < 0.05, "*", 
                                              ifelse(coef_summary_dl[,4] < 0.1, ".", ""))))
      )
      
      ft <- flextable(coef_df)
      ft <- autofit(ft)
      ft <- theme_vanilla(ft)
      
      doc <- doc %>%
        body_add_flextable(ft) %>%
        body_add_par("", style = "Normal")
      
      doc <- doc %>%
        body_add_par("3. INTERPRETASI KOEFISIEN", style = "heading 2") %>%
        body_add_par("Intercept:", style = "heading 3") %>%
        body_add_par(paste("Ketika semua variabel independen bernilai 0, nilai rata-rata dari", 
                           input$dependent_var, "diperkirakan adalah", round(coef_summary_dl[1,1], 4)), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("Variabel Independen:", style = "heading 3")
      
      for(i in 2:nrow(coef_summary_dl)) {
        var_name <- rownames(coef_summary_dl)[i]
        coef_val <- coef_summary_dl[i,1]
        p_val <- coef_summary_dl[,4]
        
        doc <- doc %>%
          body_add_par(paste("•", var_name, ": Koefisien =", round(coef_val, 4), 
                             "(p-value =", round(p_val[i], 4), ")"), style = "Normal") %>%
          body_add_par(paste("  Artinya: Setiap kenaikan 1 unit pada", var_name, "akan", 
                             ifelse(coef_val > 0, "meningkatkan", "menurunkan"), 
                             input$dependent_var, "sebesar", abs(round(coef_val, 4)), "unit."), style = "Normal") %>%
          body_add_par(paste("  Signifikansi:", 
                             ifelse(p_val[i] < 0.05, "SIGNIFIKAN", "TIDAK SIGNIFIKAN"), 
                             "pada α = 0.05"), style = "Normal") %>%
          body_add_par("", style = "Normal")
      }
      
      doc <- doc %>%
        body_add_par("4. STATISTIK KUALITAS MODEL", style = "heading 2")
      
      stats_df <- data.frame(
        "Statistik" = c("R-squared", "Adjusted R-squared", "F-statistic", "p-value F-test", 
                        "AIC", "BIC", "Residual Standard Error", "Jumlah Observasi"),
        "Nilai" = c(round(model_summary_dl$r.squared, 4), 
                    round(model_summary_dl$adj.r.squared, 4),
                    round(model_summary_dl$fstatistic[1], 4),
                    format.pval(pf(model_summary_dl$fstatistic[1], model_summary_dl$fstatistic[2], 
                                   model_summary_dl$fstatistic[3], lower.tail = FALSE), digits = 4),
                    round(AIC(regression_results_reactive$model), 4),
                    round(BIC(regression_results_reactive$model), 4),
                    round(model_summary_dl$sigma, 4),
                    nobs(regression_results_reactive$model))
      )
      
      ft2 <- flextable(stats_df)
      ft2 <- autofit(ft2)
      ft2 <- theme_vanilla(ft2)
      
      doc <- doc %>%
        body_add_flextable(ft2) %>%
        body_add_par("", style = "Normal")
      
      r_squared <- model_summary_dl$r.squared
      f_pvalue <- pf(model_summary_dl$fstatistic[1], model_summary_dl$fstatistic[2], 
                     model_summary_dl$fstatistic[3], lower.tail = FALSE)
      
      doc <- doc %>%
        body_add_par("5. KESIMPULAN DAN REKOMENDASI", style = "heading 2") %>%
        body_add_par(paste("• R-squared:", round(r_squared, 4), 
                           paste0("(", round(r_squared*100, 2), "% variasi dijelaskan)")), style = "Normal") %>%
        body_add_par(paste("• Model secara keseluruhan:", 
                           ifelse(f_pvalue < 0.05, "SIGNIFIKAN", "TIDAK SIGNIFIKAN")), style = "Normal")
      
      if(r_squared > 0.7) {
        doc <- doc %>% body_add_par("• Model memiliki daya prediksi yang SANGAT BAIK (>70%)", style = "Normal")
      } else if(r_squared > 0.5) {
        doc <- doc %>% body_add_par("• Model memiliki daya prediksi yang CUKUP BAIK (>50%)", style = "Normal")
      } else if (r_squared > 0.2) {
        doc <- doc %>% body_add_par("• Model memiliki daya prediksi yang MODERAT (20%-50%)", style = "Normal")
      } else {
        doc <- doc %>% body_add_par("• Model memiliki daya prediksi yang RENDAH (<20%)", style = "Normal")
      }
      
      sig_vars <- rownames(coef_summary_dl)[coef_summary_dl[,4] < 0.05]
      sig_vars <- sig_vars[sig_vars != "(Intercept)"]
      
      if(length(sig_vars) > 0) {
        doc <- doc %>%
          body_add_par("", style = "Normal") %>%
          body_add_par("Variabel yang signifikan mempengaruhi model:", style = "Normal")
        
        for(var in sig_vars) {
          coef_val <- coef_summary_dl[var, 1]
          p_val <- coef_summary_dl[var, 4]
          doc <- doc %>%
            body_add_par(paste("•", var, ":", ifelse(coef_val > 0, "berpengaruh positif", "berpengaruh negatif"),
                               "(p-value:", round(p_val, 4), ")"), style = "Normal")
        }
      } else {
        doc <- doc %>%
          body_add_par("", style = "Normal") %>%
          body_add_par("• Tidak ada variabel independen yang signifikan pada α = 0.05", style = "Normal")
      }
      
      doc <- doc %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("REKOMENDASI:", style = "heading 3")
      
      if(f_pvalue < 0.05 && r_squared > 0.5) {
        doc <- doc %>%
          body_add_par("✓ Model ini menunjukkan kinerja yang baik dan dapat dipertimbangkan untuk digunakan.", style = "Normal") %>%
          body_add_par("✓ Disarankan melakukan uji asumsi regresi untuk validasi lebih lanjut.", style = "Normal")
      } else {
        doc <- doc %>%
          body_add_par("⚠ Model ini perlu perbaikan. Pertimbangkan:", style = "Normal") %>%
          body_add_par("• Periksa kembali variabel-variabel yang dipilih", style = "Normal") %>%
          body_add_par("• Tangani nilai-nilai ekstrim (outlier)", style = "Normal") %>%
          body_add_par("• Pertimbangkan transformasi variabel", style = "Normal") %>%
          body_add_par("• Evaluasi multikolinearitas antar variabel independen", style = "Normal")
      }
      
      doc <- doc %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("Laporan ini dihasilkan secara otomatis oleh sistem analisis data.", 
                     style = "Normal")
      
      print(doc, target = file)
    }
  )
  # =====================================
  # 8. DOWNLOAD UMUM - SERVER LOGIC
  # =====================================
  output$download_data_mentah <- downloadHandler(
    filename = function() { paste("data_sovi_mentah_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(sovi_data, file, row.names = FALSE) }
  )
  
  output$download_report_umum <- downloadHandler(
    filename = function() {
      paste0("Laporan_Analisis_Lengkap_", Sys.Date(), ".docx")
    },
    content = function(file) {
      
      # Buat temporary R Markdown file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Mulai konten R Markdown
      rmd_content <- c(
        "---",
        "title: 'Laporan Analisis Statistik Lengkap'",
        paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
        "output: ",
        "  word_document:",
        "    reference_docx: null",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
        "library(knitr)",
        "library(dplyr)",
        "if(!require(flextable)) library(flextable)",
        "```",
        "",
        "# LAPORAN ANALISIS STATISTIK LENGKAP",
        "",
        paste("**Tanggal Analisis:** ", format(Sys.Date(), "%d %B %Y")),
        paste("**Waktu Pembuatan:** ", format(Sys.time(), "%H:%M:%S")),
        "",
        "---",
        ""
      )
      
      # BAGIAN 1: UJI ASUMSI
      rmd_content <- c(rmd_content, "# 1. HASIL UJI ASUMSI", "")
      
      if (exists("uji_asumsi_results") && (!is.null(uji_asumsi_results$shapiro) || !is.null(uji_asumsi_results$levene) || !is.null(uji_asumsi_results$bartlett))) {
        
        # Dataset info
        if (exists("cangih_data") && !is.null(cangih_data$sovi)) {
          rmd_content <- c(rmd_content,
                           "## Ringkasan Dataset",
                           paste("- **Jumlah observasi:** ", nrow(cangih_data$sovi)),
                           paste("- **Jumlah variabel:** ", ncol(cangih_data$sovi)),
                           ""
          )
        }
        
        # Uji Normalitas
        if (!is.null(uji_asumsi_results$shapiro)) {
          rmd_content <- c(rmd_content,
                           "## Uji Normalitas (Shapiro-Wilk)",
                           ""
          )
          
          if (exists("input") && !is.null(input$var_normalitas)) {
            rmd_content <- c(rmd_content, paste("**Variabel yang diuji:** ", input$var_normalitas), "")
          }
          
          if (!is.na(uji_asumsi_results$shapiro$statistic)) {
            rmd_content <- c(rmd_content,
                             paste("- **Statistik W:** ", round(uji_asumsi_results$shapiro$statistic, 4)),
                             paste("- **p-value:** ", format(uji_asumsi_results$shapiro$p.value, scientific = TRUE, digits = 4)),
                             "",
                             "**Hipotesis:**",
                             "- H₀: Data berdistribusi normal",  
                             "- H₁: Data tidak berdistribusi normal",
                             ""
            )
            
            alpha <- 0.05
            p_val <- uji_asumsi_results$shapiro$p.value
            
            if (p_val > alpha) {
              rmd_content <- c(rmd_content,
                               paste("**Kesimpulan:** GAGAL TOLAK H₀ (p-value =", format(p_val, scientific = TRUE, digits = 4), "> α = 0.05)"),
                               "",
                               "**Interpretasi:** Asumsi normalitas **TERPENUHI**.",
                               ""
              )
            } else {
              rmd_content <- c(rmd_content,
                               paste("**Kesimpulan:** TOLAK H₀ (p-value =", format(p_val, scientific = TRUE, digits = 4), "< α = 0.05)"),
                               "",
                               "**Interpretasi:** Asumsi normalitas **TIDAK TERPENUHI**.",
                               ""
              )
            }
          }
        }
        
        # Uji Homogenitas
        if (!is.null(uji_asumsi_results$levene) || !is.null(uji_asumsi_results$bartlett)) {
          rmd_content <- c(rmd_content,
                           "## Uji Homogenitas Varians",
                           ""
          )
          
          if (exists("input") && !is.null(input$var_homogenitas)) {
            rmd_content <- c(rmd_content,
                             paste("**Variabel yang diuji:** ", input$var_homogenitas),
                             paste("**Variabel pengelompokan:** ", input$group_var_asumsi),
                             ""
            )
          }
          
          # Levene's Test
          if (!is.null(uji_asumsi_results$levene) && is.null(uji_asumsi_results$levene$error)) {
            rmd_content <- c(rmd_content,
                             "### Levene's Test",
                             paste("- **Statistik F:** ", round(uji_asumsi_results$levene$`F value`[1], 4)),
                             paste("- **p-value:** ", format(uji_asumsi_results$levene$`Pr(>F)`[1], scientific = TRUE, digits = 4)),
                             ""
            )
            
            p_val_levene <- uji_asumsi_results$levene$`Pr(>F)`[1]
            alpha <- 0.05
            
            rmd_content <- c(rmd_content,
                             "**Interpretasi:**",
                             paste("- Hasil:", ifelse(p_val_levene > alpha, "Varians HOMOGEN (Asumsi terpenuhi)", "Varians TIDAK HOMOGEN (Asumsi tidak terpenuhi)")),
                             ""
            )
          }
          
          # Bartlett's Test
          if (!is.null(uji_asumsi_results$bartlett) && is.null(uji_asumsi_results$bartlett$error)) {
            rmd_content <- c(rmd_content,
                             "### Bartlett's Test",
                             paste("- **Statistik K-squared:** ", round(uji_asumsi_results$bartlett$statistic, 4)),
                             paste("- **p-value:** ", format(uji_asumsi_results$bartlett$p.value, scientific = TRUE, digits = 4)),
                             ""
            )
            
            p_val_bartlett <- uji_asumsi_results$bartlett$p.value
            alpha <- 0.05
            
            rmd_content <- c(rmd_content,
                             "**Interpretasi:**",
                             paste("- Hasil:", ifelse(p_val_bartlett > alpha, "Varians HOMOGEN (Asumsi terpenuhi)", "Varians TIDAK HOMOGEN (Asumsi tidak terpenuhi)")),
                             "",
                             "*Catatan: Uji Bartlett sensitif terhadap non-normalitas.*",
                             ""
            )
          }
        }
      } else {
        rmd_content <- c(rmd_content, "*Belum ada hasil uji asumsi yang dijalankan.*", "")
      }
      
      rmd_content <- c(rmd_content, "---", "")
      
      rmd_content <- c(rmd_content, "# 2. HASIL UJI BEDA RATA-RATA", "")
      
      if (exists("uji_rerata_results") && !is.null(uji_rerata_results$test) && !is.null(uji_rerata_results$params)) {
        
        test_obj <- uji_rerata_results$test
        desc_stats <- uji_rerata_results$desc_stats
        interpretation <- uji_rerata_results$interpretation
        params <- uji_rerata_results$params
        
        if (params$method == "one_sample") {
          method_name <- "Uji t Satu Sampel"
          rmd_content <- c(rmd_content,
                           paste("**Metode:** ", method_name),
                           paste("**Variabel:** ", params$var_name),
                           paste("**Nilai Hipotesis (μ₀):** ", params$mu0),
                           paste("**Alternatif:** ", params$alternative),
                           paste("**Confidence Level:** ", params$conf_level, "%"),
                           ""
          )
        } else {
          method_name <- "Uji t Dua Sampel"
          rmd_content <- c(rmd_content,
                           paste("**Metode:** ", method_name),
                           paste("**Variabel Numerik:** ", params$var_name),
                           paste("**Variabel Kelompok:** ", params$group_var),
                           paste("**Alternatif:** ", params$alternative),
                           paste("**Confidence Level:** ", params$conf_level, "%"),
                           paste("**Asumsi Varians Sama:** ", ifelse(params$equal_var, "Ya", "Tidak")),
                           ""
          )
        }
        
        rmd_content <- c(rmd_content, "## Statistik Deskriptif", "")
        
        if (params$method == "one_sample") {
          rmd_content <- c(rmd_content,
                           "```{r desc-stats-rata}",
                           paste0('desc_table <- data.frame('),
                           paste0('  Statistik = c("', paste(desc_stats$Statistik, collapse = '", "'), '"),'),
                           paste0('  Nilai = c("', paste(desc_stats$Nilai, collapse = '", "'), '")'),
                           ')',
                           'kable(desc_table, align = "c", caption = "Statistik Deskriptif")',
                           "```",
                           ""
          )
        } else {
          rmd_content <- c(rmd_content,
                           "```{r desc-stats-rata}",
                           paste0('desc_table <- data.frame('),
                           paste0('  Kelompok = c("', paste(desc_stats$Kelompok, collapse = '", "'), '"),'),
                           paste0('  Jumlah.Data = c("', paste(desc_stats$Jumlah.Data, collapse = '", "'), '"),'),
                           paste0('  Rata.rata = c("', paste(desc_stats$Rata.rata, collapse = '", "'), '"),'),
                           paste0('  Std.Deviasi = c("', paste(desc_stats$Std.Deviasi, collapse = '", "'), '"),'),
                           paste0('  Std.Error = c("', paste(desc_stats$Std.Error, collapse = '", "'), '"),'),
                           paste0('  Minimum = c("', paste(desc_stats$Minimum, collapse = '", "'), '"),'),
                           paste0('  Maksimum = c("', paste(desc_stats$Maksimum, collapse = '", "'), '")'),
                           ')',
                           'kable(desc_table, align = "c", caption = "Statistik Deskriptif per Kelompok")',
                           "```",
                           ""
          )
        }
        
        rmd_content <- c(rmd_content,
                         "## Hasil Uji Statistik",
                         "",
                         "```{r test-results-rata}",
                         'test_summary <- data.frame(',
                         '  Statistik = c("t-statistik", "Derajat Bebas", "p-value", "Confidence Interval"),',
                         '  Nilai = c(',
                         paste0('    "', round(test_obj$statistic, 4), '",'),
                         paste0('    "', round(test_obj$parameter, 4), '",'),
                         paste0('    "', format(test_obj$p.value, scientific = TRUE, digits = 4), '",'),
                         paste0('    "[', round(test_obj$conf.int[1], 4), ', ', round(test_obj$conf.int[2], 4), ']"'),
                         '  )',
                         ')',
                         'kable(test_summary, align = "c", caption = "Hasil Uji t-test")',
                         "```",
                         "",
                         "## Interpretasi",
                         "",
                         gsub("\n", "\n\n", interpretation),
                         ""
        )
      } else {
        rmd_content <- c(rmd_content, "*Belum ada hasil uji beda rata-rata yang dijalankan.*", "")
      }
      
      rmd_content <- c(rmd_content, "---", "")
      
      # BAGIAN 3: ANALISIS ANOVA
      rmd_content <- c(rmd_content, "# 3. HASIL ANALISIS ANOVA", "")
      
      if (exists("anova_results_reactive") && !is.null(anova_results_reactive$anova_table)) {
        
        if (exists("input")) {
          anova_type <- toupper(input$anova_type)
          rmd_content <- c(rmd_content,
                           paste("**Jenis Analisis:** ", anova_type, " ANOVA"),
                           paste("**Variabel Dependen:** ", input$dep_var),
                           paste("**Faktor 1:** ", input$factor1)
          )
          
          if (input$anova_type == "twoway") {
            rmd_content <- c(rmd_content, paste("**Faktor 2:** ", input$factor2))
            if (input$include_interaction) {
              rmd_content <- c(rmd_content, "**Interaksi:** Ya")
            }
          }
          
          rmd_content <- c(rmd_content, "", "## Formula")
          
          if (input$anova_type == "oneway") {
            formula_text <- paste(input$dep_var, "~", input$factor1)
          } else {
            if (input$include_interaction) {
              formula_text <- paste(input$dep_var, "~", input$factor1, "*", input$factor2)
            } else {
              formula_text <- paste(input$dep_var, "~", input$factor1, "+", input$factor2)
            }
          }
          
          rmd_content <- c(rmd_content, paste("**Formula:** ", formula_text), "")
        }
        
        # Statistik Deskriptif ANOVA
        rmd_content <- c(rmd_content,
                         "## Statistik Deskriptif",
                         "",
                         "```{r anova-desc}",
                         "# Recreate descriptive statistics dari anova_results_reactive",
                         "if (exists('anova_results_reactive') && !is.null(anova_results_reactive$filtered_data)) {",
                         "  data_for_desc <- anova_results_reactive$filtered_data",
                         "  if (exists('input') && input$anova_type == 'oneway') {",
                         "    desc_stats <- data_for_desc %>%",
                         "      group_by(!!sym(input$factor1)) %>%",
                         "      summarise(",
                         "        N = n(),",
                         "        Mean = round(mean(!!sym(input$dep_var), na.rm = TRUE), 3),",
                         "        SD = round(sd(!!sym(input$dep_var), na.rm = TRUE), 3),",
                         "        Min = round(min(!!sym(input$dep_var), na.rm = TRUE), 3),",
                         "        Max = round(max(!!sym(input$dep_var), na.rm = TRUE), 3),",
                         "        .groups = 'drop'",
                         "      )",
                         "  } else if (exists('input') && input$anova_type == 'twoway') {",
                         "    desc_stats <- data_for_desc %>%",
                         "      group_by(!!sym(input$factor1), !!sym(input$factor2)) %>%",
                         "      summarise(",
                         "        N = n(),",
                         "        Mean = round(mean(!!sym(input$dep_var), na.rm = TRUE), 3),",
                         "        SD = round(sd(!!sym(input$dep_var), na.rm = TRUE), 3),",
                         "        Min = round(min(!!sym(input$dep_var), na.rm = TRUE), 3),",
                         "        Max = round(max(!!sym(input$dep_var), na.rm = TRUE), 3),",
                         "        .groups = 'drop'",
                         "      )",
                         "  }",
                         "  kable(desc_stats, caption = 'Statistik Deskriptif per Grup')",
                         "} else {",
                         "  cat('Data deskriptif tidak tersedia')",
                         "}",
                         "```",
                         ""
        )
        
        # Tabel ANOVA
        rmd_content <- c(rmd_content,
                         "## Hasil ANOVA",
                         "",
                         "```{r anova-table}",
                         "# Format ANOVA table",
                         "if (exists('anova_results_reactive') && !is.null(anova_results_reactive$anova_table)) {",
                         "  anova_table <- anova_results_reactive$anova_table",
                         "  anova_table$p.value <- ifelse(anova_table$p.value < 0.001, '< 0.001',",
                         "                                sprintf('%.6f', anova_table$p.value))",
                         "  colnames(anova_table) <- c('Sumber Variasi', 'Df', 'Sum of Squares',",
                         "                            'Mean Square', 'F-value', 'P-value')",
                         "  kable(anova_table, caption = 'Tabel ANOVA', digits = 3)",
                         "} else {",
                         "  cat('Tabel ANOVA tidak tersedia')",
                         "}",
                         "```",
                         ""
        )
        
        # Interpretasi ANOVA
        rmd_content <- c(rmd_content, "## Interpretasi", "")
        
        if (exists("input") && exists("anova_results_reactive") && !is.null(anova_results_reactive$p_values)) {
          if (input$anova_type == "oneway") {
            p_val <- anova_results_reactive$p_values[input$factor1]
            rmd_content <- c(rmd_content,
                             "**Hipotesis:**",
                             paste("- H₀: Rata-rata", input$dep_var, "semua kelompok", input$factor1, "sama"),
                             paste("- H₁: Minimal ada satu kelompok", input$factor1, "yang rata-ratanya berbeda"),
                             "",
                             "**Hasil:**",
                             paste("- P-value untuk", input$factor1, "=", format.pval(p_val, digits = 4)),
                             "- Tingkat signifikansi (α) = 0.05",
                             "",
                             "**Kesimpulan:**",
                             ifelse(p_val < 0.05,
                                    paste("Tolak H₀ (p < 0.05). Terdapat perbedaan signifikan rata-rata", input$dep_var, "antar kelompok", input$factor1, "."),
                                    paste("Gagal tolak H₀ (p ≥ 0.05). Tidak terdapat perbedaan signifikan rata-rata", input$dep_var, "antar kelompok", input$factor1, ".")
                             ),
                             ""
            )
          } else {
            p_val1 <- anova_results_reactive$p_values[input$factor1]
            p_val2 <- anova_results_reactive$p_values[input$factor2]
            
            rmd_content <- c(rmd_content,
                             paste("**Efek Utama", input$factor1, ":**"),
                             paste("- P-value =", format.pval(p_val1, digits = 4)),
                             paste("- Hasil:", ifelse(p_val1 < 0.05, "Signifikan", "Tidak Signifikan")),
                             "",
                             paste("**Efek Utama", input$factor2, ":**"),
                             paste("- P-value =", format.pval(p_val2, digits = 4)),
                             paste("- Hasil:", ifelse(p_val2 < 0.05, "Signifikan", "Tidak Signifikan")),
                             ""
            )
            
            if (input$include_interaction && any(grepl(":", names(anova_results_reactive$p_values)))) {
              p_interaction <- anova_results_reactive$p_values[grep(":", names(anova_results_reactive$p_values))[1]]
              rmd_content <- c(rmd_content,
                               "**Efek Interaksi:**",
                               paste("- P-value =", format.pval(p_interaction, digits = 4)),
                               paste("- Hasil:", ifelse(p_interaction < 0.05, "Signifikan", "Tidak Signifikan")),
                               ""
              )
            }
          }
        }
        
        if (!is.null(anova_results_reactive$tukey_test) && is.list(anova_results_reactive$tukey_test)) {
          rmd_content <- c(rmd_content,
                           "## Post-Hoc Test (Tukey HSD)",
                           "",
                           "Karena ditemukan efek signifikan, dilakukan uji post-hoc untuk perbandingan berpasangan:",
                           "",
                           "```{r tukey-results}",
                           "if (exists('anova_results_reactive') && !is.null(anova_results_reactive$tukey_test)) {",
                           "  tukey_results <- anova_results_reactive$tukey_test",
                           "  for (factor_name in names(tukey_results)) {",
                           "    if (!grepl('^error', factor_name)) {",
                           "      cat('\\n### Tukey HSD untuk', factor_name, '\\n')",
                           "      print(tukey_results[[factor_name]])",
                           "    }",
                           "  }",
                           "} else {",
                           "  cat('Hasil Tukey tidak tersedia')",
                           "}",
                           "```",
                           "",
                           "**Interpretasi Post-Hoc:**",
                           "- Pasangan kelompok dengan p adj < 0.05 memiliki perbedaan rata-rata yang signifikan.",
                           "- Pasangan kelompok dengan p adj ≥ 0.05 tidak memiliki perbedaan rata-rata yang signifikan.",
                           ""
          )
        }
      } else {
        rmd_content <- c(rmd_content, "*Belum ada hasil analisis ANOVA yang dijalankan.*", "")
      }
      
      rmd_content <- c(rmd_content, "---", "")
      
      rmd_content <- c(rmd_content, "# 4. HASIL ANALISIS REGRESI LINEAR BERGANDA", "")
      
      if (exists("regression_results_reactive") && !is.null(regression_results_reactive$model)) {
        model_summary_dl <- summary(regression_results_reactive$model)
        coef_summary_dl <- model_summary_dl$coefficients
        
        if (exists("input")) {
          rmd_content <- c(rmd_content,
                           paste("**Variabel Dependen (Y):** ", input$dependent_var),
                           paste("**Variabel Independen (X):** ", paste(input$independent_vars, collapse = ", ")),
                           ""
          )
        }
        
        rmd_content <- c(rmd_content, "## Persamaan Regresi", "")
        
        eq_str <- paste0(input$dependent_var, " = ", round(coef_summary_dl[1,1], 4))
        for(i in 2:nrow(coef_summary_dl)) {
          coef_val <- coef_summary_dl[i,1]
          var_name <- rownames(coef_summary_dl)[i]
          sign_char <- ifelse(coef_val >= 0, " + ", " - ")
          eq_str <- paste0(eq_str, sign_char, abs(round(coef_val, 4)), " × ", var_name)
        }
        
        rmd_content <- c(rmd_content, paste("**", eq_str, "**"), "")
        
        rmd_content <- c(rmd_content,
                         "## Tabel Koefisien Regresi",
                         "",
                         "```{r coef-table}",
                         "if (exists('regression_results_reactive') && !is.null(regression_results_reactive$model)) {",
                         "  model_sum <- summary(regression_results_reactive$model)",
                         "  coef_sum <- model_sum$coefficients",
                         "  coef_df <- data.frame(",
                         "    Variabel = rownames(coef_sum),",
                         "    Koefisien = round(coef_sum[,1], 4),",
                         "    Std.Error = round(coef_sum[,2], 4),",
                         "    t.value = round(coef_sum[,3], 4),",
                         "    p.value = round(coef_sum[,4], 4),",
                         "    Signifikansi = ifelse(coef_sum[,4] < 0.001, '***',",
                         "                          ifelse(coef_sum[,4] < 0.01, '**',",
                         "                                 ifelse(coef_sum[,4] < 0.05, '*',",
                         "                                        ifelse(coef_sum[,4] < 0.1, '.', ''))))",
                         "  )",
                         "  kable(coef_df, caption = 'Koefisien Regresi')",
                         "} else {",
                         "  cat('Model tidak tersedia')",
                         "}",
                         "```",
                         ""
        )
        
        rmd_content <- c(rmd_content,
                         "## Interpretasi Koefisien",
                         "",
                         "### Intercept",
                         paste("Ketika semua variabel independen bernilai 0, nilai rata-rata dari", 
                               input$dependent_var, "diperkirakan adalah", round(coef_summary_dl[1,1], 4)),
                         "",
                         "### Variabel Independen"
        )
        
        for(i in 2:nrow(coef_summary_dl)) {
          var_name <- rownames(coef_summary_dl)[i]
          coef_val <- coef_summary_dl[i,1]
          p_val <- coef_summary_dl[i,4]
          
          rmd_content <- c(rmd_content,
                           paste("**", var_name, ":**"),
                           paste("- Koefisien =", round(coef_val, 4), "(p-value =", round(p_val, 4), ")"),
                           paste("- Artinya: Setiap kenaikan 1 unit pada", var_name, "akan", 
                                 ifelse(coef_val > 0, "meningkatkan", "menurunkan"), 
                                 input$dependent_var, "sebesar", abs(round(coef_val, 4)), "unit"),
                           paste("- Signifikansi:", 
                                 ifelse(p_val < 0.05, "**SIGNIFIKAN**", "TIDAK SIGNIFIKAN"), 
                                 "pada α = 0.05"),
                           ""
          )
        }
        
        rmd_content <- c(rmd_content,
                         "## Statistik Kualitas Model",
                         "",
                         "```{r model-stats}",
                         "if (exists('regression_results_reactive') && !is.null(regression_results_reactive$model)) {",
                         "  model_sum <- summary(regression_results_reactive$model)",
                         "  f_pval <- pf(model_sum$fstatistic[1], model_sum$fstatistic[2], model_sum$fstatistic[3], lower.tail = FALSE)",
                         "  stats_df <- data.frame(",
                         "    Statistik = c('R-squared', 'Adjusted R-squared', 'F-statistic', 'p-value F-test',",
                         "                  'AIC', 'BIC', 'Residual Standard Error', 'Jumlah Observasi'),",
                         "    Nilai = c(",
                         "      round(model_sum$r.squared, 4),",
                         "      round(model_sum$adj.r.squared, 4),",
                         "      round(model_sum$fstatistic[1], 4),",
                         "      format.pval(f_pval, digits = 4),",
                         "      round(AIC(regression_results_reactive$model), 4),",
                         "      round(BIC(regression_results_reactive$model), 4),",
                         "      round(model_sum$sigma, 4),",
                         "      nobs(regression_results_reactive$model)",
                         "    )",
                         "  )",
                         "  kable(stats_df, caption = 'Statistik Kualitas Model')",
                         "} else {",
                         "  cat('Model tidak tersedia')",
                         "}",
                         "```",
                         ""
        )
        
        r_squared <- model_summary_dl$r.squared
        f_pvalue <- pf(model_summary_dl$fstatistic[1], model_summary_dl$fstatistic[2], 
                       model_summary_dl$fstatistic[3], lower.tail = FALSE)
        
        rmd_content <- c(rmd_content,
                         "## Evaluasi Kualitas Model",
                         "",
                         paste("- **R-squared:**", round(r_squared, 4), 
                               paste0("(", round(r_squared*100, 2), "% variasi dapat dijelaskan oleh model)")),
                         paste("- **Model secara keseluruhan:**", 
                               ifelse(f_pvalue < 0.05, "**SIGNIFIKAN**", "TIDAK SIGNIFIKAN")),
                         ""
        )
        
        if(r_squared > 0.7) {
          rmd_content <- c(rmd_content, "- **Kualitas prediksi:** SANGAT BAIK (>70%)")
        } else if(r_squared > 0.5) {
          rmd_content <- c(rmd_content, "- **Kualitas prediksi:** CUKUP BAIK (50%-70%)")
        } else if (r_squared > 0.2) {
          rmd_content <- c(rmd_content, "- **Kualitas prediksi:** MODERAT (20%-50%)")
        } else {
          rmd_content <- c(rmd_content, "- **Kualitas prediksi:** RENDAH (<20%)")
        }
        
        rmd_content <- c(rmd_content, "", "## Variabel yang Berpengaruh Signifikan", "")
        
        sig_vars <- rownames(coef_summary_dl)[coef_summary_dl[,4] < 0.05]
        sig_vars <- sig_vars[sig_vars != "(Intercept)"]
        
        if(length(sig_vars) > 0) {
          rmd_content <- c(rmd_content, "Variabel yang signifikan pada α = 0.05:", "")
          for(var in sig_vars) {
            coef_val <- coef_summary_dl[var, 1]
            p_val <- coef_summary_dl[var, 4]
            rmd_content <- c(rmd_content,
                             paste("- **", var, ":** ", ifelse(coef_val > 0, "berpengaruh positif", "berpengaruh negatif"),
                                   " (p-value: ", round(p_val, 4), ")")
            )
          }
        } else {
          rmd_content <- c(rmd_content, "Tidak ada variabel independen yang signifikan pada α = 0.05")
        }
        
      } else {
        rmd_content <- c(rmd_content, "*Belum ada hasil analisis regresi yang dijalankan.*", "")
      }
      
      rmd_content <- c(rmd_content,
                       "---",
                       "",
                       paste("*Laporan dibuat pada:*", format(Sys.time(), "%d %B %Y pukul %H:%M:%S"))
      )
      
      writeLines(rmd_content, temp_rmd)
      
      rmarkdown::render(temp_rmd, 
                        output_format = "word_document",
                        output_file = file,
                        quiet = TRUE)
      
      unlink(temp_rmd)
    }
  )
  
  }
  
# =====================================
# RUN APP
# =====================================
shinyApp(ui = ui, server = server)

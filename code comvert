library(shiny)
library(leaflet)
library(ggplot2)

# Data kecamatan di Sleman dan koordinat TPS3R
sleman_kecamatan <- c("Depok", "Ngaglik", "Gamping", "Berbah", "Cangkringan", 
                      "Godean", "Kalasan", "Minggir", "Mlati", "Moyudan", 
                      "Ngemplak", "Pakem", "Prambanan", "Seyegan", "Sleman", 
                      "Tempel", "Turi")
# Data TPS3R dengan informasi tambahan
tps3r_data <- data.frame(
  Kecamatan = sleman_kecamatan,
  Latitude = c(-7.769, -7.678, -7.792, -7.811, -7.641, -7.769, -7.781, -7.806, 
               -7.747, -7.833, -7.702, -7.637, -7.762, -7.741, -7.716, -7.694, -7.667),
  Longitude = c(110.395, 110.398, 110.317, 110.453, 110.466, 110.297, 110.437, 
                110.276, 110.344, 110.283, 110.422, 110.378, 110.496, 110.323, 
                110.355, 110.305, 110.333),
  Nama_PSM = c("TPS3R Sample Depok", "TPS3R Sample Ngaglik", "TPS3R Sample Gamping", 
               "TPS3R Sample Berbah", "TPS3R Sample Cangkringan", "TPS3R Sample Godean", 
               "TPS3R Sample Kalasan", "TPS3R Sample Minggir", "TPS3R Sample Mlati", 
               "TPS3R Sample Moyudan", "TPS3R Sample Ngemplak", "TPS3R Sample Pakem", 
               "TPS3R Sample Prambanan", "TPS3R Sample Seyegan", "TPS3R Sample Sleman", 
               "TPS3R Kenanga Merdiko", "TPS3R Sample Turi"),
  Jenis_PSM = "TPS3R",
  Alamat = c("Jl. Sample Depok", "Jl. Sample Ngaglik", "Jl. Sample Gamping", 
             "Jl. Sample Berbah", "Jl. Sample Cangkringan", "Jl. Sample Godean", 
             "Jl. Sample Kalasan", "Jl. Sample Minggir", "Jl. Sample Mlati", 
             "Jl. Sample Moyudan", "Jl. Sample Ngemplak", "Jl. Sample Pakem", 
             "Jl. Sample Prambanan", "Jl. Sample Seyegan", "Jl. Sample Sleman", 
             "Soka Martani, Merdikorejo, Tempel, Sleman", "Jl. Sample Turi"),
  No_Telp = "0812-3456-7890",
  Gmaps_Link = c(rep("https://g.co/kgs/sample", 15), "https://g.co/kgs/f9mBxsh", "https://g.co/kgs/sample")
)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: Arial, sans-serif; background-color: #f0f9e8; }
      .title { text-align: center; color: #2e7d32; font-size: 2.5em; margin-top: 20px; }
      .welcome { text-align: center; color: #388e3c; font-size: 1.5em; }
      .btn-custom { background-color: #4caf50; color: white; border: none; padding: 10px 20px; 
                    font-size: 1.2em; margin: 10px; cursor: pointer; }
      .btn-custom:hover { background-color: #388e3c; }
      .btn-back { background-color: #ff9800; color: white; border: none; padding: 8px 16px; 
                  font-size: 1em; margin: 10px; cursor: pointer; }
      .btn-back:hover { background-color: #f57c00; }
      .info-box { background-color: #e8f5e9; padding: 15px; border-radius: 5px; margin: 10px; }
      .info-link { color: #2e7d32; cursor: pointer; text-decoration: underline; margin-left: 10px; }
      .info-link:hover { color: #388e3c; }
      .popup { position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); 
               background-color: #fff; padding: 20px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.3); 
               z-index: 1000; max-width: 300px; text-align: center; }
      .popup-overlay { position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
                       background: rgba(0,0,0,0.5); z-index: 999; }
      .user-info { position: absolute; top: 10px; left: 10px; color: #2e7d32; font-size: 1.2em; }
      .user-stats { font-size: 0.9em; color: #388e3c; }
      .emoji { font-size: 1.5em; }
    ")),
    tags$script(HTML("
      function showPopup(popupId) {
        document.getElementById('popup-overlay').style.display = 'block';
        document.getElementById(popupId).style.display = 'block';
      }
      function hidePopup(popupId) {
        document.getElementById('popup-overlay').style.display = 'none';
        document.getElementById(popupId).style.display = 'none';
      }
    "))
  ),
  
  # Kontrol halaman
  uiOutput("current_page"),
  # Popup overlay dan konten
  div(id = "popup-overlay"),
  div(id = "green-popup", class = "popup", style = "display: none;",
      p("Sampah hijau merupakan sampah organik yang kaya akan nitrogen. Terdiri dari sisa buah, sayur, kulit telur, dan rumput hijau."),
      actionButton("close_green_popup", "Tutup", class = "btn-custom")
  ),
  div(id = "brown-popup", class = "popup", style = "display: none;",
      p("Sampah cokelat merupakan sampah organik yang kaya akan karbon. Terdiri dari daun kering, ranting kecil, dan bubuk kayu."),
      actionButton("close_brown_popup", "Tutup", class = "btn-custom")
  )
)

# Server
server <- function(input, output, session) {
  # File untuk menyimpan data pengguna
  user_file <- "users.rds"
  
  # Inisialisasi data pengguna
  if (file.exists(user_file)) {
    rv <- reactiveValues(
      page = "main", 
      history = c(), 
      users = readRDS(user_file),
      current_user = NULL
    )
  } else {
    rv <- reactiveValues(
      page = "main", 
      history = c(), 
      users = data.frame(
        Name = character(), 
        Email = character(), 
        Password = character(), 
        Sent_Waste = numeric(),
        Processed_Waste = numeric(),
        stringsAsFactors = FALSE
      ),
      current_user = NULL
    )
  }
  
  # Fungsi untuk memperbarui riwayat
  update_history <- function(new_page) {
    rv$history <- c(rv$history, rv$page)
    rv$page <- new_page
  }
  
  # Fungsi untuk kembali ke halaman sebelumnya
  go_back <- function() {
    if (length(rv$history) > 0) {
      rv$page <- tail(rv$history, 1)
      rv$history <- head(rv$history, -1)
    }
  }
  
  # Simpan data pengguna ke file
  save_users <- function() {
    saveRDS(rv$users, user_file)
  }
  
  # Render UI berdasarkan halaman
  output$current_page <- renderUI({
    # Tampilkan info pengguna setelah login
    user_info <- if (!is.null(rv$current_user)) {
      tagList(
        div(class = "user-info",
            p(strong(paste("Hi,", rv$current_user$Name))),
            p(class = "user-stats", paste("Sampah yang Kamu Kirim:", rv$current_user$Sent_Waste, "kg")),
            p(class = "user-stats", paste("Sampah yang Kamu Olah:", rv$current_user$Processed_Waste, "kg"))
        )
      )
    } else {
      NULL
    }
    
    page_content <- if (rv$page == "main") {
      tagList(
        h1("Selamat Datang di Aplikasi Pengolahan Sampah Organik 🌱", class = "title"),
        p("Ayo mulai kelola sampah organikmu untuk bumi yang lebih hijau! 😊", class = "welcome"),
        actionButton("start_btn", "Start", class = "btn-custom")
      )
    } else if (rv$page == "login") {
      tagList(
        h2("Login", style = "text-align: center; color: #2e7d32;"),
        textInput("login_email", "Email", placeholder = "Masukkan email"),
        passwordInput("login_password", "Password", placeholder = "Masukkan password"),
        actionButton("login_btn", "Login", class = "btn-custom"),
        p("Belum punya akun? ", 
          actionButton("to_signup_btn", "Sign Up", class = "btn-custom")),
        actionButton("back_btn", "Kembali", class = "btn-back")
      )
    } else if (rv$page == "signup") {
      tagList(
        h2("Sign Up", style = "text-align: center; color: #2e7d32;"),
        textInput("signup_name", "Nama", placeholder = "Masukkan nama"),
        textInput("signup_email", "Email", placeholder = "Masukkan email"),
        passwordInput("signup_password", "Password", placeholder = "Masukkan password"),
        actionButton("signup_btn", "Daftar", class = "btn-custom"),
        p("Sudah punya akun? ", 
          actionButton("to_login_btn", "Login", class = "btn-custom")),
        actionButton("back_btn", "Kembali", class = "btn-back")
      )
    } else if (rv$page == "process") {
      tagList(
        h2("Pilih Cara Pengolahan Sampahmu ♻️", style = "text-align: center; color: #2e7d32;"),
        radioButtons("process_choice", "Apakah kamu ingin mengolah sampah sendiri?",
                     choices = c("Ya" = "yes", "Tidak" = "no"), inline = TRUE),
        actionButton("process_submit", "Lanjut", class = "btn-custom"),
        actionButton("back_btn", "Kembali", class = "btn-back")
      )
    } else if (rv$page == "tps") {
      tagList(
        h2("Pengiriman Sampah ke TPS3R 📍", style = "text-align: center; color: #2e7d32;"),
        selectInput("kabupaten", "Pilih Kabupaten", 
                    choices = c("Yogyakarta", "Sleman", "Gunung Kidul", "Bantul", "Kulon Progo")),
        conditionalPanel(
          condition = "input.kabupaten == 'Sleman'",
          selectInput("kecamatan", "Pilih Kecamatan", choices = sleman_kecamatan)
        ),
        numericInput("tps_waste", "Berapa kg sampah yang akan dikirim?", value = 0, min = 0),
        actionButton("tps_submit", "Cari TPS3R Terdekat", class = "btn-custom"),
        actionButton("back_btn", "Kembali", class = "btn-back"),
        uiOutput("tps_result")
      )
    } else if (rv$page == "compost") {
      tagList(
        h2("Hitung Kompos dari Sampah Organikmu 🌿", style = "text-align: center; color: #2e7d32;"),
        div(class = "info-box",
            numericInput("green_waste", 
                         div("Berapa kg sampah hijau yang kamu punya?", 
                             tags$span(class = "info-link", 
                                       onclick = "showPopup('green-popup')", 
                                       "Nggak tahu apa itu sampah hijau? Klik ini")),
                         value = 0, min = 0),
            numericInput("brown_waste", 
                         div("Berapa kg sampah cokelat yang kamu punya?", 
                             tags$span(class = "info-link", 
                                       onclick = "showPopup('brown-popup')", 
                                       "Nggak tahu apa itu sampah cokelat? Klik ini")),
                         value = 0, min = 0)
        ),
        actionButton("compost_submit", "Hitung Kompos", class = "btn-custom"),
        actionButton("back_btn", "Kembali", class = "btn-back"),
        uiOutput("compost_result")
      )
    }
    
    tagList(user_info, page_content)
  })
  
  # Tutup popup
  observeEvent(input$close_green_popup, {
    session$sendCustomMessage(type = 'hidePopup', message = list(popupId = 'green-popup'))
  })
  
  observeEvent(input$close_brown_popup, {
    session$sendCustomMessage(type = 'hidePopup', message = list(popupId = 'brown-popup'))
  })
  
  # Handler untuk JavaScript hidePopup
  session$onSessionEnded(function() {
    session$sendCustomMessage(type = 'hidePopup', message = list(popupId = 'green-popup'))
    session$sendCustomMessage(type = 'hidePopup', message = list(popupId = 'brown-popup'))
  })
  
  # Navigasi ke halaman login
  observeEvent(input$start_btn, {
    update_history("login")
  })
  
  # Navigasi ke halaman sign-up dari login
  observeEvent(input$to_signup_btn, {
    update_history("signup")
  })
  
  # Navigasi ke halaman login dari sign-up
  observeEvent(input$to_login_btn, {
    update_history("login")
  })
  
  # Logika sign-up
  observeEvent(input$signup_btn, {
    if (nchar(input$signup_name) > 0 && 
        nchar(input$signup_email) > 0 && 
        nchar(input$signup_password) > 0) {
      if (input$signup_email %in% rv$users$Email) {
        showNotification("Email sudah terdaftar! Gunakan email lain atau login.", type = "error")
      } else {
        rv$users <- rbind(rv$users, data.frame(
          Name = input$signup_name,
          Email = input$signup_email,
          Password = input$signup_password,
          Sent_Waste = 0,
          Processed_Waste = 0,
          stringsAsFactors = FALSE
        ))
        save_users()
        showNotification("Pendaftaran berhasil! Silakan login.", type = "message")
        update_history("login")
      }
    } else {
      showNotification("Lengkapi semua kolom!", type = "error")
    }
  })
  
  # Logika login
  observeEvent(input$login_btn, {
    if (nchar(input$login_email) > 0 && nchar(input$login_password) > 0) {
      user <- rv$users[rv$users$Email == input$login_email, ]
      if (nrow(user) > 0 && user$Password == input$login_password) {
        rv$current_user <- user
        update_history("process")
      } else {
        showNotification("Email atau password salah! Silakan coba lagi atau sign-up.", type = "error")
      }
    } else {
      showNotification("Masukkan email dan password!", type = "error")
    }
  })
  
  # Tombol kembali
  observeEvent(input$back_btn, {
    go_back()
  })
  
  # Navigasi berdasarkan pilihan pengolahan
  observeEvent(input$process_submit, {
    if (input$process_choice == "no") {
      update_history("tps")
    } else {
      update_history("compost")
    }
  })
  
  # Logika TPS3R
  observeEvent(input$tps_submit, {
    if (input$kabupaten == "Sleman") {
      if (input$tps_waste <= 0) {
        showNotification("Masukkan jumlah sampah yang valid!", type = "error")
      } else {
        selected_kec <- input$kecamatan
        tps <- tps3r_data[tps3r_data$Kecamatan == selected_kec, ]
        # Tambah jumlah sampah yang dikirim
        rv$users$Sent_Waste[rv$users$Email == rv$current_user$Email] <- 
          rv$users$Sent_Waste[rv$users$Email == rv$current_user$Email] + input$tps_waste
        rv$current_user$Sent_Waste <- rv$current_user$Sent_Waste + input$tps_waste
        save_users()
        
        output$tps_result <- renderUI({
          tagList(
            h3("Lokasi TPS3R Terdekat:"),
            p(strong("Nama PSM: "), tps$Nama_PSM),
            p(strong("Jenis PSM: "), tps$Jenis_PSM),
            p(strong("Alamat: "), tps$Alamat),
            p(strong("No. Telp: "), tps$No_Telp),
            p(strong("Google Maps: "), tags$a(href = tps$Gmaps_Link, "Lihat di Google Maps", target = "_blank")),
            leafletOutput("tps_map"),
            p("Silahkan menuju lokasi tersebut yaa!! Thank you 😊🌍", style = "color: #2e7d32; font-weight: bold;")
          )
        })
        output$tps_map <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            addMarkers(lat = tps$Latitude, lng = tps$Longitude, popup = tps$Nama_PSM)
        })
      }
    } else {
      output$tps_result <- renderUI({
        p("Mohon maaf, saat ini hanya tersedia data TPS3R untuk Sleman. 😔", style = "color: #d32f2f;")
      })
    }
  })
  
  # Logika perhitungan kompos
  observeEvent(input$compost_submit, {
    green <- input$green_waste
    brown <- input$brown_waste
    total_waste <- green + brown
    if (total_waste <= 0) {
      showNotification("Masukkan jumlah sampah yang valid!", type = "error")
    } else {
      compost <- (total_waste * 3) / 4
      # Simpan data sampah yang diolah
      rv$users$Processed_Waste[rv$users$Email == rv$current_user$Email] <- 
        rv$users$Processed_Waste[rv$users$Email == rv$current_user$Email] + total_waste
      rv$current_user$Processed_Waste <- rv$current_user$Processed_Waste + total_waste
      save_users()
      
      output$compost_result <- renderUI({
        tagList(
          h3("Hasil Perhitungan:"),
          p(sprintf("Dari %.2f kg sampah organik, kamu bisa menghasilkan sekitar %.2f kg kompos! 🎉", 
                    total_waste, compost), style = "color: #2e7d32;"),
          plotOutput("compost_plot"),
          h4("Langkah-langkah Mengompos:"),
          tags$ol(
            tags$li("Siapkan wadah kompos atau lubang di tanah."),
            tags$li("Campur sampah hijau dan cokelat dengan rasio seimbang."),
            tags$li("Jaga kelembapan seperti kain basah yang diperas."),
            tags$li("Aduk setiap 3-5 hari untuk aerasi."),
            tags$li("Tunggu 1-2 bulan hingga kompos matang.")
          ),
          p("Selamat mengompos sampah kami. Thank you ya udah mau ikut serta menjaga bumi! 🌱😊", 
            style = "color: #2e7d32; font-weight: bold;")
        )
      })
      output$compost_plot <- renderPlot({
        data <- data.frame(
          Category = c("Sampah Hijau", "Sampah Cokelat", "Kompos"),
          Weight = c(green, brown, compost)
        )
        ggplot(data, aes(x = Category, y = Weight, fill = Category)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = c("Sampah Hijau" = "#4caf50", 
                                       "Sampah Cokelat" = "#8d6e63", 
                                       "Kompos" = "#2e7d32")) +
          labs(title = "Komposisi Sampah dan Hasil Kompos", y = "Berat (kg)", x = "") +
          theme_minimal()
      })
    }
  })
}

# Run app
shinyApp(ui, server)



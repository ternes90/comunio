library(shiny)
library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(readxl)
library(DT)

last_update <- tryCatch(readLines("last_updated.txt", warn = FALSE), error = function(e) "unbekannt")

# ---- UI ----
ui <- navbarPage(
  "Comunio Analyse",
  
  #Pushaktualisierung
  tags$div(
    style = "text-align: right; font-size: 12px; color: grey; margin: 5px;",
    paste("Letztes Update:", last_update)
  ),
  
  id = "main_navbar",
  
  ## ---- Dashboard ----
  tabPanel("Dashboard",
           fluidPage(
             
             # Aktuelle Transfers - Zusammenfassung
             div(
               style = "margin-top: 20px; display: flex; flex-direction: column; align-items: center;",
               tags$div("Aktuelle Transfers", 
                        style = "text-align: center; font-size: 16px; font-weight: bold; color: black; margin-bottom: 10px;"),
               div(
                 style = "margin-bottom:20px; width: 100%;",
                 DTOutput("transfer_summary_today", width = "100%")
               ),
               tags$div("Aktuelle Flips", 
                        style = "text-align: center; font-size: 16px; font-weight: bold; color: black; margin-bottom: 10px;"),
               div(
                 style = "margin-bottom:20px; width: 100%;",
                 DTOutput("flip_summary_today", width = "100%")
               )
             ),
             
             # Marktwert-Zeitachse
             div(
               style = "margin-bottom: 20px;",
               plotOutput("mw_zeitachse_preview", height = 350, width = "100%", click = "mw_zeitachse_click")
             ),
             
             # Kreditrahmen-Übersicht
             div(
               style = "margin-bottom: 20px;",
               DTOutput("kreditrahmen_uebersicht_preview", width = "100%")
             ),
             
             # Mein Team Tabelle
             div(
               style = "margin-bottom: 20px;",
               DTOutput("mein_team_tabelle_preview", width = "100%")
             ),
             
             # Flip-Vorschau
             div(
               style = "margin-bottom: 20px;",
               plotOutput("flip_preview", height = 300, width = "100%", click = "flip_click")
             ),
             
             # Gebote Vorschau
             div(
               style = "margin-bottom: 20px;",
               plotOutput("gebote_preview", height = 350, width = "100%", click = "gebote_click")
             ),
             
             # Flip-Einnahmen Übersicht
             div(
               style = "margin-bottom: 20px;",
               DTOutput("flip_einnahmen_uebersicht_preview", width = "100%")
             ),
             
             # Transfermarkt Vorschau
             div(
               style = "display: flex; flex-direction: column; align-items: center;",
               tags$div("Aktueller Transfermarkt", 
                        style = "text-align: center; font-size: 16px; font-weight: bold; color: black; margin: 20px 0 10px 0;"),
               div(
                 style = "width: 100%;",
                 DTOutput("transfermarkt_preview", width = "100%")
               )
             )
           )
  ),
  
  
  
  ## ---- Marktwert-Entwicklung ----
  tabPanel("Marktwert-Entwicklung",
           tabsetPanel(
             tabPanel("MW-Verlauf", 
                      div(
                        style = "text-align: center; margin-bottom: 10px;",
                        HTML("
          <b>Legende:</b><br>
          <span style='color:#005c99; font-weight:bold;'>▍</span> Ø MW TM-Spieler &nbsp;&nbsp;
          <span style='color:darkgrey; font-weight:bold;'>▍</span> Gesamtmarktwert (alle Spieler) &nbsp;&nbsp;
          <span style='color:red; font-weight:bold;'>▍</span> Sommerpause 2024 &nbsp;&nbsp;
          <span style='color:orange; font-weight:bold;'>▍</span> Sommerpause 2021
        ")
                      ),
                      plotOutput("mw_evolution", height = 600)
             ),
             tabPanel("MW-Verlauf 24/25 (MW-Klassen)",
                      plotOutput("mw_plot"),
                      plotOutput("mw_plot_now")
             ),
             tabPanel("Hist. Saisonverläufe - Chronologie",
                      plotOutput("historical_seasons_plot_all", height = 600)
             ),
             tabPanel("Hist. Saisonverläufe - Select",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("selected_seasons", "Saisons auswählen zum Vergleich:",
                                             choices = NULL,
                                             selected = NULL)
                        ),
                        mainPanel(
                          plotOutput("historical_seasons_plot_selected", height = 600)
                        )
                      )
             )
           )
  ),
  
  ## ---- Transfermarkt ----
  tabPanel("Transfermarkt",
           tabPanel("Transfermarkt Details",
                    DTOutput("transfermarkt_detail")
           )
  ),

  ## ---- Kader-Entwicklung ----
  tabPanel("Kader-Entwicklung",
           tabsetPanel(
             id = "kader_tabs",
             tabPanel("Mein Kader", uiOutput("mein_kader")),
             tabPanel("Alle Kader", uiOutput("kader_uebersicht_ui")),
             tabPanel("Kaderwert-Plot", plotOutput("kaderwert_plot", height = 600))
           )
  ),
  
  ## ---- Bieterprofile ----
  tabPanel("Bieterprofile",
           tabsetPanel(
             id = "bieterprofile_tabs",
             tabPanel(
               "MW-Klassen",
               
               # Schlanker Zeitstrahl-Plot (kleine Höhe)
               plotOutput("mw_zeitachse_preview_schlank", height = "100px", width = "100%"),
               
               # SliderInput in voller Breite
               div(style = "width: 90%; margin: 0 auto 15px auto;",  # 90% Breite, zentriert, unten Abstand
                   sliderInput(
                     inputId = "mwclass_date_range",
                     label = "Zeitraum auswählen:",
                     min = as.Date("2025-01-01"),
                     max = as.Date("2025-12-31"),
                     value = c(as.Date("2025-06-01"), as.Date("2025-07-01")),
                     timeFormat = "%d.%m.%Y",
                     width = "100%"  # füllt den umgebenden div komplett aus
                   )
               )
               ,
               
               div(style = "width: 90%; margin: 0 auto;",
                   plotOutput("mwclassplot", height = 700, width = "100%")
               )
               
             )
             ,
             tabPanel("Punkte", plotOutput("beeswarm", height = 700)),
             tabPanel("Zeit-Trend", plotOutput("trendplot", height = 700)),
             tabPanel("Gebots-Frequenz",
                      plotOutput("gebote_pro_tag", height = 350),
                      plotOutput("gebote_pro_tag_linie", height = 350)
             ),
             tabPanel("Gebots-Frequenz je Konkurrent",
                      plotOutput("gebote_pro_tag_bieter", height = 600)
             ),
             tabPanel("Gebots-peaks",
                      plotOutput("aktive_tage_plot", height = 350),
                      plotOutput("peak_days_heatmap", height = 350)
             ),
             tabPanel("Zusammenfassung",
                      tags$div(
                        style = "display: flex; justify-content: center; align-items: center; gap: 40px; margin-bottom: 20px; margin-top: 20px;",
                        
                        # Checkbox 1
                        tags$label(
                          style = "display: flex; align-items: center; font-size: 14px; gap: 8px;",
                          tags$input(id = "show_table", type = "checkbox", style = "width: 18px; height: 18px;"),
                          "Gebote je Marktwert-Klasse anzeigen"
                        ),
                        
                        # Checkbox 2
                        tags$label(
                          style = "display: flex; align-items: center; font-size: 14px; gap: 8px;",
                          tags$input(id = "show_entries", type = "checkbox", style = "width: 18px; height: 18px;"),
                          "Transfers anzeigen"
                        )
                      ),
                      DTOutput("mwclass_summary")
             )
           )
  ),
  
  ## ---- Flip-Analyse ----
  tabPanel("Flip-Analyse",
           tabsetPanel(
             id = "flip_tabs",
             
             # TAB 1: Gesamtsumme + Kumuliert
             tabPanel("Übersicht gesamt",
                      fluidPage(
                        fluidRow(
                          column(12, plotOutput("flip_summarybar", height = 500))
                        ),
                        fluidRow(
                          column(12, plotOutput("flip_cumulative", height = 500))
                        )
                      )
             ),
             
             # TAB 2: Kategorien gestapelt + Kumuliert nach Flip-Art
             tabPanel("Flip-Arten",
                      fluidPage(
                        fluidRow(
                          column(7, plotOutput("flip_effizienz", height = 500)),
                          column(5, plotOutput("flip_cumcat", height = 500))
                        )
                      )
             ),
             
             # TAB 3: Kader-Flip + Historie
             tabPanel("Kader & Historie",
                      fluidPage(
                        fluidRow(
                          column(12, DTOutput("flip_kader"))
                        ),
                        br(),
                        fluidRow(
                          column(4,
                                 selectInput("flip_player_select", "Spieler auswählen:", choices = NULL)
                          ),
                          column(8, DTOutput("flip_player_table"))
                        )
                      )
             )
           )
  ),
  
  ## ---- Kapitalübersicht ----
  tabPanel("Kapitalübersicht",
           fluidPage(
             DTOutput("kapital_uebersicht_table")
           )
  ),
  # UI-Seite oder tags$head einfügen
  tags$script(HTML("
  $(document).on('click', '#kapital_uebersicht_table tbody td', function() {
    Shiny.setInputValue('kapital_table_cell_clicked', Math.random()); // random um mehrfaches Event zu erlauben
  });
"))
  
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Link vom Dashboard zum MW Trend tab
  observeEvent(input$mw_zeitachse_click, {
    updateNavbarPage(session, "main_navbar", selected = "Marktwert-Entwicklung")
  })

  # Kapitalübersicht
  observeEvent(input$kreditrahmen_uebersicht_preview_rows_selected, {
    # egal was ausgewählt wird, Tabwechsel:
    updateNavbarPage(session, "main_navbar", "Kapitalübersicht")
  })

  # Link vom Dashboard zum Flip tab
  observeEvent(input$flip_click, {
    updateNavbarPage(session, "main_navbar", selected = "Flip-Analyse")
  })

  # Link vom Dashboard zum Gebote tab
  observeEvent(input$gebote_click, {
    updateNavbarPage(session, "main_navbar", selected = "Bieterprofile")
  })

  # Link vom Dashboard zum Kader tab (Mein Kader)
  observeEvent(input$mein_team_tabelle_preview_rows_selected, {
    updateNavbarPage(session, "main_navbar", selected = "Kader-Entwicklung")
    updateTabsetPanel(session, "kader_tabs", selected = "Mein Kader")
  })
  
  # Link vom Dashboard zum Hypothetischen Team-Flip tab (Kader & Historie)
  observeEvent(input$flip_einnahmen_uebersicht_preview_rows_selected, {
    updateNavbarPage(session, "main_navbar", selected = "Flip-Analyse")
    updateTabsetPanel(session, "flip_tabs", selected = "Kader & Historie")
  })
  
  # Link vom Dashboard zum TM tab
  observeEvent(input$transfermarkt_preview_rows_selected, {
    if (!is.null(input$transfermarkt_preview_rows_selected)) {
      updateNavbarPage(session, "main_navbar", selected = "Transfermarkt")
    }
  })
  
  # Link vom Dashboard zum TM tab
  observeEvent(input$transfer_summary_today_rows_selected, {
    if (!is.null(input$transfer_summary_today_rows_selected)) {
      updateNavbarPage(session, "main_navbar", selected = "Kader-Entwicklung")
      updateTabsetPanel(session, "kader_tabs", selected = "Alle Kader")
    }
  })
  
  # Link vom Dashboard zum Hypothetischen Team-Flip tab (Kader & Historie)
  observeEvent(input$flip_summary_today_rows_selected, {
    if (!is.null(input$flip_summary_today_rows_selected)) {
    updateNavbarPage(session, "main_navbar", selected = "Flip-Analyse")
    updateTabsetPanel(session, "flip_tabs", selected = "Kader & Historie")
    }
  })
  
  # ---- DATEN / df / list ----
  
  ## ---- sommerpause_df ----
  sommerpause_df <- readr::read_csv("MW_Sommerpause_2024.csv") %>%
    mutate(
      Datum_raw = as.Date(x),  # x in "2024-06-06" etc.
      Datum = as.Date(format(Datum_raw, "2025-%m-%d"))
    ) %>%
    filter(Datum >= as.Date("2025-06-06")) %>%
    arrange(Datum) %>%
    mutate(
      MW_startwert = y[Datum == as.Date("2025-06-06")][1],
      MW_rel_normiert = y / MW_startwert
    )
  
  ## ---- sommerpause_21_df ----
  sommerpause_21_df <- readr::read_csv("MW_Sommerpause_2021.csv") %>%
    mutate(
      # Erst Datum als Date parsen
      Datum = as.Date(x),
      # Dann Jahr auf 2025 setzen
      Datum = as.Date(format(Datum, "2025-%m-%d")),
    ) %>%
    filter(Datum >= as.Date("2025-06-06")) %>%
    arrange(Datum) %>%
    mutate(
      MW_startwert = y[Datum == as.Date("2025-06-06")][1],
      MW_rel_normiert = y / MW_startwert
    )
  
  ## ---- teams_df / transfers / transfermarkt / ap_df / tm_df / st_df ----
  
  teams_df <- read.csv2("TEAMS_all.csv", sep = ";", stringsAsFactors = FALSE)
  
  transfers <- read.csv2("TRANSFERS_all.csv", sep = ";", na.strings = c("", "NA")) %>%
      mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"))
  
  transfermarkt <- read_csv2("TRANSFERMARKT.csv") %>%
    mutate(TM_Stand = as.Date(TM_Stand, format = "%d.%m.%Y"))
  
  ap_df <- read.csv2("ALL_PLAYERS.csv", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE, fileEncoding = "UTF-8") %>% 
    mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"),
           Marktwert = as.numeric(Marktwert))
  
  tm_df <- read.csv2("COMP_TM_RESTZEIT.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")#
  
  st_df <- readr::read_csv2("STANDINGS.csv", 
                            col_types = cols(
                              Manager = col_character(),
                              Teamwert = col_double(),
                              Datum = col_character()
                            )
  ) %>%
    mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"))
  
  ca_df <- read_delim("com_analytics_all_players.csv", delim = ";", locale = locale(encoding = "UTF-8"))
  ca_df$SPIELER <- trimws(enc2utf8(as.character(ca_df$SPIELER)))
  
  #Kaufempfehung etc.
  ca2_df <- read_delim("com_analytics_transfer_market_computer.csv", delim = ";", locale = locale(encoding = "UTF-8")) %>%
    mutate(Datum = as.Date(Datum, "%d.%m.%Y")) %>% filter(Datum == max(Datum, na.rm = TRUE))
  ca2_df$SPIELER <- trimws(enc2utf8(as.character(ca2_df$SPIELER)))
  
  ca2_df <- ca2_df %>%
    select(
      SPIELER,
      `Punkte pro Spiel` = `PUNKTE / SPIEL`,
      `Preis-Leistung` = `PREIS-LEISSTUNG`,
      Zielwert = `ZIELWERT`,
      Empfehlung = `KAUFEMPFEHLUNG`,
      Gebote = `GEBOTSVORHERSAGE`
    )
  
  
  ## ---- nickname_mapping ----
  nickname_mapping <- c(
    "Alfon" = "Alfons",
    "Nico_2510" = "Nico",
    "HenzgenT" = "Thomas",
    "Hosche" = "Christoph",
    "Crunch" = "Christian",
    "Dr. Bier" = "Dominik",
    "Calli" = "Pascal",
    "Computer" = "Computer"
  )
  
  ## ---- data_all ----
  data_all <- reactive({
    transfers <- transfers %>%
      mutate(
        Datum = as.Date(Datum, format = "%d.%m.%Y"),
        Hoechstgebot = as.numeric(Hoechstgebot),
        Zweitgebot = as.numeric(Zweitgebot),
        Hoechstgebot = ifelse(Datum == as.Date("2025-05-30") & Spieler == "Hranáč", 166000, Hoechstgebot) #Umwandeln von Fehlgebot von Alfons
      )
    
    list(transfers = transfers, transfermarkt = transfermarkt)
  })
  
  standings_df <- reactive({
    st_df %>% 
      group_by(Manager) %>%
      slice_max(Datum, with_ties = FALSE) %>%
      ungroup()
  }) 
  
  ## ---- gebotsprofil_clean (MW nur Vortag oder davor!) ----
  gebotsprofil_clean <- reactive({
    dat <- data_all()
    transfers <- dat$transfers
    transfermarkt <- dat$transfermarkt
    
    gebotsprofil <- lapply(1:nrow(transfers), function(i) {
      zeile <- transfers[i, ]
      mw_vortag <- get_MW_vortag(zeile$Spieler, zeile$Datum, transfermarkt)
      res <- data.frame(
        Datum = zeile$Datum,
        Spieler = zeile$Spieler,
        Bieter = zeile$Hoechstbietender,
        Gebot = zeile$Hoechstgebot,
        MW_vortag = mw_vortag,
        Diff_Abs = zeile$Hoechstgebot - mw_vortag,
        Diff_Prozent = round(100 * (zeile$Hoechstgebot - mw_vortag) / mw_vortag, 1),
        Typ = "Hoechstgebot"
      )
      if (!is.na(zeile$Zweitgebot) & !is.na(zeile$Zweitbietender)) {
        res2 <- data.frame(
          Datum = zeile$Datum,
          Spieler = zeile$Spieler,
          Bieter = zeile$Zweitbietender,
          Gebot = zeile$Zweitgebot,
          MW_vortag = mw_vortag,
          Diff_Abs = zeile$Zweitgebot - mw_vortag,
          Diff_Prozent = round(100 * (zeile$Zweitgebot - mw_vortag) / mw_vortag, 1),
          Typ = "Zweitgebot"
        )
        res <- rbind(res, res2)
      }
      res
    }) %>% bind_rows()
    
    # Filter auf echte Mitspieler (nicht Computer) und gültige MW_vortag
    gebotsprofil %>%
      filter(Bieter != "Computer" & !is.na(MW_vortag))
  })
  
  ## ---- für slider ----
  gebotsprofil_mwclass_filtered <- reactive({
    req(input$mwclass_date_range)
    
    gebotsprofil_mwclass() %>%
      filter(Datum >= input$mwclass_date_range[1], Datum <= input$mwclass_date_range[2])
  })
  
  observe({
    dat <- gebotsprofil_clean()  # oder woher du deine Daten nimmst
    if (nrow(dat) == 0) return()
    min_date <- min(dat$Datum)
    max_date <- max(dat$Datum)
    
    updateSliderInput(session, "mwclass_date_range",
                      min = min_date,
                      max = max_date,
                      value = c(min_date, max_date)
    )
  })
  
  ## ---- flip_player_select ----
  observe({
    updateSelectInput(session, "flip_player_select",
                      choices = sort(unique(flip_data()$Besitzer)))
  })
  
  ## ---- MW Klasse vergeben ----
  gebotsprofil_mwclass <- reactive({
    gebotsprofil_clean() %>%
      mutate(
        MW_Klasse = case_when(
          MW_vortag < 0.5e6 ~ "<0.5 Mio",
          MW_vortag < 1e6 ~ "0.5–1 Mio",
          MW_vortag < 2.5e6 ~ "1–2.5 Mio",
          MW_vortag < 5e6 ~ "2.5–5 Mio",
          MW_vortag < 10e6 ~ "5–10 Mio",
          MW_vortag >= 10e6 ~ ">10 Mio",
          TRUE ~ "unbekannt"
        )
      )
  })
  
  ## ---- Zusammenfassungstabelle für MW-Klassen
  mwclass_summary <- reactive({
    gebotsprofil_mwclass() %>%
      mutate(
        MW_Klasse = factor(
          MW_Klasse,
          levels = c(
            "<0.5 Mio", "0.5–1 Mio", "1–2.5 Mio",
            "2.5–5 Mio", "5–10 Mio", ">10 Mio"
          ),
          ordered = TRUE
        )
      ) %>%
      group_by(Bieter, MW_Klasse) %>%
      summarise(
        Anzahl_gesamt = n(),
        Anzahl_Hoechstgebote = sum(Typ == "Hoechstgebot"),
        Anzahl_Zweitgebote = sum(Typ == "Zweitgebot"),
        Durchschnitt_Abweichung = round(mean(Diff_Prozent, na.rm = TRUE), 1),
        Min = min(Diff_Prozent, na.rm = TRUE),
        Max = max(Diff_Prozent, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Bieter, MW_Klasse)
  })
  
  # ---- FUNKTIONEN ----
  ## ---- Funktion MW vom Vortag suchen ----
  get_MW_vortag <- function(spieler, datum, transfermarkt) {
    tm <- transfermarkt %>%
      filter(Spieler == spieler, TM_Stand <= (datum - days(1))) %>%
      arrange(desc(TM_Stand)) %>%
      slice(1)
    if (nrow(tm) == 1) tm$Marktwert else NA
  }
  
  # MW-Klasse bestimmen
  get_mw_klasse <- function(mw) {
    if (mw < 5e5) {
      "<0.5 Mio"
    } else if (mw < 1e6) {
      "0.5–1 Mio"
    } else if (mw < 2.5e6) {
      "1–2.5 Mio"
    } else if (mw < 5e6) {
      "2.5–5 Mio"
    } else if (mw < 1e7) {
      "5–10 Mio"
    } else {
      ">10 Mio"
    }
  }

  # ---- DASHBOARD ----
  
  ## ---- Transferaktivitäten ----
  
  output$transfer_summary_today <- DT::renderDT({
    today <- Sys.Date()
    
    # Sicherstellen, dass Zweitgebot existiert
    if (!"Zweitgebot" %in% names(transfers)) {
      transfers$Zweitgebot <- NA_real_
    }
    
    # Nur heutige Transfers (exkl. Computer)
    todays_transfers <- transfers %>%
      filter(Datum == today, Hoechstbietender != "Computer") %>%
      select(Spieler, Hoechstbietender, Hoechstgebot, Zweitgebot, Datum)
    
    mw_today <- ap_df %>%
      filter(Datum == today) %>%
      select(Spieler, Marktwert_today = Marktwert)
    
    mw_prev <- ap_df %>%
      filter(Datum == (today - 1)) %>%
      select(Spieler, Marktwert_prev = Marktwert)
    
    df <- todays_transfers %>%
      left_join(mw_today, by = "Spieler") %>%
      left_join(mw_prev, by = "Spieler") %>%
      mutate(
        Zweitgebot = as.numeric(gsub("[^0-9]", "", Zweitgebot)),
        Hoechstgebot = as.numeric(Hoechstgebot),
        Marktwert_prev = as.numeric(Marktwert_prev),
        Marktwert_today = as.numeric(Marktwert_today),
        Diff_Hoechst_prev_abs = Hoechstgebot - Marktwert_prev,
        Diff_Hoechst_prev_pct = ifelse(!is.na(Marktwert_prev) & Marktwert_prev > 0,
                                       100 * Diff_Hoechst_prev_abs / Marktwert_prev, NA),
        Flip_Potenzial = Marktwert_today - Hoechstgebot,
        Diff_Zweit_prev_pct = ifelse(!is.na(Zweitgebot) & !is.na(Marktwert_prev) & Marktwert_prev > 0,
                                     100 * (Zweitgebot - Marktwert_prev) / Marktwert_prev, NA),
        MW_Trend = case_when(
          is.na(Marktwert_prev) | is.na(Marktwert_today) ~ "–",
          Marktwert_today > Marktwert_prev ~ '<span style="color:green; font-weight:bold;">▲</span>',
          Marktwert_today < Marktwert_prev ~ '<span style="color:red; font-weight:bold;">▼</span>',
          TRUE ~ "–"
        ),
        Diff_Hoechst_prev_pct_fmt = ifelse(
          is.na(Diff_Hoechst_prev_pct), "-",
          paste0(ifelse(Diff_Hoechst_prev_pct >= 0, "+", "-"),
                 round(abs(Diff_Hoechst_prev_pct), 1), " %")
        ),
        Diff_Zweit_prev_pct_fmt = ifelse(
          is.na(Diff_Zweit_prev_pct), "-",
          paste0(ifelse(Diff_Zweit_prev_pct >= 0, "+", "-"),
                 round(abs(Diff_Zweit_prev_pct), 1), " %")
        ),
        Flip_Potenzial_fmt = ifelse(
          is.na(Flip_Potenzial), "-",
          ifelse(
            Flip_Potenzial >= 0,
            paste0('<span style="color:darkgreen; font-weight:bold;">+',
                   format(abs(Flip_Potenzial), big.mark = ".", decimal.mark = ","), " €</span>"),
            paste0('<span style="color:red; font-weight:bold;">-',
                   format(abs(Flip_Potenzial), big.mark = ".", decimal.mark = ","), " €</span>")
          )
        )
      ) %>%
      select(
        Datum,
        Marktwert_prev,
        Spieler,
        Hoechstbietender,
        Marktwert_today,
        MW_Trend,
        Hoechstgebot,
        Diff_Hoechst_prev_pct_fmt,
        Flip_Potenzial_fmt
      ) %>%
      rename(
        "Datum" = Datum,
        "MW Vortag" = Marktwert_prev,
        "Spieler" = Spieler,
        "Käufer" = Hoechstbietender,
        "MW Heute" = Marktwert_today,
        "Trend" = MW_Trend,
        "Preis" = Hoechstgebot,
        "Δ Preis (%)" = Diff_Hoechst_prev_pct_fmt,
        "Flip (€)" = Flip_Potenzial_fmt
      )
    
    datatable(
      df,
      escape = FALSE,
      rownames = FALSE,
      selection = "single",
      options = list(
        dom = 't',
        pageLength = 10,
        scrollX = TRUE,
        paging = FALSE
      )
    ) %>%
      formatCurrency(
        columns = c("MW Vortag", "MW Heute", "Preis"),
        currency = "", interval = 3, mark = ".", digits = 0
      )
  })
  
  
  
  
  ## ---- Flipaktivitäten ----
  
  output$flip_summary_today <- DT::renderDT({
    latest_date <- Sys.Date()
    
    df <- flip_data() %>%
      filter(Verkaufsdatum == latest_date) %>%
      select(Einkaufsdatum, Einkaufspreis, Spieler, Besitzer, Verkaufspreis, Gewinn) %>%
      arrange(desc(Einkaufsdatum))
    
    datatable(
      df,
      selection = "single",
      rownames = FALSE,  # <--- disable row numbers here
      options = list(dom = 't', pageLength = 10,
                     scrollX = TRUE,
                     paging = FALSE),
      colnames = c(
        "Kaufdatum",
        "Einkaufspreis",
        "Spieler",
        "Verkäufer",
        "Verkaufspreis",
        "Gewinn/Verlust (€)"
      )
    ) %>%
      formatCurrency(
        columns = c("Einkaufspreis", "Verkaufspreis", "Gewinn"),
        currency = "",  # kein Währungssymbol
        interval = 3,
        mark = ".",     # Tausenderpunkt
        digits = 0
      )
  })
  
  
  ## ---- Zeitachse ----
  output$mw_zeitachse_preview <- renderPlot({
    df <- mw_evolution_data()
    
    # Ø MW pro Tag
    plotdata <- df %>%
      group_by(TM_Stand) %>%
      summarise(MW_mean = mean(MW_rel, na.rm = TRUE), .groups = "drop")
    
    # Kombinieren
    lines_df <- bind_rows(
      plotdata %>% transmute(Datum = TM_Stand, Wert = MW_mean, Typ = "Durchschnitt TM-Spieler"),
      gesamt_mw_df %>% transmute(Datum, Wert = MW_rel_normiert, Typ = "Gesamtmarktwert")
    )
    
    ggplot() +
      geom_line(data = lines_df, aes(x = Datum, y = Wert, color = Typ), linewidth = 1.2) +
      geom_line(data = sommerpause_df, aes(x = Datum, y = MW_rel_normiert), color = "red", linetype = "dashed", linewidth = 1) +
      geom_line(data = sommerpause_21_df, aes(x = Datum, y = MW_rel_normiert), color = "orange", linetype = "dashed", linewidth = 1) +
      coord_cartesian(ylim = c(0.75, 1.4)) +
      scale_color_manual(values = c(
        "Durchschnitt TM-Spieler" = "#005c99",
        "Gesamtmarktwert" = "darkgrey"
      )) +
      labs(
        title = "Marktwerte", x = NULL, y = "relativer MW", color = NULL
      ) +
      geom_vline(xintercept = as.Date("2025-08-22"), linetype = "dotted",
                 color = "darkred", size = 1.5) +
      annotate(
        "text",
        x = as.Date("2025-08-22"),
        y = 1.02,  # ggf. anpassen je nach Plot-Skalierung
        label = "Saisonstart",
        color = "darkred",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, color = "black", face = "bold"))
  })
  
  ## ---- Mein Team ----
  output$mein_team_tabelle_preview <- DT::renderDT({
    
    df0 <- teams_df %>% filter(Manager == "Dominik")
    df0$Position <- factor(df0$Position, levels = c("Tor", "Abwehr", "Mittelfeld", "Sturm"), ordered = TRUE)
    df0 <- df0 %>% arrange(Position, Spieler)
    
    mw_aktuell <- gesamt_mw_roh %>%
      group_by(Spieler) %>%
      filter(Datum == max(Datum, na.rm=TRUE)) %>%
      summarise(Marktwert_aktuell = first(Marktwert), .groups = "drop")
    
    df_pre <- df0 %>%
      left_join(mw_aktuell, by = "Spieler") %>%
      left_join(
        gesamt_mw_roh %>%
          group_by(Spieler) %>%
          arrange(desc(Datum)) %>%
          slice(1:2) %>%
          mutate(Diff = Marktwert - lead(Marktwert)) %>%
          slice(1) %>%
          ungroup() %>%
          mutate(
            Diff_fmt = case_when(
              is.na(Diff) ~ "",
              Diff > 0 ~ sprintf("▲ %s €", format(Diff, big.mark = ".", decimal.mark = ",")),
              Diff < 0 ~ sprintf("▼ %s €", format(abs(Diff), big.mark = ".", decimal.mark = ",")),
              TRUE ~ "–"
            )
          ) %>%
          select(Spieler, Diff_fmt, Diff),
        by = "Spieler"
      ) %>%
      mutate(
        Marktwert = ifelse(is.na(Marktwert_aktuell), "-", paste0(format(Marktwert_aktuell, big.mark = ".", decimal.mark = ","), " €"))
      ) %>%
      select(Position, Spieler, Marktwert, Diff_fmt, Diff)
    
    dat <- df_pre %>% arrange(Position, Spieler)
    
    DT::datatable(
      dat %>% select(Position, Spieler, Marktwert, Diff_fmt),
      rownames = FALSE,
      options = list(
        dom = 't',
        ordering = FALSE,
        paging = FALSE
      ),
      colnames = c("Position", "Spieler", "Marktwert", "Tagesveränderung")
    ) %>%
      DT::formatStyle(
        "Diff_fmt",
        color = DT::styleEqual(unique(dat$Diff_fmt), sapply(unique(dat$Diff_fmt), function(x) {
          if (grepl("▲", x)) "#388e3c"
          else if (grepl("▼", x)) "#e53935"
          else "black"
        })),
        fontWeight = "bold"
      )
    
  })
  
  ## ---- Hypothetischer Team Flip-Übersicht aller Manager  ----
  output$flip_einnahmen_uebersicht_preview <- DT::renderDT({
    
    mw_aktuell <- gesamt_mw_roh %>%
      group_by(Spieler) %>%
      filter(Datum == max(Datum, na.rm=TRUE)) %>%
      summarise(Marktwert_aktuell = first(Marktwert), .groups = "drop")
    
    kaufpreise <- transfers %>%
      group_by(Spieler, Hoechstbietender) %>%
      arrange(desc(Datum)) %>%
      slice(1) %>%
      select(Spieler, Manager = Hoechstbietender, Kaufpreis = Hoechstgebot)
    
    df_all <- teams_df %>%
      left_join(mw_aktuell, by = "Spieler") %>%
      left_join(kaufpreise, by = c("Spieler", "Manager")) %>%
      filter(!is.na(Kaufpreis)) %>%
      mutate(Diff = Marktwert_aktuell - Kaufpreis)
    
    # Gesamt je Manager berechnen und sortieren
    gesamt_flip <- df_all %>%
      group_by(Manager) %>%
      summarise(
        sum_diff = sum(Diff, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(sum_diff)) %>%
      mutate(
        `Hypothetisches Team-Flip-Potenzial` = ifelse(
          sum_diff > 0,
          paste0("+", format(sum_diff, big.mark = ".", decimal.mark = ","), " €"),
          paste0("-", format(abs(sum_diff), big.mark = ".", decimal.mark = ","), " €")
        )
      ) %>%
      select(Manager, `Hypothetisches Team-Flip-Potenzial`)
    
    DT::datatable(
      gesamt_flip,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(className = 'dt-right', targets = 1)
        )
      ),
      escape = FALSE,
      selection = "single"
    )
  })
  
  ## ---- Kontostände ----
  output$kreditrahmen_uebersicht_preview <- DT::renderDT({
    
    dat <- data_all()
    transfers <- dat$transfers
    transactions <- readr::read_delim(
      "TRANSACTIONS.csv",
      delim = ";",
      locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ""),
      show_col_types = FALSE
    ) %>%
      mutate(
        Spieler = as.character(Spieler),
        Manager = word(Spieler, 1)  # Vornamen extrahieren
      ) %>%
      filter(!(Datum == "01.06.2025" & Manager == "Alfons" & Transaktion == -166000)) %>%
      group_by(Manager) %>%
      summarise(Transaction_Summe = sum(Transaktion, na.rm = TRUE), .groups = "drop")
    
    alle_manager <- names(startkapital_fix)
    
    ausgaben <- transfers %>%
      group_by(Hoechstbietender) %>%
      summarise(Ausgaben = sum(Hoechstgebot, na.rm = TRUE)) %>%
      rename(Manager = Hoechstbietender)
    
    einnahmen <- transfers %>%
      group_by(Besitzer) %>%
      summarise(Einnahmen = sum(Hoechstgebot, na.rm = TRUE)) %>%
      rename(Manager = Besitzer)
    
    max_datum <- max(st_df$Datum, na.rm = TRUE)
    prev_datum <- max(st_df$Datum[st_df$Datum < max_datum], na.rm = TRUE)
    
    mw_today <- st_df %>% filter(Datum == max_datum) %>% select(Manager, Teamwert_today = Teamwert)
    mw_prev <- st_df %>% filter(Datum == prev_datum) %>% select(Manager, Teamwert_prev = Teamwert)
    
    mw_diff_df <- mw_today %>%
      left_join(mw_prev, by = "Manager") %>%
      mutate(
        Teamwert_Entwicklung = Teamwert_today - Teamwert_prev,
        Teamwert_Entwicklung_fmt = case_when(
          is.na(Teamwert_Entwicklung) ~ "-",
          Teamwert_Entwicklung > 0 ~ paste0('<span style="color:green; font-weight:bold;">▲ ', format(Teamwert_Entwicklung, big.mark = ".", decimal.mark = ","), " €</span>"),
          Teamwert_Entwicklung < 0 ~ paste0('<span style="color:red; font-weight:bold;">▼ ', format(abs(Teamwert_Entwicklung), big.mark = ".", decimal.mark = ","), " €</span>"),
          TRUE ~ as.character(format(Teamwert_Entwicklung, big.mark = ".", decimal.mark = ","))
        )
      ) %>%
      select(Manager, Teamwert_Entwicklung_fmt)
    
    kapital_df <- data.frame(Manager = alle_manager, stringsAsFactors = FALSE)
    
    kapital_df <- kapital_df %>%
      left_join(mw_today, by = "Manager") %>%
      mutate(
        Teamwert_today = ifelse(is.na(Teamwert_today), 0, Teamwert_today),
        Startkapital = startkapital_fix[Manager],
        Kreditrahmen = round(Teamwert_today / 4),
        Ausgaben = ifelse(is.na(ausgaben$Ausgaben[match(Manager, ausgaben$Manager)]), 0, ausgaben$Ausgaben[match(Manager, ausgaben$Manager)]),
        Einnahmen = ifelse(is.na(einnahmen$Einnahmen[match(Manager, einnahmen$Manager)]), 0, einnahmen$Einnahmen[match(Manager, einnahmen$Manager)]),
        Transaction_Summe = ifelse(is.na(transactions$Transaction_Summe[match(Manager, transactions$Manager)]), 0, transactions$Transaction_Summe[match(Manager, transactions$Manager)]),
        Aktuelles_Kapital = Startkapital + Einnahmen - Ausgaben + Transaction_Summe,
        Verfuegbares_Kapital = Aktuelles_Kapital + Kreditrahmen
      ) %>%
      left_join(mw_diff_df, by = "Manager") %>%
      rename(
        Teamwert = Teamwert_today,
        Kontostand = Aktuelles_Kapital,
        `Verfügbares Kapital` = Verfuegbares_Kapital
      ) %>%
      select(Manager, Teamwert, Teamwert_Entwicklung_fmt, Kontostand, `Verfügbares Kapital`)
    
    DT::datatable(
      kapital_df,
      colnames = c(
        "Manager",
        "Teamwert (€)",
        "Teamwert-Entwicklung",
        "Kontostand (€)",
        "Verfügbares Kapital (€)"
      ),
      escape = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        order = list(list(5, 'desc')),  # Sortierung nach Verfügbares Kapital (5. Spalte)
        dom = 't'
      )
    ) %>%
      formatCurrency(
        columns = c("Teamwert", "Kontostand", "Verfügbares Kapital"),
        currency = "",
        interval = 3,
        mark = ".",
        digits = 0
      ) %>%
      formatStyle(
        'Kontostand',
        backgroundColor = styleInterval(0, c('salmon', NA)),
        fontWeight = styleInterval(0, c('bold', NA))
      )
  })
  
  
  ## ---- Flip preview ----
  output$flip_preview <- renderPlot({
    req(nrow(flip_data()) > 0)
    
    df <- flip_data() %>%
      group_by(Besitzer) %>%
      summarise(Gesamtgewinn = sum(Gewinn, na.rm = TRUE), .groups = "drop")
    
    lim_min <- min(df$Gesamtgewinn, na.rm = TRUE) - 1e6
    lim_max <- max(df$Gesamtgewinn, na.rm = TRUE) + 1e6
    
    ggplot(df, aes(x = reorder(Besitzer, Gesamtgewinn), y = Gesamtgewinn, fill = Gesamtgewinn > 0)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = format(Gesamtgewinn, big.mark = ".", scientific = FALSE)),
                position = position_stack(vjust = 0.5), 
                color = "black", fontface = "bold", size = 5) +
      coord_flip(ylim = c(lim_min, lim_max)) +
      scale_fill_manual(values = c("TRUE" = "#66cdaa",  # helleres Grün (Medium Aquamarine)
                                   "FALSE" = "#ff6f61")) + # helleres Rot (Coral)
      theme_minimal() +
      theme(axis.text.y = element_text(size = 15, color = "black"),
            axis.text.x = element_text(size = 12, color = "black"),
            plot.title = element_text(size = 16, face = "bold", hjust = 0, color = "black")) +
      labs(title = "Flip Gewinne/Verluste", x = "", y = "")
  })
  
  
  ## ---- Gebote preview ----
  output$gebote_preview <- renderPlot({
    req(nrow(gebotsprofil_clean()) > 0)
    plotdata <- gebotsprofil_clean() %>%
      filter(!is.na(Diff_Prozent))
    # Mittelwert über beide Typen je Bieter
    mean_total <- plotdata %>%
      group_by(Bieter) %>%
      summarise(Mean_total = mean(Diff_Prozent), .groups = "drop")
    # Mittelwerte je Typ
    means <- plotdata %>%
      group_by(Bieter, Typ) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    ggplot(plotdata, aes(x = Typ, y = Diff_Prozent, color = Typ)) +
      geom_boxplot(aes(fill = Typ), width = 0.5, outlier.shape = NA, alpha = 0.25) +
      geom_beeswarm(cex = 2, size = 2.5, alpha = 0.8) +
      # Gesamter Mittelwert als gestrichelte Linie und Text
      geom_hline(
        data = mean_total,
        aes(yintercept = Mean_total),
        linetype = "dashed", color = "grey60", linewidth = 0.9,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = mean_total,
        aes(x = 0.5, y = Mean_total, label = round(Mean_total, 1)),
        color = "grey30", # Dunkelrot
        fontface = "bold",
        size = 5,
        nudge_x = 1,
        inherit.aes = FALSE
      ) +
      # Mittelwert je Typ als Text (wie gehabt)
      # geom_text(
      #   data = means,
      #   aes(x = Typ, y = Mean, label = round(Mean, 1)),
      #   nudge_x = 0.3,
      #   color = "black",
      #   size = 4,
      #   inherit.aes = FALSE
      # ) +
      facet_wrap(~ Bieter, ncol = 4, scales = "free_y") +
      labs(
        title = "Bieterprofile",
        x = "",
        y = "Abweichung vom Vortags-MW (%)"
      ) +
      scale_color_manual(values = c("Hoechstgebot" = "#1f77b4", "Zweitgebot" = "#ff7f0e")) +
      scale_fill_manual(values = c("Hoechstgebot" = "#1f77b4", "Zweitgebot" = "#ff7f0e")) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold", hjust = 0, color = "black"),
        strip.text = element_text(face = "bold", size = 12),
        axis.text.x = element_blank()
      )
  })
  
  ## ---- Transfermarkt preview ----
  output$transfermarkt_preview <- DT::renderDT({
    
    # Transfermarkt-Daten
    tm_df$Spieler <- trimws(enc2utf8(tm_df$Spieler))
    tm_df$Marktwert_num <- as.numeric(gsub("\\.", "", tm_df$Marktwert))
    tm_df$Mindestgebot_num <- as.numeric(gsub("\\.", "", tm_df$Mindestgebot))
    tm_df$Restzeit <- trimws(tm_df$Restzeit)
    tm_df$Verein <- trimws(enc2utf8(tm_df$Verein))
    
    # Die letzten 3 Tage ermitteln
    last_dates <- sort(unique(ap_df$Datum), decreasing = TRUE)[1:3]
    names(last_dates) <- c("MW3", "MW2", "MW1") # MW1 = ältester Tag, MW3 = aktuell
    
    # Marktwerte für die letzten 3 Tage je Spieler
    mw1 <- ap_df %>% filter(Datum == last_dates["MW1"]) %>% select(Spieler, MW1 = Marktwert)
    mw2 <- ap_df %>% filter(Datum == last_dates["MW2"]) %>% select(Spieler, MW2 = Marktwert)
    mw3 <- ap_df %>% filter(Datum == last_dates["MW3"]) %>% select(Spieler, MW3 = Marktwert)
    
    # Merge alle MWs auf Transfermarkt
    tm_trend <- tm_df %>%
      left_join(mw1, by = "Spieler") %>%
      left_join(mw2, by = "Spieler") %>%
      left_join(mw3, by = "Spieler")
    
    # Trendlogik
    tm_trend$Trend <- mapply(function(mw1, mw2, mw3) {
      if (any(is.na(c(mw1, mw2, mw3)))) return("–")
      if (mw1 < mw2 && mw2 < mw3) {
        return("<span style='color:green;font-weight:bold;'>▲▲</span>")
      } else if (mw1 > mw2 && mw2 > mw3) {
        return("<span style='color:red;font-weight:bold;'>▼▼</span>")
      } else if (mw1 < mw3) {
        return("<span style='color:green;font-weight:bold;'>▲</span>")
      } else if (mw1 > mw3) {
        return("<span style='color:red;font-weight:bold;'>▼</span>")
      } else {
        return("–")
      }
    }, tm_trend$MW1, tm_trend$MW2, tm_trend$MW3)
    
    # Rest wie gehabt…
    tm_trend$Restkategorie <- dplyr::case_when(
      grepl("^2d", tm_trend$Restzeit) ~ "übermorgen",
      grepl("^1d", tm_trend$Restzeit) ~ "morgen",
      grepl("^[0-9]+h", tm_trend$Restzeit) ~ "heute",
      TRUE ~ "unbekannt"
    )
    tm_trend$Restkategorie <- factor(tm_trend$Restkategorie,
                                     levels = c("heute", "morgen", "übermorgen", "unbekannt"),
                                     ordered = TRUE)
    tm_trend$Marktwert <- paste0(format(tm_trend$Marktwert_num, big.mark = ".", decimal.mark = ","), " €")
    tm_trend$Mindestgebot <- paste0(format(tm_trend$Mindestgebot_num, big.mark = ".", decimal.mark = ","), " €")
    tm_trend$MinGeb_Unter_MW <- tm_trend$Mindestgebot_num < tm_trend$Marktwert_num
    
    tm_trend <- tm_trend %>%
      arrange(Restkategorie, desc(Marktwert_num)) %>%
      select(
        Spieler, Verein, Marktwert, Mindestgebot, Besitzer,
        Restkategorie, Trend, MinGeb_Unter_MW
      ) %>%
      rename(
        "Verbleibende Zeit" = Restkategorie,
        "Trend MW (3 Tage)" = Trend
      )
    
    # Setze den richtigen Pfad (www/logos), falls im Shiny www-Ordner!
    logo_dir <- "logos" # oder ggf. "www/logos", je nach Shiny-Ordnerstruktur
    
    # Logo erzeugen
    tm_trend$Logo <- paste0(
      '<img src="', logo_dir, '/', logo_map[tm_trend$Verein], 
      '" width="28" title="', tm_trend$Verein, '"/>'
    )
    tm_trend$Logo[is.na(logo_map[tm_trend$Verein])] <- ""
    
    DT::datatable(
      tm_trend[, c("Spieler", "Logo", "Marktwert", "Mindestgebot", "Besitzer", "Verbleibende Zeit", "Trend MW (3 Tage)")],
      colnames = c("Spieler", "Verein", "Marktwert", "Mindestgebot", "Besitzer", "Verbleibende Zeit", "Trend MW (3 Tage)"),
      rownames = FALSE,
      escape = FALSE,
      selection = "single",
      options = list(
        dom = "t",
        ordering = FALSE,
        pageLength = 20,
        columnDefs = list(
          list(className = 'dt-left', targets = 0:1),
          list(className = 'dt-right', targets = 2:6)
        )
      )
    ) %>%
      DT::formatStyle(
        'Verbleibende Zeit',
        target = 'cell',
        color = DT::styleEqual("heute", "red"),
        fontWeight = DT::styleEqual("heute", "bold")
      ) %>%
      DT::formatStyle(
        'Mindestgebot',
        target = 'cell',
        color = DT::styleEqual(
          tm_trend$Mindestgebot[tm_trend$MinGeb_Unter_MW], "green"
        )
      )
  })
  
  
  # ---- MARKTWERTENTWICKLUNG ----
  ## ---- Besitzhistorie bauen ----
  besitzhistorie <- reactive({
    transfers <- data_all()$transfers
    
    wechsel <- transfers %>%
      arrange(Spieler, Datum) %>%
      select(Spieler, Datum, Hoechstbietender) %>%
      rename(Besitzer = Hoechstbietender)
    
    besitz <- list()
    for (spieler in unique(wechsel$Spieler)) {
      sub <- wechsel %>% filter(Spieler == spieler) %>% arrange(Datum)
      for (i in 1:nrow(sub)) {
        startdatum <- sub$Datum[i]
        enddatum <- if (i < nrow(sub)) sub$Datum[i + 1] - 1 else as.Date("2100-01-01")
        besitz[[length(besitz) + 1]] <- data.frame(
          Spieler = spieler,
          Besitzer = sub$Besitzer[i],
          Startdatum = startdatum,
          Enddatum = enddatum
        )
      }
    }
    bind_rows(besitz)
  })
  
  ## ---- Marktwert-Entwicklung aller Spieler (normiert auf ersten Wert) ----
  mw_evolution_data <- reactive({
    tm <- data_all()$transfermarkt %>%
      mutate(
        TM_Stand = as.Date(TM_Stand, format = "%d.%m.%Y"),
        Besitzer_eff = nickname_mapping[Besitzer]
      )
    
    besitz <- besitzhistorie()
    
    tm_besitz <- tm %>%
      left_join(besitz, by = "Spieler") %>%
      filter(TM_Stand >= Startdatum, TM_Stand <= Enddatum)
    
    tm_besitz %>%
      group_by(Spieler, Besitzer_eff) %>%
      arrange(TM_Stand) %>%
      mutate(MW_rel = Marktwert / first(Marktwert)) %>%
      ungroup() %>%
      select(TM_Stand, Spieler, Besitzer = Besitzer_eff, Marktwert, MW_rel)
  })
  
  ## ---- Gesamtmarktwerte ----
  
  # Manuell zusätzliche Tagesdaten ergänzen
  manuelle_werte <- tibble::tibble(
    Datum = as.Date(c(
      "2025-06-01", "2025-06-02", "2025-06-03", "2025-06-04", "2025-06-05",
      "2025-06-06", "2025-06-07", "2025-06-08", "2025-06-09", "2025-06-10",
      "2025-06-11", "2025-06-12", "2025-06-13", "2025-06-14", "2025-06-15"
    )),
    Marktwert = c(
      1487390000, 1458940000, 1530070000, 1527560000, 1568600000,
      1566740000, 1609040000, 1575690000, 1603080000, 1569800000,
      1587530000, 1556450000, 1587250000, 1537530000, 1549230000
    )
  )
  
  # CSV + manuelle Werte kombinieren
  gesamt_mw_roh <- ap_df %>% 
    bind_rows(manuelle_werte)
  
  # Summieren + Normieren
  gesamt_mw_df <- gesamt_mw_roh %>%
    group_by(Datum) %>%
    summarise(
      MW_gesamt = sum(Marktwert, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      MW_rel_normiert = MW_gesamt / 1487390000  # Referenzwert vom 01.06.2025
    )
  
  ## ---- Ausgabe-Plot ----
  output$mw_evolution <- renderPlot({
    df <- mw_evolution_data()
    
    # Mittelwert + SD der TM-Spieler pro Tag
    plotdata <- df %>%
      group_by(TM_Stand) %>%
      summarise(
        MW_mean = mean(MW_rel, na.rm = TRUE),
        MW_sd = sd(MW_rel, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Kombinierte Daten für Linienplot
    lines_df <- bind_rows(
      plotdata %>%
        transmute(Datum = TM_Stand, Wert = MW_mean, Typ = "Durchschnitt TM-Spieler"),
      gesamt_mw_df %>%
        transmute(Datum, Wert = MW_rel_normiert, Typ = "Gesamtmarktwert")
    )
    
    p <- ggplot() +
      # Schattierung für SD
      geom_ribbon(
        data = plotdata,
        aes(x = TM_Stand, ymin = MW_mean - MW_sd, ymax = MW_mean + MW_sd),
        fill = "#b3cde0", alpha = 0.4
      ) +
      # Linien für MW-Verläufe
      geom_line(
        data = lines_df,
        aes(x = Datum, y = Wert, color = Typ),
        linewidth = 1.2
      ) +
      geom_point(
        data = plotdata,
        aes(x = TM_Stand, y = MW_mean),
        color = "#005c99", size = 1.5, alpha = 0.7
      ) +
      geom_vline(xintercept = as.Date("2025-08-22"), linetype = "dotted",
                 color = "darkred", size = 1.5) +
      annotate(
        "text",
        x = as.Date("2025-08-22"),
        y = 1.02,  # ggf. anpassen je nach Plot-Skalierung
        label = "Saisonstart",
        color = "darkred",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      coord_cartesian(ylim = c(0.5, 1.6)) +
      labs(
        title = "Marktwertentwicklung (relativ zum Startwert)",
        subtitle = "Schattierung = ±1 SD (TM Spieler)",
        x = "Datum",
        y = "Relativer Marktwert",
        color = "Linientyp"
      ) +
      scale_color_manual(values = c(
        "Durchschnitt TM-Spieler" = "#005c99",
        "Gesamtmarktwert" = "darkgrey"
      )) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") +
      geom_line(
        data = sommerpause_df,
        aes(x = Datum, y = MW_rel_normiert),
        color = "red",
        linewidth = 1.3,
        linetype = "dashed") + 
          geom_line(
        data = sommerpause_21_df,
        aes(x = Datum, y = MW_rel_normiert),
        color = "orange",
        linewidth = 1.3,
        linetype = "dashed")
    
    p
  })
  
  ## ---- Hist. Marktwert-Entwicklung ab 01.06.2024 (je Klasse) ----
  data <- reactive({
    df <- read.csv("marktwertverlauf_gesamt.csv", sep = ";", encoding = "UTF-8")
    
    # Datum korrekt parsen (YYYY-MM-DD → %Y-%m-%d)
    df$Datum <- as.Date(df$Datum, format = "%Y-%m-%d")
    
    # Nur Daten ab 01.06.2024
    df <- df %>% filter(Datum >= as.Date("2024-06-01"))
    
    # Marktwert bereinigen (Punkte entfernen – bei dir aber unnötig)
    df$Marktwert <- as.numeric(df$Marktwert)
    
    # Mittelwert je Spieler
    mw_spieler <- df %>%
      group_by(Spieler) %>%
      summarise(MW_mittel = mean(Marktwert, na.rm = TRUE), .groups = "drop")
    
    # Klassifikation
    df <- df %>%
      left_join(mw_spieler, by = "Spieler") %>%
      mutate(
        Klasse = case_when(
          MW_mittel < 500000 ~ "Klasse 1: <0.5 Mio",
          MW_mittel < 1000000 ~ "Klasse 2: 0.5–1 Mio",
          MW_mittel < 2500000 ~ "Klasse 3: 1–2.5 Mio",
          MW_mittel < 5000000 ~ "Klasse 4: 2.5–5 Mio",
          MW_mittel < 10000000 ~ "Klasse 5: 5–10 Mio",
          TRUE ~ "Klasse 6: >10 Mio"
        )
      )
    
    df
  })
  
  
  output$mw_plot <- renderPlot({
    df <- data()
    
    # Ø MW je Klasse & Datum
    df_plot <- df %>%
      group_by(Datum, Klasse) %>%
      summarise(MW_Ø = mean(Marktwert, na.rm = TRUE), .groups = "drop")
    
    # Normierung auf 01.06.2024
    startwerte <- df_plot %>%
      group_by(Klasse) %>%
      filter(Datum == min(Datum)) %>%
      summarise(Start_MW = first(MW_Ø), .groups = "drop")
    
    df_plot_norm <- df_plot %>%
      left_join(startwerte, by = "Klasse") %>%
      mutate(MW_normiert = MW_Ø / Start_MW) %>%
      filter(Datum <= as.Date("2024-08-15"))
    
    ggplot(df_plot_norm, aes(x = Datum, y = MW_normiert, color = Klasse)) +
      geom_line(size = 1.2) +
      labs(
        title = "Historischer norm. Marktwertverlauf pro Klasse ab 01.06.2024 (Start = 1)",
        y = "Normierter MW",
        x = "Datum",
        color = "MW-Klasse"
      )  + 
      geom_vline(xintercept = as.numeric(Sys.Date() - 365), color = "darkred", linetype = "dashed", linewidth = 1) +
      annotate(
        "text",
        x = Sys.Date() - 365,
        y = 0.7,  # ggf. anpassen
        label = "Heute vor 1 Jahr",
        color = "darkred",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      scale_color_brewer(palette = "Paired") +
      coord_cartesian(ylim = c(0.6, 1.2)) +
      theme_minimal(base_size = 14)
  })
  
  ## ---- Je Klasse Marktwert-Entwicklung ab 15.06.2025 ----
  
  data_now <- reactive({
    df <- read.csv("ALL_PLAYERS.csv", sep = ";", encoding = "UTF-8")
    df$Datum <- as.Date(df$Datum, format = "%d.%m.%Y")
    
    start_date <- as.Date("2025-06-16")
    end_date <- max(df$Datum, na.rm = TRUE)
    
    # Filtern auf Zeitraum ab 16.06.2025 bis max Datum
    df <- df %>% filter(Datum >= start_date & Datum <= end_date)
    
    df$Marktwert <- as.numeric(df$Marktwert)
    
    # Mittelwert pro Spieler (über den gefilterten Zeitraum)
    mw_spieler <- df %>%
      group_by(Spieler) %>%
      summarise(MW_mittel = mean(Marktwert, na.rm = TRUE), .groups = "drop")
    
    # Spieler in Klassen einteilen
    df <- df %>%
      left_join(mw_spieler, by = "Spieler") %>%
      mutate(
        Klasse = case_when(
          MW_mittel < 500000 ~ "Klasse 1: <0.5 Mio",
          MW_mittel < 1000000 ~ "Klasse 2: 0.5–1 Mio",
          MW_mittel < 2500000 ~ "Klasse 3: 1–2.5 Mio",
          MW_mittel < 5000000 ~ "Klasse 4: 2.5–5 Mio",
          MW_mittel < 10000000 ~ "Klasse 5: 5–10 Mio",
          TRUE ~ "Klasse 6: >10 Mio"
        ),
        Klasse = factor(Klasse, levels = c(
          "Klasse 1: <0.5 Mio",
          "Klasse 2: 0.5–1 Mio",
          "Klasse 3: 1–2.5 Mio",
          "Klasse 4: 2.5–5 Mio",
          "Klasse 5: 5–10 Mio",
          "Klasse 6: >10 Mio"
        ))
      )
    
    # Mittelwerte pro Klasse & Datum
    df_plot <- df %>%
      group_by(Datum, Klasse) %>%
      summarise(MW_Ø = mean(Marktwert, na.rm = TRUE), .groups = "drop")
    
    # Startwerte für Normierung (Datum = 16.06.2025)
    startwerte <- df_plot %>%
      filter(Datum == start_date) %>%
      select(Klasse, Start_MW = MW_Ø)
    
    # Normieren
    df_plot_norm <- df_plot %>%
      left_join(startwerte, by = "Klasse") %>%
      mutate(MW_normiert = MW_Ø / Start_MW)
    
    df_plot_norm
  })
  
  output$mw_plot_now <- renderPlot({
    df_plot_norm <- data_now()
    
    ggplot(df_plot_norm, aes(x = Datum, y = MW_normiert, color = Klasse)) +
      geom_line(size = 1.2) +
      labs(
        title = "Normierter Marktwertverlauf je Klasse (16.06. bis 15.08.2025)",
        y = "Normierter MW",
        x = "Datum",
        color = "MW-Klasse"
      ) +
      scale_color_brewer(palette = "Paired") +
      coord_cartesian(ylim = c(0.6, 1.2), xlim = as.Date(c("2025-06-16", "2025-08-15"))) +
      theme_minimal(base_size = 14)
  })
  
  
  ## ---- Historische Martkwertverläufe ----
  
  data_path <- "./global_MW"
  
  seasons <- c("2004-05", "2005-06", "2006-07", "2007-08", "2008-09",
               "2009-10", "2010-11", "2011-12", "2012-13", "2013-14",
               "2014-15", "2015-16", "2016-17", "2017-18", "2018-19",
               "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25")
  
  load_season_data <- function(season) {
    file_name <- paste0(data_path, "/historic_market_values_", season, ".csv")
    if (!file.exists(file_name)) return(NULL)
    read_csv(file_name, show_col_types = FALSE) %>%
      mutate(Saison = season)
  }
  
  # Alle Saison-Daten laden
  all_season_data <- reactive({
    dfs <- lapply(seasons, load_season_data)
    dfs <- dfs[!sapply(dfs, is.null)]
    bind_rows(dfs)
  })
  
  # Checkbox choices mit allen Saisons setzen (für Vergleichsauswahl)
  observe({
    # Saisons ohne die ersten 3 und den 5. letzten
    selected <- seasons[-c(1:3, length(seasons) - 4)]  # length(seasons)-4 ist der 5. letzte Index
    
    updateCheckboxGroupInput(session, "selected_seasons",
                             choices = seasons,
                             selected = selected)
  })
  
  
  # Historische Saisonverläufe - Alle anzeigen
  output$historical_seasons_plot_all <- renderPlot({
    df <- all_season_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = as.Date(Datum), y = Marktwert, color = Saison)) +
      geom_line() +
      labs(
        title = "Historische Marktwertverläufe aller Saisons",
        x = "Datum",
        y = "Marktwert"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Historische Saisonverläufe - Vergleichsauswahl (normalisierte Zeitachse)
  normalized_data <- reactive({
    req(input$selected_seasons)
    df <- all_season_data()
    df <- df %>% filter(Saison %in% input$selected_seasons)
    
    df <- df %>%
      mutate(Datum = as.Date(Datum)) %>%
      group_by(Saison) %>%
      mutate(
        saison_start = as.Date(paste0(substr(Saison, 1, 4), "-07-01")),
        days_since_start = as.integer(Datum - saison_start)
      ) %>%
      ungroup() %>%
      filter(days_since_start >= 0)
    
    df
  })
  
  ## ---- historical_seasons_plot_selected ----
  
  output$historical_seasons_plot_selected <- renderPlot({
    df <- normalized_data()
    req(nrow(df) > 0)
    req(input$selected_seasons)
    
    today <- Sys.Date()
    current_year <- format(today, "%Y")
    saison_start_this_year <- as.Date(paste0(current_year, "-07-01"))
    days_since_start_today <- as.numeric(today - saison_start_this_year)
    
    # Filter auf ausgewählte Saisons
    df_filtered <- df %>% filter(Saison %in% input$selected_seasons)
    
    # Mittelwert und SD pro Tag über alle ausgewählten Saisons
    summary_df <- df_filtered %>%
      group_by(days_since_start) %>%
      summarise(
        mean_MW = mean(Marktwert, na.rm = TRUE),
        sd_MW = sd(Marktwert, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Gesamtmarktwert-Daten vorbereiten (Summe, nicht normiert)
    gesamt_mw_normiert <- gesamt_mw_roh %>%
      group_by(Datum) %>%
      summarise(
        MW_rel_normiert = sum(Marktwert, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      mutate(
        days_since_start = as.numeric(Datum - saison_start_this_year)
      ) %>%
      filter(days_since_start >= 0)  # Nur ab Saisonstart
    
    # Gleitendes Fenster für Prognose: 3 Tage davor bis 3 Tage danach (Saison-Daten)
    window_start <- days_since_start_today - 3
    window_end <- days_since_start_today + 3
    
    window_data <- df_filtered %>%
      filter(days_since_start >= window_start & days_since_start <= window_end)
    
    diffs <- window_data %>%
      group_by(Saison) %>%
      arrange(days_since_start) %>%
      mutate(
        diff = Marktwert - lag(Marktwert),
        lag_mw = lag(Marktwert)
      ) %>%
      summarise(
        avg_diff = mean(diff, na.rm = TRUE),
        avg_mw = mean(lag_mw, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(avg_pct_diff = 100 * avg_diff / avg_mw)
    
    avg_pct_diff_all <- mean(diffs$avg_pct_diff, na.rm = TRUE)
    
    # Prognose für Saison-Daten
    prog_text <- if (is.na(avg_pct_diff_all)) {
      "Prognose: Nicht genügend Daten (Saison-Daten)"
    } else if (avg_pct_diff_all > 0) {
      paste0("Prognose: MW stieg historisch im Schnitt um ", round(avg_pct_diff_all, 2), " % pro Tag")
    } else if (avg_pct_diff_all < 0) {
      paste0("Prognose: MW fiel historisch im Schnitt um ", abs(round(avg_pct_diff_all, 2)), " % pro Tag")
    } else {
      "Prognose: MW ist aktuell stabil (Saison-Daten)"
    }
    
    arrow_label <- ifelse(is.na(avg_pct_diff_all), "",
                          ifelse(avg_pct_diff_all > 0, "▲",
                                 ifelse(avg_pct_diff_all < 0, "▼", "")))
    
    arrow_color <- ifelse(avg_pct_diff_all > 0, "darkgreen",
                          ifelse(avg_pct_diff_all < 0, "red", "black"))
    
    # Prognose für aktuellen Gesamtmarktwert (letzte 3 Tage)
    gesamt_window <- gesamt_mw_normiert %>%
      filter(days_since_start >= (days_since_start_today - 3) & days_since_start < days_since_start_today) %>%
      arrange(days_since_start) %>%
      mutate(diff = MW_rel_normiert - lag(MW_rel_normiert),
             lag_mw = lag(MW_rel_normiert))
    
    gesamt_diff_summary <- gesamt_window %>%
      summarise(
        avg_diff = mean(diff, na.rm = TRUE),
        avg_mw = mean(lag_mw, na.rm = TRUE)
      ) %>%
      mutate(avg_pct_diff = 100 * avg_diff / avg_mw)
    
    gesamt_avg_pct_diff <- gesamt_diff_summary$avg_pct_diff
    
    gesamt_prog_text <- if (is.na(gesamt_avg_pct_diff)) {
      "Prognose: Nicht genügend Daten (Gesamtmarktwert)"
    } else if (gesamt_avg_pct_diff > 0) {
      paste0("Prognose Aktueller MW: Steigend um ", round(gesamt_avg_pct_diff, 2), " % pro Tag")
    } else if (gesamt_avg_pct_diff < 0) {
      paste0("Prognose Aktueller MW: Fallend um ", abs(round(gesamt_avg_pct_diff, 2)), " % pro Tag")
    } else {
      "Prognose Gesamtmarktwert: Stabil"
    }
    
    gesamt_arrow_label <- ifelse(is.na(gesamt_avg_pct_diff), "",
                                 ifelse(gesamt_avg_pct_diff > 0, "▲",
                                        ifelse(gesamt_avg_pct_diff < 0, "▼", "")))
    
    gesamt_arrow_color <- ifelse(gesamt_avg_pct_diff > 0, "darkgreen",
                                 ifelse(gesamt_avg_pct_diff < 0, "red", "black"))
    
    # Grafik bauen
    ggplot(df_filtered, aes(x = days_since_start, y = Marktwert, color = Saison)) +
      # Band für Mittelwert ± SD
      geom_ribbon(data = summary_df,
                  aes(x = days_since_start, ymin = mean_MW - sd_MW, ymax = mean_MW + sd_MW),
                  fill = "grey70", alpha = 0.3, inherit.aes = FALSE) +
      # Mittelwert-Linie
      geom_line(data = summary_df, aes(x = days_since_start, y = mean_MW),
                color = "black", size = 1.5, inherit.aes = FALSE, linetype = "dashed") +
      # Saisonlinien
      geom_line(size = 1) +
      # Gesamtmarktwert-Linie (Overlay in dunkelgrau)
      geom_line(data = gesamt_mw_normiert,
                aes(x = days_since_start, y = MW_rel_normiert),
                color = "grey20", size = 1.5, inherit.aes = FALSE) +
      # Heutiger Tag
      geom_vline(xintercept = days_since_start_today,
                 color = "darkred", linetype = "dashed", linewidth = 1) +
      # Saisonstart
      geom_vline(xintercept = as.numeric(as.Date(paste0(current_year, "-08-22")) - saison_start_this_year),
                 linetype = "dotted", color = "darkred", size = 1.5) +
      annotate(
        "text",
        x = as.numeric(as.Date(paste0(current_year, "-08-22")) - saison_start_this_year),
        y = max(df_filtered$Marktwert, na.rm = TRUE) * 1.02,
        label = "Saisonstart",
        color = "darkred",
        angle = 90,
        vjust = -0.5,
        hjust = 1.5,
        fontface = "bold",
        size = 4
      ) +
      annotate(
        "text",
        x = days_since_start_today,
        y = max(df_filtered$Marktwert, na.rm = TRUE) * 0.9,
        label = "Heute",
        color = "darkred",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      annotate("text", x = Inf, y = Inf, label = prog_text,
               hjust = 1.1, vjust = 2, size = 5, color = "black", fontface = "bold") +
      annotate("text", x = Inf, y = Inf, label = arrow_label,
               hjust = 1.05, vjust = 1.25, size = 8, color = arrow_color, fontface = "bold") +
      
      annotate("text", x = Inf, y = Inf, label = gesamt_prog_text,
               hjust = 1.1, vjust = 3.5, size = 5, color = "black", fontface = "bold") +
      annotate("text", x = Inf, y = Inf, label = gesamt_arrow_label,
               hjust = 1.05, vjust = 2.5, size = 8, color = gesamt_arrow_color, fontface = "bold") +
    
      labs(
        title = "Historische Marktwertverläufe mit Mittelwert ± SD",
        x = "Tage seit 1. Juli",
        y = "Marktwert"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  
  
  # ---- TRANSFERMARKT ----
  
  ## ---- Tabelle ----
  
  output$transfermarkt_detail <- DT::renderDT({
    
    # Transfermarkt-Daten
    tm_df$Spieler <- trimws(enc2utf8(tm_df$Spieler))
    tm_df$Marktwert_num <- as.numeric(gsub("\\.", "", tm_df$Marktwert))
    tm_df$Mindestgebot_num <- as.numeric(gsub("\\.", "", tm_df$Mindestgebot))
    tm_df$Restzeit <- trimws(tm_df$Restzeit)
    tm_df$Verein <- trimws(enc2utf8(tm_df$Verein))
    
    # Die letzten 3 Tage ermitteln
    last_dates <- sort(unique(ap_df$Datum), decreasing = TRUE)[1:3]
    names(last_dates) <- c("MW3", "MW2", "MW1") # MW1 = ältester Tag, MW3 = aktuell
    
    # Marktwerte für die letzten 3 Tage je Spieler
    mw1 <- ap_df %>% filter(Datum == last_dates["MW1"]) %>% select(Spieler, MW1 = Marktwert)
    mw2 <- ap_df %>% filter(Datum == last_dates["MW2"]) %>% select(Spieler, MW2 = Marktwert)
    mw3 <- ap_df %>% filter(Datum == last_dates["MW3"]) %>% select(Spieler, MW3 = Marktwert)
    
    # Merge alle MWs auf Transfermarkt
    tm_trend <- tm_df %>%
      left_join(mw1, by = "Spieler") %>%
      left_join(mw2, by = "Spieler") %>%
      left_join(mw3, by = "Spieler")
    
    # Trendlogik
    tm_trend$Trend <- mapply(function(mw1, mw2, mw3) {
      if (any(is.na(c(mw1, mw2, mw3)))) return("–")
      if (mw1 < mw2 && mw2 < mw3) {
        return("<span style='color:green;font-weight:bold;'>▲▲</span>")
      } else if (mw1 > mw2 && mw2 > mw3) {
        return("<span style='color:red;font-weight:bold;'>▼▼</span>")
      } else if (mw1 < mw3) {
        return("<span style='color:green;font-weight:bold;'>▲</span>")
      } else if (mw1 > mw3) {
        return("<span style='color:red;font-weight:bold;'>▼</span>")
      } else {
        return("–")
      }
    }, tm_trend$MW1, tm_trend$MW2, tm_trend$MW3)
    
    # Rest wie gehabt…
    tm_trend$Restkategorie <- dplyr::case_when(
      grepl("^2d", tm_trend$Restzeit) ~ "übermorgen",
      grepl("^1d", tm_trend$Restzeit) ~ "morgen",
      grepl("^[0-9]+h", tm_trend$Restzeit) ~ "heute",
      TRUE ~ "unbekannt"
    )
    tm_trend$Restkategorie <- factor(tm_trend$Restkategorie,
                                     levels = c("heute", "morgen", "übermorgen", "unbekannt"),
                                     ordered = TRUE)
    tm_trend$Marktwert <- paste0(format(tm_trend$Marktwert_num, big.mark = ".", decimal.mark = ","), " €")
    tm_trend$Mindestgebot <- paste0(format(tm_trend$Mindestgebot_num, big.mark = ".", decimal.mark = ","), " €")
    tm_trend$MinGeb_Unter_MW <- tm_trend$Mindestgebot_num < tm_trend$Marktwert_num
    
    tm_trend <- tm_trend %>%
      arrange(Restkategorie, desc(Marktwert_num)) %>%
      select(
        Spieler, Verein, Marktwert, Mindestgebot, Besitzer,
        Restkategorie, Trend, MinGeb_Unter_MW, Marktwert_num
      ) %>%
      rename(
        "Verbleibende Zeit" = Restkategorie,
        "Trend MW (3 Tage)" = Trend
      )
    
    tm_trend$MW_Klasse <- vapply(tm_trend$Marktwert_num, get_mw_klasse, character(1))
    
    # Mittelwert-Prozente aus dem Profil
    gp_df <- gebotsprofil_mwclass() %>%
      filter(!is.na(Diff_Prozent)) %>%
      mutate(
        MW_Klasse = factor(MW_Klasse, levels = c(
          "<0.5 Mio", "0.5–1 Mio", "1–2.5 Mio", "2.5–5 Mio", "5–10 Mio", ">10 Mio"
        ))
      )
    
    # Filter unusual high bids
    gp_df <- gp_df %>% 
      filter(Typ == "Hoechstgebot") %>% 
      filter(Diff_Prozent > 0, Diff_Prozent <= 25)
    
    means_klasse <- gp_df %>%
      group_by(MW_Klasse) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    tm_trend <- tm_trend %>%
      left_join(means_klasse, by = "MW_Klasse")
    
    # Ideales Gebot berechnen – erst jetzt existiert 'Mean'
    tm_trend$IdealesGebot_num <- round(tm_trend$Marktwert_num * (1 + tm_trend$Mean / 100))
    # Prozentwert berechnen
    tm_trend$IdealesGebotProzent <- round(100 * (tm_trend$IdealesGebot_num / tm_trend$Marktwert_num - 1), 1)
    
    # Zusammenbauen als HTML (mit kleinem Prozentwert drunter)
    tm_trend$IdealesGebot <- paste0(
      format(tm_trend$IdealesGebot_num, big.mark = ".", decimal.mark = ","), " €",
      '<br><span style="font-size: 85%; color: #666;">(',
      ifelse(is.na(tm_trend$IdealesGebotProzent), "–", 
             ifelse(tm_trend$IdealesGebotProzent > 0, "+", "")),
      ifelse(is.na(tm_trend$IdealesGebotProzent), "", tm_trend$IdealesGebotProzent),
      "%)</span>"
    )
    
    # Fallback: Wenn kein Mean gefunden wurde, einfach MW nehmen
    tm_trend$IdealesGebot_num[is.na(tm_trend$IdealesGebot_num)] <- tm_trend$Marktwert_num[is.na(tm_trend$IdealesGebot_num)]
    tm_trend$IdealesGebot[is.na(tm_trend$IdealesGebot)] <- tm_trend$Marktwert[is.na(tm_trend$IdealesGebot)]
    
    # Setze den richtigen Pfad (www/logos), falls im Shiny www-Ordner!
    logo_dir <- "logos" # oder ggf. "www/logos", je nach Shiny-Ordnerstruktur
    
    # EINMAL Logo erzeugen reicht!
    tm_trend$Logo <- paste0(
      '<img src="', logo_dir, '/', logo_map[tm_trend$Verein],
      '" width="28" title="', tm_trend$Verein, '"/>'
    )
    tm_trend$Logo[is.na(logo_map[tm_trend$Verein])] <- ""
    
    #90% gebot
    # 1. Transfers laden
    transfers <- transfers %>% 
      mutate(
        Hoechstgebot = as.numeric(Hoechstgebot)
      ) %>%
      filter(!is.na(Hoechstgebot))
    
    # 2. Vortags-Marktwert ermitteln
    # Angenommen, du willst das Vortagsdatum als das max(Datum)-1 nehmen:
    max_datum <- max(transfers$Datum, na.rm = TRUE)
    vortag <- max_datum - 1
    
    vortags_mw <- transfermarkt %>%
      filter(TM_Stand == vortag) %>%
      select(Spieler, Marktwert_num = Marktwert)
    
    # 3. Transfers mit Vortagsmarktwert joinen, aber unrealisitsch hohe gebote >150% rausfiltern
    transfers_mw <- transfers %>%
      left_join(vortags_mw, by = "Spieler") %>%
      filter(!is.na(Marktwert_num)) %>%  # nur Spieler mit Vortags-MW
      mutate(MW_Klasse = vapply(Marktwert_num, get_mw_klasse, character(1))) %>%  
      mutate(Diff_per_cent =  Hoechstgebot / Marktwert_num) %>% 
      filter(Diff_per_cent>1, Diff_per_cent<1.33)
    
    # 4. 90%-Perzentil Faktor je MW_Klasse berechnen
    maxgebote_klasse <- transfers_mw  %>% 
      group_by(MW_Klasse) %>%
      summarise(Maximalgebot_90_Faktor = quantile(Diff_per_cent, probs = 0.90, na.rm = TRUE))
    
    # 5. In tm_trend joinen
    tm_trend <- tm_trend %>%
      left_join(maxgebote_klasse, by = "MW_Klasse") %>%
      mutate(
        Maximalgebot_num_rounded = ifelse(
          is.na(Maximalgebot_90_Faktor),
          NA,
          round(Marktwert_num * Maximalgebot_90_Faktor)
        ),
        MaximalgebotProzent = round(100 * (Maximalgebot_num_rounded / Marktwert_num - 1), 1)
      )
    
    # 6. HTML bauen
    tm_trend$Maximalgebot <- paste0(
      format(tm_trend$Maximalgebot_num_rounded, big.mark = ".", decimal.mark = ","), " €",
      '<br><span style="font-size: 85%; color: #666;">(',
      ifelse(is.na(tm_trend$MaximalgebotProzent), "–",
             ifelse(tm_trend$MaximalgebotProzent > 0, "+", "")),
      ifelse(is.na(tm_trend$MaximalgebotProzent), "", tm_trend$MaximalgebotProzent),
      "%)</span>"
    )
    
    # 1. Beispiel: Bieterprofil-Daten vorbereiten (nur Durchschnitt pro MW_Klasse)
    # Angenommen, bprofile_df enthält deine Tabelle mit Spalten MW_Klasse und Durchschnitt_Abweichung
    # Hier als Beispiel:
    bprofile_df <- tribble(
      ~MW_Klasse, ~Durchschnitt_Abweichung,
      "<0.5 Mio", 0.18,
      "0.5–1 Mio", 1.18,
      "1–2.5 Mio", 1.18,
      "2.5–5 Mio", 1.18,
      "5–10 Mio", 1.18,
      ">10 Mio", 0.18
    )
    
    # 3. Beispiel: tm_trend mit Marktwert
    # tm_trend <- ... (deine bestehende Tabelle mit Marktwert_num)
    
    # 4. MW-Klasse ergänzen
    tm_trend <- tm_trend %>%
      mutate(
        MW_Klasse = vapply(Marktwert_num, get_mw_klasse, character(1))
      )
    
    # 5. Durchschnittliche Abweichung pro Klasse mergen
    tm_trend <- tm_trend %>%
      left_join(bprofile_df, by = "MW_Klasse")
    
    # 6. Mindestgebot berechnen (Marktwert * (1 + Durchschnitt_Abweichung / 100))
    tm_trend <- tm_trend %>%
      mutate(
        Mindestgebot_empfohlen_num = round(Marktwert_num * (1 + Durchschnitt_Abweichung / 100)),
        Minimalgebot = paste0(format(Mindestgebot_empfohlen_num, big.mark = ".", decimal.mark = ","), " €")
      )
    
    # 7. Fallback: Wenn Durchschnitt_Abweichung fehlt, Mindestgebot = Marktwert
    tm_trend <- tm_trend %>%
      mutate(
        Minimalgebot_num = as.numeric(gsub("\\.", "", gsub(" €", "", Minimalgebot))),
        Mindestgebot_num = as.numeric(gsub("\\.", "", gsub(" €", "", Mindestgebot)))
      ) %>%
      mutate(
        Minimalgebot_num = ifelse(Minimalgebot_num < Mindestgebot_num, Mindestgebot_num, Minimalgebot_num),
        Minimalgebot = paste0(format(Minimalgebot_num, big.mark = ".", decimal.mark = ","), " €")
      )
    
    # 8. HTML bauen
    tm_trend$MinimalgebotProzent <- round(100 * (tm_trend$Minimalgebot_num / tm_trend$Marktwert_num - 1), 1)
    
    tm_trend$Minimalgebot <- paste0(
      format(tm_trend$Minimalgebot_num, big.mark = ".", decimal.mark = ","), " €",
      '<br><span style="font-size: 85%; color: #666;">(',
      ifelse(is.na(tm_trend$MinimalgebotProzent), "–",
             ifelse(tm_trend$MinimalgebotProzent > 0, "+", "")),
      ifelse(is.na(tm_trend$MinimalgebotProzent), "", tm_trend$MinimalgebotProzent),
      "%)</span>"
    )
    
    
    
    #Hist. Punkte, Kaufempfehlung etc. mergen
    tm_trend <- tm_trend %>%
      left_join(ca_df %>%
                  select(
                    "Spieler" = "SPIELER",
                    "Historische Punkteausbeute" = `Historische_Punkteausbeute`
                  ), by = c("Spieler"))
    
    tm_trend <- tm_trend %>%
     left_join(ca2_df, by = c("Spieler" = "SPIELER"))
    
    DT::datatable(
      tm_trend[, c("Spieler", "Logo", "Punkte pro Spiel", "Preis-Leistung", "Historische Punkteausbeute", "Marktwert", "Zielwert", "Mindestgebot", "Minimalgebot", "IdealesGebot", "Maximalgebot", "Gebote", "Empfehlung", "Besitzer", "Verbleibende Zeit", "Trend MW (3 Tage)")],
      colnames = c("Spieler", "Verein", "PPS", "Preis-Leistung", "Hist.", "Marktwert", "Zielwert", "Mindestgebot", "Minimalgebot", "Zuschlagsgebot", "Maximalgebot", "Gebote", "Empfehlung", "Besitzer", "Verbleibende Zeit", "Trend MW (3 Tage)"),
      rownames = FALSE,
      escape = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE,
        pageLength = 20,
        columnDefs = list(
          list(className = 'dt-left', targets = 0:1),
          list(className = 'dt-right', targets = 2:7)
        )
      )
    ) %>%
      DT::formatStyle(
        'Verbleibende Zeit',
        target = 'cell',
        color = DT::styleEqual("heute", "red"),
        fontWeight = DT::styleEqual("heute", "bold")
      ) %>%
      DT::formatStyle(
        'Mindestgebot',
        target = 'cell',
        color = DT::styleEqual(
          tm_trend$Mindestgebot[tm_trend$MinGeb_Unter_MW], "green"
        ),
        fontWeight = DT::styleEqual(
          tm_trend$Mindestgebot[tm_trend$MinGeb_Unter_MW], "bold"
        )
      )
  })
  
  
  # ---- KADER-ENTWICKLUNG ----
  ## ---- Mein Kader ----
  
  # Mapping von Vereinsnamen zu Dateinamen
  logo_map <- c(
    "1. FC Köln" = "1 FC Köln.png",
    "Bayer 04 Leverkusen" = "Bayer Leverkusen.png",
    "Borussia Dortmund" = "Borussia Dortmund.png",
    "Borussia Mönchengladbach" = "Borussia Mönchengladbach.png",
    "Eintracht Frankfurt" = "Eintracht Frankfurt.png",
    "FC Augsburg" = "FC Augsburg.png",
    "FC Bayern München" = "FC Bayern München.png",
    "Hamburger SV" = "Hamburger SV.png",
    "1.FC Heidenheim" = "Heidenheim.png",
    "RB Leipzig" = "Leipzig.png",
    "1. FSV Mainz 05" = "Mainz 05.png",
    "Sport-Club Freiburg" = "SC Freiburg.png",
    "FC St. Pauli" = "St Pauli.png",
    "SV Werder Bremen" = "SV Werder Bremen.png",
    "TSG Hoffenheim" = "TSG Hoffenheim.png",
    "1.FC Union Berlin" = "Union Berlin.png",
    "VfB Stuttgart" = "VfB Stuttgart.png",
    "VfL Wolfsburg" = "VfL Wolfsburg.png"
  )
  
  #PPS usw. mergen
  ca_df <- ca_df %>%
    select(
      SPIELER,
      Punkte_pro_Spiel = `PUNKTE PRO SPIEL`,
      Preis_Leistung = `PREIS-LEISTUNG`,
      Historische_Punkteausbeute = `HISTORISCHE PUNKTEAUSBEUTE`
    )
  
  output$mein_kader <- renderUI({
    
    df0 <- teams_df %>% filter(Manager == "Dominik")
    df0$Position <- factor(df0$Position, levels = c("Tor", "Abwehr", "Mittelfeld", "Sturm"), ordered = TRUE)
    df0 <- df0 %>% arrange(Position, Spieler)
    
    mw_aktuell <- gesamt_mw_roh %>%
      group_by(Spieler) %>%
      filter(Datum == max(Datum, na.rm=TRUE)) %>%
      summarise(Marktwert_aktuell = first(Marktwert), .groups = "drop")
    
    kaufpreise <- transfers %>%
      filter(Hoechstbietender == "Dominik") %>%
      group_by(Spieler) %>%
      arrange(desc(Datum)) %>%
      slice(1) %>%
      select(Spieler, Kaufpreis = Hoechstgebot)
    
    df_pre <- df0 %>%
      left_join(mw_aktuell, by="Spieler") %>%
      left_join(kaufpreise, by="Spieler") %>%
      mutate(
        Diff_Kauf = ifelse(is.na(Kaufpreis), NA, Marktwert_aktuell - Kaufpreis),
        Diff_Kauf_fmt = ifelse(
          is.na(Kaufpreis), "",
          case_when(
            Diff_Kauf > 0 ~ sprintf("<span style='color:#388e3c;'>+%s € seit Kauf</span>", format(Diff_Kauf, big.mark = ".", decimal.mark = ",")),
            Diff_Kauf < 0 ~ sprintf("<span style='color:#e53935;'>–%s € seit Kauf</span>", format(abs(Diff_Kauf), big.mark = ".", decimal.mark = ",")),
            TRUE ~ "<span style='color:grey;'>±0 € seit Kauf</span>"
          )
        )
      ) %>%
      left_join(
        gesamt_mw_roh %>%
          group_by(Spieler) %>%
          arrange(desc(Datum)) %>%
          slice(1:2) %>%
          mutate(Diff = Marktwert - lead(Marktwert)) %>%
          slice(1) %>%
          ungroup() %>%
          mutate(
            Marktwert_fmt = ifelse(is.na(Marktwert), "-", paste0(format(Marktwert, big.mark = ".", decimal.mark = ","), " €")),
            Diff_fmt = case_when(
              is.na(Diff) ~ "",
              Diff > 0 ~ sprintf("<span style='color:#81c784;'>▲ %s €</span>", format(Diff, big.mark = ".", decimal.mark = ",")),
              Diff < 0 ~ sprintf("<span style='color:#e57373;'>▼ %s €</span>", format(abs(Diff), big.mark = ".", decimal.mark = ",")),
              TRUE ~ "<span style='color:grey;'>–</span>"
            )
          ) %>%
          select(Spieler, Verein, Marktwert_fmt, Diff_fmt),
        by = "Spieler"
      )
    
    #PPS mergen
    df_pre <- df_pre %>%
      left_join(ca_df, by = c("Spieler" = "SPIELER"))
    
    df_pre <- df_pre %>%
      rename(
        "Punkte pro Spiel" = Punkte_pro_Spiel,
        "Preis-Leistung" = Preis_Leistung,
        "Historische Punkteausbeute" = Historische_Punkteausbeute
      )
    
    # Setze den richtigen Pfad (www/logos), falls im Shiny www-Ordner!
    logo_dir <- "logos" # oder ggf. "www/logos", je nach Shiny-Ordnerstruktur
    
    # Logo erzeugen
    df_pre$Logo <- ifelse(
      is.na(logo_map[df_pre$Verein]) | logo_map[df_pre$Verein] == "",
      "",
      paste0('<img src="', logo_dir, '/', logo_map[df_pre$Verein], '" width="28" title="', df_pre$Verein, '"/>')
    )
    
    
    grouped_sections <- lapply(split(df_pre, df_pre$Position), function(gruppe) {
      pos_name <- unique(gruppe$Position)
      rows <- lapply(seq_len(nrow(gruppe)), function(i) {
        sp <- gruppe$Spieler[i]
        v <- gruppe$Logo[i]
        pps <- gruppe$`Punkte pro Spiel`[i]
        pl <- gruppe$`Preis-Leistung`[i]
        hist <- gruppe$`Historische Punkteausbeute`[i]
        mw <- gruppe$Marktwert_fmt[i]
        diff <- gruppe$Diff_fmt[i]
        diffk <- gruppe$Diff_Kauf_fmt[i]
        
        
        sprintf(
          "<tr>
      <td style='padding:4px;'>%s</td>
      <td style='padding:4px; text-align:center;'>%s</td>
      <td style='padding:4px; text-align:center;'>%s</td>
      <td style='padding:4px; text-align:center;'>%s</td>
      <td style='padding:4px; text-align:right;'>%s</td>
      <td style='padding:4px; text-align:right;'>%s</td>
      <td style='padding:4px; text-align:right;'>%s</td>
      <td style='padding:4px; text-align:right;'>%s</td>
    </tr>",
          sp,
          v,
          ifelse(is.na(pps), "-", format(round(pps, 2), decimal.mark = ",")),
          ifelse(is.na(pl), "-", pl),
          ifelse(is.na(hist), "-", hist),
          mw, diff, diffk
        )
      })

      
      paste0(
        sprintf("<tr><th colspan='8' style='text-align:left; background:#eee; padding:4px;'>%s</th></tr>", pos_name),
        paste(rows, collapse = "\n")
      )
    })
    
    
    table_html <- paste0(
      "<table style='width:100%; border-collapse:collapse;'>",
      "<thead><tr>
  <th style='text-align:left;'>Spieler</th>
  <th style='text-align:center;'>Verein</th>
  <th style='text-align:center;'>Ø Punkte</th>
  <th style='text-align:center;'>Preis-Leistung</th>
  <th style='text-align:right;'>Historie</th>
  <th style='text-align:right;'>MW</th>
  <th style='text-align:right;'>Vortag-MW-Diff</th>
  <th style='text-align:right;'>Kauf-Diff</th>
</tr></thead>"
      ,
      paste(grouped_sections, collapse = "\n"),
      "</tbody></table>"
    )
    
    
    # Summen-Zeile für alle Spieler mit Kaufpreis
    summe <- df_pre %>%
      filter(!is.na(Kaufpreis)) %>%
      summarise(
        Gesamt = sum(Marktwert_aktuell - Kaufpreis, na.rm = TRUE)
      ) %>% pull(Gesamt)
    
    # Formatieren je nach Vorzeichen
    summe_fmt <- if (is.na(summe)) {
      ""
    } else if (summe > 0) {
      sprintf("<span style='color:#388e3c; font-weight:bold;'>Gesamtgewinn: +%s €</span>", format(summe, big.mark = ".", decimal.mark = ","))
    } else if (summe < 0) {
      sprintf("<span style='color:#e53935; font-weight:bold;'>Gesamtverlust: –%s €</span>", format(abs(summe), big.mark = ".", decimal.mark = ","))
    } else {
      "<span style='color:grey; font-weight:bold;'>±0 €</span>"
    }
    
    
    tagList(
      HTML(table_html),
      tags$div(HTML(summe_fmt), style = "margin-top:12px; font-size:1.1em;")
    )
    
  })
  
  ## ---- Alle Kader ----
  
  output$kader_uebersicht_ui <- renderUI({
    
    manager_list <- sort(setdiff(unique(teams_df$Manager), "Dominik"))
    
    # === MW/Transferdaten vorbereiten ===
    mw_aktuell <- gesamt_mw_roh %>%
      group_by(Spieler) %>%
      filter(Datum == max(Datum, na.rm = TRUE)) %>%
      summarise(Marktwert_aktuell = first(Marktwert), .groups = "drop")
    
    kaufpreise <- transfers %>%
      group_by(Spieler, Hoechstbietender) %>%
      arrange(desc(Datum)) %>%
      slice(1) %>%
      select(Spieler, Hoechstbietender, Kaufpreis = Hoechstgebot)
    
    # Vortag-MW-Diff (wie bei Mein Kader)
    mw_diff <- gesamt_mw_roh %>%
      group_by(Spieler) %>%
      arrange(desc(Datum)) %>%
      slice(1:2) %>%
      mutate(Diff = Marktwert - lead(Marktwert)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        Marktwert_fmt = ifelse(is.na(Marktwert), "-", paste0(format(Marktwert, big.mark = ".", decimal.mark = ","), " €")),
        Diff_fmt = case_when(
          is.na(Diff) ~ "",
          Diff > 0 ~ sprintf("<span style='color:#81c784;'>▲ %s €</span>", format(Diff, big.mark = ".", decimal.mark = ",")),
          Diff < 0 ~ sprintf("<span style='color:#e57373;'>▼ %s €</span>", format(abs(Diff), big.mark = ".", decimal.mark = ",")),
          TRUE ~ "<span style='color:grey;'>–</span>"
        )
      ) %>%
      select(Spieler, Marktwert_fmt, Diff_fmt)
    
    # Funktion zur Erstellung der Kader-Tabelle pro Manager
    create_kader_table <- function(manager_name) {
      df <- teams_df %>% filter(Manager == manager_name)
      df$Position <- factor(df$Position, levels = c("Tor", "Abwehr", "Mittelfeld", "Sturm"), ordered = TRUE)
      df <- df %>% arrange(Position, Spieler)
      
      # Join MW, Diff und Kaufpreis-Diff
      df_pre <- df %>%
        left_join(mw_aktuell, by = "Spieler") %>%
        left_join(mw_diff, by = "Spieler") %>%
        left_join(kaufpreise %>% filter(Hoechstbietender == manager_name), by = "Spieler") %>%
        mutate(
          Diff_Kauf = ifelse(is.na(Kaufpreis), NA, Marktwert_aktuell - Kaufpreis),
          Diff_Kauf_fmt = ifelse(
            is.na(Kaufpreis), "",
            case_when(
              Diff_Kauf > 0 ~ sprintf("<span style='color:#388e3c;'>+%s €</span>", format(Diff_Kauf, big.mark = ".", decimal.mark = ",")),
              Diff_Kauf < 0 ~ sprintf("<span style='color:#e53935;'>–%s €</span>", format(abs(Diff_Kauf), big.mark = ".", decimal.mark = ",")),
              TRUE ~ "<span style='color:grey;'>±0 €</span>"
            )
          )
        )
      
      rows <- c()
      current_pos <- NULL
      for(i in seq_len(nrow(df_pre))) {
        pos <- as.character(df_pre$Position[i])
        spieler <- df_pre$Spieler[i]
        mw <- df_pre$Marktwert_fmt[i]
        diff <- df_pre$Diff_fmt[i]
        diffk <- df_pre$Diff_Kauf_fmt[i]
        if(is.null(current_pos) || pos != current_pos) {
          rows <- c(rows, sprintf("<tr><th colspan='4' style='text-align:left; background:#eee; padding:4px;'>%s</th></tr>", pos))
          current_pos <- pos
        }
        rows <- c(
          rows,
          sprintf(
            "<tr>
            <td style='padding-left:15px;'>%s</td>
            <td style='text-align:right;'>%s</td>
            <td style='text-align:right;'>%s</td>
            <td style='text-align:right;'>%s</td>
          </tr>",
            spieler, mw, diff, diffk
          )
        )
      }
      
      table_html <- paste0(
        "<table style='border-collapse: collapse; width: 100%; margin-bottom: 20px;'>",
        "<thead>
        <tr>
          <th style='text-align:left;'>Spieler</th>
          <th style='text-align:right;'>MW</th>
          <th style='text-align:right;'>Vortag-MW-Diff</th>
          <th style='text-align:right;'>Kauf-Diff</th>
        </tr>
      </thead><tbody>",
        paste(rows, collapse = "\n"),
        "</tbody></table>"
      )
      
      tagList(
        tags$h4(manager_name, style = "margin-top: 0; margin-bottom: 5px;"),
        HTML(table_html)
      )
    }
    
    # Tabellen für alle Manager erstellen
    tables_ui <- lapply(manager_list, create_kader_table)
    # Gruppen von jeweils 4 Tabellen in flex-containern (Reihen)
    rows_ui <- split(tables_ui, ceiling(seq_along(tables_ui) / 4))
    # Jede Gruppe als flex-row mit 4 Spalten nebeneinander
    tagList(
      lapply(rows_ui, function(row_tables) {
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: flex-start; gap: 10px; margin-bottom: 30px;",
          lapply(row_tables, function(tab) {
            tags$div(
              style = "flex: 1 1 22%; box-sizing: border-box;",
              tab
            )
          })
        )
      })
    )
  })
  
  
  ## ---- Kaderwert-Entwicklung ----
  manuelle_standings <- tibble::tibble(
    Manager = rep(c(
      "Thomas", "Alfons", "Christoph", "Pascal",
      "Andreas", "Dominik", "Nico", "Christian"
    ), each = 2),
    Datum = rep(as.Date(c("2025-05-27", "2025-06-01")), times = 8),
    Teamwert = c(
      28170000, 49340000,  # Thomas
      30770000, 47310000,  # Alfons
      27380000, 52050000,  # Christoph
      29200000, 42330000,  # Pascal
      29210000, 36280000,  # Andreas
      26960000, 40120000,  # Dominik
      29470000, 41190000,  # Nico
      26860000, 38500000   # Christian
    )
  )
  
  
  standings_history_df <- reactive({
    # Kombinieren mit manuellen Einträgen
    bind_rows(st_df, manuelle_standings)
  })
  
  output$kaderwert_plot <- renderPlot({
    df <- standings_history_df() 
    
    ggplot(df, aes(x = Datum, y = Teamwert, color = Manager)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_brewer(palette = "Paired") +
      labs(
        title = "Kaderwert-Entwicklung",
        subtitle = "Teamwert pro Manager über die Zeit",
        x = "Datum",
        y = "Teamwert (€)",
        color = "Manager"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # ---- BIETERPROFILE ----
  ## ---- MW-Klassen Zeitstrahl für Boxplot+Beeswarm ----
  output$mw_zeitachse_preview_schlank <- renderPlot({
    # Kompletten Zeitraum nutzen, keine Filterung nach Slider
    ggplot(gesamt_mw_df, aes(x = Datum, y = MW_rel_normiert)) +
      geom_line(color = "darkgrey", linewidth = 1.2) +
      coord_cartesian(ylim = c(0.75, 1.4)) +
      labs(x = NULL, y = NULL, title = NULL) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })

  ## ---- MW-Klassen Boxplot+Beeswarm ----
  output$mwclassplot <- renderPlot({
    data <- gebotsprofil_mwclass_filtered()
    req(nrow(data) > 0)
    
    plotdata <- data %>%
      filter(!is.na(Diff_Prozent)) %>%
      mutate(
        MW_Klasse = factor(MW_Klasse, levels = c(
          "<0.5 Mio", "0.5–1 Mio", "1–2.5 Mio", "2.5–5 Mio", "5–10 Mio", ">10 Mio"
        ))
      )
    
    # Filter unusual high bids
    plotdata <- plotdata %>% 
      filter(Diff_Prozent<=50)
    
    # Für robustes beeswarm: Gruppengröße pro Facet
    plotdata_beeswarm <- plotdata %>%
      group_by(Bieter, MW_Klasse) %>%
      mutate(n_pts = n()) %>%
      ungroup()
    
    # Mittelwerte
    means <- plotdata %>%
      group_by(Bieter, MW_Klasse) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    # Mittelwert je MW-Klasse über alle Bieter
    means_klasse <- plotdata %>%
      group_by(MW_Klasse) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    # Anzahl Gebote über dem Mittelwert je MW-Klasse
    plotdata_ueber <- plotdata %>%
      left_join(means_klasse, by = "MW_Klasse") %>%
      group_by(MW_Klasse) %>%
      summarise(
        pct_ueber = round(100 * sum(Diff_Prozent > Mean, na.rm = TRUE) / n(), 1),
        n_total = n(),
        .groups = "drop"
      )
    
    ggplot(plotdata_beeswarm, aes(x = Bieter, y = Diff_Prozent, color = Bieter, fill = Bieter)) +
      geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.25, position = position_dodge(width = 0.7)) +
      
      # Beeswarm nur für Gruppen mit mehr als 1 Punkt
      geom_beeswarm(
        data = subset(plotdata_beeswarm, n_pts > 1),
        cex = 2, size = 2.5, alpha = 0.8, priority = "random"
      ) +
      
      # Einzelne Punkte (1 Wert pro Gruppe)
      geom_point(
        data = subset(plotdata_beeswarm, n_pts == 1),
        size = 2.5, alpha = 0.8, shape = 21
      ) +
      
      # Mittelwert-Linie
      geom_segment(
        data = means,
        aes(x = as.numeric(factor(Bieter)) - 0.4, 
            xend = as.numeric(factor(Bieter)) + 0.4, 
            y = Mean, 
            yend = Mean),
        color = "grey30",
        linetype = "dashed",
        linewidth = 0.8,
        inherit.aes = FALSE
      ) +
      
      # Mittelwert-Wert als Text
      geom_text(
        data = means,
        aes(x = as.numeric(factor(Bieter)), y = Mean, 
            label = round(Mean, 1)),
        color = "black",
        fontface = "bold",
        size = 3.5,
        vjust = -0.7,
        inherit.aes = FALSE
      ) +
      
      # -- Mittelwert-Linie je MW-Klasse --
      geom_hline(
        data = means_klasse,
        aes(yintercept = Mean),
        color = "#d62728",
        linewidth = 1.2,
        linetype = "solid",
        inherit.aes = FALSE
      ) +
      # Mittelwert-Wert je MW-Klasse als Text
      geom_text(
        data = means_klasse,
        aes(x = Inf, y = Mean, label = paste0("Ø ", round(Mean, 1), " %")),
        hjust = 1.1,
        vjust = -0.7,
        color = "#d62728",
        fontface = "bold",
        size = 3.5,
        inherit.aes = FALSE
      ) +
      # Hinzufügen von n über MW 
      geom_text(
        data = plotdata_ueber,
        aes(x = -Inf, y = Inf, label = paste0(pct_ueber, "% (n=", n_total, ")")),
        hjust = -0.1, vjust = 1.2,
        color = "#333",
        fontface = "bold",
        size = 4,
        inherit.aes = FALSE
      ) +
      
      facet_grid(. ~ MW_Klasse, scales = "free_y") +
      
      labs(
        title = "Gebotsabweichungen je Konkurrent und MW-Klasse",
        x = "Bieter",
        y = "Abweichung vom MW Vortag (%)"
      ) +
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
  })
  
  ## ---- Einzeln (Bieter, Typ) ----
  output$beeswarm <- renderPlot({
    req(nrow(gebotsprofil_clean()) > 0)
    plotdata <- gebotsprofil_clean() %>%
      filter(!is.na(Diff_Prozent))
    # Mittelwert über beide Typen je Bieter
    mean_total <- plotdata %>%
      group_by(Bieter) %>%
      summarise(Mean_total = mean(Diff_Prozent), .groups = "drop")
    # Mittelwerte je Typ
    means <- plotdata %>%
      group_by(Bieter, Typ) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    ggplot(plotdata, aes(x = Typ, y = Diff_Prozent, color = Typ)) +
      geom_boxplot(aes(fill = Typ), width = 0.5, outlier.shape = NA, alpha = 0.25) +
      geom_beeswarm(cex = 2, size = 2.5, alpha = 0.8) +
      # Gesamter Mittelwert als gestrichelte Linie und Text
      geom_hline(
        data = mean_total,
        aes(yintercept = Mean_total),
        linetype = "dashed", color = "grey60", linewidth = 0.9,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = mean_total,
        aes(x = 0.5, y = Mean_total, label = round(Mean_total, 1)),
        color = "black", # Dunkelrot
        fontface = "bold",
        size = 5,
        vjust = -0.25,
        inherit.aes = FALSE
      ) +
      # Mittelwert je Typ als Text (wie gehabt)
      geom_text(
        data = means,
        aes(x = Typ, y = Mean, label = round(Mean, 1), color = Typ),
        nudge_x = 0.4,
        fontface = "bold",
        size = 5,
        inherit.aes = FALSE
      ) +
      facet_wrap(~ Bieter, ncol = 4, scales = "free_y") +
      labs(
        title = "Gebotsabweichungen je Konkurrent",
        x = "",
        y = "Abweichung vom MW Vortag (%)"
      ) +
      scale_color_manual(values = c("Hoechstgebot" = "#1f77b4", "Zweitgebot" = "#ff7f0e")) +
      scale_fill_manual(values = c("Hoechstgebot" = "#1f77b4", "Zweitgebot" = "#ff7f0e")) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 16)
      )
  })
  
  ## ---- Gebotsverhalten über die Zeit ----
  
  output$trendplot <- renderPlot({
    req(nrow(gebotsprofil_clean()) > 0)
    plotdata <- gebotsprofil_clean() %>%
      filter(!is.na(Diff_Prozent)) %>%
      mutate(Datum = as.Date(Datum)) # falls nicht schon Date
    
    ggplot(plotdata, aes(x = Datum, y = Diff_Prozent)) +
      geom_point(alpha = 0.7, size = 2, color = "#1f77b4") +
      geom_smooth(se = FALSE, method = "loess", span = 0.6, color = "#d62728", linewidth = 1.3) +
      facet_wrap(~ Bieter, ncol = 3, scales = "free_y") +
      labs(
        title = "Entwicklung des Gebotsverhaltens über die Zeit",
        x = "Datum",
        y = "Abweichung vom MW Vortag (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        strip.text = element_text(face = "bold", size = 16)
      )
  })
  
  ## ---- Gebote pro Tag ----
  
  output$gebote_pro_tag <- renderPlot({
    df <- gebotsprofil_clean() %>%
      group_by(Datum, Bieter) %>%
      summarise(Anzahl_Gebote = n(), .groups = "drop")
    
    ggplot(df, aes(x = Datum, y = Anzahl_Gebote, fill = Bieter)) +
      geom_col(position = "stack", alpha = 0.7) +
      labs(
        title = "Anzahl aller Gebote pro Tag (gestapelt nach Bieter)",
        x = "Datum",
        y = "Gebote pro Tag",
        fill = "Bieter"
      ) +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  ## ---- Linienplot: Gebote je Konkurrent über Zeit ----
  output$gebote_pro_tag_linie <- renderPlot({
    df <- gebotsprofil_clean() %>%
      group_by(Datum, Bieter) %>%
      summarise(Anzahl_Gebote = n(), .groups = "drop")
    
    ggplot(df, aes(x = Datum, y = Anzahl_Gebote, color = Bieter)) +
      geom_line(size = 1.2) +
      geom_point(size = 2, alpha = 0.8) +
      scale_color_brewer(palette = "Paired") +
      labs(
        title = "Gebote pro Tag je Konkurrent (Linienplot)",
        x = "Datum",
        y = "Gebote pro Tag",
        color = "Bieter"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  ## ---- Durchschnittliche Gebotszahl pro Tag je Konkurrent ----
  
  gebote_tag_bieter <- reactive({
    gebotsprofil_clean() %>%
      group_by(Datum, Bieter) %>%
      summarise(Anzahl_Gebote = n(), .groups = "drop")
  })
  
  
  output$gebote_pro_tag_bieter <- renderPlot({
    df <- gebote_tag_bieter()
    ggplot(df, aes(x = Bieter, y = Anzahl_Gebote, color = Bieter, fill = Bieter)) +
      geom_boxplot(width = 0.5, alpha = 0.2, outlier.shape = NA) +
      ggbeeswarm::geom_beeswarm(cex = 2, size = 2.5, alpha = 0.8) +
      labs(
        title = "Gebotsanzahl pro Tag je Konkurrent (Boxplot + Beeswarm)",
        x = "Bieter",
        y = "Gebote pro Tag"
      ) +
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  ## ---- Aktive Tage pro Spieler ----
  
  output$aktive_tage_plot <- renderPlot({
    aktive_tage <- gebotsprofil_clean() %>%
      group_by(Bieter) %>%
      summarise(Aktive_Tage = n_distinct(Datum)) %>%
      arrange(desc(Aktive_Tage))
    
    ggplot(aktive_tage, aes(x = reorder(Bieter, -Aktive_Tage), y = Aktive_Tage, fill = Bieter)) +
      geom_col(show.legend = FALSE) +
      labs(
        title = "Anzahl aktiver Tage pro Spieler (mind. 1 Gebot/Tag)",
        x = "Bieter",
        y = "Aktive Tage"
      ) +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal(base_size = 14)
  })
  
  
  
  ## ---- Gebots-Peaks ----
  
  output$peak_days_heatmap <- renderPlot({
    # Alle möglichen Kombinationen Bieter x Datum erzeugen (auch 0 Gebote!)
    all_dates <- seq.Date(
      min(gebotsprofil_clean()$Datum, na.rm = TRUE),
      max(gebotsprofil_clean()$Datum, na.rm = TRUE),
      by = "day"
    )
    alle_bieter <- unique(gebotsprofil_clean()$Bieter)
    full_grid <- expand.grid(Datum = all_dates, Bieter = alle_bieter, stringsAsFactors = FALSE)
    
    # Tatsächliche Gebotszahlen einfügen
    gebote_tage <- gebotsprofil_clean() %>%
      group_by(Datum, Bieter) %>%
      summarise(Anzahl_Gebote = n(), .groups = "drop")
    full_heat <- full_grid %>%
      left_join(gebote_tage, by = c("Datum", "Bieter")) %>%
      mutate(Anzahl_Gebote = replace_na(Anzahl_Gebote, 0))
    
    # Markierung Wochenende und Saisonstart (z.B. 22.08.2025)
    full_heat <- full_heat %>%
      mutate(
        Wochentag = weekdays(Datum, abbreviate = TRUE),
        Wochenende = Wochentag %in% c("Sa", "So"),
        Saisonstart = Datum == as.Date("2025-08-22")
      )
    
    ggplot(full_heat, aes(x = Datum, y = Bieter, fill = Anzahl_Gebote)) +
      geom_tile(color = "grey90") +
      # Saisonstart als Outline
      geom_tile(data = filter(full_heat, Saisonstart), color = "black", size = 1.2, fill = NA) +
      # Wochenenden als Punkt-Overlay (optional, für bessere Sichtbarkeit)
      geom_point(
        data = filter(full_heat, Wochenende), aes(x = Datum, y = Bieter),
        shape = 21, fill = "transparent", color = "black", size = 2, stroke = 1, inherit.aes = FALSE
      ) +
      scale_fill_gradient(
        low = "white",
        high = "#e41a1c",   # kräftiges Rot für viele Gebote
        breaks = scales::pretty_breaks(n = 4)
      ) +
      labs(
        title = "Gebots-Aktivität aller Spieler über die Zeit (Heatmap)",
        x = "Datum",
        y = "Bieter",
        fill = "Gebote"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank()
      )
  })
  
  ## ---- Zusammenfassung als Tabelle ohne MW Klassen ----
  output$mwclass_summary <- renderDT({

    # Priorität: show_entries aktiviert → gebotsprofil_clean()-Tabelle anzeigen
    if (!is.null(input$show_entries) && input$show_entries) {

      dat <- gebotsprofil_clean() %>%
        group_by(Bieter) %>%
        summarise(
          Anzahl_gesamt = n(),
          Anzahl_Hoechstgebote = sum(Typ == "Hoechstgebot"),
          Anzahl_Zweitgebote = sum(Typ == "Zweitgebot"),
          Durchschnittsgebot = round(mean(Diff_Prozent, na.rm = TRUE), digits = 1),
          Min = min(Diff_Prozent, na.rm = TRUE),
          Max = max(Diff_Prozent, na.rm = TRUE),
          Transfers = paste0(Spieler, ": ", Diff_Prozent, "% (", Typ, " am ", format(Datum, "%d.%m.%Y"), ")", collapse = "; ")
        )

      datatable(dat, options = list(pageLength = 100))

    } else if (!is.null(input$show_table) && input$show_table) {

      datatable(mwclass_summary(), options = list(pageLength = 100))

    } else {

      dat <- gebotsprofil_clean() %>%
        group_by(Bieter) %>%
        summarise(
          Anzahl_gesamt = n(),
          Anzahl_Hoechstgebote = sum(Typ == "Hoechstgebot"),
          Anzahl_Zweitgebote = sum(Typ == "Zweitgebot"),
          Durchschnittsgebot = round(mean(Diff_Prozent, na.rm = TRUE), digits = 1),
          Min = min(Diff_Prozent, na.rm = TRUE),
          Max = max(Diff_Prozent, na.rm = TRUE)
        )

      datatable(dat, options = list(pageLength = 100))
    }
  })


  
  # ---- FLIP-ANALYSE ----
  
  # -- Flip-Gewinn vorbereiten (angenommen Einkaufspreise und Verkäufe in transfers)
  flip_data <- reactive({
    req(data_all())
    transfers <- data_all()$transfers
    
    # Käufe (nur echte Käufe durch Manager)
    einkaeufe <- transfers %>%
      filter(Hoechstbietender != "Computer") %>%
      select(Spieler, Einkaufsdatum = Datum, Einkaufspreis = Hoechstgebot, Besitzer = Hoechstbietender) %>%
      arrange(Besitzer, Spieler, Einkaufsdatum)
    
    # Verkäufe
    verkaeufe <- transfers %>%
      filter(Besitzer != Hoechstbietender) %>%
      select(Spieler, Verkaufsdatum = Datum, Verkaufspreis = Hoechstgebot, Besitzer)
    
    # Flip-Paare bauen
    flips <- list()
    
    for (i in 1:nrow(verkaeufe)) {
      verkauf <- verkaeufe[i, ]
      
      # Suche den frühesten unbenutzten Kauf
      kauf_kandidat <- einkaeufe %>%
        filter(Spieler == verkauf$Spieler,
               Besitzer == verkauf$Besitzer,
               Einkaufsdatum < verkauf$Verkaufsdatum) %>%
        arrange(Einkaufsdatum) %>%
        slice(1)
      
      if (nrow(kauf_kandidat) == 1) {
        # Flip speichern
        flips[[length(flips) + 1]] <- data.frame(
          Spieler = verkauf$Spieler,
          Besitzer = verkauf$Besitzer,
          Einkaufsdatum = kauf_kandidat$Einkaufsdatum,
          Einkaufspreis = kauf_kandidat$Einkaufspreis,
          Verkaufsdatum = verkauf$Verkaufsdatum,
          Verkaufspreis = verkauf$Verkaufspreis,
          Gewinn = verkauf$Verkaufspreis - kauf_kandidat$Einkaufspreis
        )
        
        # Den Kauf aus der Liste entfernen (= "verbraucht")
        einkaeufe <- einkaeufe %>%
          filter(!(Spieler == kauf_kandidat$Spieler &
                     Besitzer == kauf_kandidat$Besitzer &
                     Einkaufsdatum == kauf_kandidat$Einkaufsdatum))
      }
    }
    
    if (length(flips) > 0) {
      bind_rows(flips)
    } else {
      data.frame()  # leeres DF wenn keine Flips
    }
  })
  
  
  ## ---- Flip-Gesamtsumme ----
  ## -- Flip-Gewinne pro Spieler (Beeswarm & Boxplot)
  output$flip_summarybar <- renderPlot({
    req(nrow(flip_data()) > 0)
    
    flip_data() %>%
      group_by(Besitzer) %>%
      summarise(Gesamtgewinn = sum(Gewinn, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(Besitzer, Gesamtgewinn), y = Gesamtgewinn, fill = Gesamtgewinn > 0)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = round(Gesamtgewinn, 0), 
                    hjust = ifelse(Gesamtgewinn > 0, -0.1, 1.1)), 
                position = position_dodge(width = 1)) +
      scale_fill_manual(values = c("TRUE" = "#2b9348", "FALSE" = "#d00000")) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 16, face = "bold", color = "black"),
        axis.text.x = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold")
      ) +
      labs(
        title = "Flip-Gewinn/Verlust je Comunio-Spieler (gesamt)",
        x = "",
        y = "Gewinn/Verlust (€)"
      ) 
  })
  
  
  ## ---- Flip-Verlauf kummuliert über Zeit ----
  output$flip_cumulative <- renderPlot({
    req(nrow(flip_data()) > 0)
    
    flip_data() %>%
      arrange(Besitzer, Verkaufsdatum) %>%
      group_by(Besitzer) %>%
      mutate(Kumuliert = cumsum(Gewinn)) %>%
      ungroup() %>%
      ggplot(aes(x = Verkaufsdatum, y = Kumuliert, color = Besitzer)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_brewer(palette = "Paired") +
      labs(
        title = "Kumulierte Flip-Gewinne über die Zeit je Spieler",
        x = "Datum",
        y = "Kumulierte Gewinne (€)",
        color = "Spieler"
      ) +
      theme_minimal(base_size = 14)
  })
  
  ## ---- Flip-Effizienz (mit Kategorien) ----
  output$flip_effizienz <- renderPlot({
    req(nrow(flip_data()) > 0)
    
    flip_data() %>%
      mutate(
        Flip_Kategorie = case_when(
          abs(Gewinn) < 2.5e4 ~ "Micro-Flip <25k",
          abs(Gewinn) < 1e5 ~ "Mini-Flip 25–99k",
          abs(Gewinn) < 2.5e5 ~ "Klein-Flip 100–249k",
          abs(Gewinn) < 5e5 ~ "Mittel-Flip 250–499k",
          abs(Gewinn) >= 5e5 ~ "Mega-Flip ≥500k"
        ),
        Flip_Ergebnis = ifelse(Gewinn >= 0, "Gewinn", "Verlust"),
        Flip_Label = paste(Flip_Ergebnis, Flip_Kategorie, sep = " - ")
      ) %>%
      mutate(
        Flip_Label = factor(Flip_Label, levels = c(
          "Gewinn - Micro-Flip <25k",
          "Gewinn - Mini-Flip 25–99k",
          "Gewinn - Klein-Flip 100–249k",
          "Gewinn - Mittel-Flip 250–499k",
          "Gewinn - Mega-Flip ≥500k",
          "Verlust - Micro-Flip <25k",
          "Verlust - Mini-Flip 25–99k",
          "Verlust - Klein-Flip 100–249k",
          "Verlust - Mittel-Flip 250–499k",
          "Verlust - Mega-Flip ≥500k"
        ))
      ) %>%
      count(Besitzer, Flip_Label) %>%
      ggplot(aes(x = Besitzer, y = n, fill = Flip_Label)) +
      geom_col(position = "stack") +
      scale_fill_manual(
        values = c(
          "Gewinn - Micro-Flip <25k"      = "#a5d6a7",
          "Gewinn - Mini-Flip 25–99k"     = "#66bb6a",
          "Gewinn - Klein-Flip 100–249k"  = "#388e3c",
          "Gewinn - Mittel-Flip 250–499k" = "#1b5e20",
          "Gewinn - Mega-Flip ≥500k"      = "#004d40",
          "Verlust - Micro-Flip <25k"      = "#ffcdd2",
          "Verlust - Mini-Flip 25–99k"     = "#ef9a9a",
          "Verlust - Klein-Flip 100–249k"  = "#e57373",
          "Verlust - Mittel-Flip 250–499k" = "#d32f2f",
          "Verlust - Mega-Flip ≥500k"      = "#b71c1c"
        )
      ) +
      labs(
        title = "Anzahl kumuliert",
        x = "",
        y = "Anzahl Flips",
        fill = "Flip-Art"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  
  
  ## ---- FLIP-Kumuliert je Flip-Art ----
  output$flip_cumcat <- renderPlot({
    req(nrow(flip_data()) > 0)
    flip_data() %>%
      mutate(
        Flip_Kategorie = case_when(
          abs(Gewinn) < 2.5e4 ~ "Micro-Flip <25k",
          abs(Gewinn) < 1e5 ~ "Mini-Flip 25–99k",
          abs(Gewinn) < 2.5e5 ~ "Klein-Flip 100–249k",
          abs(Gewinn) < 5e5 ~ "Mittel-Flip 250–499k",
          abs(Gewinn) >= 5e5 ~ "Mega-Flip ≥500k"
        ),
        Flip_Ergebnis = ifelse(Gewinn >= 0, "Gewinn", "Verlust"),
        Flip_Label = paste(Flip_Ergebnis, Flip_Kategorie, sep = " - ")
      ) %>%
      mutate(
        Flip_Label = factor(Flip_Label, levels = c(
          "Gewinn - Micro-Flip <25k",
          "Gewinn - Mini-Flip 25–99k",
          "Gewinn - Klein-Flip 100–249k",
          "Gewinn - Mittel-Flip 250–499k",
          "Gewinn - Mega-Flip ≥500k",
          "Verlust - Micro-Flip <25k",
          "Verlust - Mini-Flip 25–99k",
          "Verlust - Klein-Flip 100–249k",
          "Verlust - Mittel-Flip 250–499k",
          "Verlust - Mega-Flip ≥500k"
        ))
      ) %>%
      group_by(Besitzer, Flip_Label) %>%
      summarise(Summe = sum(Gewinn), .groups = "drop") %>%
      ggplot(aes(x = Besitzer, y = Summe, fill = Flip_Label)) +
      geom_col(position = "stack") +
      labs(
        title = "Gewinne/Verluste kumuliert",
        x = "",
        y = "Summe Gewinn/Verlust (€)",
        fill = "Flip-Art"
      ) +
      scale_fill_manual(
        values = c(
          "Gewinn - Micro-Flip <25k"      = "#a5d6a7",
          "Gewinn - Mini-Flip 25–99k"     = "#66bb6a",
          "Gewinn - Klein-Flip 100–249k"  = "#388e3c",
          "Gewinn - Mittel-Flip 250–499k" = "#1b5e20",
          "Gewinn - Mega-Flip ≥500k"      = "#004d40",
          "Verlust - Micro-Flip <25k"      = "#ffcdd2",
          "Verlust - Mini-Flip 25–99k"     = "#ef9a9a",
          "Verlust - Klein-Flip 100–249k"  = "#e57373",
          "Verlust - Mittel-Flip 250–499k" = "#d32f2f",
          "Verlust - Mega-Flip ≥500k"      = "#b71c1c"
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "none")
  })
  
  ## ---- Hypothetischer Kader-Fip ----
  output$flip_kader <- DT::renderDT({
    
    # Aktuelles Datum ermitteln (letzter Tag in gesamt_mw_roh)
    aktuelles_datum <- max(gesamt_mw_roh$Datum, na.rm = TRUE)
    vortag <- sort(unique(gesamt_mw_roh$Datum))
    vortag <- vortag[which(vortag == aktuelles_datum) - 1]
    
    mw_aktuell <- gesamt_mw_roh %>%
      filter(Datum == aktuelles_datum) %>%
      group_by(Spieler) %>%
      summarise(Marktwert_aktuell = first(Marktwert), .groups = "drop")
    
    mw_vortag <- gesamt_mw_roh %>%
      filter(Datum == vortag) %>%
      group_by(Spieler) %>%
      summarise(Marktwert_vortag = first(Marktwert), .groups = "drop")
    
    kaufpreise <- transfers %>%
      group_by(Spieler, Hoechstbietender) %>%
      arrange(desc(Datum)) %>%
      slice(1) %>%
      select(Spieler, Manager = Hoechstbietender, Kaufpreis = Hoechstgebot)
    
    # Heute
    df_all_heute <- teams_df %>%
      left_join(mw_aktuell, by = "Spieler") %>%
      left_join(kaufpreise, by = c("Spieler", "Manager")) %>%
      mutate(Diff = Marktwert_aktuell - Kaufpreis)
    
    # Vortag
    df_all_vortag <- teams_df %>%
      left_join(mw_vortag, by = "Spieler") %>%
      left_join(kaufpreise, by = c("Spieler", "Manager")) %>%
      mutate(Diff = Marktwert_vortag - Kaufpreis)
    
    # Flip-Einnahmen heute und gestern, und Ø Flip pro Teamspieler
    flip_today <- df_all_heute %>%
      group_by(Manager) %>%
      summarise(
        sum_diff = sum(Diff, na.rm = TRUE),
        team_size = n(),
        .groups = "drop"
      )
    
    flip_yesterday <- df_all_vortag %>%
      group_by(Manager) %>%
      summarise(
        sum_diff_yesterday = sum(Diff, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Merge
    gesamt_flip <- flip_today %>%
      left_join(flip_yesterday, by = "Manager") %>%
      mutate(
        `Aktuelle hypothetische Gesamt-Flip-Einnahmen` = ifelse(
          sum_diff > 0,
          paste0("+", format(sum_diff, big.mark = ".", decimal.mark = ","), " €"),
          paste0("-", format(abs(sum_diff), big.mark = ".", decimal.mark = ","), " €")
        ),
        avg_flip = ifelse(team_size > 0, sum_diff / team_size, NA_real_),
        `Ø Flip-Einnahme pro Spieler` = dplyr::case_when(
          is.na(avg_flip)            ~ "",
          avg_flip > 0              ~ paste0("+", format(round(avg_flip), big.mark = ".", decimal.mark = ","), " €"),
          avg_flip < 0              ~ paste0("-", format(abs(round(avg_flip)), big.mark = ".", decimal.mark = ","), " €"),
          TRUE                      ~ "±0 €"
        ),
        vortag_diff = sum_diff - sum_diff_yesterday,
        `Vortag-Diff` = dplyr::case_when(
          is.na(vortag_diff)        ~ "",
          vortag_diff > 0           ~ sprintf("<span style='color:#388e3c;'>+%s €</span>", format(vortag_diff, big.mark = ".", decimal.mark = ",")),
          vortag_diff < 0           ~ sprintf("<span style='color:#e53935;'>–%s €</span>", format(abs(vortag_diff), big.mark = ".", decimal.mark = ",")),
          TRUE                      ~ "<span style='color:grey;'>±0 €</span>"
        )
      ) %>%
      arrange(desc(sum_diff)) %>%        # <<<< SORTIERUNG VOR SELECT!
      select(Manager,
             `Aktuelle hypothetische Gesamt-Flip-Einnahmen`,
             `Vortag-Diff`,
             `Ø Flip-Einnahme pro Spieler`, vortag_diff)
    
    # Teamwert am Vortag einlesen
    teamwert_vortag <- st_df %>%
      filter(Datum == vortag) %>%
      select(Manager, Teamwert_vortag = Teamwert)
    
    gesamt_flip <- gesamt_flip %>%
      left_join(teamwert_vortag, by = "Manager") %>%
      mutate(
        vortag_diff_rel = vortag_diff / as.numeric(Teamwert_vortag) * 100,
        `Vortag-Diff (% Teamwert)` = dplyr::case_when(
          is.na(vortag_diff_rel)        ~ "",
          vortag_diff_rel > 0           ~ sprintf("<span style='color:#388e3c;'>+%.2f %%</span>", vortag_diff_rel),
          vortag_diff_rel < 0           ~ sprintf("<span style='color:#e53935;'>–%.2f %%</span>", abs(vortag_diff_rel)),
          TRUE                          ~ "<span style='color:grey;'>±0 %%</span>"
        )
      ) %>%
      select(Manager,
             `Aktuelle hypothetische Gesamt-Flip-Einnahmen`,
             `Vortag-Diff`,
             `Vortag-Diff (% Teamwert)`,
             `Ø Flip-Einnahme pro Spieler`)
    
    
    DT::datatable(
      gesamt_flip,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(className = 'dt-right', targets = c(1,2,3,4))
        )
      ),
      escape = FALSE, selection = "single"
    )
  })
  
  ## ---- Flip-Historie je Spieler ----
  output$flip_player_table <- renderDT({
    req(input$flip_player_select)
    
    flip_data() %>%
      filter(Besitzer == input$flip_player_select) %>%
      select(Verkaufsdatum, Spieler, Einkaufsdatum, Einkaufspreis, Verkaufspreis, Gewinn) %>%
      arrange(desc(Verkaufsdatum)) %>%
      datatable(
        options = list(pageLength = 10),
        colnames = c("Verkaufsdatum", "Spieler", "Kaufdatum", "Einkaufspreis", "Verkaufspreis", "Gewinn/Verlust (€)")
      )
  })
  
  # ---- KAPITALÜBERSICHT ----
  
  ## ---- Kontostände je Spieler ----
  
  # Hilfsfunktion: Nur Vornamen extrahieren
  vorname <- function(name) {
    strsplit(name, " ")[[1]][1]
  }
  
  # Fixe Startkapitalwerte (vollständige Namen als Keys)
  startkapital_fix <- c(
  "Alfons" = 14230000,
  "Nico" = 15530000,
  "Andreas" = 15790000,
  "Pascal" = 15800000,
  "Thomas" = 16830000,
  "Christoph" = 17640000,
  "Christian" = 18140000,
  "Dominik" = 18040000
)

  
  # Vornamen-Index für Startkapital
  startkapital_fix_vornamen <- startkapital_fix
  names(startkapital_fix_vornamen) <- sapply(names(startkapital_fix), vorname)
  
  # ReactiveVal für geparste Teamwerte
  teamwerte_val <- reactiveVal(data.frame(Manager = character(), Kaderwert = numeric(), stringsAsFactors = FALSE))
  
  # Parsing der Teamwerte bei Klick
  observeEvent(input$parse_teamwerte, {
    req(input$teamwerte_text)
    lines <- unlist(strsplit(input$teamwerte_text, "\n"))
    
    parse_line <- function(line) {
      line <- trimws(line)
      line <- sub("^\\d+\\.\\s*", "", line)  # führende Nummern entfernen
      line <- sub("-.*$", "", line)           # Minus und dahinter entfernen
      m <- regexpr("(\\d{1,3}(?:\\.\\d{3})+)$", line, perl=TRUE)
      if (m == -1) return(NULL)
      wert_str <- regmatches(line, m)
      wert_num <- as.numeric(gsub("\\.", "", wert_str))
      manager <- substr(line, 1, m - 1)
      manager <- trimws(manager)
      if (is.na(wert_num) || wert_num <= 0 || nchar(manager) == 0) return(NULL)
      data.frame(Manager = manager, Kaderwert = wert_num, stringsAsFactors = FALSE)
    }
    
    df_list <- lapply(lines, parse_line)
    df <- do.call(rbind, df_list)
    if (is.null(df)) df <- data.frame(Manager = character(), Kaderwert = numeric())
    
    df$Vorname <- sapply(df$Manager, vorname)
    
    teamwerte_val(df)
    
    output$teamwerte_status <- renderText({
      paste0("Erfasste Teamwerte: ", nrow(df))
    })
  })
  
  # Kapitalübersicht-Tabelle
  output$kapital_uebersicht_table <- renderDT({
    dat <- data_all()
    transfers <- dat$transfers
    transactions <- readr::read_delim(
      "TRANSACTIONS.csv",
      delim = ";",
      locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ""),
      show_col_types = FALSE
    ) %>%
      mutate(
        Spieler = as.character(Spieler),
        Manager = word(Spieler, 1)  # nimmt das erste Wort → also den Vornamen
      ) %>%
      filter(!(Datum == "01.06.2025" & Manager == "Alfons" & Transaktion == -166000)) %>%  #Rausfiltern von Fehlgebot von Alfons
      group_by(Manager) %>%
      summarise(Transaction_Summe = sum(Transaktion, na.rm = TRUE), .groups = "drop")
    
    alle_manager <- names(startkapital_fix)
    
    # Summen aus Transfers
    ausgaben <- transfers %>%
      group_by(Hoechstbietender) %>%
      summarise(Ausgaben = sum(Hoechstgebot, na.rm = TRUE)) %>%
      rename(Manager = Hoechstbietender)
    
    einnahmen <- transfers %>%
      group_by(Besitzer) %>%
      summarise(Einnahmen = sum(Hoechstgebot, na.rm = TRUE)) %>%
      rename(Manager = Besitzer)
    
    # Standings-Daten laden
    kader_df <- standings_df()
    
    # Falls nötig: Vornamen extrahieren oder sonstiges Mapping, hier nehmen wir Manager-Namen direkt
    # Optional: teamnamen bereinigen oder anpassen, wenn deine Startkapitalnamen anders sind
    
    kapital_df <- data.frame(Manager = alle_manager, stringsAsFactors = FALSE)
    
    kapital_df <- kapital_df %>%
      left_join(kader_df, by = "Manager") %>%
      mutate(
        Teamwert = ifelse(is.na(Teamwert), 0, Teamwert),
        Startkapital = startkapital_fix[Manager],
        Kreditrahmen = round(Teamwert / 4),
        Ausgaben = ifelse(is.na(ausgaben$Ausgaben[match(Manager, ausgaben$Manager)]), 0, ausgaben$Ausgaben[match(Manager, ausgaben$Manager)]),
        Einnahmen = ifelse(is.na(einnahmen$Einnahmen[match(Manager, einnahmen$Manager)]), 0, einnahmen$Einnahmen[match(Manager, einnahmen$Manager)]),
        Transaction_Summe = ifelse(is.na(transactions$Transaction_Summe[match(Manager, transactions$Manager)]), 0, transactions$Transaction_Summe[match(Manager, transactions$Manager)]),
        Aktuelles_Kapital = Startkapital + Einnahmen - Ausgaben + Transaction_Summe,
        Verfügbares_Kapital = Aktuelles_Kapital + Kreditrahmen
      ) %>%
      select(Manager, Startkapital, Teamwert, Kreditrahmen, Ausgaben, Einnahmen, Transaction_Summe, Aktuelles_Kapital, Verfügbares_Kapital)
    
    datatable(
      kapital_df,
      colnames = c(
        "Manager",
        "Startkapital (€)",
        "Teamwert (€)",
        "Kreditrahmen (¼ Kaderwert) (€)",
        "Transfer-Ausgaben (€)",
        "Transfer-Einnahmen (€)",
        "Disziplinar-/Bonus-Transaktionen (€)",
        "Aktuelles Kapital (€)",
        "Verfügbares Kapital (€)"
      ),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        order = list(list(which(colnames(kapital_df) == "Verfügbares_Kapital") - 1, 'desc'))
      )
    ) %>%
      formatCurrency(
        columns = c("Startkapital", "Teamwert", "Kreditrahmen", "Ausgaben", "Einnahmen", "Transaction_Summe", "Aktuelles_Kapital", "Verfügbares_Kapital"),
        currency = "",
        interval = 3,
        mark = ".",
        digits = 0,
        dec.mark = ","
      ) %>%
      formatStyle(
        'Aktuelles_Kapital',
        backgroundColor = styleInterval(0, c('salmon', NA)),
        fontWeight = styleInterval(0, c('bold', NA))
      )
  })
  
  output$parsed_transactions_output <- renderText({
    parsed_transactions_val()
  })
  
  
}


# SHINY APP STARTEN
shinyApp(ui, server)

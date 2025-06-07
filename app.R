library(shiny)
library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(readxl)
library(DT)

# ---- Custom JS für Copy-Button ----
jsCode <- "
Shiny.addCustomMessageHandler('copyText', function(message) {
  var tempText = document.getElementById('tempClipboard');
  if (!tempText) {
    tempText = document.createElement('textarea');
    tempText.id = 'tempClipboard';
    tempText.style.position = 'fixed';
    tempText.style.opacity = 0;
    document.body.appendChild(tempText);
  }
  tempText.value = message;
  tempText.select();
  document.execCommand('copy');
});
"

# ---- UI ----
ui <- fluidPage(
  titlePanel("Comunio Bieterprofil Analyse"),
  tags$script(HTML(jsCode)),   # Füge JS-Code ins UI ein
  sidebarLayout(
    sidebarPanel(
      fileInput("transfers", "Transfers_all.xlsx", accept = c(".xlsx")),
      fileInput("transfermarkt", "TM_all.xlsx", accept = c(".xlsx")),
      helpText("Beide Dateien laden. Es wird ausschließlich der Marktwert vom Vortag (oder davor) verwendet!"),
      checkboxInput("show_table", "Zeige zusammengefasste MW-Klassen-Statistik", value = TRUE),
      tags$hr(),
      h4("Transfernews convert"),
      textAreaInput("transfer_text", "Transfernews Text einfügen", height = "250px"),
      actionButton("parse_text", "Konvertieren"),
      br(),
      uiOutput("copy_button_ui"),
      verbatimTextOutput("parsed_transfers")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("MW-Klassen Plot", plotOutput("mwclassplot", height = 700)),
        tabPanel("Bieter-Profile (Punkte)", plotOutput("beeswarm", height = 700)),
        tabPanel("Zusammenfassung", DTOutput("mwclass_summary"))
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  # -- Daten laden und vorverarbeiten
  data_all <- reactive({
    req(input$transfers, input$transfermarkt)
    transfers <- read_excel(input$transfers$datapath) %>%
      mutate(Datum = as.Date(Datum)) %>%
      mutate(Hoechstgebot = ifelse(Datum == as.Date("2025-05-30") & Spieler == "Hranáč", 166000, Hoechstgebot))
    transfermarkt <- read_excel(input$transfermarkt$datapath) %>%
      mutate(TM_Stand = as.Date(TM_Stand))
    list(transfers = transfers, transfermarkt = transfermarkt)
  })
  
  # -- Funktion MW vom Vortag suchen
  get_MW_vortag <- function(spieler, datum, transfermarkt) {
    tm <- transfermarkt %>%
      filter(Spieler == spieler, TM_Stand <= (datum - days(1))) %>%
      arrange(desc(TM_Stand)) %>%
      slice(1)
    if (nrow(tm) == 1) tm$Marktwert else NA
  }
  
  # -- Profil-Daten berechnen (MW nur Vortag oder davor!)
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
  
  # -- MW Klasse vergeben
  gebotsprofil_mwclass <- reactive({
    gebotsprofil_clean() %>%
      mutate(
        MW_Klasse = case_when(
          MW_vortag < 1e6 ~ "<1 Mio",
          MW_vortag < 5e6 ~ "1–5 Mio",
          MW_vortag >= 5e6 ~ ">5 Mio",
          TRUE ~ "unbekannt"
        )
      )
  })
  
  # -- Zusammenfassungstabelle für MW-Klassen
  mwclass_summary <- reactive({
    gebotsprofil_mwclass() %>%
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
  
  # -- MW-Klassen Boxplot+Beeswarm
  output$mwclassplot <- renderPlot({
    req(nrow(gebotsprofil_mwclass()) > 0)
    plotdata <- gebotsprofil_mwclass() %>%
      filter(!is.na(Diff_Prozent)) %>%
      mutate(MW_Klasse = factor(MW_Klasse, levels = c("<1 Mio", "1–5 Mio", ">5 Mio")), all = "all")
    means <- plotdata %>%
      group_by(Bieter, MW_Klasse) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop") %>%
      mutate(all = "all")
    ggplot(plotdata, aes(x = all, y = Diff_Prozent, color = all)) +
      geom_boxplot(aes(fill = all), width = 0.5, outlier.shape = NA, alpha = 0.25) +
      geom_beeswarm(cex = 2, size = 2.5, alpha = 0.8) +
      geom_text(
        data = means,
        aes(x = all, y = Mean, label = round(Mean, 1)),
        inherit.aes = FALSE,
        nudge_x = 0.33,
        fontface = "bold",
        size = 3.5,
        color = "black"
      ) +
      facet_grid(Bieter ~ MW_Klasse, scales = "free_y") +
      labs(
        title = "Gebotsabweichungen je Konkurrent und MW-Klasse",
        x = "",
        y = "Abweichung vom MW Vortag (%)"
      ) +
      scale_color_manual(values = c("all" = "#1f77b4")) +
      scale_fill_manual(values = c("all" = "#1f77b4")) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold")
      )
  })
  
  # -- Punktplot mit Facets (Bieter, Typ)
  output$beeswarm <- renderPlot({
    req(nrow(gebotsprofil_clean()) > 0)
    plotdata <- gebotsprofil_clean() %>%
      filter(!is.na(Diff_Prozent))
    medians <- plotdata %>%
      group_by(Bieter, Typ) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    ggplot(plotdata, aes(x = Typ, y = Diff_Prozent, color = Typ)) +
      geom_boxplot(aes(fill = Typ), width = 0.5, outlier.shape = NA, alpha = 0.25) +
      geom_beeswarm(cex = 2, size = 2.5, alpha = 0.8) +
      geom_text(
        data = medians,
        aes(x = Typ, y = Mean, label = round(Mean, 1)),
        color = "black",
        nudge_x = 0.5,
        fontface = "bold",
        size = 3.5,
        inherit.aes = FALSE
      ) +
      facet_wrap(~ Bieter, ncol = 3, scales = "free_y") +
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
        strip.text = element_text(face = "bold")
      )
  })
  
  # -- Zusammenfassung als Tabelle
  output$mwclass_summary <- renderDT({
    if (input$show_table) datatable(mwclass_summary())
  })
  
  # -- Transfernews Converter (inkl. Copy-to-Clipboard Button) ----
  parsed_text_val <- reactiveVal("")
  
  observeEvent(input$parse_text, {
    req(input$transfer_text)
    text <- input$transfer_text
    
    lines <- unlist(strsplit(text, "\n"))
    transfers <- list()
    akt_datum <- NA
    
    for (i in seq_along(lines)) {
      line <- lines[i]
      
      # 1. Hole aktuelles Datum, falls vorhanden (z.B. "06.06.25")
      if (grepl("\\d{2}\\.\\d{2}\\.\\d{2}", line)) {
        found <- regmatches(line, regexpr("\\d{2}\\.\\d{2}\\.\\d{2}", line))
        if (length(found) > 0) akt_datum <- found[1]
      }
      
      # 2. Nur relevante Transferzeilen weiter verarbeiten
      if (grepl("wechselt für", line)) {
        # --- Zeitstempel wie "11:34 - " entfernen
        line_clean <- sub("^\\d{1,2}:\\d{2} -\\s*", "", line)
        # Spielername extrahieren
        spieler <- sub("^(.*?) wechselt für.*$", "\\1", line_clean)
        # Rest wie gehabt
        betrag <- sub(".*wechselt für ([0-9\\.]+) von .*", "\\1", line_clean)
        besitzer <- sub(".*von (.*?) zu (.*)", "\\1", line_clean)
        hoechstbietender <- sub(".*von (.*?) zu (.*)", "\\2", line_clean)
        hoechstbietender <- sub("\\..*$", "", hoechstbietender)
        zweitgebot <- ""
        zweitbietender <- ""
        if (i < length(lines) && grepl("Das zweithöchste Angebot", lines[i + 1])) {
          zweitline <- lines[i + 1]
          zweitgebot <- sub(".*betrug ([0-9\\.]+) von (.*)\\.", "\\1", zweitline)
          zweitbietender <- sub(".*betrug [0-9\\.]+ von (.*)\\.", "\\1", zweitline)
        }
        transfers[[length(transfers) + 1]] <- c(
          ifelse(is.na(akt_datum), "", akt_datum),
          spieler, besitzer, betrag, hoechstbietender, zweitgebot, zweitbietender
        )
      }
    }
    
    if (length(transfers) > 0) {
      out <- sapply(transfers, function(x) paste(x, collapse = ", "))
      out <- c("Datum, Spieler, Besitzer, Hoechstgebot, Hoechstbietender, Zweitgebot, Zweitbietender", out)
      parsed_text_val(paste(out, collapse = "\n"))
      output$parsed_transfers <- renderText(parsed_text_val())
    } else {
      parsed_text_val("Keine Transfers gefunden.")
      output$parsed_transfers <- renderText("Keine Transfers gefunden.")
    }
  })
  
  # Copy-Button (nur anzeigen wenn Output vorhanden)
  output$copy_button_ui <- renderUI({
    req(parsed_text_val())
    actionButton("copy_transfers", "Copy output to clipboard", icon = icon("clipboard"))
  })
  
  observeEvent(input$copy_transfers, {
    session$sendCustomMessage(type = 'copyText', message = parsed_text_val())
  })
}

# SHINY APP STARTEN
shinyApp(ui, server)

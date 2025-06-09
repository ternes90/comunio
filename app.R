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
      #helpText("Beide Dateien laden. Es wird ausschließlich der Marktwert vom Vortag (oder davor) verwendet!"),
      checkboxInput("show_table", "Zeige zusammengefasste MW-Klassen-Statistik", value = FALSE),
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
        tabPanel("Bieter-Profile (Punkte)", plotOutput("beeswarm", height = 700)),
        tabPanel("MW-Klassen Plot", plotOutput("mwclassplot", height = 700)),
        tabPanel("Zusammenfassung", DTOutput("mwclass_summary")),
        tabPanel("Entwicklung über Zeit", plotOutput("trendplot", height = 700)),
        tabPanel("MW-Entwicklung (alle Spieler)", plotOutput("mw_evolution", height = 600)),
        tabPanel("Flip-Gesamtsumme je Spieler", plotOutput("flip_summarybar", height = 600)),
        tabPanel("Flip-Gewinne (kumuliert)", plotOutput("flip_cumulative", height = 600)),
        tabPanel("Flip-Ergebnis-Verhältnis", plotOutput("flip_effizienz", height = 600)),
        tabPanel("Flip-Historie je Spieler", 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("flip_player_select", "Spieler auswählen:", choices = NULL)
                   ),
                   mainPanel(
                     DTOutput("flip_player_table")
                   )
                 )
        )
        
        
        
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  data_all <- reactive({
    transfers <- readxl::read_excel("TRANSFERS_all.xlsx") %>%
      mutate(Datum = as.Date(Datum)) %>%
      mutate(Hoechstgebot = ifelse(Datum == as.Date("2025-05-30") & Spieler == "Hranáč", 166000, Hoechstgebot))
    
    transfermarkt <- readxl::read_excel("TM_all.xlsx") %>%
      mutate(TM_Stand = as.Date(TM_Stand, format = "%d.%m.%Y"))
    
    list(transfers = transfers, transfermarkt = transfermarkt)
  })
  
  
  observe({
    updateSelectInput(session, "flip_player_select",
                      choices = sort(unique(flip_data()$Besitzer)))
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
  
  ## ---- MW-Klassen Boxplot+Beeswarm ----
  output$mwclassplot <- renderPlot({
    req(nrow(gebotsprofil_mwclass()) > 0)
    
    plotdata <- gebotsprofil_mwclass() %>%
      filter(!is.na(Diff_Prozent)) %>%
      mutate(
        MW_Klasse = factor(MW_Klasse, levels = c(
          "<0.5 Mio", "0.5–1 Mio", "1–2.5 Mio", "2.5–5 Mio", "5–10 Mio", ">10 Mio"
        ))
      )
    
    
    # Farbpalette (alphabetisch sortierte Bieternamen)
    bieter_levels <- sort(unique(plotdata$Bieter))
    farben <- scales::hue_pal()(length(bieter_levels))
    names(farben) <- bieter_levels
    
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
      
      facet_grid(. ~ MW_Klasse, scales = "free_y") +
      
      labs(
        title = "Gebotsabweichungen je Konkurrent und MW-Klasse",
        x = "Bieter",
        y = "Abweichung vom MW Vortag (%)"
      ) +
      
      scale_color_manual(values = farben) +
      scale_fill_manual(values = farben) +
      
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
  })
  
  # -- Punktplot mit Facets (Bieter, Typ)
  output$beeswarm <- renderPlot({
    req(nrow(gebotsprofil_clean()) > 0)
    plotdata <- gebotsprofil_clean() %>%
      filter(!is.na(Diff_Prozent))
    # Mittelwert über beide Typen je Bieter
    mean_total <- plotdata %>%
      group_by(Bieter) %>%
      summarise(Mean_total = mean(Diff_Prozent), .groups = "drop")
    # Mittelwerte je Typ
    medians <- plotdata %>%
      group_by(Bieter, Typ) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    ggplot(plotdata, aes(x = Typ, y = Diff_Prozent, color = Typ)) +
      geom_boxplot(aes(fill = Typ), width = 0.5, outlier.shape = NA, alpha = 0.25) +
      geom_beeswarm(cex = 2, size = 2.5, alpha = 0.8) +
      # Gesamter Mittelwert als gestrichelte Linie und Text
      geom_hline(
        data = mean_total,
        aes(yintercept = Mean_total),
        linetype = "dashed", color = "grey80", linewidth = 0.9,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = mean_total,
        aes(x = 0.5, y = Mean_total, label = round(Mean_total, 1)),
        color = "#9b2226", # Dunkelrot
        fontface = "bold",
        size = 4,
        vjust = -0.7,
        inherit.aes = FALSE
      ) +
      # Mittelwert je Typ als Text (wie gehabt)
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

  # ---- Zusammenfassung als Tabelle ohne MW Klassen ----
  output$mwclass_summary <- renderDT({
    if (input$show_table) {
      datatable(mwclass_summary())
    } else {
      dat <- gebotsprofil_clean() %>%
        group_by(Bieter) %>%
        summarise(
          Anzahl_gesamt = n(),
          Anzahl_Hoechstgebote = sum(Typ == "Hoechstgebot"),
          Anzahl_Zweitgebote = sum(Typ == "Zweitgebot"),
          O_Abweichung = round(mean(Diff_Prozent, na.rm = TRUE), digits = 1),
          Min = min(Diff_Prozent, na.rm = TRUE),
          Max = max(Diff_Prozent, na.rm = TRUE),
          Eintraege = paste0(Spieler, ": ", Diff_Prozent, "% (", Typ, " am ", format(Datum, "%d.%m.%Y"), ")", collapse = "; ")
        )
      datatable(dat)
    }
  })
  
  # ---- Marktwert-Entwicklung aller Spieler (normiert auf ersten Wert) ----
  mw_evolution_data <- reactive({
    tm <- data_all()$transfermarkt %>%
      mutate(TM_Stand = as.Date(TM_Stand))
    # Für jeden Spieler: erster MW als Start
    tm %>%
      group_by(Spieler) %>%
      arrange(TM_Stand) %>%
      mutate(MW_rel = Marktwert / first(Marktwert)) %>%
      ungroup()
  })
  
  output$mw_evolution <- renderPlot({
    df <- mw_evolution_data()
    # Tagesmittel und SD über alle Spieler
    plotdata <- df %>%
      group_by(TM_Stand) %>%
      summarise(
        MW_mean = mean(MW_rel, na.rm = TRUE),
        MW_sd = sd(MW_rel, na.rm = TRUE),
        .groups = "drop"
      )
    ggplot(plotdata, aes(x = TM_Stand, y = MW_mean)) +
      geom_ribbon(aes(ymin = MW_mean - MW_sd, ymax = MW_mean + MW_sd), fill = "#b3cde0", alpha = 0.4) +
      geom_line(color = "#005c99", linewidth = 1.1) +
      geom_point(color = "#005c99", size = 1.5, alpha = 0.7) +
      labs(
        title = "Durchschnittliche (normierte) Marktwertentwicklung aller TM-Spieler",
        subtitle = "Relativ zum ersten dokumentierten MW je Spieler · Schattierung = ±1 SD",
        x = "Datum",
        y = "MW relativ zum Startwert"
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  # ---- Transfernews Converter (inkl. Copy-to-Clipboard Button) ----
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
        strip.text = element_text(face = "bold")
      )
  })
  
  # ---- FLIP-DATEN vorbereiten ----
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
  
  
  # ---- FLIP-GESAMTSUMME (Balkendiagramm) ----
  # -- Flip-Gewinne pro Spieler (Beeswarm & Boxplot)
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
      labs(
        title = "Flip-Gewinn/Verlust je Comunio-Spieler (gesamt)",
        x = "Spieler",
        y = "Gewinn/Verlust (€)"
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  # ---- FLIP-KUMULIERT über Zeit ----
  # -- Flip-Gewinne über Zeit (kumuliert)
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
      scale_color_brewer(palette = "Dark2") +
      labs(
        title = "Kumulierte Flip-Gewinne über die Zeit je Spieler",
        x = "Datum",
        y = "Kumulierte Gewinne (€)",
        color = "Spieler"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # ---- FLIP-EFFIZIENZ (mit Kategorien) ----
  output$flip_effizienz <- renderPlot({
    req(nrow(flip_data()) > 0)
    
    flip_data() %>%
      mutate(
        Flip_Kategorie = case_when(
          abs(Gewinn) < 0.5e5 ~ "Mini-Flip <50k",
          abs(Gewinn) < 2.5e5 ~ "Mittel-Flip <250k",
          abs(Gewinn) >= 5e5 ~ "Mega-Flip ≥500k"
        ),
        Flip_Ergebnis = ifelse(Gewinn >= 0, "Gewinn", "Verlust"),
        Flip_Label = paste(Flip_Ergebnis, Flip_Kategorie, sep = " - ")
      ) %>%
      count(Besitzer, Flip_Label) %>%
      ggplot(aes(x = Besitzer, y = n, fill = Flip_Label)) +
      geom_col(position = "stack") +
      scale_fill_manual(
        values = c(
          "Gewinn - Mini-Flip <50k" = "#a8e6a1",
          "Gewinn - Mittel-Flip <250k" = "#4caf50",
          "Gewinn - Mega-Flip ≥500k" = "#1b5e20",
          "Verlust - Mini-Flip <50k" = "#fbb4b9",
          "Verlust - Mittel-Flip <250k" = "#e41a1c",
          "Verlust - Mega-Flip ≥500k" = "#67000d"
        )
      ) +
      labs(
        title = "Flip-Ergebnis nach Kategorien je Spieler",
        x = "Spieler",
        y = "Anzahl Flips",
        fill = "Ergebnis - Kategorie"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  
  # ---- FLIP-HISTORIE je Spieler ----
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
  
  
  
}

# SHINY APP STARTEN
shinyApp(ui, server)

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
ui <- navbarPage(
  "Comunio Analyse",
  
  ## ---- Bieterprofile ----
  tabPanel("Bieterprofile",
           tabsetPanel(
             tabPanel("Punkte", plotOutput("beeswarm", height = 700)),
             tabPanel("MW-Klassen", plotOutput("mwclassplot", height = 700)),
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
                        style = "display: flex; justify-content: center; align-items: center; margin-bottom: 20px;",
                        tags$input(
                          id = "show_table",
                          type = "checkbox",
                          style = "width: 20px; height: 20px; margin: 0 10px 0 0; vertical-align: middle; margin-top: 20px;"
                        ),
                        tags$label(
                          "Zeige zusammengefasste MW-Klassen-Statistik",
                          `for` = "show_table",
                          style = "font-weight: 500; font-size: 16px; white-space: nowrap; vertical-align: middle; margin: 0;margin-top: 20px;"
                        )
                      ),
                      DTOutput("mwclass_summary")
             )
             
             
             
           )
  ),
  
  ## ---- Marktwert-Entwicklung ----
  tabPanel("Marktwert-Entwicklung",
           tabsetPanel(
             tabPanel("MW-Verlauf (alle)", 
                      checkboxInput("show_sommerpause", "Zeige `24 Sommerpause-Kurve (Overlay)", value = TRUE),
                      checkboxInput("show_sommerpause_21", "Zeige `21 Sommerpause-Kurve (Overlay)", value = TRUE),
                      plotOutput("mw_evolution", height = 600)
             )
           )
  ),
  
  ## ---- Flip-Analyse ----
  tabPanel("Flip-Analyse",
           tabsetPanel(
             tabPanel("Gesamtsumme", plotOutput("flip_summarybar", height = 600)),
             tabPanel("Kategorien (gestapelt)", plotOutput("flip_effizienz", height = 600)),
             tabPanel("Kumuliert", plotOutput("flip_cumulative", height = 600)),
             tabPanel("Kumuliert nach Flip-Art", plotOutput("flip_cumcat", height = 600)),
             tabPanel("Historie je Spieler", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("flip_player_select", "Spieler auswählen:", choices = NULL)
                        ),
                        mainPanel(
                          DTOutput("flip_player_table")
                        )
                      )
             ),
             tabPanel("Kapitalübersicht",
                      DTOutput("kapital_uebersicht_table"),
             )
             
           )
  ),
  
  ## ---- Tools ----
  tabPanel("Tools & Import",
           h4("Transfer-Annulierungen, Boni & Strafen convert"),
           textAreaInput("transactions_text", "Text für Annulierungen / Boni / Strafen einfügen", height = "300px"),
           actionButton("parse_transactions", "Konvertieren"),
           verbatimTextOutput("parsed_transactions_output")
           
  )
)


# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- DATEN / df / list ----
  
  ## ---- sommerpause_df ----
  sommerpause_df <- readr::read_csv("MW_Sommerpause_2024.csv") %>%
    mutate(
      # Erst Datum als Date parsen
      Datum = as.Date(x),
      # Dann Jahr auf 2025 setzen
      Datum = as.Date(format(Datum, "2025-%m-%d")),
    ) %>%
    filter(Datum >= as.Date("2025-06-02")) %>%
    arrange(Datum) %>%
    mutate(
      MW_startwert = y[Datum == as.Date("2025-06-02")][1],
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
    filter(Datum >= as.Date("2025-06-03")) %>%
    arrange(Datum) %>%
    mutate(
      MW_startwert = y[Datum == as.Date("2025-06-03")][1],
      MW_rel_normiert = y / MW_startwert
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
    transfers <- read.csv("TRANSFERS_all.csv", sep = ";", na.strings = c("", "NA")) %>%
      mutate(
        Datum = as.Date(Datum, format = "%d.%m.%Y"),
        Hoechstgebot = as.numeric(Hoechstgebot),
        Zweitgebot = as.numeric(Zweitgebot),
        Hoechstgebot = ifelse(Datum == as.Date("2025-05-30") & Spieler == "Hranáč", 166000, Hoechstgebot)
      )
    
    transfermarkt <- readr::read_csv2("TRANSFERMARKT.csv") %>%
      mutate(TM_Stand = as.Date(TM_Stand, format = "%d.%m.%Y"))
    
    list(transfers = transfers, transfermarkt = transfermarkt)
  })
  
  standings_df <- reactive({
    req(file.exists("STANDINGS.csv"))
    readr::read_csv2("STANDINGS.csv", col_types = cols(
      Manager = col_character(),
      Teamwert = col_double()
    ))
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
  
  # ## ---- flip_kategorien_data ----
  # flip_kategorien_data <- reactive({
  #   req(flip_data())
  #   flip_data() %>%
  #     mutate(
  #       Flip_Kategorie = case_when(
  #         abs(Gewinn) < 0.5e5 ~ "Mini-Flip <50k",
  #         abs(Gewinn) < 2.5e5 ~ "Mittel-Flip <250k",
  #         abs(Gewinn) >= 5e5 ~ "Mega-Flip ≥500k",
  #         TRUE ~ "Sonst"
  #       ),
  #       Flip_Ergebnis = ifelse(Gewinn >= 0, "Gewinn", "Verlust"),
  #       Kategorie_Label = paste(Flip_Ergebnis, Flip_Kategorie, sep = " - ")
  #     )
  # })
  
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
  
  # ---- BIETERPROFILE ----
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
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired") +
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
    if (!is.null(input$show_table) && input$show_table) {
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
 
  # ---- MARKTWERTENTWICKLUNG ----
  ## ---- Besitzhistorie bauen ----
  besitzhistorie <- reactive({
    transfers <- data_all()$transfers
    
    # alle relevanten Wechsel (sortiert)
    wechsel <- transfers %>%
      arrange(Spieler, Datum) %>%
      select(Spieler, Datum, Hoechstbietender) %>%
      rename(Besitzer = Hoechstbietender)
    
    # initialen Eintrag pro Spieler (erste bekannte Besitzperiode)
    first_transfer <- wechsel %>%
      group_by(Spieler) %>%
      slice(1) %>%
      mutate(Startdatum = Datum, Enddatum = as.Date("2100-01-01")) %>%
      ungroup()
    
    # alle Wechselpaare als Besitzzeiträume
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
        Besitzer_eff = nickname_mapping[Besitzer]  # Mapping HIER anwenden!
      )
    
    besitz <- besitzhistorie()
    
    # Join + Filter
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
    
    p <- ggplot(plotdata, aes(x = TM_Stand, y = MW_mean)) +
      geom_ribbon(aes(ymin = MW_mean - MW_sd, ymax = MW_mean + MW_sd), fill = "#b3cde0", alpha = 0.4) +
      geom_line(color = "#005c99", linewidth = 1.1) +
      geom_point(color = "#005c99", size = 1.5, alpha = 0.7) +
      labs(
        title = "Durchschnittliche (normierte) Marktwertentwicklung aller TM-Spieler",
        subtitle = "Relativ zum ersten dokumentierten MW je Spieler · Schattierung = ±1 SD",
        x = "Datum",
        y = "MW relativ zum Startwert"
      ) +
      geom_vline(xintercept = as.Date("2025-08-22"), linetype="dotted", 
                 color = "darkred", size=1.5) +
      theme_minimal(base_size = 14)
    
    if (isTRUE(input$show_sommerpause)) {
      p <- p + geom_line(
        data = sommerpause_df,
        aes(x = Datum, y = MW_rel_normiert),
        color = "red",
        linewidth = 1.3,
        linetype = "dashed"
      )
    }
    
    if (isTRUE(input$show_sommerpause_21)) {
      p <- p + geom_line(
        data = sommerpause_21_df,
        aes(x = Datum, y = MW_rel_normiert),
        color = "orange",
        linewidth = 1.3,
        linetype = "dashed"
      )
    }
    
    
    
    
    
    
    p
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
      labs(
        title = "Flip-Gewinn/Verlust je Comunio-Spieler (gesamt)",
        x = "Spieler",
        y = "Gewinn/Verlust (€)"
      ) +
      theme_minimal(base_size = 14)
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
        title = "Flip-Ergebnis nach Kategorien je Spieler",
        x = "Spieler",
        y = "Anzahl Flips",
        fill = "Ergebnis - Kategorie"
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
        title = "Kumulierte Flip-Gewinne/Verluste je Spieler und Flip-Art",
        x = "Spieler",
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
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
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
  
  ## ---- Kontostände je Spieler ----
  
  # Hilfsfunktion: Nur Vornamen extrahieren
  vorname <- function(name) {
    strsplit(name, " ")[[1]][1]
  }
  
  # Fixe Startkapitalwerte (vollständige Namen als Keys)
  startkapital_fix <- c(
    "Alfons" = 45000000 - 30770000,
    "Nico" = 45000000 - 29470000,
    "Andreas" = 45000000 - 29210000,
    "Pascal" = 45000000 - 29200000,
    "Thomas" = 45000000 - 28170000,
    "Christoph" = 45000000 - 27360000,
    "Christian" = 45000000 - 26860000,
    "Dominik" = 45000000 - 26960000
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
    transactions <- readxl::read_excel("TRANSACTIONS_all.xlsx") %>%
      mutate(Spieler = as.character(Spieler)) %>%
      group_by(Spieler) %>%
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
      formatStyle(
        'Aktuelles_Kapital',
        backgroundColor = styleInterval(0, c('salmon', NA)),
        fontWeight = styleInterval(0, c('bold', NA))
      )
  })
  
  
  # ---- TOOLS & IMPORT ----
  ## ---- Convert Transfernews ----
  # Gemeinsame Hilfsfunktion zur Datumskonvertierung
  format_datum <- function(datumsstring) {
    teile <- unlist(strsplit(datumsstring, "\\."))
    if(length(teile) == 3) {
      tag <- teile[1]
      monat <- teile[2]
      jahr <- teile[3]
      if(nchar(jahr) == 2) jahr <- paste0("20", jahr)
      sprintf("%02d.%02d.%s", as.integer(tag), as.integer(monat), jahr)
    } else {
      NA
    }
  }

  ## ---- Convert Boni & Strafen ----
  parsed_transactions_val <- reactiveVal("")
  
  observeEvent(input$parse_transactions, {
    req(input$transactions_text)
    lines <- unlist(strsplit(input$transactions_text, "\n"))
    out <- list()
    aktuelles_datum <- NA
    
    for (line in lines) {
      line <- trimws(line)
      
      if (tolower(line) == "heute") {
        aktuelles_datum <- format(Sys.Date(), "%d.%m.%Y")
        next
      }
      
      if (grepl("^\\d{2}\\.\\d{2}\\.\\d{2,4}$", line)) {
        aktuelles_datum <- format_datum(line)
        next
      }
      
      if (grepl("Transfer des Spielers .* annulliert von", line)) {
        m <- regmatches(line, regexec("Transfer des Spielers (.*?) für ([0-9\\.]+) EUR .* annulliert von (.*)", line))[[1]]
        if (length(m) == 4) {
          spieler <- m[2]
          betrag <- as.numeric(gsub("\\.", "", m[3]))
          begruendung <- paste("Transfer annulliert von", m[4])
          out[[length(out) + 1]] <- paste(aktuelles_datum, spieler, -betrag, begruendung, sep = ";")
        }
        next
      }
      
      if (grepl("Gutschrift: .* gutgeschrieben\\. Begründung:", line)) {
        m <- regmatches(line, regexec("Gutschrift: ([0-9\\.]+) wurden (.*?) vom Communityleiter .* gutgeschrieben\\. Begründung: (.*)", line))[[1]]
        if (length(m) == 4) {
          betrag <- as.numeric(gsub("\\.", "", m[2]))
          spieler <- m[3]
          begruendung <- paste("Bonus:", m[4])
          datum <- ifelse(is.na(aktuelles_datum), format(Sys.Date(), "%d.%m.%Y"), aktuelles_datum)
          out[[length(out) + 1]] <- paste(datum, spieler, betrag, begruendung, sep = ";")
        }
        next
      }
      
      if (grepl("Disziplinarstrafe: .* abgezogen\\. Begründung:", line)) {
        m <- regmatches(line, regexec("Disziplinarstrafe: ([0-9\\.]+) wurden (.*?) .* abgezogen\\. Begründung: (.*)", line))[[1]]
        if (length(m) == 4) {
          betrag <- -as.numeric(gsub("\\.", "", m[2]))
          spieler <- m[3]
          begruendung <- paste("Disziplinarstrafe:", m[4])
          datum <- ifelse(is.na(aktuelles_datum), format(Sys.Date(), "%d.%m.%Y"), aktuelles_datum)
          out[[length(out) + 1]] <- paste(datum, spieler, betrag, begruendung, sep = ";")
        }
        next
      }
    }
    
    if (length(out) == 0) {
      parsed_transactions_val("Keine gültigen Einträge gefunden.")
    } else {
      header <- "Datum;Spieler;Transaktion;Begründung"
      parsed_transactions_val(paste(c(header, out), collapse = "\n"))
    }
  })
  
  output$parsed_transactions_output <- renderText({
    parsed_transactions_val()
  })
  
  
}





# SHINY APP STARTEN
shinyApp(ui, server)

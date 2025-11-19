library(shiny)
library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(readxl)
library(DT)
library(scales)
library(later)
# Installiere nur auf Posit Connect/Cloud, sonst nur laden
is_connect <- dir.exists("/opt/connect") ||
  identical(Sys.info()[["user"]], "connect") ||
  startsWith(normalizePath(getwd(), winslash = "/"), "/cloud/")

if (is_connect && !requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly", repos = "https://cran.rstudio.com")
}
suppressPackageStartupMessages(library(plotly))
# Installiere nur auf Posit Connect/Cloud, sonst nur laden
is_connect <- dir.exists("/opt/connect") ||
  identical(Sys.info()[["user"]], "connect") ||
  startsWith(normalizePath(getwd(), winslash = "/"), "/cloud/")

if (is_connect && !requireNamespace("slider", quietly = TRUE)) {
  install.packages("slider", repos = "https://cran.rstudio.com")
}
suppressPackageStartupMessages(library(slider))

# ---- Globale Daten ----

#Startup log
LOG_PATH <- "logs/perf_log.csv"
if (!dir.exists(dirname(LOG_PATH))) dir.create(dirname(LOG_PATH), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(LOG_PATH)) {
  writeLines("timestamp;session;dt_to_tab_s;dt_to_flush_s", LOG_PATH)
}

#Save beeswarm position
pos_qr <- ggbeeswarm::position_quasirandom(width = 0.22)

## ---- Manuelle MW Werte ----
manuelle_werte <- tibble(
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

## ---- logo_map ----
logo_map <- c(
  "1. FC Köln" = "1 FC Köln.png",
  "Bayer 04 Leverkusen" = "Bayer Leverkusen.png",
  "Borussia Dortmund" = "Borussia Dortmund.png",
  "Borussia Mönchengladbach" = "Borussia Mönchengladbach.png",
  "Eintracht Frankfurt" = "Eintracht Frankfurt.png",
  "FC Augsburg" = "FC Augsburg.png",
  "FC Bayern München" = "FC Bayern München.png",
  "Hamburger SV" = "Hamburger SV.png",
  "1. FC Heidenheim 1846" = "Heidenheim.png",
  "RB Leipzig" = "Leipzig.png",
  "1. FSV Mainz 05" = "Mainz 05.png",
  "Sport-Club Freiburg" = "SC Freiburg.png",
  "FC St. Pauli" = "St Pauli.png",
  "SV Werder Bremen" = "SV Werder Bremen.png",
  "TSG Hoffenheim" = "TSG Hoffenheim.png",
  "1. FC Union Berlin" = "Union Berlin.png",
  "VfB Stuttgart" = "VfB Stuttgart.png",
  "VfL Wolfsburg" = "VfL Wolfsburg.png"
)

## ---- Startkapitalwerte ---- 
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

## ---- sp_tm Spielplan 2025 ----
sp_tm <- read.csv(
  "data/spielplan_TM_2025.csv",
  sep = ";", header = TRUE, stringsAsFactors = FALSE,
  fileEncoding = "UTF-8", check.names = FALSE, na.strings = c("", "NA")
) %>%
  transmute(
    Spieltag = as.integer(Spieltag),
    Datum    = as.Date(Datum),
    Heim     = trimws(enc2utf8(Heim)),
    Gast     = trimws(enc2utf8(Gast))
  ) %>%
  distinct() %>%
  arrange(Spieltag, Datum, Heim, Gast)

## ---- seasons ----
data_path <- "./global_MW"

seasons <- c("2004-05", "2005-06", "2006-07", "2007-08", "2008-09",
             "2009-10", "2010-11", "2011-12", "2012-13", "2013-14",
             "2014-15", "2015-16", "2016-17", "2017-18", "2018-19",
             "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25", "Sommerpause_2021", "Sommerpause_2024")

# Nur die regulären Saisons (ohne Sommerpause)
real_seasons <- seasons[!grepl("^Sommerpause", seasons)]
n_real       <- length(real_seasons)



## ---- helpers ----
# Spieler-Info reader
safe_val <- function(v) ifelse(length(v) == 0 || is.na(v) || !nzchar(as.character(v)), "", as.character(v))
safe_img <- function(url, width_px) {
  url <- safe_val(url)
  if (!nzchar(url)) return(NULL)
  tags$img(src = url, width = width_px, onerror = "this.style.display='none'")
}

# Helper function für LI-Mapping 
norm_key <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(trimws(x))
  x <- chartr("ß", "ss", x)
  x
}

# Funktion MW vom Vortag suchen 
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

# Hilfsfunktion, um auf Transfermark  t pro Zeile einen Shiny-Button zu erzeugen
shinyButton <- function(id, label = "→") {
  sprintf(
    '<button class="btn btn-xs btn-primary" onclick="Shiny.setInputValue(\'transfer_row\', %s, {priority: \'event\'})">%s</button>',
    id, label
  )
}

# GPT prompt Spieler-Info-Seite
build_prompt <- function(sp, ve) {
  # Fallback-Konverter: falls ein Argument eine Funktion ist, deparse als Fallback
  safe_chr <- function(x) {
    if (is.function(x)) return(deparse(substitute(x)))
    if (is.null(x)) return("")
    as.character(x)
  }
  
  sp_chr  <- safe_chr(sp)
  ve_chr  <- safe_chr(ve)
  today_s <- format(Sys.Date(), "%Y-%m-%d")
  
  parts <- c(
    paste0("Aufgabe: Analysiere den Bundesligaspieler ", sp_chr, " (", ve_chr, ").\n"),
    paste0("Heutiges Datum (Europe/Berlin): ", today_s, ". Nutze NUR Inhalte der letzten 30 Tage; ältere als 'älter' kennzeichnen. Zeitbezüge strikt relativ zu diesem Datum.\n"),
    "Quellen: Vereinsseiten, bundesliga.com, dfb.de, kicker.de, transfermarkt.de, LigaInsider, Comunio-Magazin, seriöse Regionalmedien. Keine Social Media/Foren. Jede Aussage mit [1], [2] … belegen; jüngste Quelle gewinnt. Unklares als 'Unklar'.\n",
    "Bestimme den nächsten Bundesligaspieltag inkl. Gegner, Datum, Ort aus offizieller Quelle.\n\n",
    "Liefere GENAU diese Abschnitte:\n",
    "- Status jetzt:\n",
    "- Einsatz 1–2 Wochen:\n",
    "- Trainerstimme:\n",
    "- Verletzung / Rückkehr:\n",
    "- Aufstellungsempfehlung:\n",
    "- Gegneranalyse nächster Spieltag:\n",
    "- Quellen (YYYY-MM-DD):"
  )
  
  # sichere Verkettung
  paste0(vapply(parts, safe_chr, FUN.VALUE = character(1)), collapse = "")
}


make_links <- function(x){
  # 1) Markdown-Links -> HTML
  x <- gsub("\\[([^\\]]+)\\]\\((https?://[^)\\s]+)\\)",
            "<a href='\\2' target='_blank'>\\1</a>", x, perl=TRUE)
  
  # 2) Nackte URLs -> HTML (breiter Zeichensatz, ohne bereits verlinkte)
  pat <- "(?<!href=['\"])\\bhttps?://[A-Za-z0-9\\-._~:/?#\\[\\]@!$&'()*+,;=%]+"
  m <- gregexpr(pat, x, perl=TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(urls){
    vapply(urls, function(u){
      # evtl. angehängte Satzzeichen am Ende abschneiden
      u2 <- sub("[)\\]\\},.?!:;]+$", "", u)
      sprintf("<a href='%s' target='_blank'>%s</a>", u2, u2)
    }, character(1))
  })
  x
}

# Helper function für Alle Teams Rechner
safe_id <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
cb_id   <- function(manager, spieler) paste0("sel__", safe_id(manager), "__", safe_id(spieler))

# Load hist seasons
load_season_data <- function(season) {
  file_name <- file.path(data_path, paste0("historic_market_values_", season, ".csv"))
  if (!file.exists(file_name)) return(NULL)
  readr::read_csv(file_name, show_col_types = FALSE) %>%
    # Spalte für Jahresanfang in 2025 nur für „Sommerpause“ umschreiben
    mutate(
      Datum_raw = as.Date(Datum),
      Datum = if (grepl("^Sommerpause", season)) {
        as.Date(format(Datum_raw, "2025-%m-%d"))
      } else {
        Datum_raw
      },
      Saison = season
    )
}

# Pfeil für Kader-Entwicklung
arrow_for <- function(a, b) {
  if (is.na(a) || is.na(b)) return("")
  if (b > a) return("<span style='color:#388e3c;font-weight:bold;'>▲</span>")
  if (b < a) return("<span style='color:#e53935;font-weight:bold;'>▼</span>")
  return("<span style='color:#757575;font-weight:bold;'>▬</span>")
}

# Hilfsfunktion für sicheres Escaping in HTML-Attributen in MEIN TEAM
safe_attr <- function(x) htmltools::htmlEscape(x, attribute = TRUE)

# Hilfsfunktion: Nur Vornamen extrahieren
vorname <- function(name) {
  strsplit(name, " ")[[1]][1]
}

# Hilfsfunktion: Marktwert vorhersagen
predict_mw_in_3d <- function(current_mw, punkte_event, summary_df) {
  if (is.na(current_mw) || is.na(punkte_event) || nrow(summary_df) == 0) return(NA_real_)
  
  # MW-Klasse basierend auf aktuellem Marktwert:
  mw_klasse <- dplyr::case_when(
    current_mw < 5e5   ~ "<0.5 Mio",
    current_mw < 1e6   ~ "0.5–1 Mio",
    current_mw < 2.5e6 ~ "1–2.5 Mio",
    current_mw < 5e6   ~ "2.5–5 Mio",
    current_mw < 1e7   ~ "5–10 Mio",
    TRUE               ~ ">10 Mio"
  )
  
  sub <- summary_df %>% filter(MW_Klasse == mw_klasse)
  
  # Fallback: wenn für diese MW-Klasse keine Daten vorliegen, nimm alle
  if (nrow(sub) == 0) sub <- summary_df
  
  if (nrow(sub) == 0) return(NA_real_)
  
  # Punktzahl als numeric
  p_val <- suppressWarnings(as.numeric(punkte_event))
  if (is.na(p_val)) return(NA_real_)
  
  # nächstliegende Punktegruppe suchen
  row <- sub %>%
    mutate(dist = abs(Punkte - p_val)) %>%
    arrange(dist) %>%
    slice(1)
  
  delta_pct <- row$mean_delta3[1]
  if (is.na(delta_pct)) return(NA_real_)
  
  current_mw * (1 + delta_pct / 100)
}


# ---- UI ----
ui <- navbarPage(
  "Comunio Analyse", 
  id = "main_navbar",
  
  ## ---- htmls-tags ----
  #Pushaktualisierung
  header = tagList(
    tags$div(
      style = "text-align: right; font-size: 12px; color: grey; margin: 5px;",
      span("Letztes Update: "),
      textOutput("last_update", inline = TRUE)
    ),
    
    tags$head(
      # --- Click-Event ---
      tags$script(HTML("
        $(document).on('click', '#kapital_uebersicht_table tbody td', function() {
          Shiny.setInputValue('kapital_table_cell_clicked', Math.random());
        });
      ")),
      
      # --- Tabellen linksbündig ---
      tags$style(HTML("
        #kreditrahmen_uebersicht_preview table.dataTable td,
        #kreditrahmen_uebersicht_preview table.dataTable th,
        #transfer_summary_today table.dataTable td,
        #transfer_summary_today table.dataTable th,
        #flip_summary_today table.dataTable td,
        #flip_summary_today table.dataTable th {
          text-align: left !important;
        }
      ")),
      
      # --- Abstände Tabs ---
      tags$style(HTML("
        .tab-content > .tab-pane { margin-top: 20px; margin-bottom: 50px; }
      ")),
      
      # --- DataTable selection/hover ---
      tags$style(HTML("
        :root { --dt-row-selected: transparent !important; }
        table.dataTable tbody tr.selected td,
        table.dataTable tbody td.selected {
          box-shadow: inset 0 0 0 9999px #D3E5FF !important;
          color: black !important;
        }
        table.dataTable tbody tr.selected td a,
        table.dataTable tbody td.selected a { color: black !important; }
        table.dataTable tbody tr:hover,
        table.dataTable tbody tr:hover td {
          background-color: #E8F2FF !important;
          color: inherit !important;
        }
      ")),
      
      # --- Copy-Prompt-Skript ---
      tags$script(HTML("
        (function(){
          function copySync(txt){
            var ta=document.createElement('textarea');
            ta.value=txt;
            ta.setAttribute('readonly','');
            ta.style.position='fixed'; ta.style.top='0'; ta.style.left='-9999px';
            document.body.appendChild(ta);
            ta.focus(); ta.select();
            var ok=false;
            try{ ok=document.execCommand('copy'); }catch(e){ ok=false; }
            document.body.removeChild(ta);
            return ok;
          }
          $(document).on('click','#copy_prompt',function(){
            var el=document.getElementById('gpt_prompt_preview');
            var txt=el ? (el.textContent || el.innerText || '') : '';
            var ok=copySync(txt);
            Shiny.setInputValue('copied_prompt',
              {ok: ok, len: txt.length, t: Date.now()},
              {priority:'event'});
            setTimeout(function(){
              window.open('https://chatgpt.com/g/g-p-683f0c4df880819194f9186282be1c2c-comunio-tipps-pro-player/project','_blank');
            }, 120);
          });
        })();
      ")),
      
      # --- GPT-Resultat-Box ---
      tags$style(HTML("
        #gpt_result_pre {
          white-space: pre-wrap; word-break: break-word;
          font-family: Calibri, 'Segoe UI', Arial, sans-serif;
          font-size: 18px; line-height: 1.55; color: #1a1a1a;
          background: #ffffff; border: 1px solid #e6e6e6;
          border-radius: 10px; padding: 14px 16px;
          box-shadow: 0 1px 2px rgba(0,0,0,.04);
        }
        #gpt_prompt_preview { white-space: pre-wrap; }
        #gpt_result_pre a { word-break: break-all; }
      ")),
      
      # --- Spieler-Card ---
      tags$style(HTML("
        .spieler-card {
          display:flex; gap:16px; align-items:center;
          border:1px solid #e5e7eb; border-radius:16px;
          padding:12px; margin:8px 0 16px 0;
        }
        .spieler-card img { border-radius:12px; object-fit:cover; }
        .spieler-card .meta { flex:1; }
        .spieler-card .verein {
          display:flex; align-items:center; gap:8px;
          font-weight:700; font-size:18px; margin-bottom:6px;
        }
        .spieler-card .list { margin:0; padding-left:16px; }
        .spieler-divider { height:1px; background:#e5e7eb; margin:10px 0 16px 0; }
      ")),
      
      # --- Mein Team Info-button ---
      tags$script(HTML("
        $(document).off('click.mkKader', '.mk-info-btn')
                   .on('click.mkKader',  '.mk-info-btn', function(e){
          e.preventDefault(); e.stopPropagation();
          var player = $(this).data('player');
          console.log('[mk-info-btn] player =', player);
          Shiny.setInputValue('mk_info_player', { name: player, t: Date.now() }, {priority:'event'});
        });
      ")),
      
      # --- Mein Team Prompt-GPT-button ---
      tags$script(HTML("
        (function(){
          var GPT_URL = 'https://chatgpt.com/g/g-p-683f0c4df880819194f9186282be1c2c-comunio-tipps-pro-player/project';
        
          function copySync(txt){
            var ta=document.createElement('textarea');
            ta.value=txt; ta.setAttribute('readonly','');
            ta.style.position='fixed'; ta.style.top='0'; ta.style.left='-9999px';
            document.body.appendChild(ta); ta.focus(); ta.select();
            var ok=false;
            try{ ok=document.execCommand('copy'); }catch(e){ ok=false; }
            document.body.removeChild(ta);
            return ok;
          }
        
          $(document).on('click','.mk-gpt-btn',function(){
            // Prompt frei definierbar in R → im data-attribute mitschicken
            var prompt = $(this).data('prompt') || '';
            copySync(prompt);
            setTimeout(function(){ window.open(GPT_URL,'_blank'); }, 120);
          });
        })();
        ")),
      tags$style(HTML("
        table.dataTable tbody td:first-child {
          text-align: left !important;
        }
      "))
    )
  ),
  
  ## ---- Dashboard ----
  tabPanel("Dashboard",
           fluidPage(
             
             div(
               style = "margin-bottom:16px; width:100%;",
               tags$div("News",
                        style = "text-align:center; font-size:16px; font-weight:bold; color:black; margin-bottom:8px;"),
               div(
                 style = paste(
                   "width:92%; max-width:720px; height:160px; margin:0 auto;",
                   "overflow-y:auto; border:1px solid #ddd; border-radius:6px; padding:8px;",
                   "background:#fafafa; text-align:left;"
                 ),
                 # überschreibt das alte Centering und setzt Row-Layout
                 tags$style(HTML("
                    #dashboard_news { 
                      display:flex; 
                      flex-direction:column; 
                      gap:2px; 
                      align-items:stretch; 
                      padding-left:20px;   /* Abstand vom linken Rand */
                    }
                    #dashboard_news .news-row { 
                      display:flex; 
                      align-items:flex-start; 
                      gap:10px; 
                      margin:4px 0; 
                      width:100%; 
                    }
                    #dashboard_news .col-spieler { min-width:180px; font-weight:600; }
                    #dashboard_news .col-rel    { min-width:90px; }
                    #dashboard_news .col-dt     { min-width:150px; }
                    #dashboard_news a.news-link { text-decoration:none; }
                    /* Mobiler Feinschliff */
                    @media (max-width: 600px) {
                      #dashboard_news .col-spieler { min-width:140px; }
                      #dashboard_news .col-dt      { min-width:130px; }
                    }
                  ")),
                 uiOutput("dashboard_news")
               )
             ),
             
             # Aktuelle Transfers - Zusammenfassung
             div(
               style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:12px; width:100%;",
               
               # Platzhalter links
               div(style = "width:120px;"),
               
               # Überschrift exakt mittig
               div(
                 style = "flex:1; text-align:center;",
                 tags$div("Aktuelle Transfers", 
                          style = "font-size:16px; font-weight:bold; color:black;")
               ),
               
               # Auswahlfeld rechts
               div(
                 style = "width:120px;",
                 selectInput("flip_day", label = NULL,
                             choices = c("Heute", "Gestern"),
                             selected = "Heute", width = "120px")
               )
             )
             ,
             
             div(
               div(
                 style = "margin-bottom:20px; width: 100%;",
                 DTOutput("transfer_summary_today", width = "100%")
               ),
               
               tags$div("Aktuelle Flips",
                        style = "text-align: center; font-size: 16px; font-weight: bold; color: black; margin-bottom: 6px;"),

               div(
                 style = "margin-bottom:20px; width: 100%;",
                 DTOutput("flip_summary_today", width = "100%")
               )
               
             ),
             
             # Marktwert-Zeitachse
             tags$div("Marktwerte", 
                      style = "text-align: center; font-size: 16px; font-weight: bold; color: black; margin-bottom: 10px;"),
             div(
               style = "margin-bottom: 20px;",
               plotOutput("mw_zeitachse_preview", height = 350, width = "100%", click = "mw_zeitachse_click")
             ),
             
             # Kreditrahmen-Übersicht
             tags$div("Kontostände", 
                      style = "text-align: center; font-size: 16px; font-weight: bold; color: black; margin-bottom: 10px;"),
             div(
               style = "margin-bottom: 20px;",
               DTOutput("kreditrahmen_uebersicht_preview", width = "100%")
             ),
             
             # Flip-Vorschau
             tags$div("Flip Übersicht", 
                      style = "text-align: center; font-size: 16px; font-weight: bold; color: black; margin-bottom: 10px;"),
             div(
               style = "margin-bottom: 20px;",
               plotOutput("flip_preview", height = 300, width = "100%", click = "flip_click")
             )
           )
  ),
  
  ## ---- Marktwert-Entwicklung ----
  tabPanel("Marktwert-Entwicklung",
           tabsetPanel(
             id = "mw_tabs",  # <— NEU
             tabPanel(title = "MW-Verlauf", value = "mw_verlauf",
                      div(
                        style = "text-align: center; margin-bottom: 10px;",
                        HTML("<b>Legende:</b><br>
             <span style='color:grey40; font-weight:bold;'>▍</span> Gesamtmarktwert (alle Spieler) &nbsp;&nbsp;
             <span style='color:orange; font-weight:bold;'>▍</span> Saison 2021/22 &nbsp;&nbsp;
             <span style='color:red; font-weight:bold;'>▍</span> Saison 2024/25" )
                        
                        # Für Sommerpause auskommentieren
                        # <span style='color:red; font-weight:bold;'>▍</span> Sommerpause 2024 &nbsp;&nbsp;  
                        # <span style='color:orange; font-weight:bold;'>▍</span> Sommerpause 2021")
                      ),
                      plotOutput("mw_evolution", height = 600),
                      br(),
                      plotOutput("mw_daily_change", height = 300)
             ),
             tabPanel(title = "MW-Verlauf 24/25 (MW-Klassen)", value = "mw_klassen",
                      plotOutput("mw_plot"),
                      plotOutput("mw_plot_now")
             ),
             tabPanel(title = "Hist. Saisonverläufe - Select", value = "mw_hist_sel",
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            column(3, actionButton("select_all", "Alle")),
                            column(3, actionButton("select_none", "Keine")),
                            column(3, actionButton("select_last3", "Custom 1")),
                            column(3, actionButton("select_custom", "Custom 2"))
                          ),
                          br(),
                          checkboxGroupInput("selected_seasons","Saisons auswählen zum Vergleich:", choices=NULL, selected=NULL)
                        ),
                        mainPanel(plotOutput("historical_seasons_plot_selected", height = 600))
                      )
             ),
             tabPanel(title = "MW-Analyse", value = "mw_analyse",
                      fluidRow(
                        column(
                          width = 12,
                          div(
                            style = "display: flex; justify-content: center; align-items: center; margin-bottom: 20px; margin-top: 20px;",
                          plotOutput("mw_weekday_effect_plot", height = "420px", width = "80%")
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 8,
                          plotOutput("mw_predict_plot", height = 360)
                        ),
                        column(
                          width = 4,
                          wellPanel(
                            tags$strong("2-Tages-Vorhersage – Einstellungen"),
                            checkboxInput("ref_use", "Analog-Boost aktiv", value = TRUE),
                            selectInput("ref_season", "Saison-Analog", choices = c("2021-22","2024-25"), selected = "2021-22"),
                            sliderInput("ref_weight", "Boost-Gewicht", min = 0, max = 1, value = 0.1, step = 0.05),
                            sliderInput("w_wd",  "Gewicht Wochentag",   min = 0, max = 1, value = 0.9,  step = 0.05),
                            sliderInput("w_perm","Gewicht lag1+lag2",   min = 0, max = 1, value = 0.05, step = 0.05),
                            sliderInput("w_l7",  "Gewicht lag7",        min = 0, max = 1, value = 0.05, step = 0.05),
                            helpText("Gewichte werden intern normalisiert.")
                          )
                        )
                      ),
                      fluidRow(
                        column(
                          width = 12,
                          div(
                            style = "width:100%; display:flex; justify-content:center;",
                            # Innen: zentriert und begrenzt
                            div(
                              style = "width:100%; max-width:1600px; margin:0 auto;",
                              # Dreispaltiges Layout
                              div(
                                style = "width:100%; display:flex; align-items:flex-start; justify-content:space-between; gap:20px; margin:20px 0; flex-wrap:nowrap;",
                                
                                # Links: Güte-Tabelle
                                div(
                                  style = "flex:1 1 0%; max-width:none;",
                                  wellPanel(style = "padding:12px;", uiOutput("mw_pred_ts_metrics"))
                                ),
                                
                                # Mitte: Zeitreihen-Plot
                                div(
                                  style = "flex:3 1 0%; max-width:none;",
                                  plotOutput("mw_pred_ts_plot", height = 420, width = "100%")
                                ),
                                
                                # Rechts: Prediction-vs-Ist
                                div(
                                  style = "flex:1 1 0%; max-width:none;",
                                  plotOutput("mw_pred_vs_actual_plot", height = 420, width = "100%")
                                )
                              )
                            )
                          )
                        )
                      )   
             )
           )
  )
  ,
  
  ## ---- Transfermarkt ----
  tabPanel("Transfermarkt",
           tabsetPanel(
             id = "transfermarkt_tabs",               # id
             tabPanel("Transfermarkt Details",
                      uiOutput("bid_prediction"),
                      uiOutput("blocked_managers"),
                      DTOutput("transfermarkt_detail")
             ),
             
             tabPanel(
               title = "Spieler-Info", value = "spieler_info",
               selectInput("spieler_select2", "Spieler:", choices = NULL),
               
               numericInput(
                 "hypo_punkte",
                 label = "Erwartete Punkte im nächsten Spiel",
                 value = 5, min = -10, max = 30, step = 1
               ),
               
               textOutput("hypo_mw_prognose"),
               
               div(
                 id = "spieler_container",
                 style = "display:flex; gap:16px; align-items:flex-start; width:100%; flex-wrap:wrap;",
                 
                 div(
                   style = "flex:1 1 300px; text-align:left; margin-left:50px;",
                   uiOutput("spieler_card_header")
                 ),
                 
                 div(
                   style = "flex:1 1 300px; text-align:left;",
                   tags$div("News",
                            style = "font-size:16px; font-weight:bold; color:black; margin-bottom:8px;"),
                   
                   div(
                     id = "spieler_news_box",
                     style = paste(
                       "width:100%; height:200px;",
                       "overflow-y:auto; -webkit-overflow-scrolling:touch;",
                       "border:1px solid #ddd; border-radius:6px; padding:8px;",
                       "background:#fafafa; text-align:left;"
                     ),
                     uiOutput("spieler_news")
                   )
                 ),
                 
                 tags$style(HTML("
                    /* Text soll nicht strecken */
                    #spieler_news_box * { overflow-wrap:anywhere; word-break:break-word; }
                
                    /* Mobil: Spalten untereinander, keine vertikale Dehnung */
                    @media (max-width: 700px) {
                      #spieler_container { flex-direction:column; align-items:stretch; }
                    }
                  "))
               ),
               
               
               div(
                 style = "display:flex; justify-content:space-around; align-items:flex-start; width:100%;",
                 
                 div(
                   style = "flex:1; text-align:center;",
                   tags$div("Liga Insider-Leistungsdaten",
                            style = "font-size:16px;font-weight:bold;color:black;margin-bottom:10px;"),
                   div(style = "margin:0 auto; width:90%;",
                       DTOutput("spieler_li_perf_summary"))
                 ),
                 
                 div(
                   style = "flex:1; text-align:center;",
                   tags$div("Liga Insider-Bilanz",
                            style = "font-size:16px;font-weight:bold;color:black;margin-bottom:10px;"),
                   div(style = "margin:0 auto; width:90%;",
                       DTOutput("spieler_li_perf_bilanz"))
                 )
               ),
               
               tags$div("Marktwertverlauf",
                        style = "text-align:center;font-size:16px;font-weight:bold;color:black;margin-bottom:10px;margin-top:20px;"),
               div(style = "margin-bottom:20px;width:100%;",
                   plotlyOutput("spieler_info_mw", height = 800, width = "100%")),
               
               tags$div("Leistungsdaten",
                        style = "text-align:center;font-size:16px;font-weight:bold;color:black;margin-bottom:10px;"),
               div(style = "display:flex; gap:16px; width:100%; align-items:flex-start; margin-bottom:20px;",
                   
                   # linke Hälfte: CA data
                   div(style = "flex:1 1 70%; text-align:center;",
                       div("com-analytics", style = "font-size:14px;font-weight:bold;color:black;margin-bottom:8px;"),
                       div(style = "margin:0 auto; width:95%;",
                           DTOutput("spieler_info_raw", width = "100%")
                       )
                   ),
                   
                   # rechte Hälfte: Preis-Leistung
                   div(style = "flex:1 1 30%; text-align:center;",
                       tags$div("Preis/Leistung", style = "font-size:14px;font-weight:bold;color:black;margin-bottom:8px;"),
                       div(style = "margin:0 auto; width:95%;",
                           plotlyOutput("spieler_price_perf_small", height = "500px", width = "100%")
                       )
                   )
               ),
               
               tags$div("Chat GPT Analyse",
                        style = "text-align:center;font-size:16px;font-weight:bold;color:black;margin-bottom:10px;"),
               selectInput("gpt_model", "Modell:", choices = c("gpt-4.1", "gpt-4o-mini"), selected = "gpt-4o-mini"),
               actionButton("gpt_run", "Research starten"),
               actionButton("copy_prompt", "Prompt kopieren"),
               div(
                 id = "gpt_prompt_container",
                 style = "position:absolute; left:-9999px; top:-9999px; width:1px; height:1px; overflow:hidden;",
                 verbatimTextOutput("gpt_prompt_preview", placeholder = TRUE)
               ),
               
               # NEU: freies Textfeld für die Antwort
               fluidRow(
                 style = "margin-bottom:200px;",
                 
                 column(
                   12,
                   uiOutput("gpt_result_box")   # ersetzt früheres DTOutput("gpt_result")
                 )
               )
               
             ),
             
             tabPanel(
               title = "Preis-Leistung",
               value = "price_perf_overview",
               
               tabsetPanel(
                 id = "pl_subtabs",
                 
                 tabPanel(
                   title = "Preis-/Leistung Overview",
                   value = "pl_overview",
                   div(
                     style = "width:100%;",
                     plotlyOutput("price_perf_plot", height = 700, width = "100%")
                   )
                 ),
                 
                 tabPanel(
                   title = "Punkte → MW-Entwicklung",
                   value = "punkte_mw_tabs",
                   div(
                     style = "width:100%; margin-bottom:20px;",
                     plotOutput("punkte_mw_avg_plot_static", height = 350, width = "100%")
                   ),
                   div(
                     style = "width:100%;",
                     plotOutput("punkte_mw_by_mwklasse", height = 350, width = "100%")
                   )
                 )
               )
             ),
             
             tabPanel(title = "Transfer-Simulator",
                      value = "transfer_simulator",
                      selectInput("spieler_select", "Spieler auf Transfermarkt:", choices = NULL),
                      fluidRow(
                        column(6, plotOutput("transfer_simulator_plot")),
                        column(3, checkboxGroupInput("angebote_select", "Angebote für Deckung:", choices = NULL)),
                        column(3, checkboxGroupInput("team_select",     "Eigene Spieler für Deckung:", choices = NULL))
                      )
             ),
             
             tabPanel(title = "Angebot-Analyse",
                      value = "angebot_analyse",
                      
                      fluidRow(
                        column(2, checkboxGroupInput("trend_filter", "Trend",
                                                     choices = c("gestiegen","gefallen","gleich"),
                                                     selected = c("gestiegen","gefallen","gleich")
                        )),
                        column(3, selectizeInput("verein_filter", "Verein", choices = NULL, multiple = TRUE)),
                        column(2, checkboxGroupInput("status_filter", "Status",
                                                     choices = c("aktiv","inaktiv"),
                                                     selected = c("aktiv","inaktiv")
                        )),
                        column(3, selectInput("color_by", "Farbkodierung",
                                              choices = c("Keine"="none","Trend"="Trend","Verein"="Verein","Status"="Status"),
                                              selected = "Trend"
                        )),
                        column(2, checkboxInput("show_labels", "Spielernamen anzeigen", value = FALSE))
                      ),
                      
                      fluidRow(
                        column(6, plotOutput("rel_angebot_vortag")),
                        column(6, plotOutput("rel_angebot_tag"))
                      )
             )
           )
  ),
  

  ## ---- Kader-Entwicklung ----
  tabPanel("Kader-Entwicklung",
           tabsetPanel(
             id = "kader_tabs",
             tabPanel(
               "Mein Kader",
               selectInput(
                 inputId = "manager_select_mein_kader",
                 label   = "Manager:",
                 choices = NULL,           # wird serverseitig befüllt
                 selected = NULL
               ),
               uiOutput("mein_kader")
             ),
             tabPanel(
               "Alle Kader",
               uiOutput("kader_uebersicht_ui")
             ),
             tabPanel(
               "Kaderwert-Plot",
               checkboxInput("smooth_mgr_curves", "Verlauf glätten (Trend je Manager)", FALSE),
               
               plotOutput("kaderwert_plot", height = 600)
             )
           )
  ),
  
  ## ---- Bieterprofile ----
  tabPanel("Bieterprofile",
           tabsetPanel(
             id = "bieterprofile_tabs",
             tabPanel(
               "Global & MW-Klassen",
               
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
               plotOutput("beeswarm", height = 700)
               ),
               
               div(style = "width: 90%; margin: 0 auto; margin-top:20px;",
                   plotlyOutput("mwclassplot", height = "700px")
               )
               
             ),
             tabPanel("Manager-Query",
                      div(style = "width: 90%; margin: 10px auto;",
                          fluidRow(
                            column(4,
                                   selectInput("seg_manager", "Manager / Bieter:", choices = NULL, width = "100%")
                            ),
                            column(4,
                                   numericInput("seg_mw", "Ziel-Marktwert (€):", value = 1e6, min = 0, step = 50000, width = "100%")
                            ),
                            column(4,
                                   numericInput("seg_tol", "Toleranz (± %):", value = 20, min = 0, max = 100, step = 5, width = "100%")
                            )
                          )
                      ),
                      div(style = "width: 90%; margin: 0 auto; margin-top:10px;",
                          plotlyOutput("manager_segment_plot", height = "700px")
                      )
             ),
             tabPanel("Zeit-Trend",
                      plotOutput("trendplot", height = 700),
                      plotOutput("gebote_pro_tag", height = 350)),
             tabPanel("Gebots-peaks",
                      plotOutput("aktive_tage_plot",       height = 350),
                      plotOutput("peak_days_heatmap",      height = 350),
                      hr(),  # optischer Trenner
                      plotOutput("bids_by_weekday_plot",   height = 350)  # <-- neu
             ),
             tabPanel("Zusammenfassung",
                      tags$div(
                        style = "display: flex; justify-content: center; align-items: center; gap: 40px;",
                        
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
                          column(6, plotOutput("flip_effizienz", height = 600)),
                          column(6, plotOutput("flip_cumcat", height = 600))
                        )
                      )
             ),
             
             # TAB 3: Kader-Flip
             tabPanel("Kader-Flip",
                      fluidPage(
                        fluidRow(
                          column(12, DTOutput("flip_kader"))
                        ),
                        div(style = "margin-top:30px;"),   # Abstand eingefügt
                        fluidRow(
                          column(12, plotlyOutput("kaufdiff_zeitlinien", height = "1500px"))
                        )
                      )
             ),
             
             # TAB 4: Flip-Historie
             tabPanel(title = "Flip-Historie", value = "mw_events_tab",
                      fluidRow(column(4, uiOutput("manager_select_ui"))),
                      br(),
                      plotlyOutput("mw_events", height = 600)
             ),
             
             # TAB 5: Historie
             tabPanel("Historie (Tabelle)",
                      fluidPage(
                        fluidRow(
                          column(2,
                                 selectInput("flip_player_select", "Spieler auswählen:", choices = c("Alle"), selected = "Alle")
                          ),
                          column(10, DTOutput("flip_player_table"))
                        )
                      )
             )
           )
  ),
  
  ## ---- Performance ----
  tabPanel("Performance",
           fluidPage(
             column(12, plotOutput("patzierungen_table", height = "800px")),
             column(12, plotOutput("podium_table", height = "200px"))
           )
            
  ),
  
  ## ---- Kapitalübersicht ----
  tabPanel("Kapitalübersicht",
           fluidPage(
             tabsetPanel(
               id = "kapital_tabs", type = "tabs",
               
               tabPanel("Übersicht & Gewinn",
                        DTOutput("kapital_uebersicht_table"),
                        div(style = "margin-bottom:15px; margin-top:40px;",
                            plotOutput("kapital_gewinn_plot", height = "360px"))
               ),
               
               tabPanel("Verlauf & Analysen",
                        div(style = "margin-bottom:15px; margin-top:40px;",
                            plotOutput("kapital_verlauf_plot", height = "420px")),
                        div(style = "margin-top:20px;",
                            DTOutput("kapital_minus_table"))
               )
             )
           )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- reactive_vals(0) / observe ----
  startup_time <- reactiveVal(NULL)
  verfuegbares_kapital_dominik <- reactiveVal(0)
  kontostand_dominik           <- reactiveVal(0)
  tm_trend_global <- reactiveVal(NULL)
  # reactiveVal für den gemeinsamen Transfermarkt DataFrame
  tm_common <- reactiveVal(NULL)
  # cache
  gebotsprofil_cache <- reactiveVal(NULL)
  spieler_stats_cache <- reactiveVal(NULL)
  dat_rel_angebote_cache <- reactiveVal(NULL)
  match_li_cache <- reactiveVal(list())
  mw_data_cache <- reactiveVal(NULL)
  mw_now_cache <- reactiveVal(NULL)
  mw_pred_cached <- reactiveVal(NULL)
  punkte_mw_should_run <- reactiveVal(FALSE)
  
  # Session-Startzeit
  session$userData$t0 <- Sys.time()
  today <- Sys.Date()
  
  # Sobald der Dashboard-Tab das erste Mal aktiv ist, Zeiten loggen
  observeEvent(input$main_navbar, {
    if (is.null(session$userData$t_dashboard) && identical(input$main_navbar, "Dashboard")) {
      session$userData$t_dashboard <- Sys.time()
      
      # Nach dem nächsten kompletten Flush einmalig loggen
      onFlushed(function() {
        t0  <- session$userData$t0
        tdb <- session$userData$t_dashboard
        tfl <- Sys.time()
        
        dt_tab   <- as.numeric(difftime(tdb, t0, units = "secs"))
        dt_flush <- as.numeric(difftime(tfl, t0, units = "secs"))
        
        # store startup time (in seconds)
        startup_time(dt_flush)
        
        line <- paste(format(tfl, "%Y-%m-%d %H:%M:%S"),
                      session$token,
                      sprintf("%.3f", dt_tab),
                      sprintf("%.3f", dt_flush),
                      sep = ";")
        
        try(write(line, file = LOG_PATH, append = TRUE), silent = TRUE)
      }, once = TRUE)
    }
  }, ignoreInit = FALSE)
  
  
  #Update stempel
  output$last_update <- renderText({
    last <- tryCatch(
      readLines("data/last_updated.txt", warn = FALSE)[1],
      error = function(e) "unbekannt"
    )
    
    st <- startup_time()
    if (is.null(st)) {
      paste0(last)
    } else {
      paste0(last, " · Startup: ", sprintf("%.3f s", st))
    }
  })
  
  # Kapital + Kontostand von Dominik beobachten und speichern
  observe({
    kapital_df <- kapital_df_reactive()
    dominiks_kapital <- kapital_df %>% filter(Manager == "Dominik")
    
    if (nrow(dominiks_kapital) == 1) {
      verfuegbares_kapital_dominik(dominiks_kapital$Verfügbares_Kapital) 
      kontostand_dominik(dominiks_kapital$Aktuelles_Kapital)
    }
  })
  
  # Action Button 1
  #Server‑Logik zum Wechseln und Vorwählen für Transfer-Simulator
  observeEvent(input$transfer_row, {
    idx <- input$transfer_row
    df  <- tm_trend_global()        # global gespeichertes DataFrame
    player <- df$Spieler[idx]

    # Tab wechseln
    updateTabsetPanel(
      session,
      inputId  = "transfermarkt_tabs",
      selected = "transfer_simulator"      # <-- exakt der Wert aus value= oben
    )

    # Spieler im SelectInput vorwählen
    updateSelectInput(
      session,
      inputId  = "spieler_select",
      selected = player
    )
  })
  
  # Action Button 2
  observeEvent(input$info_row, {
    idx <- input$info_row
    df  <- tm_trend_global()
    req(idx >= 1, idx <= nrow(df))
    player <- df$Spieler[idx]
    
    # Tab wechseln
    updateTabsetPanel(
      session,
      inputId  = "transfermarkt_tabs",
      selected = "spieler_info"
    )
    
    # Mit kleinem Delay den Spieler wählen
    later(function() {
      updateSelectInput(
        session,
        inputId  = "spieler_select2",
        selected = player
      )
    }, delay = 0.2)
  })
  
  # Action Button 3 (Mein Team)
  observeEvent(input$mk_info_player, {
    req(input$mk_info_player$name)
    player <- input$mk_info_player$name
    
    updateNavbarPage(session, "main_navbar", selected = "Transfermarkt")
    updateTabsetPanel(session, "transfermarkt_tabs", selected = "spieler_info")
    
    later(function() {
      updateSelectInput(session, "spieler_select2", selected = player)
    }, 0.2)
  })
  
  # Spieler_select2 choice update
  observe({
    choices <- ap_df %>%
      filter(Datum == today) %>%
      pull(Spieler) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "spieler_select2",
                      choices = choices,
                      selected = if (length(choices) > 0) choices[1] else NULL)
  })
  
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
    updateTabsetPanel(session, "flip_tabs", selected = "Historie (Tabelle)")
    }
  })
  
  # Bieterprofil query 
  observeEvent(gebotsprofil_mwclass_filtered(), ignoreInit = FALSE, {
    gp <- gebotsprofil_mwclass_filtered()
    req(nrow(gp) > 0)
    updateSelectInput(
      getDefaultReactiveDomain(), "seg_manager",
      choices = sort(unique(gp$Bieter)),
      selected = if (length(unique(gp$Bieter))) sort(unique(gp$Bieter))[1] else NULL
    )
  })
  
  # Gebotsprofil range 
  observe({
    dat <- gebotsprofil_clean() 
    if (nrow(dat) == 0) return()
    min_date <- min(dat$Datum)
    max_date <- max(dat$Datum)
    
    updateSliderInput(session, "mwclass_date_range",
                      min = min_date,
                      max = max_date,
                      value = c(min_date, max_date)
    )
  })
  
  # flip_player_select
  observe({
    choices <- c("Alle", sort(unique(flip_data()$Besitzer)))
    cur <- isolate(input$flip_player_select)
    sel <- if (!is.null(cur) && cur %in% choices) cur else "Alle"
    updateSelectInput(session, "flip_player_select", choices = choices, selected = sel)
  })
  
  
  # verein_filter
  observe({
    updateSelectizeInput(session, "verein_filter",
                         choices = sort(unique(ap_df$Verein)), server = TRUE)
  })
  
  # punkte_mw_tabs 
  observeEvent(input$pl_subtabs, {
    if (identical(input$pl_subtabs, "punkte_mw_tabs")) {
      punkte_mw_should_run(TRUE)
    }
  })
  
  # ---- data ----
  
  ## ---- df / transfers / transactions  ----
  
  teams_df <- read.csv2("data/TEAMS_all.csv", sep = ";", stringsAsFactors = FALSE)
  
  transfers <- read.csv2("data/TRANSFERS_all.csv", sep = ";", na.strings = c("", "NA")) %>%
    mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"),
           Hoechstgebot = as.numeric(Hoechstgebot),
           Zweitgebot = as.numeric(Zweitgebot)) %>% 
    mutate(Hoechstgebot = ifelse(Datum == as.Date("2025-05-30") & Spieler == "Hranáč", 166000, Hoechstgebot)) #Umwandeln von Fehlgebot von Alfons
  
  transactions_base <- read_delim(
    "data/TRANSACTIONS.csv",
    delim = ";",
    locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ","),
    show_col_types = FALSE
  ) %>%
    mutate(
      Spieler = as.character(Spieler),
      Manager = word(Spieler, 1)
    ) %>%
    filter(!(Datum == "01.06.2025" & Manager == "Alfons" & Transaktion == -166000)) #Herausnehmen von Fehlgebot von Alfons
  
  # Transaktionen für alle Plots inkl. Korrekturen
  transactions <- transactions_base %>%
    group_by(Manager) %>%
    summarise(Transaction_Summe = sum(Transaktion, na.rm = TRUE), .groups = "drop")
  
  # Transaktionen für Platzierungen Plot
  patzierungen_df <- transactions_base %>%
    # nur echte Platzierungsboni, Cupunio o.ä. raus
    filter(str_detect(
      Begründung,
      "^Bonus:\\s*\\d+\\s*\\.?\\s*(Platz)?\\b"
    )) %>%
    mutate(Platzierung = as.integer(str_extract(Begründung, "\\d+"))) %>%
    filter(!is.na(Platzierung), Platzierung >= 1, Platzierung <= 8) %>%
    count(Manager, Platzierung, name = "Anzahl") %>%
    complete(Manager, Platzierung = 1:8, fill = list(Anzahl = 0)) %>%
    arrange(Manager, Platzierung)
  
  transfermarkt <- read_csv2("data/TRANSFERMARKT.csv",
                             locale = locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = "."),
                             show_col_types = FALSE) %>%
    mutate(TM_Stand = as.Date(TM_Stand, format = "%d.%m.%Y"))
  
  # ALL_PLAYERS
  ap_df <- read.csv2("data/ALL_PLAYERS.csv", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE, fileEncoding = "UTF-8") %>% 
    mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"),
           Marktwert = as.numeric(Marktwert))
  
  status_latest <- ap_df %>%
    group_by(Spieler) %>%
    arrange(desc(Datum)) %>%
    slice(1) %>%
    ungroup() %>%
    select(Spieler, StatusText, StatusIcon)
  
  # COMP_TM_RESTZEIT
  tm_df <- read.csv2("data/COMP_TM_RESTZEIT.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  
  # STANDINGS
  st_df <- read_csv2("data/STANDINGS.csv", 
                     col_types = cols(
                       Manager = col_character(),
                       Teamwert = col_double(),
                       Gesamtpunkte = col_double(),
                       `letzte Punkte` = col_double(),
                       Datum = col_character()
                     ),
                     locale = locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = "."),
                     show_col_types = FALSE
  ) %>%
    mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"))
  
  # marktwertverlauf_gesamt
  mw_all <- read.csv("data/marktwertverlauf_gesamt.csv", sep = ";", encoding = "UTF-8")
  
  # LI Info 
  li_df <- read.csv2("data/LI_player_profiles.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  
  # robuster LI-Subset + Fallback-Berechnung für EQ-Werte
  li_subset <- li_df %>%
    select(any_of(c("Comunio_Name", "EQ_Gesamt_Quote", "EQ_Startelf_.",
                    "Bilanz_Gesamt_Einsätze","Bilanz_Gesamt_Startelf",
                    "Bilanz_Gesamt_Bank","Bilanz_Gesamt_Nicht_im_Kader"))) %>%
    mutate(
      Comunio_Name = trimws(as.character(Comunio_Name)),
      
      # bereits vorhandene EQ-Felder säubern (',' -> '.' ; '%' entfernen)
      EQ_Gesamt_Quote = suppressWarnings(as.numeric(gsub(",", ".", gsub("%", "", as.character(EQ_Gesamt_Quote))))),
      `EQ_Startelf_.` = suppressWarnings(as.numeric(gsub(",", ".", gsub("%", "", as.character(`EQ_Startelf_.`))))) ,
      
      # Bilanz-Felder in numerisch (nicht-numerische Zeichen entfernen; '-' -> NA)
      Bil_Einsa = suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(Bilanz_Gesamt_Einsätze)))),
      Bil_Start = suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(Bilanz_Gesamt_Startelf)))),
      Bil_Bank  = suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(Bilanz_Gesamt_Bank)))),
      Bil_NK    = suppressWarnings(as.numeric(gsub("[^0-9]", "", as.character(Bilanz_Gesamt_Nicht_im_Kader))))
    ) %>%
    # Fallback-Berechnungen nur wenn original NA und Bilanzzahlen vorhanden
    mutate(
      EQ_Gesamt_Quote = ifelse(
        is.na(EQ_Gesamt_Quote) & !is.na(Bil_Einsa),
        # Vermeide Division durch 0: falls Summe 0 -> NA
        {
          denom <- Bil_Einsa + coalesce(Bil_Bank, 0) + coalesce(Bil_NK, 0)
          ifelse(denom > 0, round(100 * (Bil_Einsa / denom), 0), NA_real_)
        },
        EQ_Gesamt_Quote
      ),
      `EQ_Startelf_.` = ifelse(
        is.na(`EQ_Startelf_.`) & !is.na(Bil_Start) & !is.na(Bil_Einsa) & Bil_Einsa > 0,
        round(100 * (Bil_Start / Bil_Einsa), 0),
        `EQ_Startelf_.`
      )
    ) %>%
    select(Comunio_Name, EQ_Gesamt_Quote, `EQ_Startelf_.`)
  
  ## ---- df lazy load ----
  load_non_dashboard_data <- function() {
    safe <- function(expr) {
      tryCatch(eval(expr), error = function(e) {
        message("Lazy-Load Fehler: ", conditionMessage(e))
        NULL
      })
    }

    # com_analytics (große Files)
    if (!exists("ca_df", inherits = TRUE) && file.exists("data/com_analytics_all_players.csv")) {
      ca_df <<- safe(quote({
        read_delim("data/com_analytics_all_players.csv", delim = ";",
                   locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ","),
                   na = c("", "NA", "N/A", "n/a", "-", "–"), show_col_types = FALSE) %>%
          mutate(SPIELER = trimws(enc2utf8(as.character(SPIELER))),
                 Datum   = as.Date(Datum, format = "%d.%m.%Y")) %>%
          filter(Datum == max(Datum, na.rm = TRUE)) %>% 
          transmute(SPIELER, Position = Positionsgruppe, Verein = VEREIN,
                    Punkte_pro_Spiel = `PUNKTE PRO SPIEL`, Gesamtpunkte = GESAMTPUNKTE,
                    Historische_Punkteausbeute = `HISTORISCHE PUNKTEAUSBEUTE`,
                    Verletzungsanfälligkeit = Verletzungsanfälligkeit)
      }))
    }
    
    if (!exists("ca2_df", inherits = TRUE) && file.exists("data/com_analytics_transfer_market_computer.csv")) {
      ca2_df <<- safe(quote({
        read_delim("data/com_analytics_transfer_market_computer.csv", delim = ";",
                   locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ","),
                   na = c("", "NA", "N/A", "n/a", "-", "–"), show_col_types = FALSE) %>%
          mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"),
                 SPIELER = trimws(enc2utf8(as.character(SPIELER)))) %>%
          filter(Datum == max(Datum, na.rm = TRUE)) %>%
          # kleine Normalisierung: einheitliche Feldnamen
          rename(
            Punkte_pro_Spiel = `PUNKTE / SPIEL`,
            Preis_Leistung   = `PREIS-LEISSTUNG`,
            Marktwert        = MARKTWERT,
            Zielwert         = ZIELWERT,
            Kaufempfehlung   = KAUFEMPFEHLUNG,
            Gebotsvorhersage = GEBOTSVORHERSAGE,
            Erwartete_Punkte = `ERWARTETE PUNKTE`
          )
      }))
    }
    
    # Angebots-Datei (Transfermarkt-Tab)
    if (!exists("angebote", inherits = TRUE) && file.exists("data/ANGEBOTE.csv")) {
      angebote <<- safe(quote({
        tmp <- read.csv2("data/ANGEBOTE.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE)
        tmp$Datum <- as.Date(tmp$Datum, format = "%d.%m.%Y")
        tmp
      }))
    }
    
  }
  
  observeEvent(input$main_navbar, {
    req(identical(input$main_navbar, "Dashboard"))
    onFlushed(function() {
      # Lade schwere Daten nach erstem Dashboard-Render
      load_non_dashboard_data()
    }, once = TRUE)
  })
  
  
  ## ---- reactive ----
  
  # Global nicht möglich
  ### ---- spieler_stats_spielplan ----
  observeEvent(input$transfermarkt_tabs, {
    #if (input$transfermarkt_tabs != "spieler_info") return()
    #if (!identical(input$main_navbar, "Transfermarkt")) return()
    if (!is.null(spieler_stats_cache())) return()
    
    path <- file.path("data", "PLAYER_STATS.csv")
    validate(need(file.exists(path), "PLAYER_STATS.csv fehlt."))
    
    tmp <- read.csv(path, sep = ";", header = TRUE, stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8", check.names = FALSE, na.strings = c("", "NA")) %>%
      mutate(
        Datum    = as.Date(Datum, format = "%d.%m.%Y"),
        Spieler  = trimws(enc2utf8(Spieler)),
        Spieltag = suppressWarnings(as.integer(Spieltag)),
        Note     = suppressWarnings(as.numeric(Note)),
        Punkte   = suppressWarnings(as.integer(Punkte)),
        Status   = trimws(enc2utf8(Status))
      ) %>%
      filter(!is.na(Spieler), Spieler != "", !is.na(Spieltag)) %>%
      distinct(Spieler, Spieltag, .keep_all = TRUE) %>%
      arrange(Spieler, Spieltag) %>%
      rename(Datum_scrape = Datum) %>%
      left_join({
        ap_df %>%
          filter(!is.na(Spieler), Spieler != "", !is.na(Verein), Verein != "") %>%
          select(Spieler, Verein, Datum) %>%
          arrange(Spieler, Datum) %>%
          group_by(Spieler) %>%
          filter(Datum == max(Datum, na.rm = TRUE)) %>%
          slice_tail(n = 1) %>%
          ungroup() %>%
          transmute(Spieler = trimws(enc2utf8(Spieler)), Verein = trimws(enc2utf8(Verein)))
      }, by = "Spieler") %>%
      left_join(sp_tm %>% rename(Matchdatum = Datum), by = "Spieltag", relationship = "many-to-many") %>%
      mutate(
        HeimGast = case_when(Verein == Heim ~ "Heim", Verein == Gast ~ "Gast", TRUE ~ NA_character_),
        Gegner = case_when(HeimGast == "Heim" ~ Gast, HeimGast == "Gast" ~ Heim, TRUE ~ NA_character_)
      ) %>%
      filter(!is.na(HeimGast), !is.na(Gegner), !is.na(Matchdatum)) %>%
      group_by(Spieler, Spieltag) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      transmute(Spieler, Verein, Spieltag, Datum = Matchdatum, Gegner, HeimGast, Note, Punkte, Status) %>%
      arrange(Spieler, Spieltag, Datum)
    
    spieler_stats_cache(tmp)
  }, ignoreInit = TRUE)
  
  spieler_stats_spielplan <- reactive({
    req(!is.null(spieler_stats_cache()))
    spieler_stats_cache()
  })
  
  ### ---- dat_rel_angebote ----
  observeEvent(input$main_navbar, {
    if (!identical(input$main_navbar, "Transfermarkt")) return()
    if (!is.null(dat_rel_angebote_cache())) return()   # already computed
    if (!exists("angebote", inherits = TRUE) || !is.data.frame(angebote)) return()
    
    # safe compute (same logic wie vorher)
    betrag_col <- if ("Angebot (€)" %in% names(angebote)) "Angebot (€)" else "Angebot"
    
    tmp <- angebote %>%
      mutate(Angebot = as.numeric(.data[[betrag_col]])) %>%
      select(Spieler, Datum, Angebot, Status)
    
    spieler_set <- unique(tmp$Spieler)
    datum_set   <- unique(tmp$Datum)
    
    mw_am_tag <- ap_df %>%
      filter(Spieler %in% spieler_set, Datum %in% datum_set) %>%
      select(Spieler, Datum, Verein, MW_am_Tag = Marktwert)
    
    mw_vortag <- ap_df %>%
      filter(Spieler %in% spieler_set, Datum %in% (datum_set - 1)) %>%
      transmute(Spieler, Datum = Datum + 1, MW_Vortag = Marktwert)
    
    res <- tmp %>%
      left_join(mw_am_tag, by = c("Spieler","Datum")) %>%
      left_join(mw_vortag, by = c("Spieler","Datum")) %>%
      mutate(
        rel_am_Tag = if_else(!is.na(MW_am_Tag)  & MW_am_Tag  > 0, Angebot / MW_am_Tag  - 1, NA_real_),
        rel_Vortag = if_else(!is.na(MW_Vortag) & MW_Vortag > 0, Angebot / MW_Vortag - 1, NA_real_),
        Trend = case_when(
          !is.na(MW_am_Tag) & !is.na(MW_Vortag) & MW_am_Tag >  MW_Vortag ~ "gestiegen",
          !is.na(MW_am_Tag) & !is.na(MW_Vortag) & MW_am_Tag <  MW_Vortag ~ "gefallen",
          !is.na(MW_am_Tag) & !is.na(MW_Vortag) & MW_am_Tag == MW_Vortag ~ "gleich",
          TRUE ~ NA_character_
        )
      )
    
    dat_rel_angebote_cache(res)
  }, ignoreInit = TRUE)
  
  # reactive wrapper (API unverändert)
  dat_rel_angebote <- reactive({
    req(!is.null(dat_rel_angebote_cache()))
    dat_rel_angebote_cache()
  })
  
  ### ---- match_li_row ----
  observeEvent(input$spieler_select2, {
    if (!identical(input$main_navbar, "Transfermarkt")) return()
    if (is.null(input$spieler_select2) || !nzchar(input$spieler_select2)) return()
    if (!exists("li_df", inherits = TRUE) || !is.data.frame(li_df)) return()

    k <- norm_key(input$spieler_select2)
    cache <- match_li_cache()
    if (!is.null(cache[[k]])) return()   # already cached

    tmp <- li_df
    tmp$.k <- norm_key(tmp$Comunio_Name)
    out <- tmp[tmp$.k == k, , drop = FALSE]
    if (nrow(out) > 0) out <- out[1, , drop = FALSE]

    cache[[k]] <- out
    match_li_cache(cache)
  }, ignoreInit = TRUE)
  
  # reactive wrapper: liefert cached row (oder empty df)
  match_li_row <- reactive({
    req(exists("li_df", inherits = TRUE), is.data.frame(li_df))
    req(input$spieler_select2)
    k <- norm_key(input$spieler_select2)
    cache <- match_li_cache()
    if (!is.null(cache[[k]])) return(cache[[k]])
    # fallback: compute synchronously if not cached (keeps API robust)
    tmp <- li_df
    tmp$.k <- norm_key(tmp$Comunio_Name)
    out <- tmp[tmp$.k == k, , drop = FALSE]
    if (nrow(out) == 0) return(out)
    out[1, , drop = FALSE]
  })
  
  ### ---- gebotsprofil_clean ----
  observeEvent(input$main_navbar, {
    if (!input$main_navbar %in% c("Transfermarkt", "Bieterprofile")) return()
    if (!is.null(gebotsprofil_cache())) return()   # already computed
    
    req(exists("transfers", inherits = TRUE), is.data.frame(transfers))
    req(exists("transfermarkt", inherits = TRUE), is.data.frame(transfermarkt))
    
    tmp <- lapply(seq_len(nrow(transfers)), function(i) {
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
        Typ = "Hoechstgebot",
        stringsAsFactors = FALSE
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
          Typ = "Zweitgebot",
          stringsAsFactors = FALSE
        )
        res <- rbind(res, res2)
      }
      res
    }) %>% bind_rows()
    
    tmp <- tmp %>% filter(Bieter != "Computer" & !is.na(MW_vortag))
    gebotsprofil_cache(tmp)
  }, ignoreInit = TRUE)
  
  # reactive wrapper (unchanged API)
  gebotsprofil_clean <- reactive({
    req(!is.null(gebotsprofil_cache()))
    gebotsprofil_cache()
  })
  
  #### ---- gebotsprofil_mwclass ----
  gebotsprofil_mwclass <- reactive({
    df <- gebotsprofil_clean()
    req(nrow(df) > 0)
    df %>%
      mutate(
        MW_Klasse = case_when(
          MW_vortag < 0.5e6 ~ "<0.5 Mio",
          MW_vortag < 1e6    ~ "0.5–1 Mio",
          MW_vortag < 2.5e6  ~ "1–2.5 Mio",
          MW_vortag < 5e6    ~ "2.5–5 Mio",
          MW_vortag < 10e6   ~ "5–10 Mio",
          MW_vortag >= 10e6  ~ ">10 Mio",
          TRUE               ~ "unbekannt"
        )
      )
  })
  
  #### ---- gebotsprofil range ----
  gebotsprofil_mwclass_filtered <- reactive({
    req(input$mwclass_date_range)
    df <- gebotsprofil_mwclass()
    req(nrow(df) > 0)
    df %>% filter(Datum >= input$mwclass_date_range[1], Datum <= input$mwclass_date_range[2])
  })
  
  #### ---- mwclass_summary ----
  mwclass_summary <- reactive({
    df <- gebotsprofil_mwclass()
    req(nrow(df) > 0)
    df %>%
      mutate(
        MW_Klasse = factor(
          MW_Klasse,
          levels = c("<0.5 Mio","0.5–1 Mio","1–2.5 Mio","2.5–5 Mio","5–10 Mio",">10 Mio"),
          ordered = TRUE
        )
      ) %>%
      group_by(Bieter, MW_Klasse) %>%
      summarise(
        Anzahl_gesamt = n(),
        Anzahl_Hoechstgebote = sum(Typ == "Hoechstgebot"),
        Anzahl_Zweitgebote    = sum(Typ == "Zweitgebot"),
        Durchschnitt_Abweichung = round(mean(Diff_Prozent, na.rm = TRUE), 1),
        Min = min(Diff_Prozent, na.rm = TRUE),
        Max = max(Diff_Prozent, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Bieter, MW_Klasse)
  })
  
  ### ---- price_perf_df ----
  price_perf_df <- reactive({
    req(ap_df, ca_df)
    
    # inline, robustes Parsen von numerischen Strings (Tausender/Dezimal)
    parse_num_inline <- function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      x <- trimws(x)
      x[x == ""] <- NA_character_
      x <- gsub("\\s+", "", x)
      
      # beide "." und "," -> "." als Dezimal (also "." entfernen, ","->".")
      idx_both <- grepl("\\.", x) & grepl(",", x)
      if (any(idx_both, na.rm = TRUE)) {
        x[idx_both] <- gsub("\\.", "", x[idx_both])
        x[idx_both] <- gsub(",", ".", x[idx_both], fixed = TRUE)
      }
      
      # nur "," -> Komma als Dezimal
      idx_comma <- !grepl("\\.", x) & grepl(",", x)
      if (any(idx_comma, na.rm = TRUE)) {
        x[idx_comma] <- gsub(",", ".", x[idx_comma], fixed = TRUE)
      }
      
      # nur "." bleibt (Punkt als Dezimal) -> nothing
      # sonst: try numeric
      suppressWarnings(as.numeric(x))
    }
    
    # aktueller Marktwert pro Spieler (letzter Datumseintrag) — robust parsen
    ap_latest <- ap_df %>%
      group_by(Spieler) %>%
      slice_max(Datum, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(Spieler, Marktwert_num = parse_num_inline(Marktwert))
    
    # ca_df: SPIELER + Roh-PPS + Gesamtpunkte
    ca_in <- ca_df %>%
      transmute(
        SPIELER = as.character(SPIELER),
        PPS_raw = as.character(`Punkte_pro_Spiel`),
        GESAMTPUNKTE = as.character(Gesamtpunkte)
      )
    
    # join + Parsen
    df <- ca_in %>%
      left_join(ap_latest, by = c("SPIELER" = "Spieler")) %>%
      mutate(
        PPS_num = parse_num_inline(PPS_raw),
        MW_num  = as.numeric(Marktwert_num)
      )
    
    # brauchbare Fälle
    mask <- df %>% filter(!is.na(MW_num) & !is.na(PPS_num) & PPS_num > 0 & PPS_num < 100)
    if (nrow(mask) < 10) {
      med_pps <- median(df$PPS_num, na.rm = TRUE)
      if (!is.finite(med_pps) || is.na(med_pps)) med_pps <- 1
      df <- df %>%
        mutate(
          expected_PPS = med_pps,
          expected_PPS = pmax(expected_PPS, 0.1),
          rel_diff = ifelse(is.na(PPS_num), NA_real_, (PPS_num - expected_PPS) / expected_PPS)
        )
    } else {
      safe_x <- log1p(mask$MW_num)
      try({
        m_loess <- loess(PPS_num ~ safe_x, data = mask, span = 0.5, control = loess.control(surface = "direct"))
        pred_all <- tryCatch(predict(m_loess, newdata = data.frame(safe_x = log1p(df$MW_num))), error = function(e) rep(NA_real_, nrow(df)))
        pred_all[is.na(pred_all)] <- median(mask$PPS_num, na.rm = TRUE)
        df$expected_PPS <- pred_all
        df$expected_PPS <- pmax(df$expected_PPS, 0.1)
        df$rel_diff <- ifelse(is.na(df$PPS_num), NA_real_, (df$PPS_num - df$expected_PPS) / df$expected_PPS)
      }, silent = TRUE)
      
      if (!"expected_PPS" %in% names(df)) {
        med_pps <- median(mask$PPS_num, na.rm = TRUE)
        if (!is.finite(med_pps) || is.na(med_pps)) med_pps <- 1
        df <- df %>% mutate(expected_PPS = med_pps, expected_PPS = pmax(expected_PPS, 0.1),
                            rel_diff = ifelse(is.na(PPS_num), NA_real_, (PPS_num - med_pps) / med_pps))
      }
    }
    
    # Score, z und Perzentil-Labeling
    df <- df %>%
      mutate(
        rel_pct = round(rel_diff * 100, 1),
        Preis_Leistung_score = rel_pct
      )
    
    sd_rel <- sd(df$rel_diff, na.rm = TRUE)
    mean_rel <- mean(df$rel_diff, na.rm = TRUE)
    df$rel_z <- ifelse(is.na(df$rel_diff) | is.na(sd_rel) | sd_rel == 0, NA_real_, (df$rel_diff - mean_rel) / sd_rel)
    
    q <- quantile(df$rel_diff, probs = c(0.10, 0.33, 0.67, 0.90), na.rm = TRUE, type = 7)
    df$Preis_Leistung_label <- case_when(
      is.na(df$rel_diff)           ~ NA_character_,
      df$rel_diff >= q[4]         ~ "Sehr gut",
      df$rel_diff >= q[3]         ~ "Gut",
      df$rel_diff >= q[2]         ~ "Durchschnittlich",
      df$rel_diff >= q[1]         ~ "Schlecht",
      TRUE                        ~ "Sehr schlecht"
    )
    df$Preis_Leistung_label[is.na(df$Preis_Leistung_label)] <- "-"
    
    df %>%
      select(SPIELER, MW_num, PPS_num, expected_PPS, Preis_Leistung_score, Preis_Leistung_label, rel_z)
  })
  
  ## ---- helper functions ----
  # HELPER: GPT prompt builder für Spieler-Info Seite (muss im server sein, da sie auf Datensätze zugreift, die global nicht vorhanden sind) 
  get_verein <- function(sp) {
    norm <- function(x) tolower(trimws(enc2utf8(as.character(x))))
    sel <- norm(sp)
    
    # 1) Prüfe ap_df wie bisher
    if (exists("ap_df", inherits = TRUE) && is.data.frame(ap_df) && nrow(ap_df) > 0) {
      tmp <- ap_df
      tmp$._k <- norm(tmp$Spieler)
      tmp$Datum <- as.Date(tmp$Datum)
      tmp2 <- tmp[tmp$._k == sel, , drop = FALSE]
      if (nrow(tmp2) > 0) {
        row <- tmp2[tmp2$Datum == Sys.Date(), , drop = FALSE]
        if (nrow(row) == 0) row <- tmp2[order(-as.numeric(tmp2$Datum)), , drop = FALSE][1, , drop = FALSE]
        ve <- as.character(row$Verein[1])
        if (!is.na(ve) && nzchar(ve)) return(ve)
      }
    }
    
    # 2) Stelle sicher, dass die ca*_Daten geladen sind (wenn Loader vorhanden)
    if (!exists("ca2_df", inherits = TRUE) && exists("load_non_dashboard_data")) {
      try(suppressWarnings(load_non_dashboard_data()), silent = TRUE)
    }
    
    # 3) Wähle mögliche Quellen in Reihenfolge
    src <- NULL
    if (exists("ca2_df", inherits = TRUE) && is.data.frame(ca2_df) && nrow(ca2_df) > 0) src <- ca2_df
    else if (exists("ca_df", inherits = TRUE) && is.data.frame(ca_df) && nrow(ca_df) > 0) src <- ca_df
    if (is.null(src)) return("")
    
    # 4) Arbeite mit einer base-data.frame-Kopie (falltolerant bzgl. Groß-/Kleinschreibung)
    src2 <- as.data.frame(src, stringsAsFactors = FALSE)
    names_up <- toupper(names(src2))
    
    # Finde Spieler-Spalte
    sp_col_idx <- which(names_up %in% c("SPIELER", "NAME"))
    if (length(sp_col_idx) == 0) return("")
    sp_col <- names(src2)[sp_col_idx[1]]
    
    # Finde Vereins-Spalte
    ver_col_idx <- which(names_up %in% c("TEAM", "VEREIN", "CLUB", "POSITION"))
    if (length(ver_col_idx) == 0) return("")
    ver_col <- names(src2)[ver_col_idx[1]]
    
    # Filter nach Spieler
    vals_sp <- tolower(trimws(enc2utf8(as.character(src2[[sp_col]]))))
    match_idx <- which(vals_sp == sel)
    if (length(match_idx) == 0) return("")
    v <- as.character(src2[[ver_col]][match_idx[1]])
    if (is.na(v)) return("")
    v
  }
  
  
  mw_aktuell_rx <- reactive({
    gesamt_mw_roh %>%
      group_by(Spieler) %>%
      filter(Datum == max(Datum, na.rm = TRUE)) %>%
      summarise(Marktwert_aktuell = first(Marktwert), .groups = "drop")
  })
  
  # ---- DASHBOARD ----
  ## ---- News ----
  
  output$dashboard_news <- renderUI({
    req(input$main_navbar == "Dashboard")
    
    req(exists("li_df", inherits = TRUE), is.data.frame(li_df))
    req(!is.null(meine_spieler()))
    
    need <- c("Comunio_Name","News1_Date","News1_URL","News2_Date","News2_URL","News3_Date","News3_URL")
    if (!all(need %in% names(li_df))) return(div("Keine News verfügbar"))
    
    kad_namen <- meine_spieler()
    li_sub <- li_df[li_df$Comunio_Name %in% kad_namen, , drop = FALSE]
    if (nrow(li_sub) == 0) return(div("Keine News verfügbar"))
    
    dates   <- as.vector(t(li_sub[, c("News1_Date","News2_Date","News3_Date")]))
    urls    <- as.vector(t(li_sub[, c("News1_URL","News2_URL","News3_URL")]))
    spieler <- rep(li_sub$Comunio_Name, each = 3)
    
    news <- data.frame(Spieler = spieler, date = trimws(dates), url = trimws(urls), stringsAsFactors = FALSE)
    news <- news[!is.na(news$url) & nzchar(news$url), , drop = FALSE]
    if (nrow(news) == 0) return(div("Keine News verfügbar"))
    
    ds <- trimws(news$date)
    ds <- gsub("[\u2013\u2014]", "-", ds)
    ds <- gsub("\\s*-\\s*-\\s*", " - ", ds, perl = TRUE)
    ds <- gsub("\\s*-\\s*", " - ", ds, perl = TRUE)
    ds <- gsub("\\s+", " ", ds)
    suppressWarnings({
      dt1 <- as.POSIXct(ds, format = "%d.%m.%Y - %H:%M", tz = "Europe/Berlin")
      dt2 <- as.POSIXct(ds, format = "%d.%m.%Y %H:%M",   tz = "Europe/Berlin")
      dt3 <- as.POSIXct(ds, format = "%d.%m.%Y",         tz = "Europe/Berlin")
    })
    dt <- dt1; idx <- is.na(dt); dt[idx] <- dt2[idx]; idx <- is.na(dt); dt[idx] <- dt3[idx]
    news$dt <- dt
    news$d  <- as.Date(news$dt, tz = "Europe/Berlin")
    
    news <- news[!is.na(news$d) & news$d >= (today - 7) & news$d <= today, , drop = FALSE]
    if (nrow(news) == 0) return(div("Keine News der letzten 7 Tage"))
    
    news <- news[order(news$dt, decreasing = TRUE, na.last = TRUE), ]
    news <- news[!duplicated(news$url), , drop = FALSE]
    if (nrow(news) > 40L) news <- news[seq_len(40L), , drop = FALSE]
    
    rel_label <- function(d) {
      if (is.na(d)) return("")
      diff <- as.integer(today - d)
      if (diff == 0) return("Heute")
      if (diff == 1) return("Gestern")
      paste0("vor ", diff, " Tagen")
    }
    rel_style <- function(lbl) if (identical(lbl, "Heute")) "color:#0a7a20;font-weight:600;" else "color:#000;"
    
    items <- lapply(seq_len(nrow(news)), function(i) {
      lbl <- rel_label(news$d[i])
      tags$div(
        class = "news-row",
        tags$span(news$Spieler[i], class = "col-spieler"),
        tags$span(lbl, class = "col-rel", style = rel_style(lbl)),
        tags$span(if (!is.na(news$dt[i])) format(news$dt[i], "%d.%m.%Y %H:%M") else news$date[i],
                  class = "col-dt"),
        # kurzer Emoji-Link, voller URL im Hover
        tags$a(href = news$url[i], target = "_blank", rel = "noopener noreferrer",
               class = "news-link", title = news$url[i], "🔗 Zur News")
      )
    })
    
    tags$div(items)
  })
  
  ## ---- Transferaktivitäten ----
  output$transfer_summary_today <- renderDT({
    req(input$main_navbar == "Dashboard")
    
    selected_date <- if (input$flip_day == "Gestern") today - 1 else today
    
    df <- transfers %>%
      filter(Datum == selected_date, Hoechstbietender != "Computer") %>%
      select(Spieler, Preis = Hoechstgebot, Hoechstbietender) %>%
      left_join(
        ap_df %>% filter(Datum == selected_date) %>% transmute(Spieler, Marktwert_today = Marktwert),
        by = "Spieler"
      ) %>%
      left_join(
        ap_df %>% filter(Datum == selected_date - 1) %>% transmute(Spieler, Marktwert_prev = Marktwert),
        by = "Spieler"
      ) %>%
      mutate(
        MW_Vortag = Marktwert_prev,
        MW_Heute  = Marktwert_today,
        Diff_pct  = ifelse(!is.na(MW_Vortag) & MW_Vortag > 0,
                           100 * (Preis - MW_Vortag) / MW_Vortag,
                           NA_real_),
        `Δ Preis (%)` = ifelse(
          is.na(Diff_pct), "-",
          paste0(ifelse(Diff_pct >= 0, "+", "-"),
                 round(abs(Diff_pct), 1), " %")
        ),
        pot        = MW_Heute - Preis,
        `Flip (€)` = case_when(
          is.na(pot)            ~ "-",
          pot >= 0              ~ paste0(
            '<span style="color:darkgreen;font-weight:bold;">+',
            format(pot, big.mark=".", decimal.mark=","), " €</span>"
          ),
          TRUE                  ~ paste0(
            '<span style="color:red;font-weight:bold;">-',
            format(abs(pot), big.mark=".", decimal.mark=","), " €</span>"
          )
        ),
        Trend      = case_when(
          is.na(MW_Vortag) | is.na(MW_Heute) ~ "–",
          MW_Heute > MW_Vortag               ~ '<span style="color:green;font-weight:bold;">▲</span>',
          MW_Heute < MW_Vortag               ~ '<span style="color:red;font-weight:bold;">▼</span>',
          TRUE                               ~ "–"
        )
      ) %>%
      transmute(
        Spieler,
        Käufer      = Hoechstbietender,
        Trend,
        `Flip (€)`,
        `MW Vortag` = MW_Vortag,
        `MW Heute`  = MW_Heute,
        Preis,
        `Δ Preis (%)`
      )
    
    datatable(
      df, escape = FALSE, rownames = FALSE, selection = "single",
      options = list(dom = 't', scrollX = TRUE, paging = FALSE)
    ) %>%
      formatCurrency(
        columns = c("MW Vortag", "MW Heute", "Preis"),
        currency = "", interval = 3, mark = ".", dec.mark = ",", digits = 0
      )
  })
  
  ## ---- Flip-Aktivitäten ----
  output$flip_summary_today <- renderDT({
    req(input$main_navbar == "Dashboard")
    
    selected_date <- if (input$flip_day == "Gestern") today - 1 else today
    
    df <- flip_data() %>%
      filter(Verkaufsdatum == selected_date) %>%
      select(Spieler, Besitzer, Gewinn, Einkaufspreis, Verkaufspreis, Einkaufsdatum) %>%
      arrange(desc(Einkaufsdatum))
    
    datatable(
      df,
      selection = "single",
      rownames = FALSE,
      options = list(dom = 't', pageLength = 10, scrollX = TRUE, paging = FALSE),
      colnames = c("Spieler","Verkäufer","Gewinn/Verlust (€)","Einkaufspreis","Verkaufspreis","Kaufdatum")
    ) %>%
      formatCurrency(columns = c("Einkaufspreis","Verkaufspreis","Gewinn"),
                     currency = "", interval = 3, mark = ".", digits = 0) %>%
      formatStyle('Gewinn', color = styleInterval(0, c('#e53935','#388e3c')), fontWeight = 'bold')
  })
  
  
  ## ---- Marktwerttrend ----
  output$mw_zeitachse_preview <- renderPlot({
    req(input$main_navbar == "Dashboard")
    
    # Vorhersage optional aus dem Cache holen (nicht direkt mw_pred() aufrufen)
    pred <- tryCatch(mw_pred_cached(), error = function(e) NULL)
    info <- if (!is.null(pred)) pred$info_txt else NULL
    
    df <- subset(gesamt_mw_df, !is.na(Datum))
    if (nrow(df) < 3) return(NULL)
    
    last_day <- max(df$Datum, na.rm = TRUE)
    win_days <- 14L
    df_short <- df[df$Datum >= (last_day - win_days), , drop = FALSE]
    if (nrow(df_short) < 3) df_short <- tail(df[order(df$Datum), ], 3)
    df_short <- df_short[!is.na(df_short$MW_rel_normiert), , drop = FALSE]
    if (nrow(df_short) < 2) return(NULL)
    
    trend_vals <- tail(df_short[order(df_short$Datum), ], 3)
    x0 <- trend_vals$Datum[1]; y0 <- trend_vals$MW_rel_normiert[1]
    x1 <- trend_vals$Datum[3]; y1 <- trend_vals$MW_rel_normiert[3]
    col_seg <- if (y1 > y0) "darkgreen" else "red"
    
    p <- ggplot(df_short, aes(Datum, MW_rel_normiert)) +
      geom_line(linewidth = 1.1, color = "darkgrey", na.rm = TRUE) +
      geom_segment(
        data = data.frame(x0=x0,y0=y0,x1=x1,y1=y1),
        aes(x=x0,y=y0,xend=x1,yend=y1),
        arrow = arrow(length = unit(0.35,"cm"), type = "closed"),
        color = col_seg, linewidth = 1.4, inherit.aes = FALSE
      ) +
      scale_x_date(limits = c(last_day - win_days, last_day), date_labels = "%d.%m.") +
      scale_y_continuous(labels = scales::label_number(big_mark=".", decimal_mark=",")) +
      labs(x = NULL, y = "relativer MW") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none")
    
    if (!is.null(info) && nzchar(info)) p <- p + labs(subtitle = info)
    p
  })
  
  
  ## ---- Kontostände ----
  output$kreditrahmen_uebersicht_preview <- renderDT({
    req(input$main_navbar == "Dashboard")
    
    kapital_df <- kapital_df_reactive() %>%
      select(
        Manager,
        Teamwert,
        Kontostand = Aktuelles_Kapital,
        `Verfügbares Kapital` = Verfügbares_Kapital
      ) %>%
      mutate(across(c(Teamwert, Kontostand, `Verfügbares Kapital`), as.numeric)) %>%
      mutate(`Teamwertpotenzial` = Teamwert + Kontostand) %>%
      select(Manager, Teamwert, Kontostand, `Teamwertpotenzial`, `Verfügbares Kapital`) %>%
      as.data.frame()
    
    idx_vk <- which(names(kapital_df) == "Verfügbares Kapital") - 1L
    
    datatable(
      kapital_df,
      rownames = FALSE,
      colnames = c("Manager","Teamwert (€)","Kontostand (€)","Teamwertpotenzial (€)","Verfügbares Kapital (€)"),
      escape = FALSE,
      options = list(
        paging = FALSE,
        autoWidth = TRUE,
        order = list(list(idx_vk, 'desc')),
        dom = 't'
      )
    ) %>%
      formatCurrency(
        columns = c("Teamwert", "Kontostand", "Teamwertpotenzial", "Verfügbares Kapital"),
        currency = "", interval = 3, mark = ".", digits = 0, dec.mark = ","
      ) %>%
      formatStyle(
        'Kontostand',
        color = styleInterval(0, c('red', 'black')),
        fontWeight = styleInterval(0, c('bold', NA))
      )
  })
  
  
  ## ---- Flip-Preview ----
  output$flip_preview <- renderPlot({
    req(input$main_navbar == "Dashboard")
    
    df <- flip_data() %>%
      group_by(Besitzer) %>%
      summarise(Gesamtgewinn = sum(Gewinn, na.rm = TRUE), .groups = "drop")
    req(nrow(df) > 0)
    
    abs_max <- max(df$Gesamtgewinn, na.rm = TRUE)
    abs_min <- min(df$Gesamtgewinn, na.rm = TRUE)
    lim_min <-  abs_min - 1e5
    lim_max <-  abs_max + 1e5
    
    ggplot(df, aes(x = reorder(Besitzer, Gesamtgewinn), y = Gesamtgewinn, fill = Gesamtgewinn > 0)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = format(Gesamtgewinn, big.mark = ".", decimal.mark = ",", scientific = FALSE)),
        position = position_stack(vjust = 0.5), size = 6
      ) +
      coord_flip(ylim = c(lim_min, lim_max)) +
      scale_fill_manual(values = c("TRUE" = "#66cdaa", "FALSE" = "#ff6f61")) +
      theme_minimal(base_size = 18) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
  })
  
  
  # ---- MARKTWERTENTWICKLUNG ----
  ## ---- Gesamtmarktwerte ----

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
  
  ## ---- MW-Verlauf ----
  output$mw_evolution <- renderPlot({
    req(input$main_navbar == "Marktwert-Entwicklung",
        input$mw_tabs      == "mw_verlauf")
    
    sp <- post_sommerpause_data()
    
    clean_df <- gesamt_mw_df %>%
      filter(!is.na(Datum)) %>%
      arrange(Datum)
    
    n   <- nrow(clean_df)
    idx <- seq(1, n, by = 5)
    
    arrow_df <- tibble(
      x0 = clean_df$Datum[idx],
      y0 = clean_df$MW_rel_normiert[idx],
      x1 = clean_df$Datum[pmin(idx + 5, n)],
      y1 = clean_df$MW_rel_normiert[pmin(idx + 5, n)]
    ) %>% mutate(color = ifelse(y1 > y0, "darkgreen", "red"))
    
    # Spieltags-Boxen (nur bis 21.12.2025)
    cutoff <- as.Date("2025-12-21")
    
    sp_rects <- sp_tm %>%
      mutate(Datum = as.Date(Datum)) %>%
      filter(Datum <= cutoff) %>%           # NEU: nur bis 21.12.2025
      group_by(Spieltag) %>%
      summarise(
        xmin = min(Datum),
        xmax = max(Datum),
        .groups = "drop"
      ) %>%
      mutate(
        xmin = xmin - 0.5,
        xmax = xmax + 0.5
      )
    
    ggplot() +
      # Boxen unterlegen
      geom_rect(
        data = sp_rects,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        fill = "grey50", alpha = 0.08, color = NA, inherit.aes = FALSE
      ) +
      
      geom_line(
        data = gesamt_mw_df %>%
          transmute(Datum, Wert = MW_rel_normiert, Typ = "Gesamtmarktwert"),
        aes(x = Datum, y = Wert, color = Typ),
        linewidth = 1.2, na.rm = TRUE
      ) +
      geom_vline(xintercept = as.Date("2025-08-22"), linetype = "dotted",
                 color = "darkred", linewidth = 1) +
      annotate("text", x = as.Date("2025-08-22") - 2, y = 0.825,
               label = "Saisonstart", color = "darkred", angle = 90,
               fontface = "bold", size = 5) +
      geom_vline(xintercept = as.Date("2025-09-07"), linetype = "dotted",
                 color = "darkgreen", linewidth = 1) +
      annotate("text", x = as.Date("2025-09-07") - 2, y = 0.875,
               label = "K1 (19/22/23) 07.09.25", color = "darkgreen", 
               angle = 90, fontface = "bold", size = 5) +
      geom_vline(xintercept = as.Date("2025-10-07"), linetype = "dotted",
                 color = "orange", linewidth = 1) +
      annotate("text", x = as.Date("2025-10-07") - 2, y = 0.86,
               label = "K2 (18/21) 07.10.25", color = "orange",
               angle = 90, fontface = "bold", size = 5) +
      geom_vline(xintercept = as.Date("2025-12-24"), linetype = "dotted",
                 color = "red", linewidth = 1.5) +
      annotate("text", x = as.Date("2025-12-24") - 2, y = 0.895,
               label = "K3 (13/14/19/24) 24.12.25", color = "red",
               angle = 90, vjust = 0.15, fontface = "bold", size = 5) +
      geom_segment(
        data = arrow_df,
        aes(x = x0, y = y0, xend = x1, yend = y1),
        arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
        color = arrow_df$color, linewidth = 1, inherit.aes = FALSE,
        alpha = 0.5
      ) +
      coord_cartesian(ylim = c(0.75, 1.45)) +
      labs(
        title = "Marktwertentwicklung (relativ zum Startwert)",
        x = "Datum", y = "Relativer Marktwert", color = "Linientyp"
      ) +
      scale_color_manual(values = c("Gesamtmarktwert" = "grey40")) +
      scale_x_date(expand = expansion(mult = c(0.01, 0.03))) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none") +
      geom_line(
        data = filter(sp, Saison == "2024-25"),
        aes(x = Datum, y = MW_rel_normiert),
        color = "red", linetype = "dotted", linewidth = 1.3, na.rm = TRUE
      ) +
      geom_line(
        data = filter(sp, Saison == "2021-22"),
        aes(x = Datum, y = MW_rel_normiert),
        color = "orange", linetype = "dotted", linewidth = 1.3, na.rm = TRUE
      )
  })
  
  ## ---- Tägliche MW-Änderung ----
  output$mw_daily_change <- renderPlot({
    req(input$main_navbar == "Marktwert-Entwicklung",
        input$mw_tabs      == "mw_verlauf")
    
    clean_df <- gesamt_mw_df %>%
      filter(!is.na(Datum)) %>%
      arrange(Datum)
    
    change_df <- clean_df %>%
      mutate(
        MW_vortag  = lag(MW_gesamt),
        abs_change = MW_gesamt - MW_vortag,
        pct_change = 100 * abs_change / MW_vortag
      ) %>%
      filter(is.finite(pct_change))  # statt nur !is.na()
    
    ggplot(change_df, aes(x = Datum, y = pct_change, fill = pct_change >= 0)) +
      geom_col() +
      geom_text(
        aes(
          label = round(pct_change, 1),
          vjust = ifelse(pct_change >= 0, -0.5, 1.5)
        ),
        size = 4
      ) +
      scale_fill_manual(values = c(`TRUE`="darkgreen", `FALSE`="red"), guide = "none") +
      scale_x_date(date_breaks = "1 week", date_labels = "%d.%m.",
                   expand = expansion(mult = c(0.01, 0.03))) +
      labs(title = "Tägliche Marktwert-Veränderung", x = "Datum", y = "∆ zum Vortag (%)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title  = element_text(size = 16)) +
      coord_cartesian(
        xlim = c(min(change_df$Datum), as.Date("2026-01-01")),   # schneidet ohne Rows zu droppen
        ylim = c(min(change_df$pct_change)*1.1, max(change_df$pct_change)*1.1)
      )
  })
  
  ## ---- Hist. Marktwert-Entwicklung ab 01.06.2024 (je Klasse) ----
  # data() reactive
  observeEvent(input$main_navbar, {
    if (!identical(input$main_navbar, "Marktwert-Entwicklung")) return()
    if (!is.null(mw_data_cache())) return()   # bereits berechnet
    
    req(exists("mw_all", inherits = TRUE), is.data.frame(mw_all))
    
    df <- mw_all
    
    # Datum robust parsen falls nötig
    if (!inherits(df$Datum, "Date")) {
      df$Datum <- suppressWarnings(as.Date(df$Datum, format = "%Y-%m-%d"))
      idx <- is.na(df$Datum)
      if (any(idx)) df$Datum[idx] <- suppressWarnings(as.Date(df$Datum[idx], format = "%d.%m.%Y"))
    }
    
    # Filter + Bereinigungen wie vorher
    df <- df %>%
      filter(!is.na(Datum) & Datum >= as.Date("2024-06-01")) %>%
      mutate(Marktwert = as.numeric(Marktwert))
    
    mw_spieler <- df %>%
      group_by(Spieler) %>%
      summarise(MW_mittel = mean(Marktwert, na.rm = TRUE), .groups = "drop")
    
    df <- df %>%
      left_join(mw_spieler, by = "Spieler") %>%
      mutate(
        Klasse = case_when(
          MW_mittel <  500000  ~ "Klasse 1: <0.5 Mio",
          MW_mittel < 1000000  ~ "Klasse 2: 0.5–1 Mio",
          MW_mittel < 2500000  ~ "Klasse 3: 1–2.5 Mio",
          MW_mittel < 5000000  ~ "Klasse 4: 2.5–5 Mio",
          MW_mittel < 10000000 ~ "Klasse 5: 5–10 Mio",
          TRUE                ~ "Klasse 6: >10 Mio"
        )
      )
    
    mw_data_cache(df)
  }, ignoreInit = TRUE)
  
  # data() reactive wrapper: feuert nur wenn Tab aktiv ist
  data <- reactive({
    req(input$main_navbar == "Marktwert-Entwicklung")
    df <- mw_data_cache()
    req(!is.null(df), nrow(df) > 0)
    df
  })
  
  ## ---- Hist. je Klasse Marktwert-Entwicklung ab 15.06.2025 ----
  
  output$mw_plot <- renderPlot({
    req(input$main_navbar == "Marktwert-Entwicklung",
        input$mw_tabs      == "mw_klassen")
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
      filter(Datum <= today - 365)
    
    
    ggplot(df_plot_norm, aes(x = Datum, y = MW_normiert, color = Klasse)) +
      geom_line(size = 1.2) +
      labs(
        title = "Historischer Marktwertverlauf je Klasse",
        y = "Normierter MW",
        x = "",
        color = "MW-Klasse"
      )  + 
      geom_vline(xintercept = as.numeric(today - 365), color = "darkred", linetype = "dashed", linewidth = 1) +
      annotate(
        "text",
        x = today - 365,
        y = 0.7,  # ggf. anpassen
        label = "Heute vor 1 Jahr",
        color = "darkred",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      scale_color_brewer(palette = "Paired") +
      coord_cartesian(ylim = c(0.6, 1.4)) +
      theme_minimal(base_size = 14) +
      scale_x_date(date_labels = "%b %y")   
  })
  
  ## ---- Je Klasse Marktwert-Entwicklung ab 15.06.2025 ----
  
  # data_now
  observeEvent(input$main_navbar, {
    if (!identical(input$main_navbar, "Marktwert-Entwicklung")) return()
    if (!is.null(mw_now_cache())) return()   # bereits berechnet
    
    # Dateiname-Präsenz prüfen
    req(exists("ap_df", inherits = TRUE), is.data.frame(ap_df))
    
    # sichere Berechnung
    df <- tryCatch({
      tmp <- ap_df
      
      # Datum robust sicherstellen
      if (!inherits(tmp$Datum, "Date")) {
        tmp$Datum <- suppressWarnings(as.Date(tmp$Datum, format = "%Y-%m-%d"))
        idx <- is.na(tmp$Datum)
        if (any(idx)) tmp$Datum[idx] <- suppressWarnings(as.Date(tmp$Datum[idx], format = "%d.%m.%Y"))
      }
      
      start_date <- as.Date("2025-06-16")
      end_date <- max(tmp$Datum, na.rm = TRUE)
      
      tmp <- tmp %>%
        filter(!is.na(Datum) & Datum >= start_date & Datum <= end_date) %>%
        mutate(Marktwert = as.numeric(Marktwert))
      
      mw_spieler <- tmp %>%
        group_by(Spieler) %>%
        summarise(MW_mittel = mean(Marktwert, na.rm = TRUE), .groups = "drop")
      
      tmp <- tmp %>%
        left_join(mw_spieler, by = "Spieler") %>%
        mutate(
          Klasse = case_when(
            MW_mittel <  500000  ~ "Klasse 1: <0.5 Mio",
            MW_mittel < 1000000  ~ "Klasse 2: 0.5–1 Mio",
            MW_mittel < 2500000  ~ "Klasse 3: 1–2.5 Mio",
            MW_mittel < 5000000  ~ "Klasse 4: 2.5–5 Mio",
            MW_mittel < 10000000 ~ "Klasse 5: 5–10 Mio",
            TRUE                ~ "Klasse 6: >10 Mio"
          ),
          Klasse = factor(Klasse, levels = c(
            "Klasse 1: <0.5 Mio","Klasse 2: 0.5–1 Mio","Klasse 3: 1–2.5 Mio",
            "Klasse 4: 2.5–5 Mio","Klasse 5: 5–10 Mio","Klasse 6: >10 Mio"
          ))
        )
      
      df_plot <- tmp %>%
        group_by(Datum, Klasse) %>%
        summarise(MW_Ø = mean(Marktwert, na.rm = TRUE), .groups = "drop")
      
      startwerte <- df_plot %>% filter(Datum == start_date) %>% select(Klasse, Start_MW = MW_Ø)
      
      df_plot_norm <- df_plot %>%
        left_join(startwerte, by = "Klasse") %>%
        mutate(MW_normiert = MW_Ø / Start_MW)
      
      df_plot_norm
    }, error = function(e) {
      message("Fehler in mw_now_cache: ", conditionMessage(e))
      data.frame()
    })
    
    mw_now_cache(df)
  }, ignoreInit = TRUE)
  
  # data_now() reactive wrapper: feuert nur wenn Tab aktiv ist
  data_now <- reactive({
    req(input$main_navbar == "Marktwert-Entwicklung")
    df <- mw_now_cache()
    req(!is.null(df), nrow(df) > 0)
    df
  })
  
  output$mw_plot_now <- renderPlot({
    req(input$main_navbar == "Marktwert-Entwicklung",
        input$mw_tabs      == "mw_klassen")
    df_plot_norm <- data_now()
    
    ggplot(df_plot_norm, aes(x = Datum, y = MW_normiert, color = Klasse)) +
      geom_line(size = 1.2) +
      labs(
        title = "Aktueller Marktwertverlauf je Klasse",
        y = "Normierter MW",
        x = "",
        color = "MW-Klasse"
      ) +
      scale_color_brewer(palette = "Paired") +
      coord_cartesian(ylim = c(0.6, 1.2)) +
      geom_vline(xintercept = as.numeric(today), color = "darkred", linetype = "dashed", linewidth = 1) +
      annotate(
        "text",
        x = today,
        y = 0.7,  # ggf. anpassen
        label = "Heute",
        color = "darkred",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      theme_minimal(base_size = 14) +
      scale_x_date(date_labels = "%b %y")   
  })
  
  ## ---- Hist. Martkwertverläufe - Chronologie ----
  
  # Alle Saison-Daten laden
  all_season_data <- reactive({
    dfs <- lapply(seasons, load_season_data)
    dfs <- dfs[!sapply(dfs, is.null)]
    bind_rows(dfs)
  })
  
  #Für Sommerpausenverlaufsvergleich
  # sommerpause_data <- reactive({
  #   all_season_data() %>%
  #     filter(Saison %in% c("Sommerpause_2021", "Sommerpause_2024")) %>%
  #     filter(Datum >= as.Date("2025-06-06")) %>%
  #     arrange(Datum) %>%
  #     group_by(Saison) %>%
  #     mutate(
  #       MW_startwert = Marktwert[Datum == as.Date("2025-06-06")][1],
  #       MW_rel_normiert = Marktwert / MW_startwert
  #     ) %>%
  #     ungroup()
  # })
  
  #Für nach-Saisonstart-Saisonsvergleich
  post_sommerpause_data <- reactive({
    all_season_data() %>%
      filter(Saison %in% c("2021-22", "2024-25")) %>% 
      mutate(Datum_raw = as.Date(Datum)) %>%
      filter(
        (Saison == "2021-22" & Datum_raw >= as.Date("2021-06-06") & Datum_raw <= as.Date("2021-12-31")) |
          (Saison == "2024-25" & Datum_raw >= as.Date("2024-06-06") & Datum_raw <= as.Date("2024-12-31"))
      ) %>%
      mutate(
        Datum = as.Date(format(Datum_raw, "2025-%m-%d"))
      ) %>% 
      distinct() %>%
      arrange(Datum) %>%
      group_by(Saison) %>%
      mutate(
        MW_startwert   = Marktwert[Datum == min(Datum)][1],
        MW_rel_normiert = Marktwert / MW_startwert
      ) %>%
      ungroup()
  })
  
  # Historische Saisonverläufe - Vergleichsauswahl (normalisierte Zeitachse)
  normalized_data <- reactive({
    req(input$selected_seasons)
    all_season_data() %>%
      filter(Saison %in% input$selected_seasons) %>%
      # statt alle zu nehmen, hier nur echte Saisons
      filter(!grepl("^Sommerpause", Saison)) %>%
      mutate(
        Datum = as.Date(Datum),
        saison_start = as.Date(paste0(substr(Saison, 1, 4), "-07-01")),
        days_since_start = as.integer(Datum - saison_start)
      ) %>%
      filter(days_since_start >= 0)
  })
  
  # Prognose Hist.
  mw_knn_pred <- reactive({
    req(input$selected_seasons)
    L <- 100          # Lookback-Tage
    k <- 5           # Anzahl Nachbarn
    
    # Historisch: pro Saison relativ zum Saisonstart normieren
    hist <- normalized_data() %>%
      group_by(Saison) %>%
      arrange(days_since_start, .by_group = TRUE) %>%
      mutate(MW_rel = Marktwert / first(Marktwert)) %>%
      ungroup()
    
    # Aktuell: Gesamtmarktwert wie oben normieren
    current_year <- format(today, "%Y")
    saison_start_this_year <- as.Date(paste0(current_year, "-07-01"))
    t0 <- as.numeric(today - saison_start_this_year)
    
    curr <- gesamt_mw_roh %>%
      group_by(Datum) %>%
      summarise(MW = sum(Marktwert, na.rm = TRUE), .groups = "drop") %>%
      mutate(days_since_start = as.numeric(Datum - saison_start_this_year)) %>%
      filter(days_since_start >= 0, days_since_start <= t0) %>%
      arrange(days_since_start) %>%
      mutate(MW_rel = MW / first(MW))
    
    # Vergleichsfenster [t0-L+1 .. t0]
    seg_curr <- curr %>%
      filter(days_since_start > t0 - L & days_since_start <= t0) %>%
      transmute(d = days_since_start - t0, MW_rel)
    
    # Distanz je Saison über das gleiche d-Raster
    dist_df <- hist %>%
      filter(days_since_start > t0 - L & days_since_start <= t0) %>%
      transmute(Saison, d = days_since_start - t0, MW_rel) %>%
      inner_join(seg_curr, by = "d", suffix = c("_hist", "_curr")) %>%
      group_by(Saison) %>%
      summarise(rmse = sqrt(mean((MW_rel_hist - MW_rel_curr)^2, na.rm = TRUE)),
                n = n(), .groups = "drop") %>%
      filter(n >= L * 0.8) %>%
      arrange(rmse)
    
    best <- head(dist_df$Saison, k)
    
    if (isTRUE(getOption("mw_include_2122", FALSE)) &&
        "2021-22" %in% dist_df$Saison && !"2021-22" %in% best) {
      best <- unique(c("2021-22", best))[seq_len(min(k, length(unique(c("2021-22", best)))))]
    }
    
    # Nachbar-Änderung für "morgen" (t0->t0+1) als Prozent
    neigh_next <- hist %>%
      filter(Saison %in% best, days_since_start %in% c(t0, t0 + 1)) %>%
      group_by(Saison) %>%
      arrange(days_since_start, .by_group = TRUE) %>%
      summarise(pct_next = 100 * (last(MW_rel) / first(MW_rel) - 1),
                .groups = "drop")
    
    pred_pct <- mean(neigh_next$pct_next, na.rm = TRUE)
    pred_sd  <- sd(neigh_next$pct_next, na.rm = TRUE)
    
    # Aktueller Wert für Plotpunkt „morgen“
    curr_today_val <- curr %>% filter(days_since_start == t0) %>% pull(MW)
    pred_abs <- curr_today_val * (pred_pct / 100)
    
    list(
      neighbors = best,
      pred_pct  = pred_pct,
      pred_sd   = pred_sd,
      t0        = t0,
      y_today   = curr_today_val,
      y_tomorrow= curr_today_val * (1 + pred_pct/100)
    )
  })
  
  # Select top 4 similar seasons
  similar_top4 <- reactive({
    L <- 100
    saison_start_this_year <- as.Date(paste0(format(today, "%Y"), "-07-01"))
    t0 <- as.numeric(today - saison_start_this_year)
    
    hist <- all_season_data() %>%
      filter(!grepl("^Sommerpause", Saison)) %>%
      mutate(
        Datum = as.Date(Datum),
        saison_start = as.Date(paste0(substr(Saison, 1, 4), "-07-01")),
        days_since_start = as.integer(Datum - saison_start)
      ) %>%
      filter(days_since_start >= 0) %>%
      group_by(Saison) %>%
      arrange(days_since_start, .by_group = TRUE) %>%
      mutate(MW_rel = Marktwert / first(Marktwert)) %>%
      ungroup()
    
    curr <- gesamt_mw_roh %>%
      group_by(Datum) %>%
      summarise(MW = sum(Marktwert, na.rm = TRUE), .groups = "drop") %>%
      mutate(days_since_start = as.numeric(Datum - saison_start_this_year)) %>%
      filter(days_since_start >= 0, days_since_start <= t0) %>%
      arrange(days_since_start) %>%
      mutate(MW_rel = MW / first(MW))
    
    seg_curr <- curr %>%
      filter(days_since_start > t0 - L & days_since_start <= t0) %>%
      transmute(d = days_since_start - t0, MW_rel)
    
    dist_df <- hist %>%
      filter(days_since_start > t0 - L & days_since_start <= t0) %>%
      transmute(Saison, d = days_since_start - t0, MW_rel) %>%
      inner_join(seg_curr, by = "d", suffix = c("_hist", "_curr")) %>%
      group_by(Saison) %>%
      summarise(rmse = sqrt(mean((MW_rel_hist - MW_rel_curr)^2, na.rm = TRUE)),
                n = n(), .groups = "drop") %>%
      filter(n >= L * 0.8) %>%
      arrange(rmse) 
    
    sel <- head(dist_df$Saison, 5)
    
    if (isTRUE(getOption("mw_include_2122", FALSE)) &&
        "2021-22" %in% dist_df$Saison && !"2021-22" %in% sel) {
      sel <- unique(c("2021-22", sel))
    }
    
    # nur echte Saisons und auf 5 begrenzen
    sel <- intersect(sel, real_seasons)[seq_len(min(5, length(sel)))]
    sel
  })
  
  options(mw_include_2122 = FALSE)  # FALSE = aus
  
  
  ## ---- Hist. Saisonverläufe - Select ----
  
  # Einmalige Initialisierung der Checkbox-Choices
  observeEvent(TRUE, {
    sel <- isolate(similar_top4())
    if (length(sel) == 0) sel <- c(real_seasons[n_real], real_seasons[n_real-2])
    sel <- sel[sel %in% real_seasons]
    updateCheckboxGroupInput(session, "selected_seasons",
                             choices = real_seasons, selected = sel)
  }, once = TRUE)
  
  
  # All Button
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(
      session, "selected_seasons",
      choices  = real_seasons,
      selected = real_seasons
    )
  })
  
  # None Button
  observeEvent(input$select_none, {
    updateCheckboxGroupInput(
      session, "selected_seasons",
      choices  = real_seasons,
      selected = character(0)
    )
  })
  
  # Custom 2 Button
  observeEvent(input$select_custom, {
    sel2 <- tail(real_seasons, 4)
    updateCheckboxGroupInput(
      session, "selected_seasons",
      choices  = real_seasons,
      selected = sel2
    )
  })
  
  # Custom 1 Button
  observeEvent(input$select_last3, {
    sel <- isolate(similar_top4())
    if (length(sel) == 0) sel <- c(real_seasons[n_real], real_seasons[n_real-2])
    sel <- sel[sel %in% real_seasons]
    updateCheckboxGroupInput(
      session, "selected_seasons",
      choices  = real_seasons,
      selected = sel
    )
  })
  
  
  output$historical_seasons_plot_selected <- renderPlot({
    req(input$main_navbar == "Marktwert-Entwicklung",
        input$mw_tabs      == "mw_hist_sel")
    df <- normalized_data()
    req(nrow(df) > 0)
    req(input$selected_seasons)
    
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
    
    # kNN-Prognose aufbereiten
    kp <- mw_knn_pred()
    knn_text <- if (is.null(kp) || is.na(kp$pred_pct)) {
      "Ähnlichkeits-Prognose: nicht genügend Daten"
    } else {
      sprintf("Ähnlichkeits-Prognose: morgen %+.2f%% (kNN, k=%d, ± %.2f)",
              kp$pred_pct, length(kp$neighbors), kp$pred_sd)
    }
    knn_arrow_label <- if (is.null(kp) || is.na(kp$pred_pct)) "" else if (kp$pred_pct > 0) "▲" else if (kp$pred_pct < 0) "▼" else ""
    knn_arrow_color <- if (is.null(kp) || is.na(kp$pred_pct)) "black" else if (kp$pred_pct > 0) "darkgreen" else if (kp$pred_pct < 0) "red" else "black"
    
    
    
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
      annotate("text", x = Inf, y = Inf, label = gesamt_prog_text,
               hjust = 1.05, vjust = 6.0, size = 5, color = "black", fontface = "bold") +
      annotate("text", x = Inf, y = Inf, label = gesamt_arrow_label,
               hjust = 1.05, vjust = 6.0, size = 5, color = gesamt_arrow_color, fontface = "bold") +
      
      annotate("text", x = Inf, y = Inf, label = prog_text,
               hjust = 1.05, vjust = 3.8, size = 5, color = "black", fontface = "bold") +
      annotate("text", x = Inf, y = Inf, label = arrow_label,
               hjust = 1.05, vjust = 3.8, size = 5, color = arrow_color, fontface = "bold") +
      
      annotate("text", x = Inf, y = Inf, label = knn_text,
               hjust = 1.05, vjust = 1.6, size = 5, color = "black", fontface = "bold") +
      annotate("text", x = Inf, y = Inf, label = knn_arrow_label,
               hjust = 1.05, vjust = 1.6, size = 5, color = knn_arrow_color, fontface = "bold") +
    
      labs(
        title = "Historische Marktwertverläufe mit Mittelwert ± SD",
        x = "Tage seit 1. Juli",
        y = "Marktwert"
      ) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom")
  })
  
  ## ---- MW-Analyse ----
  
  # Fenster: letzte 5 Wochen relativ zum jüngsten Datum
  change_df_reactive <- reactive({
    clean_df <- gesamt_mw_df %>%
      filter(!is.na(Datum)) %>%
      mutate(Datum = as.Date(Datum)) %>%
      arrange(Datum)
    
    end_date   <- max(clean_df$Datum, na.rm = TRUE)
    start_date <- end_date - 27  # 5 Wochen = 35 Tage
    
    
    df <- clean_df %>%
      mutate(
        MW_vortag  = lag(MW_gesamt),
        abs_change = MW_gesamt - MW_vortag,
        pct_change = 100 * abs_change / MW_vortag
      ) %>%
      filter(Datum >= start_date) %>%       # erst jetzt filtern
      filter(is.finite(pct_change))
    
    req(nrow(df) > 0)
    df
  })
  
  # Plot: Balken = Mittelwert, Fehlerbalken = ±SD
  output$mw_weekday_effect_plot <- renderPlot({
    df <- change_df_reactive() %>%
      mutate(
        wd_num    = as.integer(format(Datum, "%u")),  # 1=Mo ... 7=So
        wochentag = factor(c("Mo","Di","Mi","Do","Fr","Sa","So")[wd_num],
                           levels = c("Mo","Di","Mi","Do","Fr","Sa","So"), ordered = TRUE)
      )
    
    # Wochen-Bucket relativ zum jüngsten Datum 
    end_date <- max(df$Datum, na.rm = TRUE)
    df <- df %>%
      mutate(
        week_idx = pmin(5L, floor(as.numeric(end_date - Datum) / 7) + 1L),
        week_lbl = factor(paste0("W-", week_idx), levels = paste0("W-", 1:5))
      )
    
    
    sum_df <- df %>%
      group_by(wochentag) %>%
      summarise(
        mean_change = mean(pct_change, na.rm = TRUE),
        sd_change   = sd(pct_change,   na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(sum_df, aes(x = wochentag, y = mean_change, fill = mean_change >= 0)) +
      geom_col(width = 0.7) +
      # SD: grau, dashed, etwas dicker
      geom_errorbar(aes(ymin = mean_change - sd_change,
                        ymax = mean_change + sd_change),
                    width = 0.2, linewidth = 0.8,
                    colour = "grey40", linetype = "dashed") +
      # Beeswarm: einzelne Tage, farbig je Woche (W-1 ... W-4)
      geom_beeswarm(
        data = df,
        mapping = aes(x = wochentag, y = pct_change, colour = week_lbl),
        inherit.aes = FALSE,
        size = 4, cex = 1.2, dodge.width = 0.6, show.legend = TRUE
      ) +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      scale_fill_manual(values = c(`TRUE` = "darkgreen", `FALSE` = "red"), guide = "none") +
      scale_colour_manual(
        values = c("W-1"="#0072B2","W-2"="#E69F00","W-3"="#009E73","W-4"="#D55E00","W-5"="#CC79A7"),
        name = "Woche"
      ) +
      labs(title = "Wochentagseffekt", x = NULL, y = "Tagesänderung (%)") +
      theme_minimal(base_size = 16)
  })
  
  ### ===== Optionen =====
  # Inputs -> options()
  observe({
    if (!is.null(input$ref_use))    options(mw_ref_use    = input$ref_use)
    if (!is.null(input$ref_season)) options(mw_ref_season = input$ref_season)
    if (!is.null(input$ref_weight)) options(mw_ref_weight = input$ref_weight)
    if (!is.null(input$w_perm))     options(mw_w_perm     = input$w_perm)
    if (!is.null(input$w_wd))       options(mw_w_wd       = input$w_wd)
    if (!is.null(input$w_l7))       options(mw_w_lag7    = input$w_l7)
  })
  
  observeEvent(TRUE, {
    updateCheckboxInput(session, "ref_use",   value = isTRUE(getOption("mw_ref_use", TRUE)))
    updateSelectInput( session, "ref_season", selected = as.character(getOption("mw_ref_season", "2021-22")))
    updateSliderInput( session, "ref_weight", value = as.numeric(getOption("mw_ref_weight", 0.1)))
    updateSliderInput( session, "w_perm",     value = as.numeric(getOption("mw_w_perm", 0.05)))
    updateSliderInput( session, "w_wd",       value = as.numeric(getOption("mw_w_wd", 0.9)))
    updateSliderInput( session, "w_l7",       value = as.numeric(getOption("mw_w_lag7", 0.05)))
  }, once = TRUE)
  
  
  ## --- mw_pred delayed compute einmal nach X Sekunden nach App-Start (deaktiviert, nur wenn gewünscht)
  observe({
    if (is.null(mw_pred_cached())) {
      invalidateLater(1000, session)  # 1s nach Start
      mw_pred_cached( tryCatch(mw_pred(), error = function(e) NULL) )
    }
  })
  
  #### ===== mw_pred =====
  mw_pred <- reactive({
    # zuerst warten, bis Inputs initialisiert sind
    req(!is.null(input$ref_use), !is.null(input$ref_season), !is.null(input$ref_weight),
        !is.null(input$w_perm),  !is.null(input$w_wd),      !is.null(input$w_l7))
    
    # reaktive Abhängigkeit
    deps <- list(input$ref_use, input$ref_season, input$ref_weight,
                 input$w_perm,  input$w_wd,      input$w_l7)
    force(deps)
    
    clean_df <- gesamt_mw_df %>% filter(!is.na(Datum)) %>% arrange(Datum)
    
    df <- clean_df %>%
      mutate(
        MW_vortag  = lag(MW_gesamt),
        abs_change = MW_gesamt - MW_vortag,
        pct_change = 100 * abs_change / MW_vortag,
        Datum      = as.Date(Datum),
        lag1       = lag(pct_change),
        lag2       = lag(pct_change, 2),
        lag7       = lag(pct_change, 7),
        wd         = factor(as.integer(format(Datum, "%u")),
                            levels = 1:7, labels = c("Mo","Di","Mi","Do","Fr","Sa","So"))
      ) %>%
      filter(is.finite(pct_change), is.finite(lag1))
    
    if (nrow(df) < 30) return(NULL)
    
    last_date <- max(df$Datum)
    train_len <- 30
    train_df  <- df %>% filter(Datum >= last_date - train_len)
    
    # Winsorizing
    q <- quantile(train_df$pct_change, probs = c(0.05, 0.95), na.rm = TRUE)
    clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)
    train_df <- train_df %>%
      mutate(
        pct_w = clamp(pct_change, q[1], q[2]),
        lag1  = clamp(lag1,      q[1], q[2]),
        lag2  = clamp(lag2,      q[1], q[2]),
        lag7  = clamp(lag7,      q[1], q[2])
      )
    
    # Zeitgewichte (Half-Life ~9 Tage)
    half_life <- 9
    w <- 0.5 ^ (as.numeric(last_date - train_df$Datum) / half_life)
    
    # --- Nur für Wochentag: letzten Wochen ---
    wd_df <- train_df %>% filter(Datum >= last_date - 27)
    if (nrow(wd_df) < 10) wd_df <- train_df
    w_wd <- w[match(wd_df$Datum, train_df$Datum, nomatch = NA)]
    
    # --- Gewichte für Basismix aus Optionen ---
    w_perm <- as.numeric(getOption("mw_w_perm", 0.6))
    w_wd_m <- as.numeric(getOption("mw_w_wd", 0.3))
    w_lag7 <- as.numeric(getOption("mw_w_lag7", 0.1))
    wsum   <- sum(w_perm, w_wd_m, w_lag7, na.rm = TRUE)
    if (!is.finite(wsum) || wsum <= 0) { w_perm <- 1; w_wd_m <- 0; w_lag7 <- 0; wsum <- 1 }
    a_perm <- w_perm/wsum; a_wd <- w_wd_m/wsum; a_lag7 <- w_lag7/wsum
    
    # --- Modelle ---
    # Richtung (volle Spezifikation auf train_df)
    m_prob <- glm(I(pct_w > 0) ~ lag1 + lag2 + lag7 + wd,
                  data = train_df, family = quasibinomial(), weights = w)
    
    # Drei Teilmodelle für Größenprognose
    m_perm <- lm(pct_w ~ lag1 + lag2, data = train_df, weights = w)
    m_wd   <- lm(pct_w ~ wd,          data = wd_df,    weights = w_wd)
    m_l7   <- lm(pct_w ~ lag7,        data = train_df, weights = w)
    
    safe_sigma <- function(mod){
      s <- tryCatch(summary(mod)$sigma, error = function(e) NA_real_)
      if (!is.finite(s)) sd(residuals(mod), na.rm = TRUE) else s
    }
    sig_perm <- safe_sigma(m_perm)
    sig_wd   <- safe_sigma(m_wd)
    sig_l7   <- safe_sigma(m_l7)
    crit     <- qnorm(0.975)
    
    # Vorwärtsinitialisierung
    last_row   <- tail(df, 1)
    lag1_now   <- clamp(last_row$pct_change, q[1], q[2])
    lag2_now   <- clamp(last_row$lag1,       q[1], q[2])
    lag7_const <- clamp(last_row$lag7,       q[1], q[2])
    
    next_dates <- seq(last_date + 1, by = 1, length.out = 2)
    next_wd    <- factor(as.integer(format(next_dates, "%u")),
                         levels = 1:7, labels = levels(df$wd))
    
    res <- vector("list", 2)
    for (k in 1:2) {
      newd <- data.frame(lag1 = lag1_now, lag2 = lag2_now, lag7 = lag7_const,
                         wd = factor(next_wd[k], levels = levels(df$wd)))
      
      # Richtung
      p_up_k <- as.numeric(predict(m_prob, newdata = newd, type = "response"))
      
      # Teilvorhersagen + Unsicherheiten
      prP <- predict(m_perm, newdata = newd, se.fit = TRUE); fitP <- as.numeric(prP$fit); seP <- as.numeric(prP$se.fit); seP[!is.finite(seP)] <- 0
      prW <- predict(m_wd,   newdata = newd, se.fit = TRUE); fitW <- as.numeric(prW$fit); seW <- as.numeric(prW$se.fit); seW[!is.finite(seW)] <- 0
      prL <- predict(m_l7,   newdata = newd, se.fit = TRUE); fitL <- as.numeric(prL$fit); seL <- as.numeric(prL$se.fit); seL[!is.finite(seL)] <- 0
      
      # Ensemble-Mittelpunkt
      y_base <- a_perm*fitP + a_wd*fitW + a_lag7*fitL
      
      # Ensemble-Varianz-Approx
      var_predP <- seP^2 + sig_perm^2
      var_predW <- seW^2 + sig_wd^2
      var_predL <- seL^2 + sig_l7^2
      var_mix   <- (a_perm^2)*var_predP + (a_wd^2)*var_predW + (a_lag7^2)*var_predL
      se_mix    <- sqrt(pmax(0, var_mix))
      
      yhat <- y_base
      lwr  <- y_base - crit*se_mix
      upr  <- y_base + crit*se_mix
      
      # ---- Saison-Analog-Boost (unverändert)
      ref_use    <- isTRUE(input$ref_use)
      ref_season <- as.character(input$ref_season)
      ref_weight <- as.numeric(input$ref_weight)
      ref_weight <- ifelse(is.finite(ref_weight), pmin(1, pmax(0, ref_weight)), 0.1)
      
      if (ref_use && !is.na(ref_season)) {
        curr_start <- min(df$Datum, na.rm = TRUE)
        ref_start  <- if (grepl("2021", ref_season)) as.Date("2021-06-01") else
          if (grepl("2024", ref_season)) as.Date("2024-06-01") else NA
        
        get_sp_safe <- function(){
          out <- NULL
          if (exists("post_sommerpause_data", mode = "function"))
            out <- tryCatch(post_sommerpause_data(), error = function(e) NULL)
          if (is.null(out) && exists("sp", inherits = TRUE))
            out <- tryCatch(get("sp", inherits = TRUE), error = function(e) NULL)
          out
        }
        sp_all <- get_sp_safe()
        
        # only enable analog boost when ref_start valid, ref_use TRUE and history present
        if (!is.na(ref_start) && ref_use && !is.null(sp_all) && is.data.frame(sp_all)) {
          sp_ref <- sp_all %>%
            filter(Saison == ref_season) %>%
            mutate(Datum = as.Date(Datum)) %>%
            arrange(Datum) %>%
            mutate(idx = as.integer(Datum - ref_start))
          
          if (nrow(sp_ref) > 10) {
            ref_val <- function(ix) with(sp_ref, approx(x = idx, y = MW_rel_normiert, xout = ix, rule = 2, ties = "ordered")$y)
            idx_k <- as.integer(next_dates[k] - curr_start)
            v_t   <- ref_val(idx_k)
            v_tm3 <- ref_val(idx_k - 3)
            
            if (is.finite(v_t) && is.finite(v_tm3) && v_tm3 > 0) {
              delta3_ref <- 100 * (v_t / v_tm3 - 1)
              
              vol_curr <- df %>%
                mutate(delta3 = 100 * (MW_gesamt / lag(MW_gesamt, 3) - 1)) %>%
                filter(Datum > last_date - 60, is.finite(delta3)) %>%
                summarise(s = sd(delta3, na.rm = TRUE)) %>% pull(s)
              
              vol_prev <- sp_ref %>%
                mutate(prev3 = lag(MW_rel_normiert, 3),
                       delta3 = 100 * (MW_rel_normiert / prev3 - 1)) %>%
                summarise(s = sd(delta3, na.rm = TRUE)) %>% pull(s)
              
              scale_fac <- if (is.finite(vol_curr) && is.finite(vol_prev) && vol_prev > 0)
                pmin(2.0, pmax(0.5, vol_curr / vol_prev)) else 1.0
              
              analog_adj <- delta3_ref * scale_fac
              if (is.finite(analog_adj)) {
                width <- abs(upr - yhat)
                yhat  <- (1 - ref_weight)*yhat + ref_weight*analog_adj
                lwr   <- yhat - width
                upr   <- yhat + width
              }
            }
          } else {
            # zu wenig Referenzdaten -> deaktivieren
            ref_use <- FALSE
            message("Analog-Boost deaktiviert: zu wenige Referenzdaten für ", ref_season)
          }
        } else {
          # keine valide Referenzbasis -> deaktivieren
          ref_use <- FALSE
          if (is.na(ref_start)) message("Analog-Boost deaktiviert: ref_start NA für ", ref_season)
          if (is.null(sp_all) || !is.data.frame(sp_all))
            message("Analog-Boost deaktiviert: historische Saison-Daten nicht verfügbar für ", ref_season)
        }
      }
      # ---- Ende Analog-Boost
      
      res[[k]] <- data.frame(
        Datum     = as.Date(next_dates[k]),
        Wochentag = as.character(next_wd[k]),
        P_up      = p_up_k,
        E_delta   = yhat, lwr = lwr, upr = upr
      )
      
      # Lags für T+2
      lag2_now <- lag1_now
      lag1_now <- clamp(yhat, q[1], q[2])
    }
    
    pred_df <- do.call(rbind, res) %>%
      arrange(Datum) %>%
      mutate(
        Wochentag = factor(Wochentag, levels = c("Mo","Di","Mi","Do","Fr","Sa","So")),
        label_raw = paste0(as.character(Wochentag), " ", format(Datum, "%d.%m.")),
        combo_lbl = sprintf("%.0f%% → %+.2f%%", 100 * P_up, E_delta)
      )
    
    # Info-Test für Dashboard-Graphen
    p_up <- pred_df$P_up
    p_down <- 1 - p_up
    
    direction_lbl <- ifelse(p_up >= 0.5,
                            paste0("P↑ ", sprintf("%.1f%%", 100 * p_up)),
                            paste0("P↓ ", sprintf("%.1f%%", 100 * p_down)))
    
    info_txt <- paste0(
      pred_df$label_raw, ": ", direction_lbl,
      ", Δ ", sprintf("%+.2f%%", pred_df$E_delta)
    )
    
    
    list(pred_df = pred_df, info_txt = paste(info_txt, collapse = "\n"))
  })
  
  #### ---- 2-Tages Plot ----
  output$mw_predict_plot <- renderPlot({
    # ausschliesslich Cache verwenden; wenn leer, neu versuchen in 1s
    mp <- tryCatch(mw_pred_cached(), error = function(e) NULL)
    if (is.null(mp)) {
      invalidateLater(1000, session)   # re-evaluate in 1s
      return(invisible(NULL))
    }
    pred_df <- mp$pred_df
    
    pred_df$label <- factor(pred_df$label_raw, levels = pred_df$label_raw)
    
    # Skala für Δ so, dass 0% -> 0 (gemeinsamer Ursprung)
    d_min <- suppressWarnings(min(pred_df$lwr, pred_df$E_delta, na.rm = TRUE))
    d_max <- suppressWarnings(max(pred_df$upr, pred_df$E_delta, na.rm = TRUE))
    R <- max(abs(d_min), abs(d_max))
    if (!is.finite(R) || R <= 0) R <- 1
    map_y <- function(d) d / R
    inv_y <- function(y) y * R
    
    # Farben je Sicherheit
    pred_df <- pred_df %>%
      dplyr::mutate(
        p_down = 1 - P_up,
        prob_signed = ifelse(E_delta >= 0, P_up, -p_down),   # <- geändert: nutzt p_down für negative Fälle
        col_delta = dplyr::case_when(
          E_delta >= 0 & P_up > 0.75 ~ "#1B5E20",
          E_delta >= 0 & P_up > 0.25 ~ "#2E7D32",
          E_delta >= 0               ~ "#66BB6A",
          E_delta <  0 & p_down > 0.75 ~ "#B71C1C",
          E_delta <  0 & p_down > 0.25 ~ "#D32F2F",
          TRUE                         ~ "#EF5350"
        ),
        prob_lbl = ifelse(E_delta >= 0,
                          sprintf("P↑ %.1f%%", 100 * P_up),
                          sprintf("P↓ %.1f%%", 100 * p_down)),
        label_text = paste0(prob_lbl, "\n", sprintf("%+.2f%%", E_delta))
      )
    
    # Optionaler Subtitle
    ref_use <- isTRUE(getOption("mw_ref_use", TRUE))
    ref_sea <- as.character(getOption("mw_ref_season", 2021-22))
    ref_wgt <- getOption("mw_ref_weight", 0.1)
    sub_txt <- if (ref_use && is.finite(ref_wgt) && !is.na(ref_sea))
      sprintf("Analog-Boost: %s, Gewicht %.0f%%", ref_sea, 100*max(0,min(1,ref_wgt))) else NULL
    
    # dynamische Plot-Limits so, dass Δ und CIs vollständig sichtbar sind
    y_min <- min(pred_df$lwr, pred_df$E_delta, -R * 1.1, na.rm = TRUE)
    y_max <- max(pred_df$upr, pred_df$E_delta,  R * 1.1, na.rm = TRUE)
    pad   <- 0.03 * (y_max - y_min); y_min <- y_min - pad; y_max <- y_max + pad
    
    ggplot(pred_df, aes(x = label)) +
      # Wahrscheinlichkeit (zuerst, leicht transparent, liegt hinten)
      geom_col(aes(y = prob_signed * R), width = 0.55, fill = "grey70", alpha = 0.35) +
      # Δ% (darüber, voll sichtbar)
      geom_col(aes(y = E_delta, fill = col_delta),
               width = 0.3, colour = "black", linewidth = 0.2) +
      # Unsicherheit Δ (darüber)
      geom_errorbar(aes(ymin = lwr, ymax = upr),
                    width = 0.12, linewidth = 0.8,
                    linetype = "dashed", colour = "grey40") +
      # Null-Linie
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
      # Labels nahe den Prob-Balken (in Primärachse-Einheiten)
      geom_text(aes(y = ifelse(prob_signed >= 0,
                               pmin(R * 1.05, prob_signed * R + 0.06 * R),
                               pmax(-R * 1.05, prob_signed * R - 0.06 * R)),
                    label = label_text),
                size = 5, fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(
        limits = c(y_min, y_max),
        name   = "Δ Marktwert (%)",
        sec.axis = sec_axis(~ . / R, name = "Wahrscheinlichkeit (signiert)")
      ) +
      labs(title = "2-Tages-Vorhersage", subtitle = sub_txt, x = NULL, y = NULL) +
      theme_minimal(base_size = 16)
    
    
  })
  
  ##### ---- Tägliches Speichern ----
  # 1. Täglicher Tick (einmal pro Minute prüfen)
  today_tick <- reactive({
    invalidateLater(60 * 1000)
    as.Date(Sys.time())
  })

  # 2. Einmal täglich speichern (nur nach 06:15 Europe/Berlin)
  observeEvent(today_tick(), {
    now <- as.POSIXct(Sys.time(), tz = "Europe/Berlin")
    if (format(now, "%H%M") < "0615") return(invisible(NULL))
    
    pred <- mw_pred(); if (is.null(pred)) return()
    
    # kNN holen – wenn Auswahl (noch) nicht initialisiert, Fallback auf alle Saisons
    kp <- tryCatch({
      if (!is.null(isolate(input$selected_seasons)) &&
          length(isolate(input$selected_seasons)) > 0) {
        mw_knn_pred()
      } else {
        # --- Fallback kNN ohne UI-Auswahl (gleiches L/k wie oben verwenden) ---
        L <- 100; k <- 5
        saison_start_this_year <- as.Date(paste0(format(today, "%Y"), "-07-01"))
        t0 <- as.numeric(today - saison_start_this_year)
        
        hist <- all_season_data() %>%
          filter(!grepl("^Sommerpause", Saison)) %>%
          mutate(Datum = as.Date(Datum),
                 saison_start = as.Date(paste0(substr(Saison, 1, 4), "-07-01")),
                 days_since_start = as.integer(Datum - saison_start)) %>%
          filter(days_since_start >= 0) %>%
          group_by(Saison) %>%
          arrange(days_since_start, .by_group = TRUE) %>%
          mutate(MW_rel = Marktwert / first(Marktwert)) %>%
          ungroup()
        
        curr <- gesamt_mw_roh %>%
          group_by(Datum) %>%
          summarise(MW = sum(Marktwert, na.rm = TRUE), .groups = "drop") %>%
          mutate(days_since_start = as.numeric(Datum - saison_start_this_year)) %>%
          filter(days_since_start >= 0, days_since_start <= t0) %>%
          arrange(days_since_start) %>%
          mutate(MW_rel = MW / first(MW))
        
        seg_curr <- curr %>%
          filter(days_since_start > t0 - L & days_since_start <= t0) %>%
          transmute(d = days_since_start - t0, MW_rel)
        
        dist_df <- hist %>%
          filter(days_since_start > t0 - L & days_since_start <= t0) %>%
          transmute(Saison, d = days_since_start - t0, MW_rel) %>%
          inner_join(seg_curr, by = "d", suffix = c("_hist", "_curr")) %>%
          group_by(Saison) %>%
          summarise(rmse = sqrt(mean((MW_rel_hist - MW_rel_curr)^2, na.rm = TRUE)),
                    n = n(), .groups = "drop") %>%
          filter(n >= L * 0.8) %>%
          arrange(rmse)
        
        best <- head(dist_df$Saison, k)
        # 2021-22 ggf. erzwingen
        if (isTRUE(getOption("mw_include_2122", FALSE)) &&
            "2021-22" %in% dist_df$Saison && !"2021-22" %in% best) {
          best <- unique(c("2021-22", best))[seq_len(min(k, length(unique(c("2021-22", best)))))]
        }
        
        neigh_next <- hist %>%
          filter(Saison %in% best, days_since_start %in% c(t0, t0 + 1)) %>%
          group_by(Saison) %>%
          arrange(days_since_start, .by_group = TRUE) %>%
          summarise(pct_next = 100 * (last(MW_rel) / first(MW_rel) - 1),
                    .groups = "drop")
        
        list(pred_pct = mean(neigh_next$pct_next, na.rm = TRUE))
      }
    }, error = function(e) NULL)
    
    dir.create("data", showWarnings = FALSE, recursive = TRUE)
    path <- file.path("data", "mw_pred.csv")
    
    new <- pred$pred_df
    new$run_date <- as.character(as.Date(now))
    new <- new[, c("run_date","Datum","Wochentag","P_up","E_delta","lwr","upr")]
    
    new$Datum     <- as.Date(new$Datum)
    new$Wochentag <- as.character(new$Wochentag)
    
    # kNN-Prognose als Prozent-Delta für T-1 setzen
    new$knn_delta <- NA_real_
    if (!is.null(kp) && is.list(kp) && is.finite(kp$pred_pct)) {
      idx <- which(new$Datum == as.Date(new$run_date[1]) + 1L)
      if (length(idx) >= 1) new$knn_delta[idx[1]] <- kp$pred_pct
    }
    
    if (file.exists(path)) {
      old <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.null(old) && "run_date" %in% names(old) && any(old$run_date == new$run_date[1])) {
        return(invisible(NULL))
      }
      if (!is.null(old) && !"knn_delta" %in% names(old)) old$knn_delta <- NA_real_
      if (!is.null(old)) {
        old$Datum     <- as.Date(old$Datum)
        old$Wochentag <- as.character(old$Wochentag)
      }
      out <- if (is.null(old)) new else bind_rows(old, new)
    } else {
      out <- new
    }
    
    utils::write.csv(out, path, row.names = FALSE)
    message("mw_pred gespeichert: ", path)
  })
  
  # Erstelle Datensatzals reactive function für Prediction-Tabelle mw_pred_ts_metrics
  pred_ts_metrics <- reactive({
    path <- file.path("data", "mw_pred.csv")
    validate(need(file.exists(path), "Noch keine gespeicherten Vorhersagen."))
    
    pred <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    validate(need(!is.null(pred), "Vorhersage-Datei ist nicht lesbar."))
    
    model_cutoff <- as.Date("2025-10-26")
    
    pred$run_date <- as.Date(pred$run_date)
    pred$Datum    <- as.Date(pred$Datum)
    if (!"knn_delta" %in% names(pred)) pred$knn_delta <- NA_real_
    
    act_df <- gesamt_mw_df %>%
      filter(!is.na(Datum)) %>%
      transmute(Datum = as.Date(Datum),
                MW_gesamt = as.numeric(MW_gesamt)) %>%
      arrange(Datum) %>%
      mutate(
        MW_vortag    = lag(MW_gesamt),
        actual_delta = 100 * (MW_gesamt - MW_vortag) / MW_vortag
      ) %>%
      filter(is.finite(actual_delta)) %>%
      select(Datum, actual_delta)
    
    ts_df <- pred %>%
      mutate(
        horizon = case_when(
          run_date == Datum - 1L ~ "T-1",
          run_date == Datum - 2L ~ "T-2",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(horizon)) %>%
      select(Datum, horizon, E_delta) %>%
      group_by(Datum, horizon) %>%
      summarise(E_delta = mean(E_delta, na.rm = TRUE), .groups = "drop") %>%
      arrange(Datum)
    
    eps <- 1e-9
    sign0 <- function(x) ifelse(abs(x) < eps, 0, ifelse(x > 0, 1, -1))
    
    cmp <- ts_df %>%
      inner_join(act_df, by = "Datum") %>%
      mutate(
        abs_err = abs(E_delta - actual_delta),
        sign_ok = sign0(E_delta) == sign0(actual_delta),
        model   = ifelse(Datum <= model_cutoff, "M1", "M2")
      )
    
    met_by_model <- cmp %>%
      group_by(model, horizon) %>%
      summarise(
        mae = mean(abs_err, na.rm = TRUE),
        acc = mean(sign_ok, na.rm = TRUE),
        .groups = "drop"
      )
    
    pick <- function(m, h, col) {
      v <- met_by_model[[col]][met_by_model$model == m & met_by_model$horizon == h]
      ifelse(length(v) == 1 && is.finite(v), v, NA_real_)
    }
    
    # kNN T-1
    knn_cmp <- pred %>%
      filter(run_date == Datum - 1L, is.finite(knn_delta)) %>%
      inner_join(act_df, by = "Datum") %>%
      mutate(
        abs_err = abs(knn_delta - actual_delta),
        sign_ok = sign0(knn_delta) == sign0(actual_delta)
      )
    
    acc_knn <- mean(knn_cmp$sign_ok, na.rm = TRUE)
    mae_knn <- mean(knn_cmp$abs_err, na.rm = TRUE)
    
    tibble::tibble(
      Modell   = c("M1", "M1", "M2", "M2", "kNN"),
      Horizont = c("T-1", "T-2", "T-1", "T-2", "T-1"),
      Acc      = c(
        pick("M1","T-1","acc"),
        pick("M1","T-2","acc"),
        pick("M2","T-1","acc"),
        pick("M2","T-2","acc"),
        acc_knn
      ),
      MAE      = c(
        pick("M1","T-1","mae"),
        pick("M1","T-2","mae"),
        pick("M2","T-1","mae"),
        pick("M2","T-2","mae"),
        mae_knn
      )
    )
  })
  
  #### ---- Vorhersagen-Check Tabelle ----
  output$mw_pred_ts_metrics <- renderUI({
    df <- pred_ts_metrics()
    req(nrow(df) > 0)
    
    fmt_acc <- function(x) ifelse(is.finite(x), paste0(round(100*x), "%"), "n/a")
    fmt_mae <- function(x) ifelse(is.finite(x), sprintf("%.2f%%", x), "n/a")
    
    rows <- lapply(seq_len(nrow(df)), function(i) {
      tags$tr(
        tags$td(df$Modell[i]),
        tags$td(df$Horizont[i]),
        tags$td(style = "text-align:right;", fmt_acc(df$Acc[i])),
        tags$td(style = "text-align:right;", fmt_mae(df$MAE[i]))
      )
    })
    
    tags$div(
      style = "width:100%;",
      tags$strong("Güte je Modell"),
      tags$table(
        style = "width:100%; border-collapse:collapse; margin-top:8px; font-size:14px;",
        tags$thead(
          tags$tr(
            tags$th("Modell",   style="text-align:left;  border-bottom:1px solid #ddd; padding-bottom:4px;"),
            tags$th("Horizont", style="text-align:left;  border-bottom:1px solid #ddd; padding-bottom:4px;"),
            tags$th("Acc",      style="text-align:right; border-bottom:1px solid #ddd; padding-bottom:4px;"),
            tags$th("MAE",      style="text-align:right; border-bottom:1px solid #ddd; padding-bottom:4px;")
          )
        ),
        tags$tbody(rows)
      ),
      tags$div(style="color:#666; font-size:12px; margin-top:6px;",
               "Acc = Vorzeichen-Trefferquote · MAE = mittlerer |Fehler| in %")
    )
  })
  
  #### ---- Vorhersagen-Check Zeitreihen-Plot ----
  output$mw_pred_ts_plot <- renderPlot({
    path <- file.path("data", "mw_pred.csv")
    validate(need(file.exists(path), "Noch keine gespeicherten Vorhersagen."))
    
    model_cutoff <- as.Date("2025-10-26")
    
    # Vorhersagen laden
    pred <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    validate(need(!is.null(pred), "Vorhersage-Datei ist nicht lesbar."))
    
    pred$run_date <- as.Date(pred$run_date)
    pred$Datum    <- as.Date(pred$Datum)
    if (!"knn_delta" %in% names(pred)) pred$knn_delta <- NA_real_
    
    # Zeitreihen der Modellvorhersagen (T-1/T-2)
    ts_df <- pred %>%
      mutate(
        horizon = case_when(
          run_date == Datum - 1L ~ "T-1→Heute",
          run_date == Datum - 2L ~ "T-2→Heute",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(horizon)) %>%
      select(Datum, horizon, E_delta) %>%
      group_by(Datum, horizon) %>%
      summarise(E_delta = mean(E_delta, na.rm = TRUE), .groups = "drop") %>%
      arrange(Datum)
    
    validate(need(nrow(ts_df) > 0, "Keine passenden Horizonte in mw_pred.csv."))
    
    # Ist-Werte
    act_df <- gesamt_mw_df %>%
      filter(!is.na(Datum)) %>%
      transmute(Datum = as.Date(Datum),
                MW_gesamt = as.numeric(MW_gesamt)) %>%
      arrange(Datum) %>%
      mutate(
        MW_vortag    = lag(MW_gesamt),
        actual_delta = 100 * (MW_gesamt - MW_vortag) / MW_vortag
      ) %>%
      filter(is.finite(actual_delta)) %>%
      select(Datum, actual_delta)
    
    # Nur die Tage plotten, für die auch Vorhersagen existieren
    pred_dates <- sort(unique(ts_df$Datum))
    act_df <- act_df %>% filter(Datum %in% pred_dates)
    
    # kNN-Serie: T-1 bezogen (run_date == Datum - 1)
    knn_df <- pred %>%
      filter(run_date == Datum - 1L) %>%
      select(Datum, knn_delta) %>%
      filter(is.finite(knn_delta)) %>%
      arrange(Datum)
    
    ggplot(ts_df, aes(x = Datum, y = E_delta, group = horizon, color = horizon)) +
      geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.4) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.6) +
      geom_line(data = act_df,
                inherit.aes = FALSE,
                aes(x = Datum, y = actual_delta, color = "Ist-Wert", group = 1),
                linewidth = 0.8) +
      geom_point(data = act_df,
                 inherit.aes = FALSE,
                 aes(x = Datum, y = actual_delta, color = "Ist-Wert"),
                 size = 1.6) +
      geom_line(data = knn_df,
                inherit.aes = FALSE,
                aes(x = Datum, y = knn_delta, color = "kNN T-1"),
                linewidth = 0.8) +
      geom_point(data = knn_df,
                 inherit.aes = FALSE,
                 aes(x = Datum, y = knn_delta, color = "kNN T-1"),
                 size = 1.6) +
      geom_vline(xintercept = model_cutoff, linetype = "longdash", color = "grey40") +
      scale_color_manual(
        name   = NULL,
        breaks = c("T-2→Heute", "T-1→Heute", "kNN T-1", "Ist-Wert"),
        values = c("T-1→Heute" = "red", "T-2→Heute" = "blue", "kNN T-1" = "grey90", "Ist-Wert" = "black")
      ) +
      labs(
        caption = "Bis inkl. 25.10: Modell 1 (2-Tages-/Wochenrhythmus).  Ab 26.10: Modell 2 (Lag7 + Saison-Boost).  kNN: Ähnlichkeits-Prognose.",
        x = NULL, y = "Δ Marktwert (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", plot.title = element_blank())
  })
  
  #### ---- Vorhersagen-Check Heute ----
  output$mw_pred_vs_actual_plot <- renderPlot({
    path <- file.path("data","mw_pred.csv")
    validate(need(file.exists(path), "Noch keine gespeicherten Vorhersagen."))
    
    pred <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    validate(need(!is.null(pred), "Vorhersage-Datei ist nicht lesbar."))
    
    pred$run_date <- as.Date(pred$run_date)
    pred$Datum    <- as.Date(pred$Datum)
    if (!"knn_delta" %in% names(pred)) pred$knn_delta <- NA_real_
    
    act <- gesamt_mw_df %>%
      filter(!is.na(Datum)) %>%
      arrange(Datum) %>%
      mutate(
        MW_vortag  = lag(MW_gesamt),
        abs_change = MW_gesamt - MW_vortag,
        pct_change = 100 * abs_change / MW_vortag
      ) %>%
      filter(is.finite(pct_change)) %>%
      select(Datum, actual_delta = pct_change)
    
    last_actual_day <- max(act$Datum, na.rm = TRUE)
    
    cmp <- pred %>%
      inner_join(act, by = "Datum") %>%
      filter(
        Datum == last_actual_day,
        run_date %in% c(Datum - 1L, Datum - 2L)
      ) %>%
      mutate(
        horizon = ifelse(run_date == Datum - 1L, "T-1", "T-2")
      )
    
    validate(need(nrow(cmp) > 0, "Noch keine passenden Vorhersagen für den letzten Markttag."))
    
    # Reihenfolge links T-1, rechts T-2
    x_levels <- c("T-1","T-2")
    cmp$horizon <- factor(cmp$horizon, levels = x_levels)
    
    # kNN T-1 Punkt bei T-1
    knn_val <- pred %>%
      filter(Datum == last_actual_day, run_date == Datum - 1L) %>%
      summarise(v = mean(knn_delta, na.rm = TRUE), .groups = "drop") %>%
      pull(v)
    if (!is.finite(knn_val)) knn_val <- NA_real_
    knn_pt <- data.frame(horizon = factor("T-1", levels = x_levels), y = knn_val)
    
    ggplot(data = cmp, aes(x = horizon)) +
      # Prognose T-1 (rot)
      {if (any(cmp$horizon == "T-1")) geom_point(
        data = subset(cmp, horizon == "T-1"),
        aes(y = E_delta, color = "T-1"), size = 3)} +
      {if (any(cmp$horizon == "T-1")) geom_errorbar(
        data = subset(cmp, horizon == "T-1"),
        aes(ymin = lwr, ymax = upr, color = "T-1"),
        width = 0.15, linewidth = 0.4, linetype = "dashed")} +
      
      # Prognose T-2 (blau)
      {if (any(cmp$horizon == "T-2")) geom_point(
        data = subset(cmp, horizon == "T-2"),
        aes(y = E_delta, color = "T-2"), size = 3)} +
      {if (any(cmp$horizon == "T-2")) geom_errorbar(
        data = subset(cmp, horizon == "T-2"),
        aes(ymin = lwr, ymax = upr, color = "T-2"),
        width = 0.15, linewidth = 0.4, linetype = "dashed")} +
      
      # kNN T-1
      {if (is.finite(knn_val)) geom_point(
        data = knn_pt, aes(x = horizon, y = y, color = "kNN T-1"),
        size = 3, shape = 15)} +
      
      # Ist-Wert an beiden x
      geom_point(aes(y = actual_delta, color = "Ist-Wert"), shape = 17, size = 3) +
      
      geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.4) +
      scale_color_manual(
        name = NULL,
        values = c("T-1" = "red", "T-2" = "blue", "kNN T-1" = "grey80", "Ist-Wert" = "black")
      ) +
      labs(
        x = NULL, y = "Δ Marktwert (%)",
        subtitle = paste(
          sprintf("T-1: Δ %s",
                  ifelse(any(cmp$horizon == "T-1"),
                         sprintf("%+.2f%%", cmp$E_delta[cmp$horizon == "T-1"][1]),
                         "n/a")),
          sprintf("T-2: Δ %s",
                  ifelse(any(cmp$horizon == "T-2"),
                         sprintf("%+.2f%%", cmp$E_delta[cmp$horizon == "T-2"][1]),
                         "n/a")),
          sprintf("kNN T-1: %s",
                  ifelse(is.finite(knn_val), sprintf("%+.2f%%", knn_val), "n/a")),
          sep = "   "
        )
      ) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "top", plot.title = element_blank())
  })
  
  
  
  # ---- TRANSFERMARKT ----
  
  ## ---- Gebote pro Wochentag ----
  
  # avg_bids_wd
  avg_bids_wd <- reactive({
    req(!is.null(gebotsprofil_cache()))
    gp <- gebotsprofil_clean() %>%
      group_by(Datum, Bieter) %>%
      summarise(Anzahl = n(), .groups = "drop") %>%
      mutate(Datum = Datum - 1)
    
    gp %>%
      complete(
        Bieter,
        Datum = seq(min(Datum), max(Datum), by = "day"),
        fill = list(Anzahl = 0)
      ) %>%
      mutate(
        Wochentag = factor(
          weekdays(Datum, abbreviate = TRUE),
          levels = c("Mo","Di","Mi","Do","Fr","Sa","So")
        )
      ) %>%
      group_by(Bieter, Wochentag) %>%
      summarise(avg_bids = mean(Anzahl), .groups = "drop") %>%
      group_by(Bieter) %>%
      arrange(Bieter, desc(avg_bids)) %>%
      mutate(rank = dense_rank(desc(avg_bids))) %>%  # Rang 1 = persönlich stärkster Wochentag
      ungroup()
  })
  
  # Box bids
  output$bid_prediction <- renderUI({
    today_wd <- factor(
      weekdays(today, abbreviate = TRUE),
      levels = c("Mo","Di","Mi","Do","Fr","Sa","So")
    )
    
    df <- avg_bids_wd() %>%
      filter(Wochentag == today_wd) %>%
      filter(!Bieter %in% c("DeinBieterName","Dominik")) %>%
      arrange(rank, desc(avg_bids))   # Rang 1 zuerst; bei Gleichstand nach avg_bids
    
    if (nrow(df) == 0) return(NULL)
    
    labels <- vapply(seq_len(nrow(df)), function(i) {
      sprintf(
        "<strong>%s:</strong>&nbsp;Rang&nbsp;%d&nbsp;(&gt;%s&lt;)",
        df$Bieter[i],
        df$rank[i],
        format(round(df$avg_bids[i], 2), decimal.mark = ",")
      )
    }, FUN.VALUE = "")
    
    line <- paste(labels[nzchar(labels)], collapse = "&nbsp;&middot;&nbsp;")
    
    HTML(sprintf(
      "<div style='background:#f2f2f2;padding:6px;border-radius:6px;font-size:90%%;overflow-x:auto;white-space:nowrap;'>%s</div>",
      line
    ))
  })
  
  
  ## ---- Blockierte Vereine ----
  
  # blocked_by_manager
  blocked_by_manager <- reactive({
    req(teams_df, ca_df)
    
    pcm <- ca_df %>%
      select(Spieler = SPIELER, Verein) %>%
      distinct() 
    
    teams_df %>%
      mutate(Spieler = trimws(as.character(Spieler)),
             Manager = trimws(as.character(Manager))) %>%
      left_join(pcm, by = "Spieler") %>%
      mutate(Verein = ifelse(is.na(Verein), "", Verein)) %>%
      group_by(Manager, Verein) %>%
      summarise(cnt = n(), .groups = "drop") %>%
      filter(Verein != "" & cnt >= 3) %>%
      group_by(Manager) %>%
      summarise(teams = list(Verein), .groups = "drop")
  })
  
  # Box blocked
  output$blocked_managers <- renderUI({
    req(blocked_by_manager())
    blocked <- blocked_by_manager()
    if (nrow(blocked) == 0) {
      return(HTML("<div style='margin:4px 0;padding:6px;color:#666;font-size:90%;'>Keine Manager mit ≥3 Spielern desselben Vereins gefunden.</div>"))
    }
    blocked <- blocked %>% slice_head(n = 8)
    items <- vapply(seq_len(nrow(blocked)), function(i) {
      mgr <- blocked$Manager[i]
      teams <- blocked$teams[[i]]
      if (length(teams) == 0) return("")
      logos_html <- vapply(teams, function(tm) {
        lm <- logo_map[tm]
        if (is.na(lm) || lm == "") sprintf("<span style='margin-left:6px;font-size:90%%;'>%s</span>", tm)
        else sprintf('<img src="logos/%s" title="%s" style="height:18px;max-width:40px;width:auto;vertical-align:middle;margin-left:6px;object-fit:contain;"/>', lm, tm)
      }, FUN.VALUE = "")
      sprintf("<strong>%s:</strong>&nbsp;%s", mgr, paste(logos_html, collapse = ""))
    }, FUN.VALUE = "")
    line <- paste(items[nzchar(items)], collapse = "&nbsp;&nbsp;&middot;&nbsp;&nbsp;")
    HTML(sprintf("<div style='background:#f2f2f2;padding:6px;border-radius:6px;font-size:90%%;overflow-x:auto;white-space:nowrap;'>%s</div>", line))
  })
  
  ## ---- Tabelle ----
  
  # Observer oder reactive, der tm_common einmal neu berechnet, sobald tm_df oder ap_df sich ändern:
  observeEvent(list(tm_df, ap_df), {
    
    df <- tm_df %>%
      mutate(
        Spieler          = trimws(enc2utf8(Spieler)),
        Marktwert_num    = as.numeric(gsub("\\.", "", Marktwert)),
        Mindestgebot_num = as.numeric(gsub("\\.", "", Mindestgebot)),
        Restzeit         = trimws(Restzeit),
        Verein           = trimws(enc2utf8(Verein))
      )
    
    # --- letzte 3 Tage & Merge ---
    last_dates <- sort(unique(ap_df$Datum), decreasing=TRUE)[1:3]
    names(last_dates) <- c("MW3","MW2","MW1")
    mw_list <- lapply(c("MW1","MW2","MW3"), function(nm) {
      ap_df %>% 
        filter(Datum == last_dates[nm]) %>% 
        select(Spieler, !!nm := Marktwert)
    })
    df <- Reduce(function(x,y) left_join(x,y,by="Spieler"), 
                 c(list(df), mw_list)
    )
    
    # --- Trend berechnen ---
    df$Trend <- mapply(function(m1,m2,m3) {
      if(any(is.na(c(m1,m2,m3)))) return("–")
      if(m1<m2 && m2<m3)          "<span style='color:green;font-weight:bold;'>▲▲</span>"
      else if(m1>m2 && m2>m3)     "<span style='color:red;font-weight:bold;'>▼▼</span>"
      else if(m1<m3)              "<span style='color:green;font-weight:bold;'>▲</span>"
      else if(m1>m3)              "<span style='color:red;font-weight:bold;'>▼</span>"
      else                        "–"
    }, df$MW1, df$MW2, df$MW3, SIMPLIFY=TRUE)
    
    # --- Restzeit-Kategorien ---
    df$Restkategorie <- case_when(
      grepl("^2d", df$Restzeit)    ~ "übermorgen",
      grepl("^1d", df$Restzeit)    ~ "morgen",
      grepl("^[0-9]+h", df$Restzeit)~ "heute",
      TRUE                          ~ "unbekannt"
    )
    df$Restkategorie <- factor(
      df$Restkategorie,
      levels = c("heute","morgen","übermorgen","unbekannt"),
      ordered = TRUE
    )
    
    # --- Formatieren & Flags ---
    df$Marktwert   <- paste0(format(df$Marktwert_num,  big.mark=".", decimal.mark=",")," €")
    df$Mindestgebot<- paste0(format(df$Mindestgebot_num,big.mark=".", decimal.mark=",")," €")
    df$MinGeb_Unter_MW <- df$Mindestgebot_num < df$Marktwert_num
    
    # --- Sortierung & Rename für beide Outputs ---
    df <- df %>%
      arrange(Restkategorie, desc(Marktwert_num)) %>%
      rename(
        "Verbleibende Zeit"    = Restkategorie,
        "Trend MW (3 Tage)"    = Trend
      )
    
    # --- Logos ---
    logo_dir <- "logos"
    df$Logo <- paste0(
      '<img src="', logo_dir, '/', logo_map[df$Verein],
      '" width="28" title="', df$Verein, '"/>'
    )
    df$Logo[ is.na(logo_map[df$Verein]) ] <- ""
    
    # --- in reactiveVal speichern ---
    tm_common(df)
  })
  
  output$transfermarkt_detail <- DT::renderDT({
    req(input$transfermarkt_tabs == "Transfermarkt Details")   # nur wenn Tab aktiv
    # Basis‐Tabelle aus reactiveVal holen
    df <- tm_common()
    req(nrow(df) > 0)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    
    # 1) MW-Klasse bestimmen
    df$MW_Klasse <- vapply(df$Marktwert_num, get_mw_klasse, character(1))
    
    # 2) Gebotsprofil-Daten für Ideales Gebot
    gp_df <- gebotsprofil_mwclass() %>%
      filter(!is.na(Diff_Prozent)) %>%
      mutate(
        MW_Klasse = factor(
          MW_Klasse,
          levels = c("<0.5 Mio","0.5–1 Mio","1–2.5 Mio","2.5–5 Mio","5–10 Mio",">10 Mio")
        )
      ) %>%
      filter(Typ == "Hoechstgebot", Diff_Prozent > 0, Diff_Prozent <= 100)
    
    means_klasse <- gp_df %>%
      group_by(MW_Klasse) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    df <- df %>%
      left_join(means_klasse, by = "MW_Klasse")
    
    # 3) Ideales Gebot: dynamisch auf Mindestgebot oder Marktwert
    df <- df %>% 
      mutate(
        # Basis-Wert auswählen
        base_val = ifelse(MinGeb_Unter_MW, Mindestgebot_num, Marktwert_num),
        # Zuschlagsgebot berechnen
        IdealesGebot_num    = round(base_val * (1 + Mean/100)),
        IdealesGebotProzent = round(100 * (IdealesGebot_num / base_val - 1), 1),
        # HTML-Label
        IdealesGebot = paste0(
          format(IdealesGebot_num, big.mark = ".", decimal.mark = ","), " €",
          "<br><span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(IdealesGebotProzent), "–",
                 paste0(ifelse(IdealesGebotProzent > 0, "+",""), 
                        IdealesGebotProzent, "%")
          ),
          ")</span>"
        )
      ) %>%
      # Fallback: wenn kein Profil-Mean vorliegt, dann einfach base_val * 1.3 (30% mehr) anzeigen
      mutate(
        IdealesGebot_num = ifelse(is.na(IdealesGebot_num), base_val, IdealesGebot_num),
        IdealesGebot     = ifelse(
          is.na(IdealesGebotProzent),
          paste0(format(base_val * 1.3, big.mark=".", decimal.mark=","), " €"),
          IdealesGebot
        )
      ) %>%
      select(-base_val)
    
    
    # 4) Maximalgebot (90%-Perzentil) dynamisch auf Mindestgebot oder Marktwert
    transfers_clean <- transfers %>%
      filter(!is.na(Hoechstgebot))
    
    max_datum <- max(transfers_clean$Datum, na.rm = TRUE)
    vortag    <- max_datum - 1
    
    # eindeutiger Vortag-MW pro Spieler (höchster MW, falls doppelt)
    vortags_mw <- transfermarkt %>%
      filter(TM_Stand == vortag) %>%
      arrange(Spieler, desc(Marktwert)) %>%
      distinct(Spieler, .keep_all = TRUE) %>%
      transmute(Spieler, Marktwert_num = Marktwert)
    
    # optionaler Check
    stopifnot(!any(duplicated(vortags_mw$Spieler)))
    
    transfers_mw <- transfers_clean %>%
      left_join(vortags_mw, by = "Spieler") %>%
      filter(!is.na(Marktwert_num)) %>%
      mutate(
        MW_Klasse     = vapply(Marktwert_num, get_mw_klasse, character(1)),
        Diff_per_cent = Hoechstgebot / Marktwert_num
      ) %>%
      filter(Diff_per_cent > 1, Diff_per_cent < 1.33)
    
    maxgebote_klasse <- transfers_mw %>%
      group_by(MW_Klasse) %>%
      summarise(Maximalgebot_90_Faktor = quantile(Diff_per_cent, probs = 0.9), .groups = "drop")
    
    df <- df %>%
      left_join(maxgebote_klasse, by = "MW_Klasse") %>%
      mutate(
        # Basis-Wert wählen
        base_val = ifelse(MinGeb_Unter_MW, Mindestgebot_num, Marktwert_num),
        # Maximalgebot berechnen
        Maximalgebot_num = ifelse(
          is.na(Maximalgebot_90_Faktor),
          NA,
          round(base_val * Maximalgebot_90_Faktor)
        ),
        MaximalgebotProzent = round(100 * (Maximalgebot_num / base_val - 1), 1),
        # HTML-Label
        Maximalgebot = paste0(
          format(Maximalgebot_num, big.mark = ".", decimal.mark = ","), " €",
          "<br><span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(MaximalgebotProzent), "–",
                 paste0(ifelse(MaximalgebotProzent > 0, "+",""),
                        MaximalgebotProzent, "%")
          ),
          ")</span>"
        )
      ) %>%
      # Fallback: wenn kein Faktor da, Einfach base_val *1.3 anzeigen
      mutate(
        Maximalgebot_num = ifelse(is.na(Maximalgebot_num), base_val, Maximalgebot_num),
        Maximalgebot     = ifelse(
          is.na(MaximalgebotProzent),
          paste0(format(base_val * 1.3, big.mark=".", decimal.mark=","), " €"),
          Maximalgebot
        )
      ) %>%
      select(-base_val)
    
    
    # 5) Minimalgebot anhand eines Beispiel-Bieterprofils,
    #    nun auf Basis des Mindestgebots statt des Marktwerts
    bprofile_df <- tribble(
      ~MW_Klasse,   ~Durchschnitt_Abweichung,
      "<0.5 Mio",   0.18,
      "0.5–1 Mio",  1.18,
      "1–2.5 Mio",  1.18,
      "2.5–5 Mio",  1.18,
      "5–10 Mio",   1.18,
      ">10 Mio",    0.18
    )
    
    df <- df %>%
      left_join(bprofile_df, by = "MW_Klasse") %>%
      mutate(
        # statt Marktwert_num verwenden wir jetzt Mindestgebot_num
        Mindestgebot_empfohlen_num = round(
          Mindestgebot_num * (1 + Durchschnitt_Abweichung / 100)
        ),
        Minimalgebot = paste0(
          format(Mindestgebot_empfohlen_num, big.mark = ".", decimal.mark = ","), " €"
        )
      ) %>%
      # sicherstellen, dass wir nie unter dem originalen Mindestgebot landen
      mutate(
        Minimalgebot_num = as.numeric(gsub("\\.", "", gsub(" €", "", Minimalgebot))),
        Mindestgebot_num  = as.numeric(gsub("\\.", "", gsub(" €", "", Mindestgebot))),
        Minimalgebot_num  = ifelse(
          Minimalgebot_num < Mindestgebot_num,
          Mindestgebot_num,
          Minimalgebot_num
        ),
        Minimalgebot = paste0(
          format(Minimalgebot_num, big.mark = ".", decimal.mark = ","), " €"
        ),
        MinimalgebotProzent = round(
          100 * (Minimalgebot_num / Marktwert_num - 1),
          1
        )
      ) %>%
      mutate(
        Minimalgebot = paste0(
          format(Minimalgebot_num, big.mark = ".", decimal.mark = ","), " €",
          "<br><span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(MinimalgebotProzent), "–",
                 paste0(ifelse(MinimalgebotProzent>0, "+",""), MinimalgebotProzent, "%")
          ),
          ")</span>"
        )
      )
    
    # 6) Historische Punkte & Details mergen
    df <- df %>%
      left_join(
        ca_df %>%
          select(
            SPIELER,
            Position,
            Gesamtpunkte,
            Punkte_pro_Spiel
          ),
        by = c("Spieler" = "SPIELER")
      ) %>%
      mutate(
             across(
               c(Position, Punkte_pro_Spiel),
               ~ ifelse(is.na(.x) | .x == "", "-", as.character(.x))
             )
      ) %>%  left_join(
        price_perf_df() %>% transmute(SPIELER, `Preis-Leistung` = Preis_Leistung_label, Preis_Leistung_Score = Preis_Leistung_score),
        by = c("Spieler" = "SPIELER")
      )
    
    # 6.25) LI-Info E/S11 hinter Preis-Leistung Feld anzeigen
    df <- df %>%
      left_join(li_subset, by = c("Spieler" = "Comunio_Name")) %>%
      mutate(
        # roh reinigen, Prozentzeichen entfernen, Dezimal-Komma -> Punkt
        EQ_Gesamt_num = suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", EQ_Gesamt_Quote)))),
        EQ_S11_num    = suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", `EQ_Startelf_.`)))),
        
        # runden / NA handhaben (gute Basis für Sortierung)
        EQ_Gesamt_num = ifelse(is.na(EQ_Gesamt_num), NA_real_, round(EQ_Gesamt_num)),
        EQ_S11_num    = ifelse(is.na(EQ_S11_num),    NA_real_, round(EQ_S11_num)),
        
        # Anzeige-Strings (keine Prozentzeichen, damit DataTables numerisch sortiert)
        EQ_Gesamt = ifelse(is.na(EQ_Gesamt_num), "-", as.character(EQ_Gesamt_num)),
        EQ_S11    = ifelse(is.na(EQ_S11_num),    "-", as.character(EQ_S11_num))
      )
    
    
    
    # 6.5) Status hinzufügen
    df <- df %>%
      left_join(status_latest, by = "Spieler") %>%
      mutate(
        StatusTag = ifelse(
          is.na(StatusIcon) | StatusIcon == "" | StatusIcon == "icons-status-active-dark",
          "",
          sprintf('<img src="%s/%s.png" width="20" height="20" title="%s"/>',
                  "extracted_icons", StatusIcon, ifelse(is.na(StatusText), "", StatusText))
        )
      )
    
    # 7) Global speichern (falls benötigt)
    tm_trend_global(df)
    
    # 8) Aktion-Spalte bauen
    df$action <- sprintf(
      '<button class="btn btn-xs btn-primary view-btn" data-row="%d">→</button>',
      seq_len(nrow(df))
    )
    
    # 9) Aktion2-Spalte bauen
    df$action2 <- sprintf(
      '<button class="btn btn-xs btn-info info-btn" data-row="%d">i</button>',
      seq_len(nrow(df))
    )
    
    
    # Copy-Buttons in die Tabelle
    df <- df %>%
      mutate(
        Minimalgebot = paste0(
          format(Minimalgebot_num, big.mark=".", decimal.mark=","),
          " € ",
          "<button class='btn btn-xs btn-light copy-btn' data-value='",
          Minimalgebot_num, "' title='Kopieren'>📋</button>",
          "<br><span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(MinimalgebotProzent),"–",
                 paste0(ifelse(MinimalgebotProzent>0,"+",""),
                        MinimalgebotProzent,"%")),
          ")</span>"
        ),
        IdealesGebot = paste0(
          format(IdealesGebot_num, big.mark=".", decimal.mark=","),
          " € ",
          "<button class='btn btn-xs btn-light copy-btn' data-value='",
          IdealesGebot_num, "' title='Kopieren'>📋</button>",
          "<br><span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(IdealesGebotProzent),"–",
                 paste0(ifelse(IdealesGebotProzent>0,"+",""),
                        IdealesGebotProzent,"%")),
          ")</span>"
        ),
        Maximalgebot = paste0(
          format(Maximalgebot_num, big.mark=".", decimal.mark=","), " € ",
          "<button class='btn btn-xs btn-light copy-btn' data-value='",
          Maximalgebot_num, "' title='Kopieren'>📋</button>",
          "<br><span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(MaximalgebotProzent),"–",
                 paste0(ifelse(MaximalgebotProzent>0,"+",""),
                        MaximalgebotProzent,"%")),
          ")</span>"
        )
      )
    
    df <- df %>%
      mutate(
        Gebote_minmax = paste0(
          "<div style='margin:0;padding:0;line-height:1.1;font-size:12px'>",
          "<div style='margin:0;padding:0'><strong>Min:</strong> ",
          format(Minimalgebot_num, big.mark=".", decimal.mark=","), " € ",
          "<button class='btn btn-xs btn-light copy-btn' style='padding:0 4px;line-height:1.1;font-size:11px' data-value='", Minimalgebot_num, "' title='Kopieren'>📋</button>",
          " <span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(MinimalgebotProzent),"–", paste0(ifelse(MinimalgebotProzent>0,"+",""), MinimalgebotProzent, "%")),
          ")</span></div>",
          "<div style='margin:0;padding:0'><strong>Ideal:</strong> ",
          format(IdealesGebot_num, big.mark=".", decimal.mark=","), " € ",
          "<button class='btn btn-xs btn-light copy-btn' style='padding:0 4px;line-height:1.1;font-size:11px' data-value='", IdealesGebot_num, "' title='Kopieren'>📋</button>",
          " <span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(IdealesGebotProzent),"–", paste0(ifelse(IdealesGebotProzent>0,"+",""), IdealesGebotProzent, "%")),
          ")</span></div>",
          "<div style='margin:0;padding:0'><strong>Max:</strong> ",
          format(Maximalgebot_num, big.mark=".", decimal.mark=","), " € ",
          "<button class='btn btn-xs btn-light copy-btn' style='padding:0 4px;line-height:1.1;font-size:11px' data-value='", Maximalgebot_num, "' title='Kopieren'>📋</button>",
          " <span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(MaximalgebotProzent),"–", paste0(ifelse(MaximalgebotProzent>0,"+",""), MaximalgebotProzent, "%")),
          ")</span></div>",
          "</div>"
        )
      )

    # fehlende Spalten auffüllen
    needed <- c("Logo","Spieler","StatusTag","Position","Gesamtpunkte","Punkte_pro_Spiel","Marktwert","Mindestgebot",
                "Gebote_minmax","Preis-Leistung", "EQ_Gesamt","EQ_S11","EQ_Gesamt_num","EQ_S11_num",
                "Verbleibende Zeit","Trend MW (3 Tage)","action2","action")
    
    
    
    for (nm in setdiff(needed, names(df))) df[[nm]] <- NA_character_
    sub_df <- df[, needed, drop = FALSE]
    
    # List-Spalten zu String (falls vorhanden)
    is_list_col <- vapply(sub_df, is.list, logical(1))
    if (any(is_list_col)) sub_df[is_list_col] <- lapply(sub_df[is_list_col], function(x) vapply(x, toString, ""))
    
    # NACH sub_df gebaut wurde
    sub_df$MinGeb_Unter_MW <- df$MinGeb_Unter_MW
    
    # indices für columnDefs (0-basiert für JS)
    disp_eq_idx   <- which(names(sub_df) == "EQ_Gesamt") - 1L
    disp_s11_idx  <- which(names(sub_df) == "EQ_S11") - 1L
    num_eq_idx    <- which(names(sub_df) == "EQ_Gesamt_num") - 1L
    num_s11_idx   <- which(names(sub_df) == "EQ_S11_num") - 1L
    
    DT::datatable(
      sub_df,
      colnames = c(
        "","Spieler","","Position","Punkte","PPS","Marktwert",
        "Mindestgebot","Gebote (Min/Ideal/Max)","Preis-Leistung", "EQ (%)", "S11 (%)",
        "Maximalgebot" = NULL,  # entfällt
        "Angebotsende","Trend","Info","Rechner",""
      )
      ,
      escape    = FALSE,
      class = 'compact',
      selection = "none",
      rownames  = FALSE,
      callback = JS(
        # JS-Fallback-Funktion
        "function fallbackCopy(text) {",
        "  var ta = document.createElement('textarea');",
        "  ta.value = text;",
        "  ta.style.position = 'fixed'; ta.style.top = 0; ta.style.left = 0;",
        "  document.body.appendChild(ta);",
        "  ta.focus(); ta.select();",
        "  try { document.execCommand('copy'); } catch (err) { console.error(err); }",
        "  document.body.removeChild(ta);",
        "}",
        # Transfer-Simulator Button
        "table.on('click', 'button.view-btn', function() {",
        "  Shiny.setInputValue('transfer_row', $(this).data('row'), {priority:'event'});",
        "});",
        # Copy-Button mit Clipboard-API + Fallback
        "table.on('click', 'button.copy-btn', function() {",
        "  var val = $(this).attr('data-value');",
        "  if (navigator.clipboard && navigator.clipboard.writeText) {",
        "    navigator.clipboard.writeText(val)",
        "      .catch(function() { fallbackCopy(val); });",
        "  } else {",
        "    fallbackCopy(val);",
        "  }",
        "});",
        # Action button 2
        "table.on('click', 'button.info-btn', function() {",
        "  Shiny.setInputValue('info_row', $(this).data('row'), {priority:'event'});",
        "});"
      ),
      options = list(
        dom        = "t",
        ordering   = TRUE,
        pageLength = 20,
        columnDefs = list(
          list(className = 'dt-right', targets = 0),
          list(className = 'dt-left',   targets = c(1, 2)),
          list(className = 'dt-center', targets = 3:7),
          list(className = 'dt-left',   targets = 8),
          list(className = 'dt-center', targets = 9:(ncol(sub_df)-1)),
          list(visible = FALSE, targets = ncol(sub_df) - 1),
          # NUMERISCHER HACK: numerische Spalten ausblenden...
          list(visible = FALSE, targets = c(num_eq_idx, num_s11_idx)),
          # ...und Display-Spalten so sortieren lassen, dass sie orderData auf die numerischen Spalten verweisen
          list(orderData = num_eq_idx,  targets = disp_eq_idx),
          list(orderData = num_s11_idx, targets = disp_s11_idx)
        )
      )
      
    ) %>% 
      formatStyle(
        'Verbleibende Zeit',
        target     = 'cell',
        color      = styleEqual("heute","red"),
        fontWeight = styleEqual("heute","bold")
      ) %>% 
      formatStyle(
        'Mindestgebot',
        valueColumns='MinGeb_Unter_MW',
        color=styleEqual(c(TRUE), "green"),
        fontWeight=styleEqual(c(TRUE), "bold")
      )
  })
  
  ## ---- Spieler Info ----
  
  ### ----  aktueller Marktwert des ausgewählten Spielers (letzter Stand aus ap_df) ----
  current_player_mw <- reactive({
    req(input$spieler_select2, ap_df)
    
    norm_spieler <- function(x) trimws(enc2utf8(as.character(x)))
    
    ap_df %>%
      mutate(
        Datum   = as.Date(Datum),
        Spieler = norm_spieler(Spieler),
        Marktwert = as.numeric(Marktwert)
      ) %>%
      filter(Spieler == norm_spieler(input$spieler_select2)) %>%
      filter(!is.na(Marktwert)) %>%
      arrange(desc(Datum)) %>%
      slice(1) %>%
      pull(Marktwert) %>%
      { if (length(.) == 0) NA_real_ else . }
  })
  
  output$hypo_mw_prognose <- renderText({
    req(punkte_mw_summary_cached())
    mw_now <- current_player_mw()
    pts    <- input$hypo_punkte
    
    if (is.null(pts) || is.na(mw_now)) return("Keine Prognose möglich (fehlender Marktwert oder Punkte).")
    
    mw_pred <- predict_mw_in_3d(
      current_mw   = mw_now,
      punkte_event = pts,
      summary_df   = punkte_mw_summary_cached()   # <-- statt punkte_mw_summary()
    )
    
    
    if (is.na(mw_pred)) return("Keine Prognose möglich (zu wenige Vergleichsdaten).")
    
    delta_abs <- mw_pred - mw_now
    delta_pct <- 100 * delta_abs / mw_now
    
    fmt_eur <- function(x) paste0(format(round(x), big.mark = ".", decimal.mark = ","), " €")
    
    paste0(
      "Aktueller MW: ", fmt_eur(mw_now),
      " → erwarteter MW in 3 Tagen: ", fmt_eur(mw_pred),
      " (", sprintf("%+.1f", delta_pct), " %)"
    )
  })
  
  ### ---- Header-Card Render ----
  output$spieler_card_header <- renderUI({
    row <- match_li_row()
    if (is.null(row) || nrow(row) == 0) return(NULL)
    
    foto   <- safe_val(row$Profilfoto_URL[1])
    wappen <- safe_val(row$Vereinswappen_URL[1])
    
    verein <- safe_val(row$Verein[1])
    pos    <- safe_val(row$Position[1])
    trikot <- safe_val(row$Trikotnummer[1])
    vertrag<- safe_val(row$Vertrag_bis[1])
    nation <- safe_val(row$Nationalität[1])
    geb    <- safe_val(row$Geburtstag[1])
    alter  <- safe_val(row$Alter[1])
    groesse<- safe_val(row$Größe[1])
    fuss   <- safe_val(row$Starker_Fuß[1])
    fit    <- safe_val(row$Fitness[1])
    
    
    tags$div(class = "spieler-card",
             # Bild links
             if (nzchar(foto)) tags$img(src = foto, width = "240", height = "240", onerror = "this.style.display='none'"),
             # Text rechts
             tags$div(class = "meta",
                      tags$div(class = "verein",
                               if (nzchar(wappen)) tags$img(src = wappen, width = "24", height = "24", onerror = "this.style.display='none'"),
                               verein
                      ),
                      tags$ul(class = "list",
                              if (nzchar(pos))     tags$li(paste("Position:", pos)),
                              if (nzchar(trikot))  tags$li(paste("Trikotnummer:", trikot)),
                              if (nzchar(vertrag)) tags$li(paste("Vertrag bis:", vertrag)),
                              if (nzchar(nation))  tags$li(paste("Nationalität:", nation)),
                              if (nzchar(geb))     tags$li(paste("Geburtstag:", geb)),
                              if (nzchar(alter))   tags$li(paste("Alter:", alter)),
                              if (nzchar(groesse)) tags$li(paste("Größe:", groesse)),
                              if (nzchar(fuss))    tags$li(paste("Starker Fuß:", fuss)),
                              if (nzchar(fit))     tags$li(paste("Fitness:", fit))
                      )
             )
    )
  })
  
  ### ---- LI-News ----
  output$spieler_news <- renderUI({
    li_row <- match_li_row()
    if (is.null(li_row) || !is.data.frame(li_row) || nrow(li_row) == 0) return(div("Keine LI-News"))
    
    need <- c("News1_Date","News1_URL","News2_Date","News2_URL","News3_Date","News3_URL")
    if (!all(need %in% names(li_row))) return(div("Keine LI-News"))
    
    news <- data.frame(
      date = trimws(c(li_row$News1_Date, li_row$News2_Date, li_row$News3_Date)),
      url  = trimws(c(li_row$News1_URL,  li_row$News2_URL,  li_row$News3_URL)),
      stringsAsFactors = FALSE
    )
    news <- news[!is.na(news$url) & nzchar(news$url), , drop = FALSE]
    if (nrow(news) == 0) return(div("Keine LI-News"))
    
    # Datum robust normalisieren + parsen
    ds <- trimws(news$date)
    ds <- gsub("[\u2013\u2014]", "-", ds)                    # en/em dash -> "-"
    ds <- gsub("\\s*-\\s*-\\s*", " - ", ds, perl = TRUE)     # " - - " -> " - "
    ds <- gsub("\\s*-\\s*", " - ", ds, perl = TRUE)          # alle "-"-Varianten vereinheitlichen
    ds <- gsub("\\s+", " ", ds)                              # Mehrfachspaces reduzieren
    
    suppressWarnings({
      dt1 <- as.POSIXct(ds, format = "%d.%m.%Y - %H:%M", tz = "Europe/Berlin")
      dt2 <- as.POSIXct(ds, format = "%d.%m.%Y %H:%M",   tz = "Europe/Berlin")
      dt3 <- as.POSIXct(ds, format = "%d.%m.%Y",         tz = "Europe/Berlin")
    })
    dt <- dt1; idx <- is.na(dt); dt[idx] <- dt2[idx]; idx <- is.na(dt); dt[idx] <- dt3[idx]
    news$dt <- dt
    news$d  <- as.Date(news$dt, tz = "Europe/Berlin")
    
    news <- news[order(news$dt, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
    
    rel_label <- function(d) {
      if (is.na(d)) return("")
      diff <- as.integer(today - d)
      if (diff == 0) return("Heute")
      if (diff == 1) return("Gestern")
      paste0("vor ", diff, " Tagen")
    }
    rel_style <- function(lbl) if (identical(lbl, "Heute")) "color:#0a7a20;font-weight:600;" else "color:#000;"
    
    items <- lapply(seq_len(nrow(news)), function(i) {
      lbl <- rel_label(news$d[i])
      tags$div(
        style = "display:flex; align-items:flex-start; gap:10px; margin:8px 0;",
        tags$img(src = "icons/news.png", width = "80", height = "60", alt = "News"),
        tags$div(
          tags$div(lbl, style = paste0("font-size:13px; margin-bottom:2px;", rel_style(lbl))),
          tags$div(
            style = "display:flex; align-items:center; gap:8px;",
            tags$span(if (!is.na(news$dt[i])) format(news$dt[i], "%d.%m.%Y %H:%M") else news$date[i]),
            tags$a(href = news$url[i], target = "_blank", rel = "noopener noreferrer",
                   style = "text-decoration:none; overflow-wrap:anywhere;", news$url[i])
          )
        )
      )
    })
    
    tagList(tags$div(items))
  })
  

  ### ----  LI: Summary (Note/Punkte/EQ) ----
  output$spieler_li_perf_summary <- renderDT({
    row <- match_li_row()
    if (is.null(row) || nrow(row) == 0) {
      return(datatable(data.frame(Hinweis = "Keine LI-Daten gefunden"),
                       options = list(dom = 't', paging = FALSE)))
    }
    
    keep <- c(
      "Ø_Note","Punkte_gesamt",
      "EQ_Gesamt_Quote","EQ_Gesamt_Min_von","EQ_Gesamt_Min_total",
      "EQ_Startelf_.","EQ_Bank_.","EQ_Verletzt_.",
      "EQ_Einwechslungen_.","EQ_Ausgewechselt_.","EQ_NichtimKader_."
    )
    cols <- intersect(keep, names(row))
    if (length(cols) == 0) {
      return(datatable(data.frame(Hinweis = "Keine Summary-Spalten in li_df"),
                       options = list(dom = 't', paging = FALSE)))
    }
    
    df <- row[, cols, drop = FALSE]
    
    map <- c(
      "Ø_Note"              = "📊 Durchschnittsnote",
      "Punkte_gesamt"       = "⭐ Gesamtpunkte",
      "EQ_Gesamt_Quote"     = "📈 Einsatzquote %",
      "EQ_Gesamt_Min_von"   = "⏱️ Minuten von",
      "EQ_Gesamt_Min_total" = "⏲️ Minuten gesamt",
      "EQ_Startelf_."       = "🟢 Startelf %",
      "EQ_Bank_."           = "🟡 Bank %",
      "EQ_Verletzt_."       = "❌ Verletzt %",
      "EQ_Einwechslungen_." = "↪︎ Eingewechselt %",
      "EQ_Ausgewechselt_."  = "↩︎ Ausgewechselt %",
      "EQ_NichtimKader_."   = "🚫 Nicht im Kader %"
    )
    
    df <- df %>%
      select(where(~ !all(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "Metrik", values_to = "Wert") %>%
      mutate(Metrik = ifelse(Metrik %in% names(map), map[Metrik], Metrik))
    
    datatable(
      df,                             
      escape = FALSE, rownames = FALSE,
      options = list(dom = 't', scrollX = TRUE, paging = FALSE)
    )
    
  })
  
  ### ----  LI: Bilanz (Gesamt/BL/Pokal)  ----
  output$spieler_li_perf_bilanz <- renderDT({
    row <- match_li_row()
    if (is.null(row) || nrow(row) == 0) {
      return(datatable(data.frame(Hinweis = "Keine LI-Daten gefunden"), options = list(dom = 't', paging = FALSE)))
    }
    
    # alle Bilanz_* Spalten sammeln
    bcols <- grep("^Bilanz_", names(row), value = TRUE)
    
    df_long <- row %>%
      select(matches("^Bilanz_")) %>%
      mutate(across(everything(), ~ na_if(as.character(.), ""))) %>%
      mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>%
      pivot_longer(
        everything(),
        names_to   = c("Wettbewerb","Metrik"),
        names_pattern = "^Bilanz_(Gesamt|BL|Pokal)_(.*)$",
        values_to  = "value",
        values_drop_na = FALSE
      ) %>%
      select(Wettbewerb, Metrik, value)
    
    # Reihenfolge + Labels
    ord <- c("Einsätze","Startelf","Einwechslungen","Auswechslungen","Bank","Nicht_im_Kader",
             "Tore","Vorlagen","Gelbe","Karten_GelbRot","Karten_Rot")
    
    emoji <- c(
      "Einsätze"="👟", "Startelf"="🟢", "Einwechslungen"="↪︎", "Auswechslungen"="↩︎",
      "Bank"="🟡", "Nicht_im_Kader"="🚫",
      "Tore"="⚽", "Vorlagen"="🎯", "Gelbe"="🟨", "Karten_GelbRot"="🟨🟥", "Karten_Rot"="🟥"
    )
    
    text <- c(
      "Einsätze"="Einsätze", "Startelf"="Startelf", "Einwechslungen"="Einwechslungen", "Auswechslungen"="Auswechslungen",
      "Bank"="Bank", "Nicht_im_Kader"="Nicht im Kader",
      "Tore"="Tore", "Vorlagen"="Vorlagen", "Gelbe"="Gelbe Karten",
      "Karten_GelbRot"="Gelb-Rot", "Karten_Rot"="Rote Karten"
    )
    
    df_wide <- df_long %>%
      mutate(Metrik = factor(Metrik, levels = ord),
             Wettbewerb = factor(Wettbewerb, levels = c("Gesamt","BL","Pokal"))) %>%
      complete(Wettbewerb, Metrik) %>%                         # fehlende Kombis ergänzen
      group_by(Wettbewerb, Metrik) %>%
      summarise(value = suppressWarnings(as.numeric(first(value))), .groups = "drop") %>%
      mutate(
        Metrik_chr = as.character(Metrik),
        Metrik_lbl = paste0(emoji[Metrik_chr], " ", text[Metrik_chr])
      ) %>%
      select(Metrik = Metrik_lbl, Wettbewerb, value) %>%
      pivot_wider(names_from = Wettbewerb, values_from = value) %>%
      arrange(match(gsub("^.+? ", "", Metrik), text[ord])) %>%
      select(Metrik, any_of(c("Gesamt","BL","Pokal")))
    
    # leere Spalten entfernen
    df_wide <- df_wide %>% select(where(~ !all(is.na(.))))
    
    datatable(
      df_wide,                              
      escape = FALSE, rownames = FALSE,
      options = list(dom = 't', scrollX = TRUE, paging = FALSE)
    )
    
  })
  
  ### ----  Spieler MW+Punkte  ----
  output$spieler_info_mw <- renderPlotly({
    req(input$spieler_select2, mw_all, ap_df, spieler_stats_spielplan())
    
    norm    <- function(x) tolower(trimws(enc2utf8(as.character(x))))
    fmt_eur <- function(x) ifelse(is.na(x), "-", paste0(format(round(x), big.mark=".", decimal.mark=","), " €"))
    
    sel <- norm(input$spieler_select2)
    
    hist <- mw_all %>%
      mutate(Datum = as.Date(Datum),
             Marktwert = as.numeric(Marktwert),
             K = norm(Spieler))
    
    daily <- ap_df %>%
      mutate(Datum = as.Date(Datum),
             Marktwert = as.numeric(Marktwert),
             K = norm(Spieler))
    
    df <- bind_rows(mutate(hist, src = "hist"),
                    mutate(daily, src = "daily")) %>%
      filter(K == sel) %>%
      arrange(Datum, src) %>%
      group_by(Datum) %>% slice_tail(n = 1) %>% ungroup()
    
    req(nrow(df) > 0)
    
    events <- spieler_stats_spielplan() %>%
      mutate(K = norm(Spieler)) %>%
      filter(K == sel) %>%
      mutate(
        Punkte = suppressWarnings(as.integer(Punkte)),
        Note   = suppressWarnings(as.numeric(Note)),
        HG     = ifelse(HeimGast == "Heim", "H", ifelse(HeimGast == "Gast", "G", NA_character_)),
        hover  = paste0(
          format(Datum, "%Y-%m-%d"),
          "<br><b>Punkte:</b> ", ifelse(is.na(Punkte), "-", Punkte),
          "<br><b>Note:</b> ", ifelse(is.na(Note), "-", sprintf("%.1f", Note)),
          "<br><b>Gegner:</b> ", ifelse(is.na(Gegner), "-", Gegner), " (", HG, ")"
        )
      ) %>%
      filter(!is.na(Punkte)) %>%
      arrange(Datum)
    
    mw_mean   <- mean(df$Marktwert, na.rm = TRUE)
    mw_median <- median(df$Marktwert, na.rm = TRUE)
    mw_max    <- max(df$Marktwert, na.rm = TRUE)
    mw_curr   <- df$Marktwert[which.max(df$Datum)]
    
    df_hover <- df %>%
      transmute(
        x_dt = as.POSIXct(Datum),
        Marktwert = Marktwert,
        hover = paste0(
          format(Datum, "%Y-%m-%d"),
          "<br><b>Marktwert:</b> ", fmt_eur(Marktwert)
        )
      )
    
    p <- plot_ly()
    
    # Punkte-Bars (rechte Achse), breite Balken, KEINE Textlabels im Balken
    if (nrow(events) > 0) {
      bar_width_ms <- 24 * 60 * 60 * 1000 * 1.2
      p <- p %>%
        add_bars(
          data         = events,
          x            = ~as.POSIXct(Datum),
          y            = ~as.numeric(Punkte),
          yaxis        = "y2",
          name         = "Punkte",
          width        = bar_width_ms,
          marker       = list(color = "rgba(211,47,47,0.85)",
                              line = list(color = "#b71c1c", width = 0.6)),
          text         = ~hover,                       # nur für Tooltip
          textposition = "none",                       # << keine Textanzeige im Balken
          hovertemplate = "%{text}<extra></extra>",
          showlegend   = FALSE
        )
    }
    
    # MW-Linie (Tooltip an der Linie)
    p <- p %>%
      add_lines(
        data   = df_hover,
        x      = ~x_dt,
        y      = ~Marktwert,
        name   = "Marktwert",
        text   = ~hover,
        hovertemplate = "%{text}<extra></extra>",
        line   = list(width = 2, color = "#1f77b4"),
        showlegend = FALSE
      )
    
    # Annotationen
    ann <- list(
      list(
        text = paste0(
          "<b>Aktuell:</b> ", fmt_eur(mw_curr),
          " · <b>Mittel:</b> ", fmt_eur(mw_mean),
          " · <b>Median:</b> ", fmt_eur(mw_median),
          " · <b>Hoch:</b> ", fmt_eur(mw_max)
        ),
        x = 0, y = 1.10, xref = "paper", yref = "paper",
        showarrow = FALSE, align = "left",
        font = list(size = 18)
      )
    )
    
    # Graue 0-Linien für y und y2
    zero_lines <- list(
      list(type = "line", xref = "paper", yref = "y",  x0 = 0, x1 = 1, y0 = 0, y1 = 0,
           line = list(color = "#bdbdbd", width = 1, dash = "dot")),
      list(type = "line", xref = "paper", yref = "y2", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
           line = list(color = "#bdbdbd", width = 1, dash = "dot"))
    )
    
    p %>%
      layout(
        margin = list(t = 70, r = 20, l = 10),
        annotations = ann,
        shapes = zero_lines,
        hovermode = "x unified",
        xaxis  = list(type = "date", autorange = TRUE, title = list(text = "")),
        yaxis  = list(title = list(text = ""), showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = list(text = ""),
          showticklabels = FALSE,
          zeroline = FALSE,
          showgrid = FALSE,
          range = c(-30, 30),
          tickmode = "linear",
          dtick = 5
        ),
        barmode = "overlay",
        bargap  = 0,
        hoverlabel = list(align = "left")
      )
    
  })
  
  ### ----  Com-Analytics ----
  output$spieler_info_raw <- DT::renderDT({
    req(input$spieler_select2)
    
    search_name <- tolower(trimws(enc2utf8(as.character(input$spieler_select2))))
    
    left  <- ca_df  %>% mutate(.S = tolower(trimws(enc2utf8(as.character(SPIELER))))) %>% filter(.S == search_name) %>% select(-.S)
    right <- ca2_df %>% mutate(.S = tolower(trimws(enc2utf8(as.character(SPIELER))))) %>% filter(.S == search_name) %>% select(-.S)
    
    if (nrow(left) == 0 && nrow(right) == 0) {
      return(DT::datatable(data.frame(Meldung = "Keine Daten für diesen Spieler"),
                           escape = FALSE, rownames = FALSE,
                           options = list(dom = 't', scrollX = TRUE, paging = FALSE)))
    }
    
    df <- left %>%
      left_join(right, by = "SPIELER", suffix = c("", "_TM")) %>%
      rename(
        Spieler  = SPIELER,
        PPS      = Punkte_pro_Spiel,
        `Preis/Leistung` = Preis_Leistung,
        Historie = Historische_Punkteausbeute
      ) %>%
      mutate(
        Marktwert_num = as.numeric(gsub("\\.", "", gsub("[^0-9,\\.]","", `Marktwert`))),
        Zielwert_num  = as.numeric(gsub("\\.", "", gsub("[^0-9,\\.]","", `Zielwert`))),
        Marktwertprognose_num = Marktwert_num - Zielwert_num,
        Marktwertprognose = ifelse(is.na(Marktwertprognose_num), "-", format(round(Marktwertprognose_num), big.mark = ".", decimal.mark = ","))
      ) %>% select(-any_of(c("Marktwert_num","Zielwert_num","Marktwertprognose_num"))) %>% 
      # alte/raw-Spalten entfernen falls noch vorhanden
      select(-POSITION,-TEAM,-PUNKTE,-BESITZER,-Punkte_pro_Spiel_TM) %>%
      select(where(~ !all(is.na(.))))
    
    if (ncol(df) == 0) {
      return(DT::datatable(data.frame(Meldung = "Keine Felder nach Filter"),
                           escape = FALSE, rownames = FALSE,
                           options = list(dom = 't', scrollX = TRUE, paging = FALSE)))
    }
    
    # Key-Value Darstellung (Spalten vorher in character konvertieren; bestimmte Felder entfernen)
    kv <- df %>%
      select(-any_of(c("Spieler","Position","Verein","Datum"))) %>%
      mutate(across(everything(), ~ as.character(.))) %>%
      pivot_longer(everything(), names_to = "Feld", values_to = "Wert") %>%
      mutate(Feld = case_when(
        Feld == "PPS"                ~ "Punkte / Spiel (Montag)",
        Feld == "Preis/Leistung"     ~ "Preis / Leistung (Montag)",
        Feld == "Historie"           ~ "Historische Punkteausbeute",
        Feld == "Marktwertprognose"  ~ "Marktwertprognose (com-analytics)",
        Feld == "Kaufempfehlung"     ~ "Kaufempfehlung",
        Feld == "Gebotsvorhersage"   ~ "Gebotsvorhersage",
        Feld == "Erwartete_Punkte"   ~ "Erwartete Punkte",
        TRUE                         ~ Feld
      ))
    
    DT::datatable(
      kv,
      escape = FALSE, rownames = FALSE,
      options = list(
        dom = 't', scrollX = TRUE, paging = FALSE, ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = 1))
      ),
      colnames = c("Feld", "Wert")
    )
    
  })
  
  ### ---- Preis-Leistungs Spieler-Info Graph ----
  output$spieler_price_perf_small <- renderPlotly({
    req(price_perf_df(), ca_df, input$spieler_select2)
    df <- price_perf_df() %>%
      left_join(
        ca_df %>% transmute(SPIELER = as.character(SPIELER), Gesamtpunkte = as.character(Gesamtpunkte)),
        by = "SPIELER"
      ) %>%
      filter(!is.na(MW_num) & !is.na(PPS_num))
    if (nrow(df) == 0) return(plotly_empty())
    
    sel <- as.character(input$spieler_select2)
    
    df <- df %>%
      mutate(
        MW_fmt = paste0(format(round(MW_num), big.mark = ".", decimal.mark = ","), " €"),
        hover = paste0(
          "Spieler: ", SPIELER,
          "<br>PPS: ", formatC(PPS_num, format = "f", digits = 2, decimal.mark = ","),
          "<br>Marktwert: ", ifelse(is.na(MW_fmt), "-", MW_fmt),
          "<br>Gesamtpunkte: ", ifelse(is.na(Gesamtpunkte) | Gesamtpunkte == "NA", "-", Gesamtpunkte),
          "<br>Erwartet (PPS): ", ifelse(is.na(expected_PPS), "-", formatC(expected_PPS, format = "f", digits = 2, decimal.mark = ",")),
          "<br>Label: ", Preis_Leistung_label
        )
      )
    
    # optional: loess-Trend (nur wenn genügend Punkte)
    pred_df <- NULL
    if (nrow(df) >= 6) {
      df <- df %>% mutate(log_mw = log1p(MW_num))
      lo <- tryCatch(
        loess(PPS_num ~ log_mw, data = df, span = 0.5, control = loess.control(surface = "direct")),
        error = function(e) NULL
      )
      if (!is.null(lo)) {
        new_log <- seq(min(df$log_mw, na.rm = TRUE), max(df$log_mw, na.rm = TRUE), length.out = 200)
        pred <- tryCatch(predict(lo, newdata = data.frame(log_mw = new_log)), error = function(e) rep(NA_real_, length(new_log)))
        pred_df <- data.frame(MW_num = exp(new_log) - 1, pred = pred) %>% filter(is.finite(pred))
      }
    }
    
    y_max <- max(df$PPS_num, na.rm = TRUE)
    
    df$Preis_Leistung_label <- factor(df$Preis_Leistung_label,
                                      levels = c("Sehr gut","Gut","Durchschnittlich","Schlecht","Sehr schlecht","-"))
    
    cols <- c(
      "Sehr gut"         = "#006d2c",
      "Gut"              = "#2ca25f",
      "Durchschnittlich" = "#fdae61",
      "Schlecht"         = "#f46d43",
      "Sehr schlecht"    = "#d73027",
      "-"                = "#bdbdbd"
    )
    
    p <- ggplot(df, aes(x = MW_num, y = PPS_num, text = hover, color = Preis_Leistung_label)) +
      geom_point(size = 3, alpha = 0.25) +
      scale_color_manual(values = cols, na.value = "#bdbdbd")
    
    if (!is.null(pred_df) && nrow(pred_df) > 0) {
      p <- p + geom_line(data = pred_df, aes(x = MW_num, y = pred), inherit.aes = FALSE,
                         linetype = "dashed", size = 0.7, color = "black")
    }
    
    p <- p +
      scale_x_log10() +
      labs(x = "Marktwert (log)", y = "Punkte pro Spiel (PPS)", color = "") +
      coord_cartesian(ylim = c(0, max(ceiling(y_max * 0.95), 5))) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_text(size = 9),
        plot.margin = margin(6,6,6,6),
        legend.title = element_blank(),
        legend.text = element_text(size = 8)
      )
    
    # ausgewählten Spieler oben drüber (sichtbar, alpha = 1)
    if (!is.null(sel) && sel %in% df$SPIELER) {
      p <- p + geom_point(data = df %>% filter(SPIELER == sel),
                          aes(x = MW_num, y = PPS_num),
                          inherit.aes = FALSE,
                          color = "#d73027", size = 5, alpha = 1)
    }
    
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest", margin = list(l = 40, r = 20, t = 8, b = 40))
  })
  
  ### ----  GPT ----
  # Prompt zentral bauen
  gpt_prompt <- reactive({
    req(input$spieler_select2)
    
    sp_raw <- input$spieler_select2
    sp_chr <- if (is.function(sp_raw)) deparse(substitute(sp_raw)) else as.character(sp_raw)
    
    ve_raw <- tryCatch(get_verein(sp_chr), error = function(e) "") 
    ve_chr <- if (is.function(ve_raw)) deparse(substitute(ve_raw)) else as.character(ve_raw)
    
    build_prompt(sp_chr, ve_chr)
  })
  
  output$gpt_prompt_preview <- renderText(gpt_prompt())
  
  # GPT-Call und Prompt-Copy
  observeEvent(input$gpt_run, {
    req(input$spieler_select2)
    load_id <- showNotification("🔍 Spieler-Research läuft …", type="message", duration=NULL, closeButton=FALSE)
    on.exit(try(removeNotification(load_id), silent=TRUE), add=TRUE)
    
    withProgress(message="Research läuft …", value=0, {
      incProgress(0.10, detail="Init")
      if (!requireNamespace("reticulate", quietly=TRUE)) install.packages("reticulate", repos="https://cran.rstudio.com")
      library(reticulate)
      
      incProgress(0.10, detail="Lade Python")
      if (!exists("query_player_text", mode="function")) {
        key <- Sys.getenv("OPENAI_API_KEY"); if (!nzchar(key)) { showNotification("OPENAI_API_KEY fehlt.", type="error"); return() }
        reticulate::py_run_string(sprintf("import os; os.environ['OPENAI_API_KEY'] = %s", jsonlite::toJSON(key, auto_unbox=TRUE)))
        reticulate::source_python("gpt_player.py")
      }
      
      incProgress(0.20, detail="Prompt")
      prompt_str <- gpt_prompt()              
      mdl <- if (!is.null(input$gpt_model) && nzchar(input$gpt_model)) input$gpt_model else "gpt-4o-mini"
      
      incProgress(0.40, detail="API")
      # Spieler + Verein einmal sauber herleiten:
      sp <- input$spieler_select2
      ve <- get_verein(sp)
      res_txt <- tryCatch(py_to_r(query_player_text(sp, ve, mdl, prompt_str)),
                          error=function(e){ showNotification(paste("query_player_text:", e$message), type="error"); "" })
      
      incProgress(0.20, detail="Render")
      output$gpt_result_box <- renderUI({
        if (!nzchar(res_txt)) return("")
        html_txt <- make_links(res_txt)
        tags$pre(id="gpt_result_pre", HTML(html_txt))
      })
      
    })
  })
  
  ## ---- Preis-Leistung ----
  output$price_perf_plot <- renderPlotly({
    req(price_perf_df(), ca_df)
    
    df <- price_perf_df() %>%
      left_join(
        ca_df %>% transmute(SPIELER = as.character(SPIELER),
                            Gesamtpunkte = as.character(Gesamtpunkte)),
        by = "SPIELER"
      ) %>%
      filter(!is.na(MW_num) & !is.na(PPS_num) & PPS_num > 0 & PPS_num < 200) %>%
      arrange(MW_num)
    
    if (nrow(df) == 0) return(plotly_empty())
    
    df <- df %>%
      mutate(
        MW_fmt = paste0(format(round(MW_num), big.mark = ".", decimal.mark = ","), " €"),
        hover = paste0(
          "Spieler: ", SPIELER,
          "<br>PPS: ", formatC(PPS_num, format = "f", digits = 2, decimal.mark = ","),
          "<br>Marktwert: ", ifelse(is.na(MW_fmt), "-", MW_fmt),
          "<br>Gesamtpunkte: ", ifelse(is.na(Gesamtpunkte) | Gesamtpunkte == "NA", "-", Gesamtpunkte),
          "<br>Erwartet (PPS): ", ifelse(is.na(expected_PPS), "-", formatC(expected_PPS, format = "f", digits = 2, decimal.mark = ",")),
          "<br>Label: ", Preis_Leistung_label
        )
      )
    
    # Trendkurve (loess auf log1p(MW_num)) — nur wenn genug Punkte
    pred_df <- NULL
    if (nrow(df) >= 6) {
      df <- df %>% mutate(log_mw = log1p(MW_num))
      lo <- tryCatch(
        loess(PPS_num ~ log_mw, data = df, span = 0.5, control = loess.control(surface = "direct")),
        error = function(e) NULL
      )
      if (!is.null(lo)) {
        new_log <- seq(min(df$log_mw, na.rm = TRUE), max(df$log_mw, na.rm = TRUE), length.out = 200)
        pred <- tryCatch(predict(lo, newdata = data.frame(log_mw = new_log)), error = function(e) rep(NA_real_, length(new_log)))
        pred_df <- data.frame(MW_num = exp(new_log) - 1, pred = pred)
        pred_df <- pred_df %>% filter(is.finite(pred))
      }
    }
    
    # y-Limit-Sicherung
    y_max <- max(c(df$PPS_num), na.rm = TRUE)
    
    # Reihenfolge sicherstellen
    df$Preis_Leistung_label <- factor(df$Preis_Leistung_label,
                                      levels = c("Sehr gut","Gut","Durchschnittlich","Schlecht","Sehr schlecht","-"))
    
    # Farbpalette (kontrastreich, lesbar)
    cols <- c(
      "Sehr gut"        = "#006d2c",  # dunkelgrün
      "Gut"             = "#2ca25f",  # grün
      "Durchschnittlich"= "#fdae61",  # orange-gelb
      "Schlecht"        = "#f46d43",  # orange-rot
      "Sehr schlecht"   = "#d73027",  # rot
      "-"               = "#bdbdbd"   # grau für fehlende Werte
    )
    
    p <- ggplot(df, aes(x = MW_num, y = PPS_num, text = hover)) +
      geom_point(aes(color = Preis_Leistung_label), size = 3, alpha = 0.9)
    
    if (!is.null(pred_df) && nrow(pred_df) > 0) {
      p <- p + geom_line(data = pred_df, aes(x = MW_num, y = pred), inherit.aes = FALSE,
                         linetype = "dashed", size = 0.8, color = "black")
    }
    
    p <- p +
      scale_x_log10() +
      labs(x = "Marktwert (log)", y = "Punkte pro Spiel (PPS)", color = "Preis/Leistung") +
      coord_cartesian(ylim = c(0, max(ceiling(y_max*0.95), 5))) +
      scale_color_manual(values = cols, na.value = "#bdbdbd") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
  })
  
  ## ---- Preis-Performance ----
  punkte_mw_relation <- reactive({
    if (!punkte_mw_should_run()) return(NULL)
    
    req(spieler_stats_spielplan(), ap_df)
    
    norm_spieler <- function(x) trimws(enc2utf8(as.character(x)))
    
    # Saisonzeitraum
    season_start <- as.Date("2025-08-22")
    season_end   <- Sys.Date()
    
    # Events: nur Einsätze mit Punkten im Saisonzeitraum
    stats <- spieler_stats_spielplan() %>%
      mutate(
        Datum   = as.Date(Datum),
        Spieler = norm_spieler(Spieler),
        Punkte  = suppressWarnings(as.numeric(Punkte))
      ) %>%
      filter(!is.na(Punkte),
             Datum >= season_start,
             Datum <= season_end) %>%
      select(Spieler, Datum, Spieltag, Punkte, Note, Status) %>%
      arrange(Spieler, Datum)
    
    if (nrow(stats) == 0) return(tibble::tibble())
    
    # Marktwerte bis heute, nur Spieler aus stats
    mw <- ap_df %>%
      mutate(
        Datum     = as.Date(Datum),
        Spieler   = norm_spieler(Spieler),
        Marktwert = as.numeric(Marktwert)
      ) %>%
      filter(Spieler %in% unique(stats$Spieler),
             Datum <= season_end) %>%
      arrange(Spieler, Datum) %>%
      select(Spieler, Datum, Marktwert)
    
    if (nrow(mw) == 0) return(tibble::tibble())
    
    mw_list <- split(mw, mw$Spieler)
    
    res <- stats %>%
      rowwise() %>%
      mutate(
        cur_sp = Spieler,
        cur_dt = Datum,
        mw_before = {
          tbl <- mw_list[[cur_sp]]
          if (is.null(tbl) || nrow(tbl) == 0) {
            NA_real_
          } else {
            tmp <- tbl %>%
              filter(Datum <= cur_dt) %>%
              arrange(desc(Datum)) %>%
              slice_head(n = 1) %>%
              pull(Marktwert)
            if (length(tmp) == 0) NA_real_ else tmp
          }
        },
        mw_3d = {
          tbl <- mw_list[[cur_sp]]
          if (is.null(tbl) || nrow(tbl) == 0) {
            NA_real_
          } else {
            tmp <- tbl %>%
              filter(Datum > cur_dt, Datum <= cur_dt + 3) %>%
              arrange(Datum) %>%
              slice_head(n = 1) %>%
              pull(Marktwert)
            if (length(tmp) == 0) NA_real_ else tmp
          }
        }
      ) %>%
      select(-cur_sp, -cur_dt) %>%
      ungroup() %>%
      mutate(
        delta_3_abs = ifelse(is.na(mw_before) | is.na(mw_3d), NA_real_, mw_3d - mw_before),
        delta_3_pct = ifelse(is.na(delta_3_abs) | mw_before == 0, NA_real_, 100 * delta_3_abs / mw_before)
      ) %>%
      mutate(
        MW_Klasse = case_when(
          is.na(mw_before)           ~ NA_character_,
          mw_before < 5e5            ~ "<0.5 Mio",
          mw_before < 1e6            ~ "0.5–1 Mio",
          mw_before < 2.5e6          ~ "1–2.5 Mio",
          mw_before < 5e6            ~ "2.5–5 Mio",
          mw_before < 1e7            ~ "5–10 Mio",
          TRUE                       ~ ">10 Mio"
        )
      ) %>%
      filter(!is.na(delta_3_pct))
    
    res
  })
  
  punkte_mw_summary <- reactive({
    if (!punkte_mw_should_run()) return(NULL)
    req(punkte_mw_relation())
    
    df <- punkte_mw_relation() %>%
      filter(!is.na(Punkte), !is.na(MW_Klasse)) %>%
      group_by(MW_Klasse, Punkte) %>%
      summarise(
        mean_delta3 = mean(delta_3_pct, na.rm = TRUE),
        n           = n(),
        .groups     = "drop"
      )
    
    # --- CSV speichern ---
    try({
      write.csv2(df, "data/punkte_mw_summary_cache.csv",
                 row.names = FALSE, fileEncoding = "UTF-8")
    }, silent = TRUE)
    
    df
  })
  
  punkte_mw_summary_cached <- reactive({
    # Wenn CSV existiert → laden
    path <- "data/punkte_mw_summary_cache.csv"
    if (file.exists(path)) {
      df <- try(read.csv2(path, stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8"), silent = TRUE)
      if (!inherits(df, "try-error")) {
        df$Punkte <- as.numeric(df$Punkte)
        df$mean_delta3 <- as.numeric(df$mean_delta3)
        return(df)
      }
    }
    # sonst warten auf die Reactive (Tab muss geöffnet sein)
    punkte_mw_summary()
  })
  
  
  output$punkte_mw_avg_plot_static <- renderPlot({
    req(punkte_mw_relation())
    df <- punkte_mw_relation()
    if (nrow(df) == 0) return(invisible(NULL))
    
    df <- df %>% mutate(Punkte = suppressWarnings(as.numeric(Punkte)))
    
    agg <- df %>%
      filter(!is.na(Punkte)) %>%
      group_by(Punkte) %>%
      summarise(
        mean3 = mean(delta_3_pct, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(agg) == 0) return(invisible(NULL))
    
    x_vals <- sort(unique(agg$Punkte))
    
    y_min <- suppressWarnings(min(agg$mean3, na.rm = TRUE))
    y_max <- suppressWarnings(max(agg$mean3, na.rm = TRUE))
    if (!is.finite(y_min) || !is.finite(y_max)) {
      y_min <- -1
      y_max <- 1
    }
    pad  <- 0.05 * (y_max - y_min + 1e-6)
    ylim <- c(y_min - pad, y_max + pad)
    
    y3 <- agg$mean3[match(x_vals, agg$Punkte)]
    
    plot(
      x = x_vals, y = y3,
      type = "n",
      xlab = "Punkte",
      ylab = "Durchschnittliche Marktwertänderung nach 3 Tagen (%)",
      xlim = range(x_vals),
      ylim = ylim,
      main = "MW-Änderung nach Punktegröße (3 Tage)"
    )
    
    abline(h = 0, lty = 3)
    lines(x_vals, y3, type = "b", pch = 19, col = "blue")
  })
  
  
  output$punkte_mw_by_mwklasse <- renderPlot({
    req(punkte_mw_relation())
    df <- punkte_mw_relation()
    if (nrow(df) == 0) return(invisible(NULL))
    
    df <- df %>%
      mutate(Punkte = suppressWarnings(as.numeric(Punkte))) %>%
      filter(!is.na(Punkte), !is.na(MW_Klasse))
    
    if (nrow(df) == 0) return(invisible(NULL))
    
    agg <- df %>%
      group_by(MW_Klasse, Punkte) %>%
      summarise(
        mean3 = mean(delta_3_pct, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(agg) == 0) return(invisible(NULL))
    
    klassen <- sort(unique(agg$MW_Klasse))
    x_vals  <- sort(unique(agg$Punkte))
    
    y_min <- suppressWarnings(min(agg$mean3, na.rm = TRUE))
    y_max <- suppressWarnings(max(agg$mean3, na.rm = TRUE))
    if (!is.finite(y_min) || !is.finite(y_max)) {
      y_min <- -1
      y_max <- 1
    }
    pad  <- 0.05 * (y_max - y_min + 1e-6)
    ylim <- c(y_min - pad, y_max + pad)
    
    plot(
      x = x_vals, y = rep(NA_real_, length(x_vals)),
      type = "n",
      xlab = "Punkte",
      ylab = "Durchschnittliche MW-Änderung 3 Tage (%)",
      xlim = range(x_vals),
      ylim = ylim,
      main = "MW-Änderung nach Punkten und MW-Klasse (3 Tage)"
    )
    abline(h = 0, lty = 3)
    
    cols <- c("blue", "red", "darkgreen", "orange", "purple", "brown")
    cols <- rep(cols, length.out = length(klassen))
    
    for (i in seq_along(klassen)) {
      k   <- klassen[i]
      sub <- agg %>% filter(MW_Klasse == k)
      xs  <- sort(unique(sub$Punkte))
      ys  <- sub$mean3[match(xs, sub$Punkte)]
      
      lines(xs, ys, type = "b", pch = 19, col = cols[i])
    }
    
    legend(
      "topright",
      legend = klassen,
      col    = cols,
      pch    = 19,
      bty    = "n",
      title  = "MW-Klasse (vor Event)"
    )
  })
  
  
  
  ## ---- Transfer-Simulator ----
  # Reactive DataFrames vorbereiten
  meine_spieler <- reactive({
    req(teams_df)
    
    teams_df %>%
      filter(Manager == "Dominik") %>%
      pull(Spieler)
  })
  
  # Aktuelle Marktwerte
  aktuelle_mw <- reactive({
    req(ap_df)
    
    max_datum <- max(ap_df$Datum, na.rm = TRUE)
    ap_df %>%
      filter(Datum == max_datum) %>%
      transmute(Spieler, Marktwert = as.numeric(Marktwert))
  })
  
  angebote_df <- reactive({
    req(input$main_navbar == "Transfermarkt")                       # korrekt prüfen
    req(exists("angebote", inherits = TRUE), is.data.frame(angebote)) # Datei muss geladen sein
    req(!is.null(meine_spieler()), length(meine_spieler()) > 0)     # eigene Spieler vorhanden
    
    # robusten Spaltennamen-Check
    betrag_col <- if ("Angebot (€)" %in% names(angebote)) "Angebot (€)" else "Angebot"
    
    df <- angebote %>%
      mutate(Status = ifelse(is.na(Status), "aktiv", Status)) %>%
      filter(Status == "aktiv") %>%
      mutate(Spieler = trimws(Spieler))
    
    # wenn Spalte fehlt, erzeugen, sonst bereinigen
    if (!betrag_col %in% names(df)) {
      df$Angebot <- NA_real_
    } else {
      df <- df %>% mutate(Angebot = as.numeric(gsub("\\.", "", .data[[betrag_col]])))
    }
    
    df <- df %>%
      select(Spieler, Datum, Angebot, Status)
    
    # join mit aktuellen Marktwerten (aktuelle_mw stellt selbst sicher, dass ap_df existiert)
    df <- df %>%
      filter(Spieler %in% meine_spieler()) %>%
      left_join(aktuelle_mw(), by = "Spieler") %>%
      mutate(
        Marktwert     = ifelse(is.na(Marktwert), Angebot, Marktwert),
        Kreditverlust = Marktwert / 4
      )
    
    df
  })
  
  
  team_spieler_df <- reactive({
    req(input$main_navbar, "Transfermarkt")
    
    aktuelle_mw() %>%
      semi_join(teams_df %>% filter(Manager == "Dominik"), by = "Spieler")
  })
  
  # UI-Inputs befüllen
  observeEvent(input$main_navbar, {
    if (!identical(input$main_navbar, "Transfermarkt")) return()
    
    # warte auf nötige Daten (tm_df und angebote_df)
    req(exists("tm_df", inherits = TRUE), is.data.frame(tm_df))
    req(!is.null(angebote_df()))   # angebote_df() hat eigene reqs
    
    # 1) Spieler-Liste (tm_df kann lazy geladen werden)
    updateSelectInput(
      session, "spieler_select",
      choices = sort(unique(tm_df$Spieler))
    )
    
    # 3) Angebote-Checkboxes
    a_df <- angebote_df()
    if (nrow(a_df) > 0) {
      angebot_labels <- paste0(
        a_df$Spieler,
        " (", format(a_df$Angebot, big.mark = ".", decimal.mark = ","), " €)"
      )
      angebot_ids <- paste0("A", seq_len(nrow(a_df)))
      choices_a <- setNames(angebot_ids, angebot_labels)
    } else {
      choices_a <- character(0)
    }
    updateCheckboxGroupInput(session, "angebote_select", choices = choices_a)
    
    # 4) Eigene Team-Spieler-Checkboxes
    t_df <- team_spieler_df()   # team_spieler_df() benötigt ebenfalls tm_df/teams_df; hat eigene reqs
    if (nrow(t_df) > 0) {
      team_labels <- paste0(
        t_df$Spieler,
        " (", format(t_df$Marktwert, big.mark = ".", decimal.mark = ","), " €)"
      )
      team_ids <- paste0("T", seq_len(nrow(t_df)))
      choices_t <- setNames(team_ids, team_labels)
    } else {
      choices_t <- character(0)
    }
    updateCheckboxGroupInput(session, "team_select", choices = choices_t)
    
  }, ignoreInit = TRUE)
  
  
  # 5) Plot rendern
  output$transfer_simulator_plot <- renderPlot({
    req(input$spieler_select)
    
    # — (1) Mindestgebot parsen —
    sel     <- tm_df %>% filter(Spieler == input$spieler_select)
    mindest <- suppressWarnings(as.numeric(gsub("\\.", "", sel$Mindestgebot[1])))
    
    # — (2) Kontostand & (3) Kreditrahmen —
    kont        <- kontostand_dominik()
    all_team    <- sum(team_spieler_df()$Marktwert, na.rm = TRUE)
    init_credit <- all_team / 4
    
    # — (4) Selektionen & (5) Erlöse/Verluste —
    a_df <- angebote_df()
    t_df <- team_spieler_df()
    
    angebot_ids        <- if (nrow(a_df) > 0) paste0("A", seq_len(nrow(a_df))) else character(0)
    angebot_map        <- setNames(a_df$Angebot, angebot_ids)
    angebot_kredit_map <- setNames(a_df$Kreditverlust, angebot_ids)
    angebot_name_map   <- setNames(a_df$Spieler, angebot_ids)
    
    team_ids      <- if (nrow(t_df) > 0) paste0("T", seq_len(nrow(t_df))) else character(0)
    team_mw_map   <- setNames(t_df$Marktwert, team_ids)
    team_name_map <- setNames(t_df$Spieler, team_ids)
    
    sel_off_ids  <- if (is.null(input$angebote_select)) character(0) else input$angebote_select
    sel_team_ids <- if (is.null(input$team_select))     character(0) else input$team_select
    
    sel_off  <- sum(unname(angebot_map[sel_off_ids]), na.rm = TRUE)
    sel_team <- sum(unname(team_mw_map[sel_team_ids]), na.rm = TRUE)
    
    loss_off  <- sum(unname(angebot_kredit_map[sel_off_ids]), na.rm = TRUE)
    loss_team <- sel_team / 4
    final_credit <- init_credit - loss_off - loss_team
    
    names_off  <- paste(unname(angebot_name_map[sel_off_ids]), collapse = ", ")
    names_team <- paste(unname(team_name_map[sel_team_ids]), collapse = ", ")
    sold_names <- paste(c(names_off, names_team)[c(nchar(names_off) > 0, nchar(names_team) > 0)],
                        collapse = "; ")
    
    # (6) Zuflüsse zuerst aufs Defizit —
    deficit  <- max(0, -kont)
    use_off  <- min(sel_off, deficit)
    remain_d <- deficit - use_off
    use_team <- min(sel_team, max(0, remain_d))
    
    rem_off  <- sel_off  - use_off   # Überschuss ins Guthaben
    rem_team <- sel_team - use_team
    
    adj_kont        <- kont + use_off + use_team      # nach Defizitdeckung
    new_kontostand  <- adj_kont + rem_off + rem_team  # tatsächlicher neuer Kontostand
    
    # (7) Segmente —
    kont_pos <- max(new_kontostand, 0)        # positiver Kontostand
    kre_pos  <- max(final_credit, 0)          # Kreditrahmen (Kapital)
    neg_pos  <- -max(0, -new_kontostand)      # Defizit (<0, sonst 0)
    
    seg_levels <- c("Kontostand -","Kreditrahmen","Kontostand +")
    
    df_avail_pos <- tibble::tibble(
      x       = "Verfügbar",
      segment = factor(c("Kontostand +","Kreditrahmen"), levels = seg_levels),
      value   = c(kont_pos, kre_pos),
      label   = c("", "")
    )
    
    df_avail_neg <- tibble::tibble(
      x       = "Verfügbar",
      segment = factor("Kontostand -", levels = seg_levels),
      value   = neg_pos,
      label   = ""
    )
    
    df_mindest <- tibble::tibble(type = "Mindestgebot", value = mindest)
    
    # Label-Positionen
    df_pos <- df_avail_pos %>%
      group_by(x) %>% arrange(x, segment) %>%
      mutate(ypos = cumsum(value) - value/2) %>%
      ungroup()
    
    df_neg <- df_avail_neg %>%
      group_by(x) %>% arrange(x, value) %>%
      mutate(ypos = cumsum(value) - value/2) %>%
      ungroup()
    
    df_plot <- bind_rows(df_pos, df_neg) %>%
      mutate(stack_order = as.integer(factor(segment, levels = seg_levels)))
    
    # Kontostand-Label knapp neben y=0
    y_span <- max(c(df_avail_pos$value[df_avail_pos$x == "Verfügbar"], df_mindest$value), na.rm = TRUE)
    offset <- 0.03 * max(1, y_span)
    
    titel_neu <- (new_kontostand != kont)
    label_txt <- paste0(if (titel_neu) "Neuer Kontostand: " else "Aktueller Kontostand: ",
                        format(round(new_kontostand, 0), big.mark = ".", decimal.mark = ","))
    
    kont_df <- tibble::tibble(x = "Verfügbar", y = 0 - offset, lab = label_txt)
    
    # Haken
    total_avail <- max(new_kontostand, 0) + max(final_credit, 0)
    ok1 <- total_avail >= mindest                  # Gesamt reicht
    ok2 <- new_kontostand >= mindest               # Kontostand allein reicht
    y_checks <- max(0, total_avail) * 1.05
    
    # (8) Plot —
    ggplot2::ggplot() +
      ggplot2::geom_col(
        data = df_mindest,
        ggplot2::aes(x = "Mindestgebot", y = value),
        fill = "grey70", width = 0.6
      ) +
      ggplot2::geom_col(
        data = df_plot,
        ggplot2::aes(x = x, y = value, fill = segment),
        width = 0.6
      ) +
      ggplot2::geom_text(
        data = df_plot %>% filter(abs(value) > 1e-9, label != ""),
        ggplot2::aes(x = x, y = ypos, label = label),
        colour = "black", size = 4
      ) +
      ggplot2::geom_text(
        data = kont_df,
        ggplot2::aes(x = x, y = y, label = lab),
        fontface = "bold", size = 4.2
      ) +
      ggplot2::geom_text(
        data = df_mindest,
        ggplot2::aes(x = "Mindestgebot", y = value/2,
                     label = format(value, big.mark = ".", decimal.mark = ",")),
        colour = "black", size = 4
      ) +
      ggplot2::geom_hline(yintercept = mindest, linetype = "dashed",
                          color = "darkred", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "black", linewidth = 0.5) +
      # Zwei Häkchen nebeneinander (beide bei x="Verfügbar", horizontal genudged)
      ggplot2::geom_text(
        data = data.frame(x = "Verfügbar", y = y_checks,
                          lab = if (ok1) "\u2713" else "\u2717",
                          col = if (ok1) "green" else "red"),
        ggplot2::aes(x = x, y = y, label = lab),
        colour = if (ok1) "green" else "red", size = 8,
        position = ggplot2::position_nudge(x = -0.15)
      ) +
      ggplot2::geom_text(
        data = data.frame(x = "Verfügbar", y = y_checks,
                          lab = if (ok2) "\u2713" else "\u2717",
                          col = if (ok2) "green" else "red"),
        ggplot2::aes(x = x, y = y, label = lab),
        colour = if (ok2) "green" else "red", size = 8,
        position = ggplot2::position_nudge(x =  0.15)
      ) +
      ggplot2::annotate("text", x = 2, y = max(0, total_avail) * 1.15,
                        label = sold_names, size = 4, colour = "black") +
      ggplot2::scale_fill_manual(
        values = c(
          "Kontostand -" = "#fb9a99",
          "Kontostand +" = "#b2df8a",
          "Kreditrahmen" = "#a6cee3"
        ),
        breaks = c("Kontostand -","Kontostand +","Kreditrahmen"),
        labels = c("Defizit","Kapital","Kreditrahmen")
      ) +
      ggplot2::scale_x_discrete(limits = c("Mindestgebot","Verfügbar")) +
      ggplot2::scale_y_continuous(labels = scales::label_comma(big.mark=".", decimal.mark=",")) +
      ggplot2::labs(x = NULL, y = "Betrag (€)", fill = NULL) +
      ggplot2::theme_minimal(base_size = 16)
  })
  

  
  ## ---- Angebote-Analyse ----
  
  # Vortag
  output$rel_angebot_vortag <- renderPlot({
    df <- dat_rel_angebote() %>% filter(!is.na(rel_Vortag))
    if (!is.null(input$trend_filter)  && length(input$trend_filter)  > 0) df <- df %>% filter(Trend  %in% input$trend_filter)
    if (!is.null(input$verein_filter) && length(input$verein_filter) > 0) df <- df %>% filter(Verein %in% input$verein_filter)
    if (!is.null(input$status_filter) && length(input$status_filter) > 0) df <- df %>% filter(Status %in% input$status_filter)
    validate(need(nrow(df) > 0, "Keine Daten nach Filter."))
    
    df <- df %>% mutate(lbl = paste0(Spieler, " (", scales::percent(rel_Vortag, accuracy = 1), ")"))
    
    m <- mean(df$rel_Vortag, na.rm = TRUE)
    
    p <- ggplot(df, aes(x = "Vortag", y = rel_Vortag)) +
      geom_boxplot(width = 0.2, outlier.shape = NA,
                   fill = "#ff6b6b", color = "#ff6b6b", alpha = 0.25)
    
    if (isTRUE(input$show_labels) && !requireNamespace("ggrepel", quietly = TRUE)) { 
      install.packages("ggrepel", repos="https://cran.rstudio.com") 
      library(ggrepel) }
    
    if (input$color_by == "Trend") {
      p <- p + geom_point(aes(color = Trend), position = pos_qr, size = 3.5, alpha = 0.6,
                          show.legend = FALSE)
      if (isTRUE(input$show_labels)) {
        p <- p + ggrepel::geom_text_repel(aes(label = lbl, color = Trend),  position = pos_qr, size = 4, max.overlaps = 50, seed = 1, show.legend = FALSE)
      }
    } else if (input$color_by == "Verein") {
      p <- p + geom_point(aes(color = Verein), position = pos_qr, size = 3.5, alpha = 0.6,
                          show.legend = FALSE)
      if (isTRUE(input$show_labels)) {
        p <- p + ggrepel::geom_text_repel(aes(label = lbl, color = Verein), position = pos_qr, size = 4, max.overlaps = 50, seed = 1, show.legend = FALSE)
      }
    } else if (input$color_by == "Status") {
      p <- p + geom_point(aes(color = Status), position = pos_qr, size = 3.5, alpha = 0.6,
                          show.legend = FALSE)
      if (isTRUE(input$show_labels)) {
        p <- p + ggrepel::geom_text_repel(aes(label = lbl, color = Status), position = pos_qr, size = 4, max.overlaps = 50, seed = 1, show.legend = FALSE)
      }
    } else {
      p <- p + geom_point(position = pos_qr, size = 3.5, alpha = 0.6, color = "#ff6b6b",
                          show.legend = FALSE)
      if (isTRUE(input$show_labels)) {
        p <- p + ggrepel::geom_text_repel(aes(label = lbl), position = pos_qr, size = 4, max.overlaps = 50, seed = 1, color = "#ff6b6b", show.legend = FALSE)
      }
    }
    
    p +
      geom_hline(yintercept = m, linetype = "dashed", color = "#ff6b6b") +
      annotate("text", x = .5, y = m,
               label = paste0("Mittelwert ", scales::percent(m, accuracy = 0.1)),
               vjust = -0.6, color = "#ff6b6b") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = NULL, y = "Angebot vs. Marktwert am Vortag",
           title = "Verteilung: Angebot relativ zum Marktwert am Vortag") +
      theme_minimal(base_size = 14)
  })
  
  output$rel_angebot_tag <- renderPlot({
    df <- dat_rel_angebote() %>% filter(!is.na(rel_am_Tag))
    if (!is.null(input$trend_filter)  && length(input$trend_filter)  > 0) df <- df %>% filter(Trend  %in% input$trend_filter)
    if (!is.null(input$verein_filter) && length(input$verein_filter) > 0) df <- df %>% filter(Verein %in% input$verein_filter)
    if (!is.null(input$status_filter) && length(input$status_filter) > 0) df <- df %>% filter(Status %in% input$status_filter)
    validate(need(nrow(df) > 0, "Keine Daten nach Filter."))
    
    df <- df %>% mutate(lbl = paste0(Spieler, " (", scales::percent(rel_am_Tag, accuracy = 1), ")"))
    
    m <- mean(df$rel_am_Tag, na.rm = TRUE)
    
    p <- ggplot(df, aes(x = "Am Tag", y = rel_am_Tag)) +
      geom_boxplot(width = 0.2, outlier.shape = NA,
                   fill = "#ff6b6b", color = "#ff6b6b", alpha = 0.25)
    
    if (input$color_by == "Trend") {
      p <- p + geom_point(aes(color = Trend), position = pos_qr, size = 3.5, alpha = 0.6)
      if (isTRUE(input$show_labels)) {
        p <- p + ggrepel::geom_text_repel(aes(label = lbl, color = Trend),  position = pos_qr, size = 4, max.overlaps = 50, seed = 1, show.legend = FALSE)
      }
    } else if (input$color_by == "Verein") {
      p <- p + geom_point(aes(color = Verein), position = pos_qr, size = 3.5, alpha = 0.6)
      if (isTRUE(input$show_labels)) {
        p <- p + ggrepel::geom_text_repel(aes(label = lbl, color = Verein), position = pos_qr, size = 4, max.overlaps = 50, seed = 1, show.legend = FALSE)
      }
    } else if (input$color_by == "Status") {
      p <- p + geom_point(aes(color = Status), position = pos_qr, size = 3.5, alpha = 0.6)
      if (isTRUE(input$show_labels)) {
        p <- p + ggrepel::geom_text_repel(aes(label = lbl, color = Status), position = pos_qr, size = 4, max.overlaps = 50, seed = 1, show.legend = FALSE)
      }
    } else {
      p <- p + geom_point(position = pos_qr, size = 3.5, alpha = 0.6, color = "#ff6b6b")
      if (isTRUE(input$show_labels)) {
        p <- p + ggrepel::geom_text_repel(aes(label = lbl), position = pos_qr, size = 4, max.overlaps = 50, seed = 1, color = "#ff6b6b", show.legend = FALSE)
      }
    }
    
    p +
      geom_hline(yintercept = m, linetype = "dashed", color = "#ff6b6b") +
      annotate("text", x = .5, y = m,
               label = paste0("Mittelwert ", scales::percent(m, accuracy = 0.1)),
               vjust = -0.6, color = "#ff6b6b") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = NULL, y = "Angebot vs. Marktwert am Angebots-Tag",
           title = "Verteilung: Angebot relativ zum Marktwert am Angebots-Tag") +
      theme_minimal(base_size = 14)
  })
  
  # ---- KADER-ENTWICKLUNG ----
  ## ---- Mein Kader ----
  
  # Server: choices für manager_select_mein_kader aktualisieren
  observe({
    req(exists("transfers") && nrow(transfers) > 0)
    mgrs <- setdiff(sort(unique(transfers$Besitzer)), "Computer")
    if (length(mgrs) == 0) return()
    sel <- if ("Dominik" %in% mgrs) "Dominik" else mgrs[1]
    updateSelectInput(session, "manager_select_mein_kader", choices = mgrs, selected = sel)
  })
  
  # 1) Reaktive Daten-Vorbereitung
  mein_kader_df <- reactive({
    req(input$manager_select_mein_kader)            # neu: abhängig vom Select
    df0 <- teams_df %>%
      filter(Manager == input$manager_select_mein_kader) %>%
      mutate(
        Position = factor(Position,
                          levels = c("Tor", "Abwehr", "Mittelfeld", "Sturm"),
                          ordered = TRUE)
      ) %>%
      arrange(Position, Spieler)
    
    # sicherstellen, dass Marktwert numerisch ist
    gesamt_mw_roh$Marktwert <- as.numeric(gesamt_mw_roh$Marktwert)
    
    # Aktueller Marktwert
    mw_aktuell <- gesamt_mw_roh %>%
      group_by(Spieler) %>%
      filter(Datum == max(Datum, na.rm = TRUE)) %>%
      summarise(Marktwert_aktuell = first(Marktwert), .groups = "drop")
    
    # Kaufpreise
    kaufpreise <- transfers %>%
      filter(Hoechstbietender == input$manager_select_mein_kader) %>%
      group_by(Spieler) %>%
      arrange(desc(Datum)) %>%
      slice(1) %>%
      select(Spieler, Kaufpreis = Hoechstgebot)
    
    # --- 3-Pfeil-Trend über die letzten 4 Tage (MW1=ältester, MW4=jüngster) ---
    dates_vec <- gesamt_mw_roh %>%
      distinct(Datum) %>%
      pull(Datum) %>%
      as.Date() %>%
      na.omit() %>%
      sort(increasing = TRUE)
    
    if (length(dates_vec) >= 2) {
      last_dates <- tail(dates_vec, 4)
      # Labels in zeitlich aufsteigender Reihenfolge, damit m1->m2, m2->m3, m3->m4 passt
      nm <- paste0("MW", seq_along(last_dates))              # MW1..MWk
      names(last_dates) <- nm
      
      mw_wide <- gesamt_mw_roh %>%
        filter(Datum %in% last_dates) %>%
        mutate(tag = names(last_dates)[match(Datum, last_dates)]) %>%
        select(Spieler, tag, Marktwert) %>%
        distinct(Spieler, tag, .keep_all = TRUE) %>%
        tidyr::pivot_wider(names_from = tag, values_from = Marktwert)
      
      df_pre <- df0 %>%
        left_join(mw_aktuell, by = "Spieler") %>%
        left_join(mw_wide,    by = "Spieler") %>%
        mutate(
          Trend3 = {
            # baue Pfeile von alt->neu: (MW1->MW2), (MW2->MW3), (MW3->MW4)
            a1 <- if ("MW1" %in% names(.)) .$MW1 else NA_real_
            a2 <- if ("MW2" %in% names(.)) .$MW2 else NA_real_
            a3 <- if ("MW3" %in% names(.)) .$MW3 else NA_real_
            a4 <- if ("MW4" %in% names(.)) .$MW4 else NA_real_
            
            paste0(
              mapply(arrow_for, a1, a2),
              mapply(arrow_for, a2, a3),
              mapply(arrow_for, a3, a4)
            )
          }
        ) %>%
        select(-any_of(c("MW1","MW2","MW3","MW4")))
    } else {
      df_pre <- df0 %>%
        left_join(mw_aktuell, by = "Spieler") %>%
        mutate(Trend3 = "–")
    }
    
    df_pre <- df_pre %>%
      left_join(kaufpreise, by = "Spieler") %>%
      mutate(
        Diff_Kauf = Marktwert_aktuell - Kaufpreis,
        Diff_Kauf_fmt = case_when(
          is.na(Kaufpreis) ~ "",
          Diff_Kauf > 0    ~ sprintf("<span style='color:#388e3c;'>+%s €</span>",
                                     format(Diff_Kauf, big.mark=".", decimal.mark=",")),
          Diff_Kauf < 0    ~ sprintf("<span style='color:#e53935;'>-%s €</span>",
                                     format(abs(Diff_Kauf), big.mark=".", decimal.mark=",")),
          TRUE             ~ "<span style='color:grey;'>±0 €</span>"
        )
      ) %>%
      # Tages-Diff + aktuelle MW-Formatierung
      left_join(
        gesamt_mw_roh %>%
          group_by(Spieler) %>%
          arrange(desc(Datum)) %>%
          slice(1:2) %>%
          mutate(Diff = Marktwert - lead(Marktwert)) %>%
          slice(1) %>%
          ungroup() %>%
          mutate(
            Marktwert_fmt = paste0(format(Marktwert, big.mark=".", decimal.mark=","), " €"),
            Diff_fmt      = case_when(
              is.na(Diff) ~ "",
              Diff > 0    ~ sprintf("<span style='color:#388e3c;'>▲ %s €</span>", format(Diff, big.mark=".", decimal.mark=",")),
              Diff < 0    ~ sprintf("<span style='color:#e53935;'>▼ %s €</span>", format(abs(Diff), big.mark=".", decimal.mark=",")),
              TRUE        ~ "–"
            )
          ) %>%
          select(Spieler, Verein, Marktwert_fmt, Diff_fmt),
        by = "Spieler"
      ) %>%
      left_join(ca_df %>% select(-Verein, -Position, -Gesamtpunkte), by = c("Spieler" = "SPIELER")) %>%
      rename(
        "Punkte pro Spiel"           = Punkte_pro_Spiel
      ) %>%  left_join(
        price_perf_df() %>% transmute(SPIELER, `Preis-Leistung` = Preis_Leistung_label, Preis_Leistung_Score = Preis_Leistung_score),
        by = c("Spieler" = "SPIELER")
      )
    
    # --- LI: Einsatzquote + Startelf-Prozent (kompakt "E/S11") ---
    df_pre <- df_pre %>%
      left_join(li_subset, by = c("Spieler" = "Comunio_Name")) %>%
      mutate(
        Einsatz_S11_fmt = case_when(
          is.na(EQ_Gesamt_Quote) & is.na(`EQ_Startelf_.`) ~ "-",
          TRUE ~ sprintf("%s/%s&nbsp;%%",
                         ifelse(is.na(EQ_Gesamt_Quote), "-", as.character(round(EQ_Gesamt_Quote))),
                         ifelse(is.na(`EQ_Startelf_.`),   "-", as.character(round(`EQ_Startelf_.`))))
        )
      )
    
    # StatusIcons
    df_pre <- df_pre %>%
      left_join(status_latest, by = "Spieler") %>%
      mutate(
        StatusTag = ifelse(
          is.na(StatusIcon) | StatusIcon == "" | StatusIcon == "icons-status-active-dark",
          "",
          sprintf(
            '<img src="%s/%s.png" width="20" height="20" title="%s"/>',
            "extracted_icons", StatusIcon, StatusText %||% ""
          )
        )
      )
    
    # Logo-Spalte
    logo_dir <- "logos"
    df_pre$Logo <- ifelse(
      is.na(logo_map[df_pre$Verein]),
      "",
      paste0('<img src="', logo_dir, '/', logo_map[df_pre$Verein],
             '" width="28" title="', df_pre$Verein, '"/>')
    )
    
    df_pre
  })
  
  # 2) Output
  output$mein_kader <- renderUI({
    df_pre <- mein_kader_df()
    
    grouped_sections <- lapply(split(df_pre, df_pre$Position), function(gruppe) {
      pos_name <- unique(gruppe$Position)
      rows <- lapply(seq_len(nrow(gruppe)), function(i) {
        sp    <- gruppe$Spieler[i]
        stat  <- gruppe$StatusTag[i]
        v     <- gruppe$Logo[i]
        pps   <- gruppe$`Punkte pro Spiel`[i]
        es11  <- gruppe$Einsatz_S11_fmt[i]
        pl    <- gruppe$`Preis-Leistung`[i]
        mw    <- gruppe$Marktwert_fmt[i]
        diff  <- gruppe$Diff_fmt[i]
        diffk <- gruppe$Diff_Kauf_fmt[i]
        trend3<- gruppe$Trend3[i]
        
        btn <- sprintf(
          "<button class='btn btn-xs btn-info mk-info-btn' data-player='%s'>i</button>",
          safe_attr(sp)
        )
        
        btn_gpt <- sprintf(
          "<button class='btn btn-xs btn-success mk-gpt-btn' data-row='%d' data-prompt='%s'>GPT</button>",
          i,
          safe_attr(sprintf(
            "Analysiere den Bundesligaspieler %s (%s)...",
            sp, gruppe$Verein[i]
          ))
        )
        
        mask <- ap_df$Spieler == sp
        mw_vec <- suppressWarnings(as.numeric(ap_df$Marktwert[mask]))
        dt_vec <- as.Date(ap_df$Datum[mask])
        
        mw_mean   <- ifelse(length(mw_vec) > 0, round(mean(mw_vec, na.rm = TRUE)), NA)
        mw_median <- ifelse(length(mw_vec) > 0, round(median(mw_vec, na.rm = TRUE)), NA)
        mw_max    <- ifelse(length(mw_vec) > 0, round(max(mw_vec, na.rm = TRUE)), NA)
        mw_min    <- ifelse(length(mw_vec) > 0, round(min(mw_vec, na.rm = TRUE)), NA)
        
        if (length(dt_vec) > 0 && any(!is.na(dt_vec))) {
          idx_curr <- which.max(dt_vec)
          mw_curr  <- suppressWarnings(as.numeric(ap_df$Marktwert[mask][idx_curr]))
        } else {
          mw_curr <- NA
        }
        
        prompt_str <- paste0(
          "Aufgabe: Analysiere den Bundesligaspieler ", sp, " (", gruppe$Verein[i], ").\n",
          "Heutiges Datum (Europe/Berlin): ", today, ". Nutze NUR Inhalte der letzten 30 Tage; ältere als 'älter' kennzeichnen. Zeitbezüge strikt relativ zu diesem Datum.\n",
          "Quellen: Vereinsseiten, bundesliga.com, dfb.de, kicker.de, transfermarkt.de, LigaInsider, Comunio-Magazin, seriöse Regionalmedien. Keine Social Media/Foren. Jede Aussage mit [1], [2] … belegen; jüngste Quelle gewinnt. Unklares als 'Unklar'.\n",
          "Bestimme den nächsten Bundesligaspieltag inkl. Gegner, Datum, Ort aus offizieller Quelle.\n\n",
          "Comunio-Marktwertdaten (nur Zusatz, keine Quelle): Aktuell=", mw_curr,
          "; Mittel=", mw_mean, "; Median=", mw_median, "; Hoch=", mw_max, "; Tief=", mw_min, ".\n",
          "Beurteile das MW-Potenzial relativ zu MW_max.\n\n",
          "Liefere GENAU diese Abschnitte:\n",
          "- Status jetzt:\n",
          "- Einsatz 1–2 Wochen:\n",
          "- Trainerstimme:\n",
          "- Verletzung / Rückkehr:\n",
          "- MW-Potenzial (Comunio):\n",
          "- Aufstellungsempfehlung:\n",
          "- Gegneranalyse nächster Spieltag:\n",
          "- Quellen (YYYY-MM-DD):"
        )
        
        btn_gpt <- sprintf(
          "<button class='btn btn-xs btn-success mk-gpt-btn' data-row='%d' data-prompt='%s'>GPT</button>",
          i,
          safe_attr(prompt_str)
        )
        
        
        sprintf(
          "<tr>
           <td style='padding:4px; text-align:center; width:36px; white-space:nowrap;'>%s</td>
           <td style='padding:4px; text-align:left;'>%s</td>
           <td style='padding:4px; text-align:left;'>%s</td> 
           <td style='padding:4px; text-align:left;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
         </tr>",
          v,sp,stat,trend3,diff,diffk,
          mw, 
          ifelse(is.na(pps), "-", format(round(pps, 2), decimal.mark=",")),
          es11,
          ifelse(is.na(pl),  "-", pl),
          btn, btn_gpt
        )
      })
      
      paste0(
        sprintf("<tr><th colspan='13' style='text-align:left; background:#eee; padding:6px 0 4px 20px;'>%s</th></tr>", pos_name),
        paste(rows, collapse = "\n")
      )
    })
    
    table_html <- paste0(
      "<table style='width:100%; border-collapse:collapse;'>",
      "<thead><tr>
       <th style='text-align:center;'> </th>
       <th style='text-align:left;'>Spieler</th>
       <th style='text-align:left;'>Status</th>
       <th style='text-align:left;'>Trend</th>
       <th style='text-align:center;'>Δ-Vortag</th>
       <th style='text-align:center;'>Δ-Kauf</th>
       <th style='text-align:center;'>MW</th>
       <th style='text-align:center;'>PPS</th>
       <th style='text-align:center;'>E/S11&nbsp;%</th>
       <th style='text-align:center;'>Preis-Leistung</th>
       <th style='text-align:center;'> </th>
       <th style='text-align:center;'> </th>
     </tr></thead>",
      paste(grouped_sections, collapse = "\n"),
      "</tbody></table>"
    )
    
    tagList(HTML(table_html))
  })
  
  
  ## ---- Alle Kader ----
  
  output$sum_selected_mw <- renderUI({
    df_vals <- teams_df %>%
      left_join(mw_aktuell_rx(), by = "Spieler") %>%
      transmute(id = cb_id(Manager, Spieler),
                mw = as.numeric(Marktwert_aktuell))
    
    sel <- vapply(df_vals$id, function(id) isTRUE(input[[id]]), logical(1))
    total_mw <- sum(df_vals$mw[sel], na.rm = TRUE)
    
    # NEU: Kontostand je Manager
    konten <- kapital_df_reactive() %>%
      select(Manager, Kontostand = Aktuelles_Kapital) %>%
      mutate(Kontostand = as.numeric(Kontostand)) %>%
      as.data.frame()
    
    sel_manager <- if (is.null(input$saldo_manager)) "Keiner" else input$saldo_manager
    add_saldo <- if (!is.null(sel_manager) && sel_manager != "Keiner") {
      val <- konten$Kontostand[konten$Manager == sel_manager]
      ifelse(length(val) == 1 && !is.na(val), val, 0)
    } else 0
    
    total <- total_mw + add_saldo
    
    HTML(sprintf(
      "<div style='font-weight:bold; text-align:center; font-size:16px; margin:10px 0;'>
     MW-Summe%s: %s €
   </div>",
      if (add_saldo != 0) paste0(" (+ Kontostand ", sel_manager, ")") else "",
      format(total, big.mark = ".", decimal.mark = ",")
    ))
  })
  
  
  output$kader_uebersicht_ui <- renderUI({
    manager_list <- sort(unique(teams_df$Manager))
    mw_aktuell <- mw_aktuell_rx()
    
    # Auswahl, wessen Kontostand eingerechnet wird
    saldo_ctrl <- div(
      style = "text-align:center; margin-bottom:10px;",
      radioButtons(
        inputId = "saldo_manager",
        label = "Kalkulation für:",
        choices = c("Keiner", manager_list),
        selected = "Keiner",
        inline = TRUE
      )
    )
    
    kaufpreise <- transfers %>%
      group_by(Spieler, Hoechstbietender) %>%
      arrange(desc(Datum)) %>%
      slice(1) %>%
      select(Spieler, Hoechstbietender, Kaufpreis = Hoechstgebot)
    
    mw_diff <- gesamt_mw_roh %>%
      group_by(Spieler) %>%
      arrange(desc(Datum)) %>%
      slice(1:2) %>%
      mutate(Diff = Marktwert - lead(Marktwert)) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        Marktwert_fmt = ifelse(is.na(Marktwert), "-", paste0(format(Marktwert, big.mark = ".", decimal.mark = ","), "")),
        Diff_fmt = case_when(
          is.na(Diff) ~ "",
          Diff > 0 ~ sprintf("<span style='color:#81c784;'>▲ %s</span>", format(Diff, big.mark = ".", decimal.mark = ",")),
          Diff < 0 ~ sprintf("<span style='color:#e57373;'>▼ %s</span>", format(abs(Diff), big.mark = ".", decimal.mark = ",")),
          TRUE ~ "<span style='color:grey;'>–</span>"
        )
      ) %>%
      select(Spieler, Marktwert_fmt, Diff_fmt)
    
    create_kader_table <- function(manager_name) {
      df <- teams_df %>% filter(Manager == manager_name)
      df$Position <- factor(df$Position, levels = c("Tor","Abwehr","Mittelfeld","Sturm"), ordered = TRUE)
      df <- df %>% arrange(Position, Spieler)
      
      df_pre <- df %>%
        left_join(mw_aktuell, by = "Spieler") %>%
        left_join(mw_diff,    by = "Spieler") %>%
        left_join(kaufpreise %>% filter(Hoechstbietender == manager_name), by = "Spieler") %>%
        mutate(
          Diff_Kauf = ifelse(is.na(Kaufpreis), NA, Marktwert_aktuell - Kaufpreis),
          Diff_Kauf_fmt = ifelse(
            is.na(Kaufpreis), "",
            case_when(
              Diff_Kauf > 0 ~ sprintf("<span style='color:#388e3c;'>+%s</span>", format(Diff_Kauf, big.mark = ".", decimal.mark = ",")),
              Diff_Kauf < 0 ~ sprintf("<span style='color:#e53935;'>-%s</span>", format(abs(Diff_Kauf), big.mark = ".", decimal.mark = ",")),
              TRUE ~ "<span style='color:grey;'>±0 €</span>"
            )
          )
        )
      
      rows <- c(); current_pos <- NULL
      for (i in seq_len(nrow(df_pre))) {
        pos     <- as.character(df_pre$Position[i])
        spieler <- df_pre$Spieler[i]
        mw      <- df_pre$Marktwert_fmt[i]
        diff    <- df_pre$Diff_fmt[i]
        diffk   <- df_pre$Diff_Kauf_fmt[i]
        cid     <- cb_id(manager_name, spieler)
        
        if (is.null(current_pos) || pos != current_pos) {
          rows <- c(rows, sprintf(
            "<tr style='background:#eee; line-height:2;'>
             <th colspan='5' style='text-align:left; padding:0 2px;'>%s</th>
           </tr>", pos))
          current_pos <- pos
        }
        
        cb_html <- as.character(
          checkboxInput(inputId = cid, label = NULL, value = FALSE, width = "auto") %>%
            tagAppendAttributes(style = "margin-left:5px; padding:0; height:10px; vertical-align:middle; position:relative; top:-5px;")
        )
        
        rows <- c(rows, sprintf(
          "<tr style='line-height:2;'>
           <td style='padding:0 0 0 6px; line-height:1;'>%s</td>
           <td style='text-align:right; padding:0; line-height:1;'>%s</td>
           <td style='text-align:center; width:10px; padding:0; line-height:2;'>%s</td>
           <td style='text-align:right; padding:0; line-height:1;'>%s</td>
           <td style='text-align:right; padding:0; line-height:1;'>%s</td>
         </tr>",
          spieler, mw, cb_html, diff, diffk
        ))
      }
      
      table_html <- paste0(
        "<table style='border-collapse:collapse; width:100%; margin-bottom:12px; font-size:13px; line-height:2;'>",
        "<thead>
       <tr style='line-height:2;'>
          <th style='text-align:left; padding:0;'>Spieler</th>
          <th style='text-align:right; padding:0;'>MW (€)</th>
          <th style='text-align:center; padding:0; width:10px;'> </th>
          <th style='text-align:right; padding:0;'>Δ Vortag (€)</th>
          <th style='text-align:right; padding:0;'>Δ Kauf (€)</th>
       </tr>
     </thead><tbody>",
        paste(rows, collapse = "\n"),
        "</tbody></table>"
      )
      
      tagList(
        tags$h4(manager_name, style = "margin-top:0; margin-bottom:4px;"),
        HTML(table_html)
      )
    }
    
    tables_ui <- lapply(manager_list, create_kader_table)
    rows_ui   <- split(tables_ui, ceiling(seq_along(tables_ui) / 4))
    
    tagList(
      saldo_ctrl,                  # NEU
      uiOutput("sum_selected_mw"),
      lapply(rows_ui, function(row_tables) {
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:flex-start; gap:8px; margin-bottom:18px;",
          lapply(row_tables, function(tab) {
            tags$div(style = "flex:1 1 22%; box-sizing:border-box;", tab)
          })
        )
      })
    )
  })
  
  
  
  ## ---- Kaderwert-Entwicklung ----
  manuelle_standings <- tibble(
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
  
  standings_history_df <- reactive({
    df <- bind_rows(st_df, manuelle_standings) %>%
      mutate(Datum = as.Date(Datum))
    
    if (isTRUE(input$smooth_mgr_curves)) {
      df <- df %>% filter(lubridate::wday(Datum, week_start = 1) == 5)  # Freitag
    }
    df
  })
  
  output$kaderwert_plot <- renderPlot({
    df <- standings_history_df()
    
    p <- ggplot(df, aes(x = Datum, y = Teamwert, color = Manager))
    
    if (isTRUE(input$smooth_mgr_curves)) {
      p <- p + geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1.4)
    } else {
      p <- p + geom_line(linewidth = 1.2) + geom_point(size = 2, alpha = 0.7)
    }
    
    p +
      scale_color_brewer(palette = "Paired") +
      labs(
        title = "Kaderwert-Entwicklung",
        subtitle = "Teamwert pro Manager über die Zeit",
        x = "Datum",
        y = "Teamwert (€)",
        color = "Manager"
      ) +
      theme_minimal(base_size = 16)
  })
  
  
  # ---- BIETERPROFILE ----
  ## ---- MW-Klassen Zeitstrahl für Boxplot+Beeswarm ----
  output$mw_zeitachse_preview_schlank <- renderPlot({
    # Kompletten Zeitraum nutzen, keine Filterung nach Slider
    ggplot(gesamt_mw_df, aes(x = Datum, y = MW_rel_normiert)) +
      geom_line(color = "darkgrey", linewidth = 1.2, na.rm = TRUE) +
      coord_cartesian(ylim = c(0.75, 1.4)) +
      labs(x = NULL, y = NULL, title = NULL) +
      theme_minimal(base_size = 16) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  ## ---- Einzeln (Bieter, Typ) ----
  output$beeswarm <- renderPlot({
    data <- gebotsprofil_mwclass_filtered()
    req(nrow(data) > 0)
    
    plotdata <- data %>%
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
      geom_boxplot(aes(fill = Typ), width = 0.5, outlier.shape = NA, alpha = 0.5) +
      geom_beeswarm(cex = 2, size = 2.5, alpha = 0.5) +
      # Gesamter Mittelwert als gestrichelte Linie und Text
      geom_hline(
        data = mean_total,
        aes(yintercept = Mean_total),
        linetype = "dashed", color = "grey75", linewidth = 0.9, na.rm = TRUE
      ) +
      geom_text(
        data = mean_total,
        aes(x = 1.5, y = Mean_total, label = round(Mean_total, 1)),
        color = "grey50", # Dunkelrot
        fontface = "bold",
        size = 5
      ) +
      # Mittelwert je Typ als Text (wie gehabt)
      geom_text(
        data = means,
        aes(x = Typ, y = Mean, label = round(Mean, 1)),
        color = "black",
        fontface = "bold",
        size = 5
      ) +
      facet_wrap(~ Bieter, ncol = 4, scales = "free_y") +
      labs(
        title = "Gebotsabweichungen je Konkurrent",
        x = "",
        y = "Abweichung vom MW Vortag (%)"
      ) +
      scale_color_manual(values = c("Hoechstgebot" = "#1f77b4", "Zweitgebot" = "#ff7f0e")) +
      scale_fill_manual(values = c("Hoechstgebot" = "#1f77b4", "Zweitgebot" = "#ff7f0e")) +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 16)
      )
  })

  ## ---- MW-Klassen Boxplot+Beeswarm ----
  mwclass_plot_obj <- reactive({
    data <- gebotsprofil_mwclass_filtered()
    req(nrow(data) > 0)
    
    plotdata <- data %>%
      filter(!is.na(Diff_Prozent)) %>%
      mutate(
        MW_Klasse = factor(
          MW_Klasse,
          levels = c("<0.5 Mio","0.5–1 Mio","1–2.5 Mio","2.5–5 Mio","5–10 Mio",">10 Mio")
        )
      ) 
      #filter(Diff_Prozent <= 50) filter
    
    plotdata_beeswarm <- plotdata %>%
      group_by(Bieter, MW_Klasse) %>%
      mutate(n_pts = n()) %>%
      ungroup()
    
    means <- plotdata %>%
      group_by(Bieter, MW_Klasse) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    means_klasse <- plotdata %>%
      group_by(MW_Klasse) %>%
      summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    plotdata_ueber <- plotdata %>%
      left_join(means_klasse, by = "MW_Klasse") %>%
      group_by(MW_Klasse) %>%
      summarise(
        pct_ueber = round(100 * sum(Diff_Prozent > Mean, na.rm = TRUE) / n(), 1),
        n_total = n(),
        .groups = "drop"
      )
    
    ggplot(
      plotdata_beeswarm,
      aes(
        x = Bieter, y = Diff_Prozent, color = Bieter, fill = Bieter,
        text = paste0(
          "Spieler: ", Spieler, "\n",
          "Bieter: ", Bieter, "\n",
          "MW-Klasse: ", MW_Klasse, "\n",
          "Abweichung: ", round(Diff_Prozent, 1), " %"
        )
      )
    ) +
      # Ø pro Bieter als schmale Querleiste (diskrete x-Achse-kompatibel)
      geom_crossbar(
        data = means,
        aes(x = Bieter, y = Mean, ymin = Mean, ymax = Mean),
        width = 0.7, linetype = "dotted", color = "grey40", linewidth = 0.2,
        inherit.aes = FALSE
      ) +
    
      # Punktewolke
      geom_beeswarm(
        data = subset(plotdata_beeswarm, n_pts > 1),
        cex = 2, size = 2, alpha = 0.8, priority = "random"
      ) +
      # Einzelpunkte
      geom_point(
        data = subset(plotdata_beeswarm, n_pts == 1),
        size = 2, alpha = 0.8, shape = 21
      ) +
      # Ø-Wert je Bieter beschriften
      geom_text(
        data = means,
        aes(x = Bieter, y = Mean, label = round(Mean, 1)),
        color = "black", fontface = "bold", size = 4, vjust = -0.7,
        inherit.aes = FALSE
      ) +
      # Ø je MW-Klasse
      geom_hline(
        data = means_klasse, aes(yintercept = Mean),
        color = "#d62728", linewidth = 0.5, linetype = "solid"
      ) +
      geom_text(
        data = means_klasse,
        aes(x = 1.5, y = Mean+20, label = paste0("Ø ", round(Mean, 1), " %")),
        color = "#d62728", fontface = "bold", size = 4,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = plotdata_ueber,
        aes(x = 2.5, y = 50, label = paste0(pct_ueber, "% (n=", n_total, ")")),
        color = "#333", fontface = "bold", size = 4,
        inherit.aes = FALSE
      ) +
      facet_grid(. ~ MW_Klasse, scales = "free_y") +
      labs(
        title = "",
        x = "Bieter", y = "Abweichung vom MW Vortag (%)"
      ) +
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 30, hjust = 1)
      ) 
  })
  
  
  # einziges Rendering
  output$mwclassplot <- renderPlotly({
    p <- mwclass_plot_obj()
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
  })
  
  ## ---- Query ----
  
  # Kandidatenliste für Vortags-MW (Reihenfolge = Priorität)
  ref_candidates <- c("MW_Vortag","MW_vortag","MW_vortag_eur","MW_vortag_€","MW_vortag_euro")
  
  manager_segment_df <- reactive({
    data <- gebotsprofil_mwclass_filtered()
    req(nrow(data) > 0, input$seg_manager, !is.na(input$seg_mw), !is.na(input$seg_tol))
    
    ref_candidates <- c("MW_Vortag","MW_vortag","MW_vortag_eur","MW_vortag_€","MW_vortag_euro")
    ref_col <- intersect(ref_candidates, names(data))[1]
    validate(need(!is.null(ref_col), "Spalte für Vortags-MW nicht gefunden."))
    
    df <- data %>%
      filter(Bieter == input$seg_manager) %>%
      filter(!is.na(.data[[ref_col]]), !is.na(Diff_Prozent)) %>%
      mutate(Ref_MW = as.numeric(.data[[ref_col]]))
    
    target <- as.numeric(input$seg_mw)
    tol    <- as.numeric(input$seg_tol) / 100
    rng    <- target * c(1 - tol, 1 + tol)
    df     <- df %>% filter(Ref_MW >= rng[1], Ref_MW <= rng[2])
    
    # Datum robust parsen (ohne laute Warnungen)
    if ("Datum" %in% names(df)) {
      dchr <- as.character(df$Datum)
      dchr[!nzchar(dchr)] <- NA_character_
      df$Datum <- dplyr::coalesce(
        suppressWarnings(lubridate::ymd(dchr, quiet = TRUE)),
        suppressWarnings(lubridate::dmy(dchr, quiet = TRUE)),
        suppressWarnings(as.Date(df$Datum, tryFormats = c("%Y-%m-%d","%d.%m.%Y")))
      )
    } else {
      df$Datum <- as.Date(NA)
    }
    
    amount_candidates <- c("Gebot","Hoechstgebot","Zweitgebot","Betrag","Gebot_EUR")
    amt_col <- intersect(amount_candidates, names(df))[1]
    if (is.null(amt_col)) {
      df <- df %>% mutate(Gebot_EUR = round(Ref_MW * (1 + Diff_Prozent/100)))
    } else {
      df <- df %>% mutate(Gebot_EUR = as.numeric(stringr::str_replace_all(.data[[amt_col]], "[^0-9-]", "")))
    }
    
    if ("Typ" %in% names(df)) {
      df$Typ <- factor(df$Typ, levels = c("Hoechstgebot","Zweitgebot"))
    }
    
    fmt_eur0 <- function(x) ifelse(is.na(x), "–", paste0(format(round(x), big.mark=".", decimal.mark=","), " €"))
    
    df %>%
      mutate(
        tooltip = paste0(
          "Spieler: ", dplyr::coalesce(Spieler, ""), "\n",
          "Typ: ", dplyr::coalesce(as.character(Typ), ""), "\n",
          "Gebot: ", fmt_eur0(Gebot_EUR), "\n",
          "Vortags-MW: ", fmt_eur0(Ref_MW), "\n",
          "Abweichung: ", round(Diff_Prozent, 1), " %\n",
          "Datum: ", ifelse(is.na(Datum), "", format(Datum, "%d.%m.%Y"))
        )
      )
  })
  
  output$manager_segment_plot <- renderPlotly({
    df <- manager_segment_df()
    req(nrow(df) > 0)
    
    means <- df %>%
      dplyr::group_by(Typ) %>%
      dplyr::summarise(Mean = mean(Diff_Prozent), .groups = "drop")
    
    p <- ggplot(df, aes(x = Typ, y = Diff_Prozent, color = Typ, fill = Typ, text = tooltip)) +
      geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.45) +
      ggbeeswarm::geom_beeswarm(cex = 2, size = 2.5, alpha = 0.8, priority = "random") +
      geom_hline(data = means, 
                 aes(yintercept = Mean),
                 color = "grey60", linetype = "dashed", linewidth = 0.6) +
      geom_text(data = means, inherit.aes = FALSE,
                aes(x = Typ, y = Mean, label = round(Mean, 1)),
                color = "black", fontface = "bold", size = 5, vjust = -0.6) +
      labs(title = "", x = "", y = "Abweichung vom MW Vortag (%)") +
      scale_color_manual(values = c("Hoechstgebot" = "#1f77b4", "Zweitgebot" = "#ff7f0e")) +
      scale_fill_manual(values = c("Hoechstgebot" = "#1f77b4", "Zweitgebot" = "#ff7f0e")) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
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
  
  ## ---- Aktive Wochentage ----
  output$bids_by_weekday_plot <- renderPlot({
    df <- avg_bids_wd()
    
    ggplot(df, aes(x = Wochentag, y = avg_bids)) +
      geom_col(fill = "darkcyan") +
      facet_wrap(~ Bieter, ncol = 4, scales = "free_y") +
      labs(
        title = "Ø Gebote (vom Vortag) pro Manager und Wochentag",
        x     = "Wochentag",
        y     = "Ø Gebote"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        strip.text  = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title  = element_text(face = "bold")
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
  # -- Flip-Gewinn vorbereiten: alle Kauf→Verkauf-Zyklen je Spieler×Besitzer
  flip_data <- reactive({
    req(transfers)
    
    # --- Events für Käufe und Verkäufe ---
    einkaeufe <- transfers %>%
      filter(tolower(Hoechstbietender) != "computer") %>%
      transmute(Spieler,
                Besitzer = Hoechstbietender,
                Datum,
                Preis = Hoechstgebot,
                type = "buy")
    
    verkaeufe <- transfers %>%
      filter(!is.na(Besitzer), tolower(Besitzer) != tolower(Hoechstbietender)) %>%
      transmute(Spieler,
                Besitzer,
                Datum,
                Preis = Hoechstgebot,
                type = "sell")
    
    # --- gemeinsamer Event-Stream ---
    events <- bind_rows(einkaeufe, verkaeufe) %>%
      arrange(Spieler, Besitzer, Datum,
              factor(type, levels = c("buy","sell"))) %>% # bei gleichem Tag: buy vor sell
      group_by(Spieler, Besitzer) %>%
      mutate(Zyklus = cumsum(type == "buy")) %>%
      ungroup()
    
    einkaeufe_seq <- events %>%
      filter(type == "buy") %>%
      transmute(Spieler, Besitzer,
                Einkaufsdatum = Datum,
                Einkaufspreis = Preis,
                Zyklus)
    
    verkaeufe_seq <- events %>%
      filter(type == "sell") %>%
      group_by(Spieler, Besitzer, Zyklus) %>%
      slice_min(Datum, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(Spieler, Besitzer,
                Verkaufsdatum = Datum,
                Verkaufspreis = Preis,
                Zyklus)
    
    matches <- einkaeufe_seq %>%
      inner_join(verkaeufe_seq,
                 by = c("Spieler","Besitzer","Zyklus")) %>%
      filter(Verkaufsdatum >= Einkaufsdatum) %>%
      transmute(Spieler, Besitzer,
                Einkaufsdatum, Einkaufspreis,
                Verkaufsdatum, Verkaufspreis,
                Gewinn = Verkaufspreis - Einkaufspreis)
    
    unmatched <- einkaeufe_seq %>%
      anti_join(matches %>% select(Spieler, Besitzer, Einkaufsdatum),
                by = c("Spieler","Besitzer","Einkaufsdatum")) %>%
      transmute(Spieler, Besitzer,
                Einkaufsdatum, Einkaufspreis,
                Verkaufsdatum = as.Date(NA),
                Verkaufspreis = NA_real_,
                Gewinn = NA_real_)
    
    bind_rows(matches, unmatched) %>%
      arrange(Spieler, Besitzer, Einkaufsdatum)
  })
  
  ## ---- Flip-Gesamtsumme ----
  ## -- Flip-Gewinne pro Spieler (Beeswarm & Boxplot)
  output$flip_summarybar <- renderPlot({
    req(nrow(flip_data()) > 0)
    
    df <- flip_data() %>%
      group_by(Besitzer) %>%
      summarise(Gesamtgewinn = sum(Gewinn, na.rm = TRUE), .groups = "drop")
    
    abs_max <- max(df$Gesamtgewinn, na.rm = TRUE)
    abs_min <- min(df$Gesamtgewinn, na.rm = TRUE)
    lim_min <-  abs_min - 2e6
    lim_max <-  abs_max + 2e6
    
    ggplot(df, aes(x = reorder(Besitzer, Gesamtgewinn), y = Gesamtgewinn, fill = Gesamtgewinn > 0)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(
          label = format(round(Gesamtgewinn, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE),
          hjust = ifelse(Gesamtgewinn > 0, -0.1, 1.1)
        ),
        position = position_dodge(width = 1),
        size = 8
      ) +
      scale_fill_manual(values = c("TRUE" = "#66cdaa", "FALSE" = "#ff6f61")) +
      coord_flip(ylim = c(lim_min, lim_max)) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 16, face = "bold", color = "black"),
        axis.text.x = element_text(size = 16),
        plot.title = element_text(size = 16)
      ) +
      labs(
        title = "Flip-Gewinn/Verlust je Comunio-Spieler (gesamt)",
        x = "",
        y = ""
      )
  })
  
  ## ---- Flip-Verlauf kumuliert über Zeit ----
  output$flip_cumulative <- renderPlot({
    req(nrow(flip_data()) > 0)
    
    flip_data() %>%
      arrange(Besitzer, Verkaufsdatum) %>%
      group_by(Besitzer) %>%
      mutate(Kumuliert = cumsum(Gewinn)) %>%
      ungroup() %>%
      ggplot(aes(x = Verkaufsdatum, y = Kumuliert, color = Besitzer)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      geom_point(size = 2, alpha = 0.7, na.rm = TRUE) +
      scale_color_brewer(palette = "Paired") +
      labs(
        title = "Kumulierte Flip-Gewinne über die Zeit je Spieler",
        x = "",
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
          is.na(Gewinn) ~ "Kein Verkauf",
          abs(Gewinn) < 2.5e4 ~ "Micro-Flip <25k",
          abs(Gewinn) < 1e5   ~ "Mini-Flip 25–99k",
          abs(Gewinn) < 5e5 ~ "Klein-Flip 100–499k",
          abs(Gewinn) < 1e6   ~ "Mittel-Flip 500–999k",
          TRUE                ~ "Mega-Flip ≥1mio"
        ),
        Flip_Ergebnis = case_when(
          is.na(Gewinn) ~ "Noch offen",
          Gewinn >= 0   ~ "Gewinn",
          TRUE          ~ "Verlust"
        ),
        Flip_Label = paste(Flip_Ergebnis, Flip_Kategorie, sep = " - ")
      ) %>%
      mutate(
        Flip_Label = factor(Flip_Label, levels = c(
          "Gewinn - Micro-Flip <25k",
          "Gewinn - Mini-Flip 25–99k",
          "Gewinn - Klein-Flip 100–499k",
          "Gewinn - Mittel-Flip 500–999k",
          "Gewinn - Mega-Flip ≥1mio",
          "Verlust - Micro-Flip <25k",
          "Verlust - Mini-Flip 25–99k",
          "Verlust - Klein-Flip 100–499k",
          "Verlust - Mittel-Flip 500–999k",
          "Verlust - Mega-Flip ≥1mio",
          "Noch offen - Kein Verkauf"
        ))
      ) %>%
      count(Besitzer, Flip_Label) %>%
      ggplot(aes(x = Besitzer, y = n, fill = Flip_Label)) +
      geom_col(position = "stack") +
      scale_fill_manual(
        values = c(
          "Gewinn - Micro-Flip <25k"      = "#a5d6a7",
          "Gewinn - Mini-Flip 25–99k"     = "#66bb6a",
          "Gewinn - Klein-Flip 100–499k"  = "#388e3c",
          "Gewinn - Mittel-Flip 500–999k" = "#1b5e20",
          "Gewinn - Mega-Flip ≥1mio"      = "#004d40",
          "Verlust - Micro-Flip <25k"      = "#ffcdd2",
          "Verlust - Mini-Flip 25–99k"     = "#ef9a9a",
          "Verlust - Klein-Flip 100–499k"  = "#e57373",
          "Verlust - Mittel-Flip 500–999k" = "#d32f2f",
          "Verlust - Mega-Flip ≥1mio"      = "#b71c1c",
          "Noch offen - Kein Verkauf"     = "#b0bec5"
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
  
  ## ---- Flip kumuliert je Flip-Art ----
  output$flip_cumcat <- renderPlot({
    req(nrow(flip_data()) > 0)
    flip_data() %>%
      mutate(
        Flip_Kategorie = case_when(
          abs(Gewinn) < 2.5e4 ~ "Micro-Flip <25k",
          abs(Gewinn) < 1e5 ~ "Mini-Flip 25–99k",
          abs(Gewinn) < 5e5 ~ "Klein-Flip 100–499k",
          abs(Gewinn) < 1e6 ~ "Mittel-Flip 500–999k",
          abs(Gewinn) >= 1e6 ~ "Mega-Flip ≥1mio"
        ),
        Flip_Ergebnis = ifelse(Gewinn >= 0, "Gewinn", "Verlust"),
        Flip_Label = paste(Flip_Ergebnis, Flip_Kategorie, sep = " - ")
      ) %>%
      mutate(
        Flip_Label = factor(Flip_Label, levels = c(
          "Gewinn - Micro-Flip <25k",
          "Gewinn - Mini-Flip 25–99k",
          "Gewinn - Klein-Flip 100–499k",
          "Gewinn - Mittel-Flip 500–999k",
          "Gewinn - Mega-Flip ≥1mio",
          "Verlust - Micro-Flip <25k",
          "Verlust - Mini-Flip 25–99k",
          "Verlust - Klein-Flip 100–499k",
          "Verlust - Mittel-Flip 500–999k",
          "Verlust - Mega-Flip ≥1mio"
        ))
      ) %>%
      group_by(Besitzer, Flip_Label) %>%
      summarise(Summe = sum(Gewinn), .groups = "drop") %>%
      ggplot(aes(x = Besitzer, y = Summe, fill = Flip_Label)) +
      geom_col(position = "stack", na.rm = TRUE) +
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
          "Gewinn - Klein-Flip 100–499k"  = "#388e3c",
          "Gewinn - Mittel-Flip 500–999k" = "#1b5e20",
          "Gewinn - Mega-Flip ≥1mio"      = "#004d40",
          "Verlust - Micro-Flip <25k"      = "#ffcdd2",
          "Verlust - Mini-Flip 25–99k"     = "#ef9a9a",
          "Verlust - Klein-Flip 100–499k"  = "#e57373",
          "Verlust - Mittel-Flip 500–999k" = "#d32f2f",
          "Verlust - Mega-Flip ≥1mio"      = "#b71c1c"
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
        `Ø Flip-Einnahme pro Spieler` = case_when(
          is.na(avg_flip)            ~ "",
          avg_flip > 0              ~ paste0("+", format(round(avg_flip), big.mark = ".", decimal.mark = ","), " €"),
          avg_flip < 0              ~ paste0("-", format(abs(round(avg_flip)), big.mark = ".", decimal.mark = ","), " €"),
          TRUE                      ~ "±0 €"
        ),
        vortag_diff = sum_diff - sum_diff_yesterday,
        `Vortag-Diff` = case_when(
          is.na(vortag_diff)        ~ "",
          vortag_diff > 0           ~ sprintf("<span style='color:#388e3c;'>+%s €</span>", format(vortag_diff, big.mark = ".", decimal.mark = ",")),
          vortag_diff < 0           ~ sprintf("<span style='color:#e53935;'>-%s €</span>", format(abs(vortag_diff), big.mark = ".", decimal.mark = ",")),
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
        `Vortag-Diff (% Teamwert)` = case_when(
          is.na(vortag_diff_rel)        ~ "",
          vortag_diff_rel > 0           ~ sprintf("<span style='color:#388e3c;'>+%.2f %%</span>", vortag_diff_rel),
          vortag_diff_rel < 0           ~ sprintf("<span style='color:#e53935;'>-%.2f %%</span>", abs(vortag_diff_rel)),
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
        ordering = TRUE,
        order = list(list(3, 'desc')),  # Index der Spalte von 0 an gezählt
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(className = 'dt-right', targets = c(1,2,3,4))
        )
      ),
      escape = FALSE, selection = "single"
    )
    
  })
  
  ## ---- Zeitbalken-Flip ----
  output$kaufdiff_zeitlinien <- renderPlotly({
    req(teams_df, gesamt_mw_roh, transfers)
    
    manager_list <- sort(unique(teams_df$Manager))
    
    # Marktwert aktuell je Spieler
    mw_aktuell <- gesamt_mw_roh %>%
      mutate(
        Datum = trimws(as.character(Datum)),
        Datum = na_if(Datum, ""),
        Datum = coalesce(
          ymd(Datum, quiet = TRUE),
          dmy(Datum, quiet = TRUE),
          suppressWarnings(as.Date(Datum))
        ),
        Marktwert = as.numeric(Marktwert)
      ) %>%
      group_by(Spieler) %>%
      filter(Datum == max(Datum, na.rm = TRUE)) %>%
      summarise(Marktwert_aktuell = first(Marktwert), .groups = "drop")
    
    
    # letzter Kauf je (Spieler, Manager)
    kaufpreise <- transfers %>%
      mutate(
        Datum = trimws(as.character(Datum)),
        Datum = na_if(Datum, ""),
        Datum = coalesce(
          dmy(Datum, quiet = TRUE),
          ymd(Datum, quiet = TRUE),
          suppressWarnings(as.Date(Datum))
        ),
        Hoechstgebot = as.numeric(gsub("\\.", "", as.character(Hoechstgebot)))
      ) %>%
      group_by(Spieler, Hoechstbietender) %>%
      slice_max(order_by = Datum, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        Spieler,
        Manager   = Hoechstbietender,
        Kaufdatum = as.Date(Datum),
        Kaufpreis = Hoechstgebot
      )
    
    
    # Plot-Daten
    plot_df <- teams_df %>%
      select(Manager, Spieler) %>%
      left_join(mw_aktuell, by = "Spieler") %>%
      left_join(kaufpreise, by = c("Manager","Spieler")) %>%
      mutate(
        Diff_Kauf = ifelse(is.na(Kaufpreis), NA_real_, Marktwert_aktuell - Kaufpreis),
        pos       = Diff_Kauf >= 0
      ) %>%
      filter(!is.na(Kaufpreis), !is.na(Diff_Kauf), !is.na(Kaufdatum)) %>%
      mutate(Manager = factor(Manager, levels = manager_list))
    
    req(nrow(plot_df) > 0)
    
    # Achsenbegrenzung +30 Tage
    x_min <- min(plot_df$Kaufdatum, na.rm = TRUE)
    x_max <- max(plot_df$Kaufdatum, na.rm = TRUE) + days(30)
    
    # Tooltip
    fmt_eur <- function(x) format(round(x, 0), big.mark = ".", decimal.mark = ",")
    plot_df <- plot_df %>%
      mutate(
        Kaufdatum_fmt = format(Kaufdatum, "%d.%m.%Y"),
        Diff_lbl      = ifelse(Diff_Kauf >= 0,
                               paste0("+", fmt_eur(Diff_Kauf), " €"),
                               paste0("-", fmt_eur(abs(Diff_Kauf)), " €")),
        tooltip = paste0(
          "Spieler: ", Spieler,
          "<br>Manager: ", Manager,
          "<br>Kaufdatum: ", Kaufdatum_fmt,
          "<br>Kaufpreis: ", fmt_eur(Kaufpreis), " €",
          "<br>MW aktuell: ", fmt_eur(Marktwert_aktuell), " €",
          "<br>Diff: ", Diff_lbl
        )
      )
    
    p <- ggplot(plot_df, aes(x = Kaufdatum, y = Diff_Kauf, group = Spieler, fill = pos)) +
      geom_col(aes(text = tooltip),
               width = 12,
               position = position_dodge2(width = 12, preserve = "single"),
               show.legend = FALSE) +
      geom_hline(yintercept = 0, linewidth = 0.5, color = "grey60", linetype = "dashed") +
      geom_point(aes(y = 0, text = tooltip),
                 size = 1.8, alpha = 0.9,
                 position = position_dodge2(width = 12, preserve = "single")) +
      scale_fill_manual(values = c(`TRUE` = "#388e3c", `FALSE` = "#e53935"), guide = "none") +
      facet_grid(Manager ~ ., scales = "fixed", switch = "y") +
      scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        expand = expansion(mult = c(0.01, 0.01))
      ) +
    
      scale_x_date(limits = c(x_min, x_max),
                   date_breaks = "1 week", date_labels = "%d.%m.",
                   expand = expansion(mult = c(0, 0))) +
      labs(x = "", y = "") +
      theme_minimal(base_size = 16) +
      theme(
        strip.placement   = "outside",
        strip.text.y.left = element_text(face = "bold", size = 16),
        panel.grid.minor  = element_blank(),
        panel.grid.major  = element_blank()
      )
    
    p <- p +
      theme(
        panel.spacing.y     = unit(0.8, "lines"),
        panel.border        = element_rect(colour = "#c7c7c7", fill = NA, linewidth = 0.6),
        strip.background.y  = element_rect(fill = "#f2f2f2", colour = "#c7c7c7")
      )
    
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE,
             hoverlabel = list(align = "left"))
    
  })
  
  ## ---- Flip-Historie: MW-events ----
  output$manager_select_ui <- renderUI({
    # Besitzer ohne "Computer"
    mgrs <- setdiff(unique(transfers$Besitzer), "Computer")
    selectInput(
      inputId  = "manager_select",
      label    = "Manager auswählen:",
      choices  = sort(mgrs),
      selected = sort(mgrs)[5]
    )
  })
  
  output$mw_events <- renderPlotly({
    req(input$manager_select, gesamt_mw_df)
    
    # MW absolut + normiert
    clean_df <- gesamt_mw_df %>%
      filter(!is.na(Datum)) %>%
      transmute(Datum = as.Date(Datum),
                       MW_gesamt = as.numeric(MW_gesamt)) %>%
      arrange(Datum)
    validate(need(nrow(clean_df) > 1, "Keine MW-Zeitreihe."))
    base_mw <- clean_df$MW_gesamt[1]
    clean_df$MW_norm <- clean_df$MW_gesamt / base_mw
    
    # Flip-Daten (nur Verkäufe des gewählten Managers)
    fd <- flip_data(); validate(need(!is.null(fd), "Keine Flip-Daten."))
    sells <- fd %>%
      filter(Besitzer == input$manager_select, !is.na(Verkaufsdatum)) %>%
      mutate(
        Verkaufsdatum = as.Date(Verkaufsdatum),
        Einkaufsdatum = as.Date(Einkaufsdatum),
        Gewinn        = as.numeric(Gewinn),
        Einkaufspreis = as.numeric(Einkaufspreis),
        Verkaufspreis = as.numeric(Verkaufspreis)
      )
    validate(need(nrow(sells) > 0, "Keine passenden Verkäufe gefunden."))
    
    # Farben
    sells$col <- ifelse(sells$Gewinn >= 0, "darkgreen", "red")
    
    # Tooltip-Text (wird NICHT als Label gezeichnet)
    fmt_eur <- function(x) format(round(x,0), big.mark=".", decimal.mark=",", trim=TRUE)
    sells$hover_text <- sprintf(
      "%s<br>Verkauf: %s · Gewinn: %s €<br>Kauf: %s · %s €<br>Verkaufspreis: %s €",
      sells$Spieler,
      format(sells$Verkaufsdatum, "%d.%m.%Y"),
      fmt_eur(sells$Gewinn),
      format(sells$Einkaufsdatum, "%d.%m.%Y"),
      fmt_eur(sells$Einkaufspreis),
      fmt_eur(sells$Verkaufspreis)
    )
    
    p <- plot_ly()
    
    # Linie: MW normiert (linke Achse)
    p <- p %>% add_lines(
      data = clean_df, x = ~Datum, y = ~MW_norm, name = "MW normiert",
      hovertemplate = "%{x|%d.%m.%Y}<br>MW normiert: %{y:.3f}<extra></extra>",
      yaxis = "y"
    )
    
    ## Aufteilen nach Vorzeichen
    sells_pos <- sells %>% filter(Gewinn > 0)
    sells_neg <- sells %>% filter(Gewinn < 0)
    
    # POSITIV
    p <- p %>% add_bars(
      data = sells_pos,
      x = ~Verkaufsdatum, y = ~Gewinn, yaxis = "y2",
      split = ~Spieler,
      name = "Gewinn",
      marker = list(color = "rgba(0,128,0,0.95)",
                    line = list(width = 1, color = "black")),
      hovertext = ~hover_text,    # <-- statt text/hovertemplate
      hoverinfo = "text",
      showlegend = FALSE
    )
    
    # NEGATIV
    p <- p %>% add_bars(
      data = sells_neg,
      x = ~Verkaufsdatum, y = ~Gewinn, yaxis = "y2",
      split = ~Spieler,
      name = "Verlust",
      marker = list(color = "rgba(200,0,0,0.95)",
                    line = list(width = 1, color = "black")),
      hovertext = ~hover_text,    # <-- statt text/hovertemplate
      hoverinfo = "text",
      showlegend = FALSE
    )
    
    
    # Layout: relative statt stack
    p <- p %>% layout(
      barmode = "relative",
      bargap  = 0.35,
      xaxis = list(title = "", type = "date"),
      yaxis  = list(title = "MW normiert (Start = 1)", rangemode = "tozero"),
      yaxis2 = list(title = "Gewinn (€)", overlaying = "y", side = "right",
                    tickformat = ",.0f", zeroline = TRUE),
      showlegend = FALSE,
      margin = list(l = 60, r = 70, t = 40, b = 40)
    )
    
    p
  })
  
  ## ---- Flip-Historie je Spieler ----
  output$flip_player_table <- renderDT({
    req(input$flip_player_select)
    
    mw_today <- ap_df %>%
      filter(Datum == today) %>%
      select(Spieler, Marktwert)
    
    # Daten
    df_all <- flip_data()                    # ungefiltert für stabile Farb-Map
    df <- df_all
    show_owner <- input$flip_player_select == "Alle"
    if (!show_owner) df <- df %>% filter(Besitzer == input$flip_player_select)
    
    df <- df %>%
      left_join(mw_today, by = "Spieler")
    
    # Stabile Farbzuordnung für alle Manager
    cols_base <- c(
      "#A6CEE3","#1F78B4","#B2DF8A","#33A02C",
      "#FB9A99","#E31A1C","#FDBF6F","#FF7F00",
      "#CAB2D6","#6A3D9A","#FFFF99","#B15928"
    )
    owners_all <- sort(unique(df_all$Besitzer))
    cols_vec   <- rep_len(cols_base, length(owners_all))
    col_map    <- setNames(adjustcolor(cols_vec, alpha.f = 0.05), owners_all)
    
    if (show_owner) {
      dt <- df %>%
        select(Besitzer, Verkaufsdatum, Spieler, Einkaufsdatum, Einkaufspreis, Verkaufspreis, Marktwert, Gewinn) %>%
        arrange(desc(Verkaufsdatum)) %>%
        datatable(
          options = list(dom = "ft", paging = TRUE, pageLength = 20),
          colnames = c("Besitzer", "Verkaufsdatum", "Spieler", "Kaufdatum", "Einkaufspreis", "Verkaufspreis", "Aktueller Marktwert", "Gewinn/Verlust (€)")
        ) %>%
        formatStyle(
          "Besitzer",
          target = "row",
          backgroundColor = styleEqual(names(col_map), unname(col_map))
        ) %>%
        formatStyle("Gewinn", color = styleInterval(0, c("#C62828", "#2E7D32"))) %>%  # <0 rot, >=0 grün
        formatStyle("Gewinn", color = styleEqual(0, "#666666"))                       # 0 grau
    } else {
      dt <- df %>%
        select(Verkaufsdatum, Spieler, Einkaufsdatum, Einkaufspreis, Verkaufspreis, Marktwert, Gewinn) %>%
        arrange(desc(Verkaufsdatum)) %>%
        datatable(
          options = list(dom = "t", paging = FALSE),
          colnames = c("Verkaufsdatum", "Spieler", "Kaufdatum", "Einkaufspreis", "Verkaufspreis", "Aktueller Marktwert", "Gewinn/Verlust (€)")
        ) %>%
        formatStyle("Gewinn", color = styleInterval(0, c("#C62828", "#2E7D32"))) %>%
        formatStyle("Gewinn", color = styleEqual(0, "#666666"))
      
    }
    
    dt
  })
  
  # ---- PERFORMANCE ----
  ## ---- Platzierungen je Spieler ----
  output$patzierungen_table <- renderPlot({
    ymax <- max(patzierungen_df$Anzahl, na.rm = TRUE) + 1
    
    ggplot(patzierungen_df, aes(x = Platzierung, y = Anzahl, fill = Manager)) +
      geom_col() +
      facet_wrap(~ Manager, ncol = 2, scales = "free_y") +
      scale_x_continuous(breaks = 1:8) +
      scale_fill_brewer(palette = "Paired", drop = FALSE) +
      scale_y_continuous(
        limits = c(0, ymax),
        breaks = seq(0, ymax, 1),
        expand = expansion(mult = c(0, 0))
      ) +
      guides(fill = "none") +
      theme_minimal(base_size = 18) +
      theme(
        panel.spacing.y    = unit(1, "lines"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text         = element_text(size = 20, face = "bold")
      )
  })
  
  ## ---- Podium ----
  # feste Manager-Reihenfolge für konsistente Farben
  manager_levels <- patzierungen_df %>%
    distinct(Manager) %>%
    arrange(Manager) %>%
    pull(Manager)
  
  podium_df <- patzierungen_df %>%
    filter(Platzierung %in% 1:3) %>%
    mutate(
      Manager = factor(Manager, levels = manager_levels),
      Platzierung = factor(Platzierung, levels = c(3, 2, 1))  # 1 ganz oben
    ) %>%
    complete(Platzierung, Manager, fill = list(Anzahl = 0)) %>%
    arrange(Platzierung, Manager)
  
  output$podium_table <- renderPlot({
    ggplot(podium_df, aes(x = Anzahl, y = Platzierung, fill = Manager)) +
      geom_col(position = "stack") +
      scale_x_continuous(breaks = function(x) seq(0, ceiling(max(x)), 1)) +
      scale_fill_brewer(palette = "Paired", drop = FALSE) +
      labs(x = "", y = "Platz", fill = "Manager") +
      theme_minimal(base_size = 18) +
      theme(
        axis.text.y = element_text(face = "bold", size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
  
  # ---- KAPITALÜBERSICHT ----
  ## ---- Kontostände je Spieler ----
  
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
  
  ### ---- kapital_df_reactive ----
  kapital_df_reactive <- reactive({
    req(transfers)
    
    alle_manager <- names(startkapital_fix)
    
    ausgaben <- transfers %>%
      group_by(Hoechstbietender) %>%
      summarise(Ausgaben = sum(Hoechstgebot, na.rm = TRUE)) %>%
      rename(Manager = Hoechstbietender)
    
    einnahmen <- transfers %>%
      group_by(Besitzer) %>%
      summarise(Einnahmen = sum(Hoechstgebot, na.rm = TRUE)) %>%
      rename(Manager = Besitzer)
    
    # Teamwert-Vergleich
    max_datum <- max(st_df$Datum, na.rm = TRUE)
    prev_datum <- max(st_df$Datum[st_df$Datum < max_datum], na.rm = TRUE)
    
    mw_today <- st_df %>% filter(Datum == max_datum) %>% select(Manager, Teamwert_today = Teamwert)
    mw_prev  <- st_df %>% filter(Datum == prev_datum) %>% select(Manager, Teamwert_prev = Teamwert)
    
    mw_diff_df <- mw_today %>%
      left_join(mw_prev, by = "Manager") %>%
      mutate(
        Teamwert_Entwicklung = Teamwert_today - Teamwert_prev,
        Entwicklung_Trend = case_when(
          is.na(Teamwert_Entwicklung) ~ "-",
          Teamwert_Entwicklung > 0 ~ paste0("▲ ", format(Teamwert_Entwicklung, big.mark = ".", decimal.mark = ","), " €"),
          Teamwert_Entwicklung < 0 ~ paste0("▼ ", format(abs(Teamwert_Entwicklung), big.mark = ".", decimal.mark = ","), " €"),
          TRUE ~ "0 €"
        )
      ) %>%
      select(Manager, Entwicklung_Trend)
    
    # NEU: Punkte-Bonus je Spieltag nur einmal zählen
    punkte_bonus_df <- st_df %>%
      group_by(Manager) %>%
      arrange(Datum, .by_group = TRUE) %>%
      mutate(
        gp_prev  = lag(Gesamtpunkte, default = 0),
        counted  = (`letzte Punkte` >= 0) & (Gesamtpunkte - gp_prev == `letzte Punkte`)
      ) %>%
      filter(counted) %>%
      summarise(Punkte_Bonus = sum(`letzte Punkte`, na.rm = TRUE) * 10000, .groups = "drop")
    
    df <- data.frame(Manager = alle_manager, stringsAsFactors = FALSE) %>%
      left_join(mw_today, by = "Manager") %>%
      mutate(
        Teamwert = ifelse(is.na(Teamwert_today), 0, Teamwert_today),
        Startkapital = startkapital_fix[Manager],
        Kreditrahmen = round(Teamwert / 4),
        Ausgaben = ausgaben$Ausgaben[match(Manager, ausgaben$Manager)],
        Einnahmen = einnahmen$Einnahmen[match(Manager, einnahmen$Manager)],
        Transaction_Summe = transactions$Transaction_Summe[match(Manager, transactions$Manager)],
        Punkte_Bonus = punkte_bonus_df$Punkte_Bonus[match(Manager, punkte_bonus_df$Manager)],
        Ausgaben = ifelse(is.na(Ausgaben), 0, Ausgaben),
        Einnahmen = ifelse(is.na(Einnahmen), 0, Einnahmen),
        Transaction_Summe = ifelse(is.na(Transaction_Summe), 0, Transaction_Summe),
        Punkte_Bonus = ifelse(is.na(Punkte_Bonus), 0, Punkte_Bonus),
        Aktuelles_Kapital = Startkapital + Einnahmen - Ausgaben + Transaction_Summe + Punkte_Bonus,
        Verfügbares_Kapital = Aktuelles_Kapital + Kreditrahmen
      ) %>%
      left_join(mw_diff_df, by = "Manager") %>%
      select(Manager, Startkapital, Teamwert, Entwicklung_Trend, Kreditrahmen,
             Ausgaben, Einnahmen, Transaction_Summe, Punkte_Bonus, Aktuelles_Kapital, Verfügbares_Kapital)
    
    df
  })
  
  ### ---- kapital_ts_reactive ----
  kapital_ts_reactive <- reactive({
    req(transfers)
    alle_manager <- names(startkapital_fix)
    
    stopifnot(all(c("Datum","Manager","Gesamtpunkte","letzte Punkte") %in% names(st_df)))
    st_df <- st_df %>% mutate(Datum = as.Date(Datum))
    dates <- sort(unique(st_df$Datum))
    req(length(dates) > 0)
    d0 <- min(dates)
    
    base <- expand.grid(Datum = dates, Manager = alle_manager, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    
    # Transfers als Cash-Deltas
    xfers_buy <- transfers %>%
      transmute(Datum = as.Date(Datum), Manager = Hoechstbietender, delta = -as.numeric(Hoechstgebot)) %>%
      filter(!is.na(Datum), !is.na(Manager))
    xfers_sell <- transfers %>%
      transmute(Datum = as.Date(Datum), Manager = Besitzer,       delta =  as.numeric(Hoechstgebot)) %>%
      filter(!is.na(Datum), !is.na(Manager))
    xfers <- bind_rows(xfers_buy, xfers_sell)
    
    # optionale transactions
    tx_raw <-
      if (exists("transactions") && is.data.frame(transactions) && all(c("Manager","Transaction_Summe") %in% names(transactions))) {
        if ("Datum" %in% names(transactions)) {
          transactions %>% mutate(Datum = as.Date(Datum)) %>% transmute(Manager, Datum, delta_tx = as.numeric(Transaction_Summe))
        } else {
          transactions %>% mutate(Datum = d0) %>% transmute(Manager, Datum, delta_tx = as.numeric(Transaction_Summe))
        }
      } else {
        tibble(Manager = character(), Datum = as.Date(character()), delta_tx = numeric())
      }
    
    # Punkte-Bonus pro Tag (nur counted)
    punkte_tag <- st_df %>%
      group_by(Manager) %>%
      arrange(Datum, .by_group = TRUE) %>%
      mutate(gp_prev = lag(Gesamtpunkte, default = 0),
             counted = (`letzte Punkte` >= 0) & (Gesamtpunkte - gp_prev == `letzte Punkte`),
             bonus = if_else(counted, as.numeric(`letzte Punkte`) * 10000, 0)) %>%
      ungroup() %>%
      group_by(Manager, Datum) %>%
      summarise(bonus = sum(bonus, na.rm = TRUE), .groups = "drop")
    
    # Offsets vor d0
    pre_xfers <- xfers %>%
      filter(Datum < d0) %>%
      group_by(Manager) %>%
      summarise(pre_delta = sum(delta, na.rm = TRUE), .groups = "drop")
    
    pre_tx <- tx_raw %>%
      filter(Datum < d0) %>%
      group_by(Manager) %>%
      summarise(pre_tx = sum(delta_tx, na.rm = TRUE), .groups = "drop")
    
    pre_bonus <- punkte_tag %>%
      filter(Datum < d0) %>%
      group_by(Manager) %>%
      summarise(pre_bonus = sum(bonus, na.rm = TRUE), .groups = "drop")
    
    offsets <- tibble(Manager = alle_manager) %>%
      left_join(pre_xfers,  by = "Manager") %>%
      left_join(pre_tx,     by = "Manager") %>%
      left_join(pre_bonus,  by = "Manager") %>%
      mutate(across(c(pre_delta, pre_tx, pre_bonus), ~replace_na(., 0)),
             Startkapital = startkapital_fix[Manager],
             offset0 = Startkapital + pre_delta + pre_tx + pre_bonus) %>%
      select(Manager, offset0)
    
    # Deltas ab d0
    xfers_d <- xfers %>%
      filter(Datum >= d0) %>%
      group_by(Manager, Datum) %>%
      summarise(delta = sum(delta, na.rm = TRUE), .groups = "drop")
    
    tx_d <- tx_raw %>%
      filter(Datum >= d0) %>%
      group_by(Manager, Datum) %>%
      summarise(delta_tx = sum(delta_tx, na.rm = TRUE), .groups = "drop")
    
    bonus_d <- punkte_tag %>%
      filter(Datum >= d0)
    
    ts <- base %>%
      left_join(xfers_d, by = c("Manager","Datum")) %>%
      left_join(tx_d,     by = c("Manager","Datum")) %>%
      left_join(bonus_d,  by = c("Manager","Datum")) %>%
      mutate(delta = replace_na(delta, 0),
             delta_tx = replace_na(delta_tx, 0),
             bonus = replace_na(bonus, 0)) %>%
      left_join(offsets,  by = "Manager") %>%
      arrange(Manager, Datum) %>%
      group_by(Manager) %>%
      mutate(kum = cumsum(delta + delta_tx + bonus),
             Kontostand = offset0 + kum) %>%
      ungroup()
    
    # NEU: nur ab 22.08.2025
    ts %>% filter(Datum >= as.Date("2025-08-22"))
  })
  
  ## ---- Haupt-Tabelle ----
  output$kapital_uebersicht_table <- renderDT({
    kapital_df <- kapital_df_reactive()
    rownames(kapital_df) <- NULL
    
    datatable(
      kapital_df,
      rownames = FALSE,
      colnames = c(
        "Manager",
        "Startkapital (€)",
        "Teamwert (€)",
        "Entwicklung zum Vortag",
        "Kreditrahmen (¼ Kaderwert) (€)",
        "Transfer-Ausgaben (€)",
        "Transfer-Einnahmen (€)",
        "Disziplinar-/Bonus-Transaktionen (€)",
        "Punkte-Bonus (€)",
        "Aktuelles Kapital (€)",
        "Verfügbares Kapital (€)"
      ),
      options = list(
        dom = 't',
        paging = FALSE,  
        autoWidth = TRUE,
        order = list(list(which(colnames(kapital_df) == "Verfügbares_Kapital") - 1, 'desc'))
      )
    ) %>%
      formatCurrency(
        columns = c("Startkapital","Teamwert","Kreditrahmen","Ausgaben","Einnahmen","Transaction_Summe", "Punkte_Bonus","Aktuelles_Kapital","Verfügbares_Kapital"),
        currency = "", interval = 3, mark = ".", digits = 0, dec.mark = ","
      ) %>%
      formatStyle(
        'Aktuelles_Kapital',
        color      = styleInterval(0, c('red','black')),
        fontWeight = styleInterval(0, c('bold', NA))
      ) -> dt  # Pipe-Ergebnis zwischenspeichern
    
    # automatische Farb-Mappings für Entwicklung_Trend
    trends <- sort(unique(kapital_df$Entwicklung_Trend))
    colors <- sapply(trends, function(x) {
      if      (grepl("^▲", x)) "green"
      else if (grepl("^▼", x)) "red"
      else                      "black"
    })
    
    dt %>%
      formatStyle(
        'Entwicklung_Trend',
        color      = styleEqual(trends, colors),
        fontWeight = "bold"
      )
  })
  
  ## ---- Absolute Gewinne ----
  ### ---- Flip-Saldo je Manager (Gewinne + Verluste) ----
  flip_gewinne_df <- reactive({
    req(flip_data())
    flip_data() %>%
      mutate(Gewinn = as.numeric(Gewinn)) %>%
      filter(!is.na(Gewinn)) %>%
      group_by(Besitzer) %>%
      summarise(Flip_Saldo = sum(Gewinn, na.rm = TRUE), .groups = "drop") %>%
      rename(Manager = Besitzer)
  })
  
  ### ---- Gesamtsumme pro Manager: Flip + Transaktionen + Punkte-Bonus ----
  kapital_summe_df <- reactive({
    kdf <- kapital_df_reactive() %>%
      select(Manager, Transaction_Summe, Punkte_Bonus)
    
    flips <- flip_gewinne_df()
    
    kdf %>%
      left_join(flips, by = "Manager") %>%
      mutate(
        Transaction_Summe = coalesce(Transaction_Summe, 0),
        Punkte_Bonus      = coalesce(Punkte_Bonus, 0),
        Flip_Saldo        = coalesce(Flip_Saldo, 0),
        Gesamt            = Flip_Saldo + Transaction_Summe + Punkte_Bonus
      )
  })
  
  ### ---- Gesamtsumme ----
  output$kapital_gewinn_plot <- renderPlot({
    df <- kapital_summe_df()
    req(nrow(df) > 0)
    
    # Sortierung
    df <- df %>%
      arrange(desc(Gesamt)) %>%
      mutate(Manager_x   = factor(Manager, levels = Manager),
             Manager_fill= factor(Manager, levels = sort(unique(Manager))))
    
    # Achsenlimits berechnen
    y_min <- min(df$Gesamt, na.rm = TRUE) - 3e6
    y_max <- max(df$Gesamt, na.rm = TRUE) + 3e6
    
    ggplot(df, aes(x = Manager_x, y = Gesamt, fill = Manager_fill)) +
      geom_col() +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.7) +
      geom_text(
        aes(label = format(Gesamt, big.mark = ".", decimal.mark = ",")),
        vjust = ifelse(df$Gesamt >= 0, -0.5, 1.3),
        size  = 5
      ) +
      labs(x = NULL, y = "€", title = "Gesamtgewinne je Manager") +
      scale_y_continuous(
        limits = c(y_min, y_max),
        labels = function(x) format(x, big.mark = ".", decimal.mark = ",")
      ) +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal(base_size = 16) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "none"
      )
  })
  
  ## ---- Über die Zeit ----
  output$kapital_verlauf_plot <- renderPlot({
    ts <- kapital_ts_reactive()
    req(nrow(ts) > 0)
    
    ggplot(ts, aes(x = Datum, y = Kontostand, group = Manager)) +
      geom_col(aes(fill = Kontostand > 0), width = 0.5, alpha = 0.5) + 
      geom_line(aes(color = Manager), linewidth = 1.5) +              
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", linewidth = 0.4) +
      facet_wrap(~ Manager, scales = "free_y") +
      scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "palegreen1"), guide = "none") +
      scale_color_brewer(palette = "Paired") +
      labs(x = NULL, y = "Kontostand (€)", color = NULL) +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank()
      )
  })
  
  ## ---- Zeit im Minus ----
  output$kapital_minus_table <- DT::renderDT({
    ts <- kapital_ts_reactive()
    req(nrow(ts) > 0)
    
    stats <- ts %>%
      arrange(Manager, Datum) %>%
      group_by(Manager) %>%
      mutate(
        is_minus = Kontostand < 0,
        start_run = is_minus & (is.na(lag(is_minus)) | !lag(is_minus))
      ) %>%
      summarise(
        Tage_total     = n(),
        Tage_im_Minus  = sum(is_minus, na.rm = TRUE),
        Anteil_Minus   = if (Tage_total > 0) 100 * sum(is_minus, na.rm = TRUE) / Tage_total else 0,
        Episoden_Minus = sum(start_run, na.rm = TRUE),
        Tiefstes_Minus = if (any(is_minus, na.rm = TRUE)) min(Kontostand[is_minus], na.rm = TRUE) else 0,
        .groups = "drop"
      ) %>%
      mutate(Anteil_Minus = round(Anteil_Minus, 1)) %>%
      select(
        Manager,
        `Anteil Zeit im Minus (%)` = Anteil_Minus,
        `Tage im Minus` = Tage_im_Minus,
        `Episoden im Minus` = Episoden_Minus,
        `Tiefstes Minus (€)` = Tiefstes_Minus
      ) %>%
      as.data.frame()
    
    
    DT::datatable(
      stats,
      rownames = FALSE,
      options = list(paging = FALSE, autoWidth = TRUE, dom = 't')
    ) %>%
      formatCurrency(
        columns = c("Tiefstes Minus (€)"),
        currency = "", interval = 3, mark = ".", digits = 0, dec.mark = ","
      )
  })
}

# SHINY APP STARTEN
shinyApp(ui, server)

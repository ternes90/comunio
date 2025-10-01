library(shiny)
library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(readxl)
library(DT)
library(scales)
library(later)

# ---- UI ----
ui <- navbarPage(
  "Comunio Analyse", 
  id = "main_navbar",
  
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
             <span style='color:darkgrey; font-weight:bold;'>▍</span> Gesamtmarktwert (alle Spieler) &nbsp;&nbsp;
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
             tabPanel(title = "Käufe und Verkäufe", value = "mw_events_tab",
                      fluidRow(column(4, uiOutput("manager_select_ui"))),
                      br(),
                      plotOutput("mw_events", height = 600)
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
                      DTOutput("transfermarkt_detail")
             ),
             
             tabPanel(
               title = "Spieler-Info", value = "spieler_info",
               radioButtons(
                 "spieler_filter",
                 label = NULL,
                 choices = c("Transfermarkt", "Alle", "Mein Team"),
                 selected = "Transfermarkt",
                 inline = TRUE
               ),
               selectInput("spieler_select2", "Spieler:", choices = NULL),
               
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
                   plotOutput("spieler_info_mw", height = 350, width = "100%")),
               
               tags$div("Leistungsdaten",
                        style = "text-align:center;font-size:16px;font-weight:bold;color:black;margin-bottom:10px;"),
               div(style = "margin-bottom:20px;width:100%;",
                   DTOutput("spieler_info_raw", width = "100%")),
               
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
               uiOutput("mein_kader")
             ),
             tabPanel(
               "Alle Kader",
               uiOutput("kader_uebersicht_ui")
             ),
             tabPanel(
               "Kaderwert-Plot",
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
               
               div(style = "text-align:center; margin-bottom:15px; margin-top:20px;",
                   actionButton("toggle_plotly", "Interaktiv aktivieren")
               ),
               
               div(style = "width: 90%; margin: 0 auto;",
                   uiOutput("mwclass_container")
               )
               
             ),
             tabPanel("Zeit-Trend", plotOutput("trendplot", height = 700)),
             tabPanel("Gebots-Frequenz",
                      plotOutput("gebote_pro_tag", height = 350),
                      plotOutput("gebote_pro_tag_linie", height = 350)
             ),
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
                          column(12, plotOutput("kaufdiff_zeitlinien", height = "1500px"))
                        )
                      )
             ),
             
             # TAB 4: Historie
             tabPanel("Historie",
                      fluidPage(
                        fluidRow(
                          column(2,
                                 selectInput("flip_player_select", "Spieler auswählen:", choices = NULL)
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
             DTOutput("kapital_uebersicht_table"),
             div(
               style = "margin-bottom:15px; margin-top:40px;",
               plotOutput("kapital_gewinn_plot", height = "360px")
             )
           )
  )
  
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  #Save beeswarm position
  pos_qr <- ggbeeswarm::position_quasirandom(width = 0.22)
  #Update stempel
  output$last_update <- renderText({
    tryCatch(
      readLines("data/last_updated.txt", warn = FALSE)[1],
      error = function(e) "unbekannt"
    )
  })
  
  verfuegbares_kapital_dominik <- reactiveVal(0)
  kontostand_dominik           <- reactiveVal(0)
  tm_trend_global <- reactiveVal(NULL)
  # ---- Status für Plotly ----
  plotly_on <- reactiveVal(FALSE)
  
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
  # Server‑Logik zum Wechseln und Vorwählen für Transfer-Simulator
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
    
    # Filter zuerst setzen
    updateRadioButtons(
      session,
      inputId  = "spieler_filter",
      selected = "Transfermarkt"
    )
    
    # Mit kleinem Delay den Spieler wählen
    later(function() {
      updateSelectInput(
        session,
        inputId  = "spieler_select2",
        selected = player
      )
    }, delay = 0.2)   # 200 ms reicht meist
  })
  
  # Action Button 3 (Mein Team)
  observeEvent(input$mk_info_player, {
    req(input$mk_info_player$name)
    player <- input$mk_info_player$name
    
    updateNavbarPage(session, "main_navbar", selected = "Transfermarkt")
    updateTabsetPanel(session, "transfermarkt_tabs", selected = "spieler_info")
    updateRadioButtons(session, "spieler_filter", selected = "Mein Team")
    
    later(function() {
      updateSelectInput(session, "spieler_select2", selected = player)
    }, 0.2)
  })
  
  # Spielerlisten für Spiler-Info wählen
  observeEvent(input$spieler_filter, {
    sel <- input$spieler_filter
    
    if (sel == "Transfermarkt") {
      updateSelectInput(session, "spieler_select2",
                        choices = sort(unique(tm_df$Spieler))
      )
      
    } else if (sel == "Mein Team") {
      choices <- teams_df %>%
        dplyr::filter(Manager == "Dominik") %>%
        dplyr::pull(Spieler) %>%
        sort()
      updateSelectInput(session, "spieler_select2", choices = choices)
      
    } else if (sel == "Alle") {
      ap_df <- read.csv2(
        "data/ALL_PLAYERS.csv",
        sep = ";", na.strings = c("", "NA"),
        stringsAsFactors = FALSE, fileEncoding = "UTF-8"
      ) %>%
        dplyr::mutate(
          Datum = as.Date(Datum, format = "%d.%m.%Y"),
          Marktwert = as.numeric(Marktwert)
        )
      
      choices <- ap_df %>%
        dplyr::filter(Datum == Sys.Date()) %>%
        dplyr::pull(Spieler) %>%
        sort()
      updateSelectInput(session, "spieler_select2", choices = choices)
    }
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
    updateTabsetPanel(session, "flip_tabs", selected = "Kader & Historie")
    }
  })
  
  # ---- Daten / df / list / functions ----
  
  ## ---- teams_df / transfers / transfermarkt / ap_df / tm_df / st_df ----
  
  teams_df <- read.csv2("data/TEAMS_all.csv", sep = ";", stringsAsFactors = FALSE)
  
  transfers <- read.csv2("data/TRANSFERS_all.csv", sep = ";", na.strings = c("", "NA")) %>%
      mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"))
  
  # Basis einmal vor dem Summieren abzweigen
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
    filter(!(Datum == "01.06.2025" & Manager == "Alfons" & Transaktion == -166000))
  
  # Dein unverändertes Objekt für andere Plots
  transactions <- transactions_base %>%
    group_by(Manager) %>%
    summarise(Transaction_Summe = sum(Transaktion, na.rm = TRUE), .groups = "drop")
  
  # Platzierungen auf Basis der gleichen Quelle
  patzierungen_df <- transactions_base %>%
    filter(str_detect(Begründung, "^Bonus")) %>%
    mutate(Platzierung = as.integer(str_extract(Begründung, "\\d+"))) %>%
    filter(!is.na(Platzierung), Platzierung >= 1, Platzierung <= 8) %>%
    count(Manager, Platzierung, name = "Anzahl") %>%
    complete(Manager, Platzierung = 1:8, fill = list(Anzahl = 0)) %>%
    arrange(Manager, Platzierung)
  
  transfermarkt <- read_csv2("data/TRANSFERMARKT.csv",
                             locale = locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = "."),
                             show_col_types = FALSE) %>%
    mutate(TM_Stand = as.Date(TM_Stand, format = "%d.%m.%Y"))
  
  ap_df <- read.csv2("data/ALL_PLAYERS.csv", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE, fileEncoding = "UTF-8") %>% 
    mutate(Datum = as.Date(Datum, format = "%d.%m.%Y"),
           Marktwert = as.numeric(Marktwert))
  
  status_latest <- ap_df %>%
    group_by(Spieler) %>%
    arrange(desc(Datum)) %>%
    slice(1) %>%
    ungroup() %>%
    select(Spieler, StatusText, StatusIcon)
  
  tm_df <- read.csv2("data/COMP_TM_RESTZEIT.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  
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

  ca_df <- read_delim(
    "data/com_analytics_all_players.csv",
    delim = ";",
    locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ","),
    show_col_types = FALSE
  ) %>%
    mutate(
      SPIELER = trimws(enc2utf8(as.character(SPIELER))),
      Datum   = as.Date(Datum, format = "%d.%m.%Y")
    ) %>%
    filter(Datum == max(Datum, na.rm = TRUE)) %>%
    transmute(
      SPIELER,
      Position = Positionsgruppe,
      Verein = POSITION,
      Punkte_pro_Spiel = `PUNKTE PRO SPIEL`,
      Gesamtpunkte = GESAMTPUNKTE,
      Preis_Leistung = `PREIS-LEISTUNG`,
      Historische_Punkteausbeute = `HISTORISCHE PUNKTEAUSBEUTE`,
      Verletzungsanfälligkeit = Verletzungsanfälligkeit
    )
  
  #Kaufempfehung etc.
  ca2_df <- read_delim(
    "data/com_analytics_transfer_market_computer.csv",
    delim = ";",
    locale = locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ","),
    show_col_types = FALSE
  ) %>%
    mutate(
      Datum   = as.Date(Datum, format = "%d.%m.%Y"),
      SPIELER = trimws(enc2utf8(as.character(SPIELER)))
    ) %>%
    filter(Datum == max(Datum, na.rm = TRUE))
  
  #All player hist. MW
  mw_all <- read.csv("data/marktwertverlauf_gesamt.csv", sep = ";", encoding = "UTF-8")
  
  #Angebote
  angebote <- read.csv2(
    "data/ANGEBOTE.csv",
    sep = ";",
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8",
    check.names = FALSE) %>%
    mutate(Datum = as.Date(Datum, format = "%d.%m.%Y")) 
  
  # --- Reactive: Trend + Verein hinzufügen ---
  dat_rel_angebote <- reactive({
    betrag_col <- if ("Angebot (€)" %in% names(angebote)) "Angebot (€)" else "Angebot"
    
    a <- angebote %>%
      mutate(Angebot = as.numeric(.data[[betrag_col]])) %>%
      select(Spieler, Datum, Angebot, Status)
    
    spieler_set <- unique(a$Spieler)
    datum_set   <- unique(a$Datum)
    
    mw_am_tag <- ap_df %>%
      filter(Spieler %in% spieler_set, Datum %in% datum_set) %>%
      select(Spieler, Datum, Verein, MW_am_Tag = Marktwert)
    
    mw_vortag <- ap_df %>%
      filter(Spieler %in% spieler_set, Datum %in% (datum_set - 1)) %>%
      transmute(Spieler, Datum = Datum + 1, MW_Vortag = Marktwert)
    
    a %>%
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
  })
  
  ## ---- Liga Insider Info ----
  li_df <- read.csv2("data/LI_player_profiles.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  
  match_li_row <- reactive({
    req(input$spieler_select2)
    req(exists("li_df", inherits = TRUE), is.data.frame(li_df))
    k <- norm_key(input$spieler_select2)
    tmp <- li_df
    tmp$k <- norm_key(tmp$Comunio_Name)
    out <- tmp[tmp$k == k, , drop = FALSE]
    if (nrow(out) == 0) return(out)
    out[1, , drop = FALSE]
  })
  
  safe_val <- function(v) ifelse(length(v) == 0 || is.na(v) || !nzchar(as.character(v)), "", as.character(v))
  safe_img <- function(url, width_px) {
    url <- safe_val(url)
    if (!nzchar(url)) return(NULL)
    tags$img(src = url, width = width_px, onerror = "this.style.display='none'")
  }
  
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
  
  ## ---- verein_filter ----
  observe({
    updateSelectizeInput(session, "verein_filter",
                         choices = sort(unique(ap_df$Verein)), server = TRUE)
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
  
  ## ---- MW-Klasse bestimmen ----
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
  
  ## ---- Hilfsfunktion, um pro Zeile einen Shiny-Button zu erzeugen ----
  shinyButton <- function(id, label = "→") {
    sprintf(
      '<button class="btn btn-xs btn-primary" onclick="Shiny.setInputValue(\'transfer_row\', %s, {priority: \'event\'})">%s</button>',
      id, label
    )
  }
  
  ## ---- Render UIs ----
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
  
  # ---- Helper function für LI-Mapping ----
  norm_key <- function(x) {
    x <- enc2utf8(as.character(x))
    x <- tolower(trimws(x))
    x <- chartr("ß", "ss", x)
    x
  }
  
  # ---- DASHBOARD ----
  
  ## ---- News ----
  
  output$dashboard_news <- renderUI({
    req(exists("li_df", inherits = TRUE), is.data.frame(li_df))
    req(!is.null(mein_kader_df()))
    kad <- mein_kader_df()
    req("Spieler" %in% names(kad))
    
    need <- c("Comunio_Name","News1_Date","News1_URL","News2_Date","News2_URL","News3_Date","News3_URL")
    if (!all(need %in% names(li_df))) return(div("Keine News verfügbar"))
    
    kad_namen <- unique(kad$Spieler)
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
    
    today <- as.Date(Sys.time(), tz = "Europe/Berlin")
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
  output$transfer_summary_today <- DT::renderDT({
    selected_date <- if (input$flip_day == "Gestern") Sys.Date() - 1 else Sys.Date()
    
    df <- transfers %>%
      filter(Datum == selected_date, Hoechstbietender != "Computer") %>%
      left_join(
        ap_df %>% filter(Datum == selected_date)   %>% select(Spieler, Marktwert_today = Marktwert),
        by = "Spieler"
      ) %>%
      left_join(
        ap_df %>% filter(Datum == selected_date-1) %>% select(Spieler, Marktwert_prev  = Marktwert),
        by = "Spieler"
      ) %>%
      mutate(
        Preis     = as.numeric(Hoechstgebot),
        MW_Vortag = as.numeric(Marktwert_prev),
        MW_Heute  = as.numeric(Marktwert_today),
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
  output$flip_summary_today <- DT::renderDT({
    selected_date <- if (input$flip_day == "Gestern") Sys.Date() - 1 else Sys.Date()
    
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
    df <- subset(gesamt_mw_df, !is.na(Datum))
    if (nrow(df) < 3) return(NULL)
    
    last_day <- max(df$Datum, na.rm = TRUE)
    win_days <- 7L
    df_short <- df[df$Datum >= (last_day - win_days), , drop = FALSE]
    if (nrow(df_short) < 3) df_short <- tail(df[order(df$Datum), ], 3)
    
    # NAs in y entfernen
    df_short <- df_short[!is.na(df_short$MW_rel_normiert), , drop = FALSE]
    if (nrow(df_short) < 2) return(NULL)
    
    trend_vals <- tail(df_short[order(df_short$Datum), ], 3)
    x0 <- trend_vals$Datum[1]; y0 <- trend_vals$MW_rel_normiert[1]
    x1 <- trend_vals$Datum[3]; y1 <- trend_vals$MW_rel_normiert[3]
    col_seg <- if (y1 > y0) "darkgreen" else "red"
    
    ggplot(df_short, aes(Datum, MW_rel_normiert)) +
      geom_line(linewidth = 1.1, color = "darkgrey", na.rm = TRUE) +
      # Ein-Zeilen-Daten für Segment
      geom_segment(
        data = data.frame(x0 = x0, y0 = y0, x1 = x1, y1 = y1),
        aes(x = x0, y = y0, xend = x1, yend = y1),
        arrow = arrow(length = unit(0.35, "cm"), type = "closed"),
        color = col_seg, linewidth = 1.4, inherit.aes = FALSE
      ) +
      scale_x_date(limits = c(last_day - win_days, last_day),
                   date_labels = "%d.%m.") +
      # explizite Zahlformatierung vermeidet prettyNum-Warnung
      scale_y_continuous(labels = scales::label_number(big_mark = ".", decimal_mark = ",")) +
      labs(x = NULL, y = "relativer MW") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none")
  })
  
  ## ---- Kontostände ----
  output$kreditrahmen_uebersicht_preview <- DT::renderDT({
    kapital_df <- kapital_df_reactive() %>%
      select(
        Manager,
        Teamwert,
        Kontostand = Aktuelles_Kapital,
        `Verfügbares Kapital` = Verfügbares_Kapital
      ) %>%
      mutate(across(c(Teamwert, Kontostand, `Verfügbares Kapital`), as.numeric)) %>%
      as.data.frame()
    
    idx_vk <- which(names(kapital_df) == "Verfügbares Kapital") - 1L  # 0-basiert
    
    DT::datatable(
      kapital_df,
      rownames = FALSE,
      colnames = c("Manager","Teamwert (€)","Kontostand (€)","Verfügbares Kapital (€)"),
      escape = FALSE,
      options = list(
        paging = FALSE,
        autoWidth = TRUE,
        order = list(list(idx_vk, 'desc')),
        dom = 't'
      )
    ) %>%
      formatCurrency(
        columns = c("Teamwert", "Kontostand", "Verfügbares Kapital"),
        currency = "", interval = 3, mark = ".", digits = 0, dec.mark = ","
      ) %>%
      formatStyle(
        'Kontostand',
        color = styleInterval(0, c('red', 'black')),
        fontWeight = styleInterval(0, c('bold', NA))
      )
  })
  
  
  output$flip_preview <- renderPlot({
    req(input$main_navbar == "Dashboard")
    df <- flip_data() %>%
      group_by(Besitzer) %>%
      summarise(Gesamtgewinn = sum(Gewinn, na.rm = TRUE), .groups = "drop")
    req(nrow(df) > 0)
    
    abs_max <- max(abs(df$Gesamtgewinn), na.rm = TRUE)
    lim_min <- -abs_max - 1e5
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
  # Manuell zusätzliche Tagesdaten ergänzen
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
    # sp <- sommerpause_data() #Sommerpause
    
    # 1) Saubere Daten (ohne NA) und sortiert
    clean_df <- gesamt_mw_df %>%
      filter(!is.na(Datum)) %>%
      arrange(Datum)
    
    # 2) Indices für 5-Tage-Fenster (letztes Fenster darf kürzer sein)
    n   <- nrow(clean_df)
    idx <- seq(1, n, by = 5)
    
    # 3) Arrow-Dataframe zusammenbauen
    arrow_df <- tibble(
      x0 = clean_df$Datum[idx],
      y0 = clean_df$MW_rel_normiert[idx],
      x1 = clean_df$Datum[pmin(idx + 5, n)],
      y1 = clean_df$MW_rel_normiert[pmin(idx + 5, n)]
    ) %>%
      mutate(
        color = ifelse(y1 > y0, "darkgreen", "red")
      )
    
    
    ggplot() +
      # Linien für MW-Verläufe
      geom_line(
        data = gesamt_mw_df %>%
          transmute(Datum, Wert = MW_rel_normiert, Typ = "Gesamtmarktwert"),
        aes(x = Datum, y = Wert, color = Typ),
        linewidth = 1.2,
        na.rm     = TRUE
      ) +
      geom_vline(xintercept = as.Date("2025-08-22"), linetype = "dotted",
                 color = "darkred", linewidth = 1.5) +
      annotate(
        "text",
        x = as.Date("2025-08-22"),
        y = 0.65,  # ggf. anpassen je nach Plot-Skalierung
        label = "Saisonstart",
        color = "darkred",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      geom_vline(xintercept = as.Date("2025-09-07"), linetype = "dotted",
                 color = "darkgreen", linewidth = 1.5) +
      annotate(
        "text",
        x = as.Date("2025-09-07"),
        y = 0.75,  # ggf. anpassen je nach Plot-Skalierung
        label = "Kippunkt 1 (19/22/23) 07.09.25",
        color = "darkgreen",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      geom_vline(xintercept = as.Date("2025-10-07"), linetype = "dotted",
                 color = "orange", linewidth = 1.5) +
      annotate(
        "text",
        x = as.Date("2025-10-07"),
        y = 0.8,  # ggf. anpassen je nach Plot-Skalierung
        label = "Kippunkt 2 (18/21) 07.10.25",
        color = "orange",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      geom_vline(xintercept = as.Date("2025-12-24"), linetype = "dotted",
                 color = "red", size = 1.5) +
      annotate(
        "text",
        x = as.Date("2025-12-24"),
        y = 0.95,  # ggf. anpassen je nach Plot-Skalierung
        label = "Kippunkt 3 (13/14/19/24) 24.12.25",
        color = "red",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      geom_segment(
        data        = arrow_df,
        aes(x = x0, y = y0, xend = x1, yend = y1),
        arrow       = arrow(length = unit(0.3, "cm"), type = "closed"),
        color       = arrow_df$color,
        linewidth = 1.2,
        inherit.aes = FALSE
      ) +
      
      coord_cartesian(ylim = c(0.5, 1.6)) +
      labs(
        title = "Marktwertentwicklung (relativ zum Startwert)",
        x = "Datum",
        y = "Relativer Marktwert",
        color = "Linientyp"
      ) +
      scale_color_manual(values = c(
        "Gesamtmarktwert" = "darkgrey"
      )) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none") +
      # --- hier kommen die beiden Sommerpause‑Linien aus sp ---
      geom_line(
        #data = filter(sp, Saison == "Sommerpause_2024"), #Sommerpause
        data = filter(sp, Saison == "2024-25"),
        aes(x = Datum, y = MW_rel_normiert),
        color    = "red",
        linetype = "dashed",
        linewidth= 1.3,
        na.rm     = TRUE
      ) +
      geom_line(
        #data = filter(sp, Saison == "Sommerpause_2021"), #Sommerpause
        data = filter(sp, Saison == "2021-22"),
        aes(x = Datum, y = MW_rel_normiert),
        color    = "orange",
        linetype = "dashed",
        linewidth= 1.3,
        na.rm     = TRUE
      )
    
  })
  
  ## ---- Tägliche ME-Änderung-Verlauf ----
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
      scale_x_date(date_breaks = "1 week", date_labels = "%d.%m.") +
      labs(title = "Tägliche Marktwert-Veränderung", x = "Datum", y = "∆ zum Vortag (%)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title  = element_text(size = 16)) +
      coord_cartesian(
        xlim = c(min(change_df$Datum), as.Date("2026-01-01")),   # schneidet ohne Rows zu droppen
        ylim = c(min(change_df$pct_change)*1.1, max(change_df$pct_change)*1.1)
      )
  })
  
  
  ## ---- MW-events ----
  output$mw_events <- renderPlot({
    req(input$main_navbar == "Marktwert-Entwicklung",
        input$mw_tabs      == "mw_events_tab")
    req(input$manager_select)
    
    # 1) Gesamtmarktwert-Daten
    clean_df <- gesamt_mw_df %>%
      filter(!is.na(Datum)) %>%
      arrange(Datum)
    
    y_min <- min(clean_df$MW_rel_normiert, na.rm = TRUE)
    y_max <- max(clean_df$MW_rel_normiert, na.rm = TRUE)
    
    # 2) Transfer-Historie aufbereiten
    tr <- transfers %>%
      mutate(Datum = as.Date(Datum, "%d.%m.%Y")) %>%
      arrange(Datum) %>%
      group_by(Spieler) %>%
      ungroup()
    
    # 3a) Buy-Events: Manager ist Höchstbietender
    buys <- tr %>%
      filter(Hoechstbietender == input$manager_select) %>%
      mutate(type = "buy")
    
    # 3b) Sell-Events: Manager war Vorbesitzer und Computer kauft
    sells <- tr %>%
      filter(Besitzer == input$manager_select, Hoechstbietender == "Computer") %>%
      mutate(type = "sell")
    
    # 3c) Zusammentragen
    events <- bind_rows(buys, sells)
    req(nrow(events) > 0)
    
    # 4) Y-Offsets pro Tag berechnen, damit Punkte nicht überlappen
    events <- events %>%
      arrange(Datum) %>%
      group_by(Datum) %>%
      mutate(
        n     = n(),
        idx   = row_number(),
        y_dot = 1.2 + (idx - (n + 1) / 2) * 0.05
      ) %>%
      ungroup()
    
    # 5) Zeichnen
    ggplot() +
      # a) Gesamt-Marktwert
      geom_line(
        data      = clean_df,
        aes(x = Datum, y = MW_rel_normiert),
        color     = "darkgrey",
        size      = 1.2,
        na.rm     = TRUE
      ) +
      # b) Vertikale Linien für buy/sell
      geom_vline(
        data = events,
        aes(xintercept = as.numeric(Datum), color = type),
        size = 0.1
      ) +
      # c) Punkte auf den y_dot-Positionen
      geom_point(
        data = events,
        aes(x = Datum, y = y_dot, color = type),
        size = 3
      ) +
      # d) Spielernamen daneben
      geom_text(
        data     = events,
        aes(x = Datum, y = y_dot, label = Spieler, color = type),
        position = position_jitter(width  = 0,
                                   height = 0.21),
        size     = 4
      ) +
      # e) Farben festlegen, Legende ausblenden
      scale_color_manual(
        values = c(buy = "darkgreen", sell = "red"),
        guide  = FALSE
      ) +
      # f) Saisonstart-Linie & Label
      geom_vline(
        xintercept = as.Date("2025-08-22"),
        linetype   = "dotted",
        color      = "darkred",
        size       = 1.5
      ) +
      annotate(
        "text",
        x     = as.Date("2025-08-22"),
        y     = 1.25,
        label = "Saisonstart",
        angle = 90,
        vjust = -0.5,
        fontface= "bold",
        size   = 4,
        color  = "darkred"
      ) +
      # g) Limits & Theme
      coord_cartesian(ylim = c(0.5, 1.6)) +
      labs(
        x = "Datum",
        y = "Relativer Marktwert"
      ) +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none")
  })
  

  ## ---- Hist. Marktwert-Entwicklung ab 01.06.2024 (je Klasse) ----
  data <- reactive({
    df <- read.csv("data/marktwertverlauf_gesamt.csv", sep = ";", encoding = "UTF-8")
    
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
      filter(Datum <= Sys.Date()- 365)
    
    
    ggplot(df_plot_norm, aes(x = Datum, y = MW_normiert, color = Klasse)) +
      geom_line(size = 1.2) +
      labs(
        title = "Historischer Marktwertverlauf je Klasse",
        y = "Normierter MW",
        x = "",
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
      theme_minimal(base_size = 14) +
      scale_x_date(date_labels = "%b %y")   
  })
  
  ## ---- Je Klasse Marktwert-Entwicklung ab 15.06.2025 ----
  
  data_now <- reactive({
    df <- ap_df
    
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
      geom_vline(xintercept = as.numeric(Sys.Date()), color = "darkred", linetype = "dashed", linewidth = 1) +
      annotate(
        "text",
        x = Sys.Date(),
        y = 0.7,  # ggf. anpassen
        label = "Heute",
        color = "darkred",
        angle = 90,
        vjust = -0.5,
        fontface = "bold",
        size = 4
      ) +
      theme_minimal(base_size = 14) +
      scale_x_date(date_labels = "%b %y")   # 
  })
  
  
  ## ---- Hist. Martkwertverläufe - Chronologie ----
  
  data_path <- "./global_MW"
  
  seasons <- c("2004-05", "2005-06", "2006-07", "2007-08", "2008-09",
               "2009-10", "2010-11", "2011-12", "2012-13", "2013-14",
               "2014-15", "2015-16", "2016-17", "2017-18", "2018-19",
               "2019-20", "2020-21", "2021-22", "2022-23", "2023-24", "2024-25", "Sommerpause_2021", "Sommerpause_2024")
  
  # Nur die regulären Saisons (ohne Sommerpause)
  real_seasons <- seasons[!grepl("^Sommerpause", seasons)]
  n_real       <- length(real_seasons)
  
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
  
  ## ---- Hist. Saisonverläufe - Select ----
  
  # Einmalige Initialisierung der Checkbox-Choices
  observeEvent(TRUE, {
    sel <- c(real_seasons[n_real], real_seasons[n_real-2])
    updateCheckboxGroupInput(
      session, "selected_seasons",
      choices  = real_seasons,
      selected = sel
    )
  }, once = TRUE)
  
  # Button-Handler nur noch auf real_seasons beziehen:
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(
      session, "selected_seasons",
      choices  = real_seasons,
      selected = real_seasons
    )
  })
  observeEvent(input$select_none, {
    updateCheckboxGroupInput(
      session, "selected_seasons",
      choices  = real_seasons,
      selected = character(0)
    )
  })
  observeEvent(input$select_custom, {
    sel2 <- tail(real_seasons, 4)
    updateCheckboxGroupInput(
      session, "selected_seasons",
      choices  = real_seasons,
      selected = sel2
    )
  })
  observeEvent(input$select_last3, {
    sel <- c(real_seasons[n_real], real_seasons[n_real-2])
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
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom")
  })
  
  # ---- TRANSFERMARKT ----
  
  ## ---- INFO: Durchschnittliche Gebote pro Wochentag ----
  
  # 1) avg_bids_wd als Reactive (einmalig im Server)
  avg_bids_wd <- reactive({
    gp <- gebotsprofil_clean() %>%
      group_by(Datum, Bieter) %>%
      summarise(Anzahl = n(), .groups = "drop") %>%
      mutate(Datum = Datum - 1)
    
    gp %>%
      complete(
        Bieter,
        Datum = seq(min(Datum), max(Datum), by = "day"),
        fill = list(Anzahl = 0)
      ) %>% mutate(
      Wochentag = factor(
        weekdays(Datum, abbreviate = TRUE),
        levels = c("Mo","Di","Mi","Do","Fr","Sa","So")
      )
    ) %>%
      group_by(Bieter, Wochentag) %>%
      summarise(avg_bids = mean(Anzahl), .groups = "drop")
  })
  
  # 2) Vorhersage-Box für heute
  output$bid_prediction <- renderUI({
    # Welcher Wochentag ist heute?
    today_wd <- factor(
      weekdays(Sys.Date(), abbreviate = TRUE),
      levels = c("Mo","Di","Mi","Do","Fr","Sa","So")
    )
    
    # Top-3 (ohne dich selbst und Dominik)
    top3 <- avg_bids_wd() %>%
      filter(
        Wochentag == today_wd,
        !Bieter %in% c("DeinBieterName","Dominik")
      ) %>%
      arrange(desc(avg_bids)) %>%
      slice_head(n = 3)
    
    # Wenn nichts da ist, gar nichts anzeigen
    if (nrow(top3) == 0) return(NULL)
    
    # Beschriftung bauen
    labels <- sprintf(
      "<b>%s</b> (Ø %s Gebote)",
      top3$Bieter,
      format(round(top3$avg_bids, 2), decimal.mark = ",")
    )
    
    HTML(paste0(
      "<div style='margin:8px 0; padding:8px; background:#f9f9f9; border:1px solid #ddd; border-radius:4px;'>",
      "<strong>Erwartete Bieter heute (", 
      format(Sys.Date(), "%A, %d.%m.%Y"), "):</strong> ",
      paste(labels, collapse = " | "),
      "</div>"
    ))
  })
  

  ## ---- Tabelle ----
  
  # 1) reactiveVal für den gemeinsamen DataFrame
  tm_common <- reactiveVal(NULL)
  
  # 2) Observer oder reactive, der tm_common einmal neu berechnet, sobald tm_df oder ap_df sich ändern:
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
      # Fallback: wenn kein Profil-Mean vorliegt, dann einfach base_val anzeigen
      mutate(
        IdealesGebot_num = ifelse(is.na(IdealesGebot_num), base_val, IdealesGebot_num),
        IdealesGebot     = ifelse(
          is.na(IdealesGebotProzent),
          paste0(format(base_val, big.mark=".", decimal.mark=","), " €"),
          IdealesGebot
        )
      ) %>%
      select(-base_val)
    
    
    # 4) Maximalgebot (90%-Perzentil) dynamisch auf Mindestgebot oder Marktwert
    transfers_clean <- transfers %>%
      mutate(Hoechstgebot = as.numeric(Hoechstgebot)) %>%
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
      # Fallback: wenn kein Faktor da, Einfach base_val anzeigen
      mutate(
        Maximalgebot_num = ifelse(is.na(Maximalgebot_num), base_val, Maximalgebot_num),
        Maximalgebot     = ifelse(
          is.na(MaximalgebotProzent),
          paste0(format(base_val, big.mark=".", decimal.mark=","), " €"),
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
        ca_df %>% select(Spieler = SPIELER, `Historische Punkteausbeute` = Historische_Punkteausbeute, Verletzungsanfälligkeit),
        by = "Spieler"
      ) %>%
      left_join(
        ca2_df %>%
            select(
              SPIELER,
              POSITION,
              PUNKTE,
              `Punkte pro Spiel` = `PUNKTE / SPIEL`,
              `Preis-Leistung` = `PREIS-LEISSTUNG`,
              Zielwert = `ZIELWERT`,
              Empfehlung = `KAUFEMPFEHLUNG`,
              Gebote = `GEBOTSVORHERSAGE`
            ),
        by = c("Spieler" = "SPIELER")
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
          "<div>",
          "<div><strong>Min:</strong> ",
          format(Minimalgebot_num, big.mark=".", decimal.mark=","), " € ",
          "<button class='btn btn-xs btn-light copy-btn' data-value='", Minimalgebot_num, "' title='Kopieren'>📋</button>",
          " <span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(MinimalgebotProzent),"–",
                 paste0(ifelse(MinimalgebotProzent>0,"+",""), MinimalgebotProzent, "%")),
          ")</span></div>",
          "<div><strong>Ideal:</strong> ",
          format(IdealesGebot_num, big.mark=".", decimal.mark=","), " € ",
          "<button class='btn btn-xs btn-light copy-btn' data-value='", IdealesGebot_num, "' title='Kopieren'>📋</button>",
          " <span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(IdealesGebotProzent),"–",
                 paste0(ifelse(IdealesGebotProzent>0,"+",""), IdealesGebotProzent, "%")),
          ")</span></div>",
          "<div><strong>Max:</strong> ",
          format(Maximalgebot_num, big.mark=".", decimal.mark=","), " € ",
          "<button class='btn btn-xs btn-light copy-btn' data-value='", Maximalgebot_num, "' title='Kopieren'>📋</button>",
          " <span style='font-size:85%;color:#666;'>(",
          ifelse(is.na(MaximalgebotProzent),"–",
                 paste0(ifelse(MaximalgebotProzent>0,"+",""), MaximalgebotProzent, "%")),
          ")</span></div>",
          "</div>"
        )
      )
    
    
    # fehlende Spalten auffüllen
    needed <- c("Spieler","StatusTag","POSITION","Logo","PUNKTE","Punkte pro Spiel","Preis-Leistung", "Verletzungsanfälligkeit",
                "Historische Punkteausbeute","Marktwert","Zielwert","Mindestgebot", "Gebote_minmax",
                "Gebote","Empfehlung","Besitzer","Verbleibende Zeit","Trend MW (3 Tage)","action2","action")
    
    
    for (nm in setdiff(needed, names(df))) df[[nm]] <- NA_character_
    sub_df <- df[, needed, drop = FALSE]
    
    # List-Spalten zu String (falls vorhanden)
    is_list_col <- vapply(sub_df, is.list, logical(1))
    if (any(is_list_col)) sub_df[is_list_col] <- lapply(sub_df[is_list_col], function(x) vapply(x, toString, ""))
    
    # NACH sub_df gebaut wurde
    sub_df$MinGeb_Unter_MW <- df$MinGeb_Unter_MW
    
    DT::datatable(
      sub_df,
      colnames = c(
        "Spieler"," ","Position","Verein","Punkte","PPS","Preis-Leistung", "<span style='color:red;'>✚</span> Risiko","Hist.","Marktwert",
        "Zielwert","Mindestgebot","Gebote (Min/Ideal/Max)", "Gebote",
        "Maximalgebot" = NULL,  # entfällt
        "Empfehlung","Besitzer",
        "Angebotsende","Trend","Info","Rechner",""
      )
      ,
      escape    = FALSE,
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
        ordering   = FALSE,
        pageLength = 20,
        columnDefs = list(
          list(className = 'dt-left',  targets = 0:1),
          list(className = 'dt-right', targets = 2:7),
          list(className = 'dt-center',targets = ncol(sub_df)-2),
          list(visible=FALSE, targets=ncol(sub_df) - 1)
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
  
  ## ---- LI-News ----
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
    
    # ---- Datum robust normalisieren + parsen ----
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
    
    today <- as.Date(Sys.time(), tz = "Europe/Berlin")
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
  
  ## ---- LI-Leistungsdaten Render ----
  ### ---- LI-Leistungsdaten: Summary (Note/Punkte/EQ) ----
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
  
  ### ---- LI-Leistungsdaten: Bilanz (Gesamt | BL | Pokal) ----
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
  
  
  ## ---- MW ----
  output$spieler_info_mw <- renderPlot({
    req(input$spieler_select2, mw_all, ap_df)
    
    norm <- function(x) tolower(trimws(enc2utf8(as.character(x))))
    sel  <- norm(input$spieler_select2)
    
    hist <- mw_all %>%
      mutate(Datum = as.Date(Datum),
             Marktwert = as.numeric(Marktwert),
             K = norm(Spieler),
             src = factor("hist", levels = c("hist","daily")))
    
    daily <- ap_df %>%
      mutate(Datum = as.Date(Datum),
             Marktwert = as.numeric(Marktwert),
             K = norm(Spieler),
             src = factor("daily", levels = c("hist","daily")))
    
    df <- bind_rows(hist, daily) %>%
      filter(K == sel) %>%
      arrange(Datum, src) %>%         # hist zuerst, daily danach
      group_by(Datum) %>% slice_tail(n = 1) %>% ungroup()
    
    req(nrow(df) > 0)
    x_min <- as.Date("2024-06-01")
    x_max <- max(df$Datum, na.rm = TRUE)
    
    ggplot(df, aes(x = Datum, y = Marktwert)) +
      geom_line() +
      geom_point(size = 1) +
      scale_x_date(limits = c(x_min, x_max)) +
      labs(title = "",
           x = NULL, y = "Marktwert (€)") +
      theme_minimal(base_size = 16)
  })
  
  ## ---- Leistungsdaten ----
  output$spieler_info_raw <- DT::renderDT({
    req(input$spieler_select2)
    
    left  <- ca_df  %>% filter(SPIELER == input$spieler_select2)
    right <- ca2_df %>% filter(SPIELER == input$spieler_select2)
    
    df <- left %>% left_join(right, by = "SPIELER", suffix = c("", "_TM")) %>% 
      rename(Spieler = SPIELER, PPS = Punkte_pro_Spiel, `Preis/Leistung` = Preis_Leistung, Historie = Historische_Punkteausbeute) %>%
      select(-any_of(c("POSITION","TEAM","PUNKTE","PUNKTE / SPIEL"))) %>%
      select(where(~ !all(is.na(.))))
    
    DT::datatable(
      df,
      escape = FALSE, rownames = FALSE,
      options = list(dom = 't', scrollX = TRUE, paging = FALSE)
    )
  })
  
  ## ---- GPT ----
  ### ---- Helpers ----
  get_verein <- function(sp) {
    norm <- function(x) tolower(trimws(enc2utf8(as.character(x))))
    sel  <- norm(sp)
    
    if (exists("ap_df", inherits=TRUE) && is.data.frame(ap_df) && nrow(ap_df) > 0) {
      tmp <- ap_df %>%
        mutate(.k = norm(Spieler), Datum = as.Date(Datum)) %>%
        filter(.k == sel)
      if (nrow(tmp) > 0) {
        today <- Sys.Date()
        row <- tmp %>% filter(Datum == today) %>% slice_tail(n=1)
        if (nrow(row) == 0) row <- tmp %>% arrange(desc(Datum)) %>% slice_head(n=1)
        ve <- as.character(row$Verein[1])
        if (nzchar(ve)) return(ve)
      }
    }
    
    src <- if (exists("ca2_df", inherits=TRUE)) ca2_df
    else if (exists("ca_df2", inherits=TRUE)) ca_df2
    else if (exists("ca_df",  inherits=TRUE)) ca_df
    else NULL
    if (is.null(src) || !is.data.frame(src) || nrow(src) == 0) return("")
    tmp2 <- src %>% mutate(.k = norm(SPIELER)) %>% filter(.k == sel)
    if (nrow(tmp2) == 0) return("")
    col <- if ("TEAM" %in% names(src)) "TEAM" else if ("VEREIN" %in% names(src)) "VEREIN" else return("")
    v <- as.character(tmp2[[col]][1]); ifelse(is.na(v), "", v)
  }
  
  build_prompt <- function(sp, ve) {
    today <- format(Sys.Date(), "%Y-%m-%d")
    paste0(
      "Gib GENAU dieses Format zurück:\n",
      "Spieler\nVerein\n\n",
      ">>> Die erste Zeile muss mit '", sp, "' beginnen. ",
      ">>> Die zweite Zeile muss mit '", ve, "' beginnen. ",
      "Falls der Verein leer/unsicher ist, trage den korrekt ermittelten aktuellen Verein dort ein. <<<\n",
      "Info: Kurz, faktenbasiert zu Rolle/Status; Einsatz 1–2 Wochen; Trainerstimme; Verletzung; Falls Verletzung, Rückkehrzeitpunkt; Wechsel. ",
      "Keine Semikolons in 'Info'. KEINE Markdown-Links. Jede Aussage mit [1], [2] … belegen. ",
      "Am Ende von 'Info' eine neue Zeile 'Quellen:' und die URLs mit Datum YYYY-MM-DD, bevorzugt Vereinsseiten, bundesliga.com, dfb.de, kicker.de, transfermarkt.de, lokale Qualitätsmedien. ",
      "Meide Social Media.\n\n",
      "Stammplatz: <Zahl 0.0–3.0 in 0.5-Schritten>\n",
      "Potenzial: <Zahl 0.0–3.0 in 0.5-Schritten>\n",
      "Wechselwahrscheinlichkeit: <Zahl 0.0–3.0 in 0.5-Schritten>\n\n",
      "Suchfenster: primär letzte 60 Tage, sonst ältere verlässliche Quellen mit Datum. ",
      "Stand: ", today, ". Antworte NUR in diesem Format."
    )
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
  
  ### ---- Prompt zentral bauen ----
  gpt_prompt <- reactive({
    req(input$spieler_select2)
    sp <- input$spieler_select2
    ve <- get_verein(sp)
    build_prompt(sp, ve)
  })
  
  output$gpt_prompt_preview <- renderText(gpt_prompt())
  
  ### ---- GPT-Call und Prompt-Copy ----
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
      prompt_str <- gpt_prompt()                  # <— einheitlich
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
  
  ## ---- Transfer-Simulator ----
  # Reactive DataFrames vorbereiten
  meine_spieler <- reactive({
    teams_df %>%
      dplyr::filter(Manager == "Dominik") %>%
      dplyr::pull(Spieler)
  })
  
  # Aktuelle Marktwerte
  aktuelle_mw <- reactive({
    max_datum <- max(ap_df$Datum, na.rm = TRUE)
    ap_df %>%
      dplyr::filter(Datum == max_datum) %>%
      dplyr::transmute(Spieler, Marktwert = as.numeric(Marktwert))
  })
  
  # Rohdaten der Angebote
  angebote_raw <- angebote %>%
    dplyr::mutate(Status = ifelse(is.na(Status), "aktiv", Status))
  
  angebote_df <- reactive({
    angebote_raw %>%
      dplyr::filter(Status == "aktiv") %>%
      dplyr::rename(Angebot = `Angebot (€)`) %>%
      dplyr::mutate(
        Spieler = trimws(Spieler),
        Angebot = as.numeric(gsub("\\.", "", Angebot))
      ) %>%
      dplyr::filter(Spieler %in% meine_spieler()) %>%
      dplyr::left_join(aktuelle_mw(), by = "Spieler") %>%
      dplyr::mutate(
        Marktwert     = ifelse(is.na(Marktwert), Angebot, Marktwert),
        Kreditverlust = Marktwert / 4
      )
  })
  
  team_spieler_df <- reactive({
    aktuelle_mw() %>%
      dplyr::semi_join(teams_df %>% dplyr::filter(Manager == "Dominik"), by = "Spieler")
  })
  
  # UI-Inputs befüllen
  observe({
    # 1) Spieler-Liste
    updateSelectInput(
      session, "spieler_select",
      choices = sort(unique(tm_df$Spieler))
    )
    
    # 2) Zweite Spieler-Liste
    updateSelectInput(
      session, "spieler_select2",
      choices = sort(unique(tm_df$Spieler))
    )
    
    # 3) Angebote-Checkboxes (eindeutige IDs)
    if (nrow(angebote_df()) > 0) {
      angebot_labels <- paste0(
        angebote_df()$Spieler,
        " (", format(angebote_df()$Angebot, big.mark = ".", decimal.mark = ","), " €)"
      )
      angebot_ids <- paste0("A", seq_len(nrow(angebote_df())))
      choices_a <- setNames(angebot_ids, angebot_labels)
    } else {
      choices_a <- character(0)
    }
    updateCheckboxGroupInput(session, "angebote_select", choices = choices_a)
    
    # 4) Eigene Team-Spieler-Checkboxes (eindeutige IDs)
    if (nrow(team_spieler_df()) > 0) {
      team_labels <- paste0(
        team_spieler_df()$Spieler,
        " (", format(team_spieler_df()$Marktwert, big.mark = ".", decimal.mark = ","), " €)"
      )
      team_ids <- paste0("T", seq_len(nrow(team_spieler_df())))
      choices_t <- setNames(team_ids, team_labels)
    } else {
      choices_t <- character(0)
    }
    updateCheckboxGroupInput(session, "team_select", choices = choices_t)
  })
  
  # 5) Plot rendern
  output$transfer_simulator_plot <- renderPlot({
    req(input$spieler_select)
    
    # — (1) Mindestgebot parsen —
    sel     <- tm_df %>% dplyr::filter(Spieler == input$spieler_select)
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
      dplyr::group_by(x) %>% dplyr::arrange(x, segment) %>%
      dplyr::mutate(ypos = cumsum(value) - value/2) %>%
      dplyr::ungroup()
    
    df_neg <- df_avail_neg %>%
      dplyr::group_by(x) %>% dplyr::arrange(x, value) %>%
      dplyr::mutate(ypos = cumsum(value) - value/2) %>%
      dplyr::ungroup()
    
    df_plot <- dplyr::bind_rows(df_pos, df_neg) %>%
      dplyr::mutate(stack_order = as.integer(factor(segment, levels = seg_levels)))
    
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
        data = df_plot %>% dplyr::filter(abs(value) > 1e-9, label != ""),
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
                          color = "darkred", size = 0.5) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "black", size = 0.5) +
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
  
  # Hilfsfunktion für sicheres Escaping in HTML-Attributen
  safe_attr <- function(x) htmltools::htmlEscape(x, attribute = TRUE)
  
  # 1) Reaktive Daten-Vorbereitung
  mein_kader_df <- reactive({
    # Basis: alle Spieler von Dominik
    df0 <- teams_df %>%
      filter(Manager == "Dominik") %>%
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
      filter(Hoechstbietender == "Dominik") %>%
      group_by(Spieler) %>%
      arrange(desc(Datum)) %>%
      slice(1) %>%
      select(Spieler, Kaufpreis = Hoechstgebot)
    
    # --- 3-Tage-Trend berechnen (MW1 = jüngster Tag) ---
    dates_vec <- gesamt_mw_roh %>%
      distinct(Datum) %>%
      pull(Datum) %>%
      na.omit() %>%
      sort(decreasing = TRUE)
    
    if (length(dates_vec) >= 3) {
      last_dates <- dates_vec[1:3]
      names(last_dates) <- c("MW1","MW2","MW3")
      mw_list <- lapply(names(last_dates), function(nm) {
        gesamt_mw_roh %>%
          filter(Datum == last_dates[[nm]]) %>%
          select(Spieler, !!nm := Marktwert)
      })
    } else {
      mw_list <- list()
    }
    
    df_pre <- df0 %>%
      left_join(mw_aktuell, by = "Spieler")
    
    if (length(mw_list) > 0) {
      df_pre <- Reduce(function(x, y) left_join(x, y, by = "Spieler"),
                       c(list(df_pre), mw_list)) %>%
        mutate(
          Trend3 = mapply(function(m1, m2, m3) {
            if (any(is.na(c(m1, m2, m3)))) return("–")
            if (m3 < m2 && m2 < m1)      "<span style='color:#388e3c;font-weight:bold;'>▲▲</span>"
            else if (m3 > m2 && m2 > m1) "<span style='color:#e53935;font-weight:bold;'>▼▼</span>"
            else if (m3 < m1)            "<span style='color:#388e3c;font-weight:bold;'>▲</span>"
            else if (m3 > m1)            "<span style='color:#e53935;font-weight:bold;'>▼</span>"
            else                         "–"
          }, MW1, MW2, MW3, SIMPLIFY = TRUE)
        ) %>%
        select(-any_of(c("MW1","MW2","MW3")))
    } else {
      df_pre$Trend3 <- "–"
    }
    
    df_pre <- df_pre %>%
      left_join(kaufpreise, by = "Spieler") %>%
      mutate(
        Diff_Kauf = Marktwert_aktuell - Kaufpreis,
        Diff_Kauf_fmt = case_when(
          is.na(Kaufpreis) ~ "",
          Diff_Kauf > 0    ~ sprintf("<span style='color:#388e3c;'>+%s € seit Kauf</span>",
                                     format(Diff_Kauf, big.mark=".", decimal.mark=",")),
          Diff_Kauf < 0    ~ sprintf("<span style='color:#e53935;'>-%s € seit Kauf</span>",
                                     format(abs(Diff_Kauf), big.mark=".", decimal.mark=",")),
          TRUE             ~ "<span style='color:grey;'>±0 € seit Kauf</span>"
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
        "Punkte pro Spiel"           = Punkte_pro_Spiel,
        "Preis-Leistung"             = Preis_Leistung,
        "Historische Punkteausbeute" = Historische_Punkteausbeute
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
        pl    <- gruppe$`Preis-Leistung`[i]
        hist  <- gruppe$`Historische Punkteausbeute`[i]
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
        
        sprintf(
          "<tr>
           <td style='padding:4px; text-align:center; width:36px; white-space:nowrap;'>%s</td>
           <td style='padding:4px;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td> 
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:center;'>%s</td>
           <td style='padding:4px; text-align:right;'>%s</td>
           <td style='padding:4px; text-align:right;'>%s</td>
           <td style='padding:4px; text-align:right;'>%s</td>
         </tr>",
          trend3, sp, stat, btn, btn_gpt, v,
          ifelse(is.na(pps), "-", format(round(pps, 2), decimal.mark=",")),
          ifelse(is.na(pl),  "-", pl),
          ifelse(is.na(hist), "-", hist),
          mw, diff, diffk
        )
      })
      
      paste0(
        sprintf("<tr><th colspan='12' style='text-align:left; background:#eee; padding:4px;'>%s</th></tr>", pos_name),
        paste(rows, collapse = "\n")
      )
    })
    
    table_html <- paste0(
      "<table style='width:100%; border-collapse:collapse;'>",
      "<thead><tr>
       <th style='text-align:center;'> </th>
       <th style='text-align:left;'>Spieler</th>
       <th style='text-align:center;'> </th>
       <th style='text-align:center;'> </th>
       <th style='text-align:center;'> </th>
       <th style='text-align:center;'>Verein</th>
       <th style='text-align:center;'>Ø Punkte</th>
       <th style='text-align:center;'>Preis-Leistung</th>
       <th style='text-align:center;'>Historische Punkteausbeute</th>
       <th style='text-align:right;'>MW</th>
       <th style='text-align:right;'>Vortag-MW-Diff</th>
       <th style='text-align:right;'>Kauf-Diff</th>
     </tr></thead>",
      paste(grouped_sections, collapse = "\n"),
      "</tbody></table>"
    )
    
    tagList(HTML(table_html))
  })
  
  
  ## ---- Alle Kader ----
  output$kader_uebersicht_ui <- renderUI({
    
    manager_list <- sort(unique(teams_df$Manager))
    
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
              Diff_Kauf < 0 ~ sprintf("<span style='color:#e53935;'>-%s €</span>", format(abs(Diff_Kauf), big.mark = ".", decimal.mark = ",")),
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
      ) %>%
      filter(Diff_Prozent <= 50)
    
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
        width = 0.7, linetype = "dashed", color = "grey30", linewidth = 0.6,
        inherit.aes = FALSE
      ) +
      # Punktewolke
      geom_beeswarm(
        data = subset(plotdata_beeswarm, n_pts > 1),
        cex = 2, size = 3, alpha = 0.8, priority = "random"
      ) +
      # Einzelpunkte
      geom_point(
        data = subset(plotdata_beeswarm, n_pts == 1),
        size = 3, alpha = 0.8, shape = 21
      ) +
      # Ø-Wert je Bieter beschriften
      geom_text(
        data = means,
        aes(x = Bieter, y = Mean, label = round(Mean, 1)),
        color = "black", fontface = "bold", size = 6, vjust = -0.7,
        inherit.aes = FALSE
      ) +
      # Ø je MW-Klasse
      geom_hline(
        data = means_klasse, aes(yintercept = Mean),
        color = "#d62728", linewidth = 1.2, linetype = "solid"
      ) +
      geom_text(
        data = means_klasse,
        aes(x = Inf, y = Mean, label = paste0("Ø ", round(Mean, 1), " %")),
        hjust = 1.1, vjust = -0.7, color = "#d62728", fontface = "bold", size = 6,
        inherit.aes = FALSE
      ) +
      geom_text(
        data = plotdata_ueber,
        aes(x = -Inf, y = Inf, label = paste0(pct_ueber, "% (n=", n_total, ")")),
        hjust = -0.1, vjust = 1.2, color = "#333", fontface = "bold", size = 6,
        inherit.aes = FALSE
      ) +
      facet_grid(. ~ MW_Klasse, scales = "free_y") +
      labs(
        title = "Gebotsabweichungen je Konkurrent und MW-Klasse",
        x = "Bieter", y = "Abweichung vom MW Vortag (%)"
      ) +
      scale_color_brewer(palette = "Paired") +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
  })
  
  
  # Container-Umschaltung
  output$mwclass_container <- renderUI({
    if (plotly_on()) {
      plotlyOutput("mwclassplot", height = 700)
    } else {
      plotOutput("mwclassplot_static", height = 700)
    }
  })
  
  # Statischer Plot immer verfügbar
  output$mwclassplot_static <- renderPlot({
    p <- mwclass_plot_obj()
    print(p)
  })
  
  # Button: Plotly aktivieren, ggf. installieren und laden, dann Renderer setzen
  observeEvent(input$toggle_plotly, {
    if (!plotly_on()) {
      # aktivieren
      if (!requireNamespace("plotly", quietly = TRUE)) {
        install.packages("plotly", repos = "https://cran.rstudio.com")
      }
      library(plotly)
      plotly_on(TRUE)
      updateActionButton(session, "toggle_plotly", label = "Zurück zu statischem Plot")
      output$mwclassplot <- renderPlotly({
        p <- mwclass_plot_obj()
        ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
      })
    } else {
      # deaktivieren
      plotly_on(FALSE)
      updateActionButton(session, "toggle_plotly", label = "Interaktiv aktivieren (Plotly)")
    }
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
      geom_line(size = 1.2, na.rm = TRUE  ) +
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
    req(data_all())
    transfers <- data_all()$transfers
    
    # --- Events für Käufe und Verkäufe ---
    einkaeufe <- transfers %>%
      filter(tolower(Hoechstbietender) != "computer") %>%
      transmute(Spieler,
                Besitzer = Hoechstbietender,
                Datum,
                Preis = as.numeric(Hoechstgebot),
                type = "buy")
    
    verkaeufe <- transfers %>%
      filter(!is.na(Besitzer), tolower(Besitzer) != tolower(Hoechstbietender)) %>%
      transmute(Spieler,
                Besitzer,
                Datum,
                Preis = as.numeric(Hoechstgebot),
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
    
    abs_max <- max(abs(df$Gesamtgewinn), na.rm = TRUE)
    lim_min <- -abs_max - 1e6
    lim_max <-  abs_max + 1e6
    
    ggplot(df, aes(x = reorder(Besitzer, Gesamtgewinn), y = Gesamtgewinn, fill = Gesamtgewinn > 0)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(
          label = format(round(Gesamtgewinn, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE),
          hjust = ifelse(Gesamtgewinn > 0, -0.1, 1.1)
        ),
        position = position_dodge(width = 1)
      ) +
      scale_fill_manual(values = c("TRUE" = "#2b9348", "FALSE" = "#d00000")) +
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
  
  
  ## ---- Flip-Verlauf kummuliert über Zeit ----
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
  
  
  ## ---- FLIP-Kumuliert je Flip-Art ----
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
  
  ## ---- Zeitbalken-Fip
  ### ---- Kauf-Diff Zeitlinien je Manager (Balken am Kaufdatum) ----
  output$kaufdiff_zeitlinien <- renderPlot({
    req(teams_df, gesamt_mw_roh, transfers)
    
    manager_list <- sort(unique(teams_df$Manager))
    
    # Marktwert aktuell je Spieler
    mw_aktuell <- gesamt_mw_roh %>%
      group_by(Spieler) %>%
      filter(Datum == max(Datum, na.rm = TRUE)) %>%
      summarise(Marktwert_aktuell = first(Marktwert), .groups = "drop")
    
    # letzter Kauf je (Spieler, Manager) inkl. Kaufdatum
    kaufpreise <- transfers %>%
      group_by(Spieler, Hoechstbietender) %>%
      arrange(desc(Datum)) %>%
      slice(1) %>%
      transmute(
        Spieler,
        Manager = Hoechstbietender,
        Kaufdatum = as.Date(Datum),
        Kaufpreis = as.numeric(Hoechstgebot)
      )
    
    # Plot-Daten: aktueller MW + letzter Kauf pro Manager-Kader
    plot_df <- teams_df %>%
      select(Manager, Spieler) %>%
      left_join(mw_aktuell, by = "Spieler") %>%
      left_join(kaufpreise, by = c("Manager", "Spieler")) %>%
      mutate(
        Diff_Kauf = ifelse(is.na(Kaufpreis), NA_real_, Marktwert_aktuell - Kaufpreis),
        pos = Diff_Kauf >= 0
      ) %>%
      filter(!is.na(Kaufpreis), !is.na(Diff_Kauf)) %>%
      mutate(
        Manager = factor(Manager, levels = manager_list),
        Kaufdatum = as.Date(Kaufdatum)
      )
    
    req(nrow(plot_df) > 0)
    
    # Format-Helfer
    fmt_eur <- function(x) format(x, big.mark = ".", decimal.mark = ",")
    plot_df$label <- ifelse(
      plot_df$Diff_Kauf >= 0,
      paste0("+", fmt_eur(round(plot_df$Diff_Kauf, 0)), " €"),
      paste0("-", fmt_eur(abs(round(plot_df$Diff_Kauf, 0))), " €")
    )
    
    ggplot(plot_df, aes(x = Kaufdatum, y = Diff_Kauf, group = Spieler, fill = pos)) +
      # Balken am Kaufdatum, nach Spieler leicht gedodged, damit Überlagerung reduziert wird
      geom_col(width = 12, position = position_dodge2(width = 12, preserve = "single"), show.legend = FALSE) +
      # Null-Linie
      geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
      # Punkt am Baseline exakt am Kaufdatum
      geom_point(aes(y = 0), size = 1.8, alpha = 0.9,
                 position = position_dodge2(width = 12, preserve = "single")) +
      # Wert-Label ober-/unterhalb des Balkens
      geom_text(
        aes(label = label, vjust = ifelse(Diff_Kauf >= 0, -0.2, 1.2)),
        size = 3.5,
        position = position_dodge2(width = 12, preserve = "single")
      ) +
      scale_fill_manual(values = c(`TRUE` = "#388e3c", `FALSE` = "#e53935")) +
      facet_grid(Manager ~ ., scales = "free_y", switch = "y") +
      scale_y_continuous(
        labels = function(x) paste0(fmt_eur(x), " €"),
        expand = expansion(mult = c(0.05, 0.15))
      ) +
      scale_x_date(date_breaks = "1 week", date_labels = "%d.%m.") +
      scale_fill_manual(values = c(`TRUE` = "#388e3c", `FALSE` = "#e53935"), guide = "none") +
      labs(x = "", y = "") +
      theme_minimal(base_size = 14) +
      theme(
        strip.placement = "outside",
        strip.text.y.left = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank()
      )
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
  
  ## ---- Flip-Historie je Spieler ----
  output$flip_player_table <- renderDT({
    req(input$flip_player_select)
    today <- Sys.Date()
    mw_today <- ap_df %>% 
      filter(Datum == today) %>% 
      select(Spieler, Marktwert)
    
    flip_data() %>%
      filter(Besitzer == input$flip_player_select) %>%
      left_join(mw_today, by = "Spieler") %>% 
      select(Verkaufsdatum, Spieler, Einkaufsdatum, Einkaufspreis, Verkaufspreis, Marktwert, Gewinn) %>%
      arrange(desc(Verkaufsdatum)) %>%
      datatable(
        options = list(dom = "t",
                       paging = FALSE),
        colnames = c("Verkaufsdatum", "Spieler", "Kaufdatum", "Einkaufspreis", "Verkaufspreis", "Aktueller Marktwert", "Gewinn/Verlust (€)")
      )
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
  kapital_df_reactive <- reactive({
    dat <- data_all()
    transfers <- dat$transfers
    
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
  
  # ---- Flip-Saldo je Manager (Gewinne + Verluste) ----
  flip_gewinne_df <- reactive({
    req(flip_data())
    flip_data() %>%
      mutate(Gewinn = as.numeric(Gewinn)) %>%
      filter(!is.na(Gewinn)) %>%
      group_by(Besitzer) %>%
      summarise(Flip_Saldo = sum(Gewinn, na.rm = TRUE), .groups = "drop") %>%
      rename(Manager = Besitzer)
  })
  
  # ---- Gesamtsumme pro Manager: Flip + Transaktionen + Punkte-Bonus ----
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
  
  # ---- Einfacher Balkenplot: Gesamtsumme, sortiert absteigend, Farben nach Manager (Paired), Textgröße 16 ----
  output$kapital_gewinn_plot <- renderPlot({
    df <- kapital_summe_df()
    req(nrow(df) > 0)
    
    # Sortierung
    df <- df %>%
      arrange(desc(Gesamt)) %>%
      mutate(Manager_x   = factor(Manager, levels = Manager),
             Manager_fill= factor(Manager, levels = sort(unique(Manager))))
    
    # Achsenlimits berechnen
    y_min <- min(df$Gesamt, na.rm = TRUE) - 1e6
    y_max <- max(df$Gesamt, na.rm = TRUE) + 1e6
    
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
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "none"
      )
  })
  
  
}


# SHINY APP STARTEN
shinyApp(ui, server)

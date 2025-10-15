# bundesliga_spielplan_und_events_2025_26.R
# Benötigt: R >= 4.1; Pakete: pdftools, stringr, dplyr, tidyr, readr, lubridate, purrr, tibble
# Optional für ICS: ical (CRAN). Wenn nicht installiert, werden ICS übersprungen.

suppressPackageStartupMessages({
  library(pdftools)
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

# --------------------------
# Konfiguration
# --------------------------
pdf_url <- "https://media.dfl.de/sites/2/2025/06/N2P9nxxrw4_Bundesliga_Spielplan_2025_2026.pdf"

# ICS-Quellen (Beispiele eintragen, z.B. UCL/EL/DFB/NT-Spiele)
ics_urls <- c(
  # "https://.../uefa_champions_league_2025_26.ics",
  # "https://.../uefa_europa_league_2025_26.ics",
  # "https://.../fifa_international_windows_2025_26.ics"
)

# Ausgaben
out_csv_spielplan <- "bundesliga_2025_26_spielplan.csv"
out_csv_events    <- "fussball_events_merged_2025_26.csv"

# CSV-Delimiter
delim <- ";"

# --------------------------
# Download PDF
# --------------------------
tmp_pdf <- tempfile(fileext = ".pdf")
download.file(pdf_url, tmp_pdf, mode = "wb", quiet = TRUE)

# --------------------------
# Heuristische Parser
# --------------------------
txt <- pdftools::pdf_text(tmp_pdf)

norm_ws <- function(x) gsub("\\s+", " ", trimws(x))

is_spieltag_header <- function(line) {
  grepl("^\\s*[0-9]{1,2}\\s*\\.\\s*Spieltag\\b", line)
}

extract_spieltag_nr <- function(line) {
  as.integer(sub("^\\s*([0-9]{1,2}).*$", "\\1", line))
}

is_datumzeile <- function(line) {
  # 22.08.2025  oder 22.–24.08.2025  oder 22.08.2025, 20:30 Uhr
  grepl("([0-3]?\\d\\.[01]?\\d\\.20\\d{2})", line)
}

is_match_line <- function(line) {
  # "Heim – Gast" / "Heim - Gast" / "Heim — Gast"
  grepl("\\s[–—-]\\s", line)
}

split_match <- function(line) {
  parts <- strsplit(line, "\\s[–—-]\\s", perl = TRUE)[[1]]
  if (length(parts) == 2) {
    tibble(heim = norm_ws(parts[1]), gast = norm_ws(parts[2]))
  } else {
    tibble(heim = NA_character_, gast = NA_character_)
  }
}

extract_time <- function(line) {
  m <- str_match(line, "([01]?\\d|2[0-3]):[0-5]\\d")
  if (!is.na(m[,1])) m[,1] else NA_character_
}

parse_single_date <- function(s) {
  m <- str_match(s, "([0-3]?\\d\\.[01]?\\d\\.20\\d{2})")
  if (is.na(m[,2])) return(NA_Date_)
  suppressWarnings(lubridate::dmy(m[,2]))
}

rows <- list()
current_spieltag <- NA_integer_
current_date_raw <- NA_character_
current_kickoff  <- NA_character_

for (page in seq_along(txt)) {
  lines <- unlist(strsplit(txt[[page]], "\n", fixed = TRUE))
  lines <- norm_ws(lines)
  lines <- lines[nchar(lines) > 0]
  
  for (i in seq_along(lines)) {
    ln <- lines[i]
    
    if (is_spieltag_header(ln)) {
      current_spieltag <- extract_spieltag_nr(ln)
      next
    }
    
    if (is_datumzeile(ln)) {
      current_date_raw <- ln
      current_kickoff  <- extract_time(ln)
      next
    }
    
    if (is_match_line(ln) && !is.na(current_spieltag)) {
      mg <- split_match(ln)
      rows[[length(rows)+1]] <- tibble(
        spieltag   = current_spieltag,
        datum_raw  = current_date_raw,
        anstoss    = current_kickoff,
        heim       = mg$heim,
        gast       = mg$gast,
        quelle     = "DFL_PDF"
      )
    }
  }
}

rows <- purrr::compact(rows)
if (length(rows) == 0L) {
  stop("Parser fand 0 Spiele. Regex für Spieltag/Datum/Match prüfen.")
}

spielplan_df <- dplyr::bind_rows(rows)
need <- c("spieltag", "datum_raw", "anstoss", "heim", "gast", "quelle")
miss <- setdiff(need, names(spielplan_df))
if (length(miss)) stop(sprintf("Fehlende Spalten nach Parsing: %s", paste(miss, collapse = ", ")))

spielplan_df <- spielplan_df %>%
  mutate(
    spieltag = as.integer(spieltag),
    datum    = parse_single_date(datum_raw),
    anstoss  = ifelse(!is.na(anstoss), anstoss, NA_character_)
  ) %>%
  arrange(spieltag, heim)

# --------------------------
# Optional: ICS einlesen
# --------------------------
read_ics_safely <- function(url) {
  if (!requireNamespace("ical", quietly = TRUE)) {
    message("Hinweis: Paket 'ical' nicht installiert. ICS werden übersprungen.")
    return(tibble())
  }
  tf <- tempfile(fileext = ".ics")
  utils::download.file(url, tf, mode = "wb", quiet = TRUE)
  df <- tryCatch(ical::ical_parse_df(tf), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(tibble())
  
  to_dt <- function(x) suppressWarnings(lubridate::as_datetime(x, tz = "UTC"))
  loc_tz <- "Europe/Berlin"
  
  as_tibble(df) %>%
    transmute(
      spieltag   = NA_integer_,
      datum_raw  = NA_character_,
      datum      = as_date(with_tz(to_dt(DTSTART), tzone = loc_tz)),
      anstoss    = {
        t <- with_tz(to_dt(DTSTART), tzone = loc_tz)
        out <- format(t, "%H:%M")
        ifelse(is.na(out) | out == "NA", NA_character_, out)
      },
      heim       = NA_character_,
      gast       = NA_character_,
      bewerb     = dplyr::coalesce(CATEGORIES, "ICS"),
      ereignis   = SUMMARY,
      quelle     = "ICS"
    )
}

ics_df <- purrr::map_dfr(ics_urls, read_ics_safely)

# --------------------------
# Zusammenführen + Export
# --------------------------
bundesliga_events <- spielplan_df %>%
  mutate(
    bewerb  = "Bundesliga",
    ereignis = paste0("Spieltag ", spieltag, ": ", heim, " – ", gast)
  ) %>%
  select(spieltag, datum_raw, datum, anstoss, bewerb, ereignis, heim, gast, quelle)

all_events <- bind_rows(bundesliga_events, ics_df) %>%
  arrange(coalesce(datum, as_date("2100-01-01")), bewerb, ereignis)

# Exporte (mit Semikolon)
readr::write_delim(bundesliga_events, out_csv_spielplan, delim = delim, na = "")
readr::write_delim(all_events, out_csv_events, delim = delim, na = "")

message("Fertig.")
message(sprintf("Bundesliga CSV: %s", normalizePath(out_csv_spielplan)))
message(sprintf("Alle Events CSV: %s", normalizePath(out_csv_events)))

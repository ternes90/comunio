# gpt_player.py – Websuche-only, Endnoten mit Voll-URLs, Recency-Validierung, 2-Pass, gpt-4.1

import re
from datetime import date, datetime
from openai import OpenAI

client = OpenAI()

DEFAULT_MODEL = "gpt-4.1"
CSV_HEADER = ["Spieler", "Verein", "Info", "Stammplatzwahrscheinlichkeit", "Potenzial", "Bundesliga_2025_26"]

# ---------- Utils ----------

DATE_PAT = re.compile(r"(\d{4}-\d{2}-\d{2}|\d{1,2}\.\d{1,2}\.\d{4})")

def _clamp_num(x: str) -> str:
    try:
        v = float(str(x).replace(",", "."))
    except Exception:
        return "0.0"
    if v < 0: v = 0.0
    if v > 3: v = 3.0
    return f"{v:.1f}"

def _norm_spaces(s: str) -> str:
    return " ".join((s or "").split())

def _has_refs_and_sources(info: str) -> bool:
    if "Quellen:" not in info: return False
    if "[" not in info or "]" not in info: return False
    # mind. eine Referenznummer im Fließtext
    if not re.search(r"\[\d+\]", info): return False
    # mind. eine URL in der Quellenliste
    if not re.search(r"https?://", info): return False
    return True

def _dates_in_sources(info: str):
    # extrahiert Datumsangaben aus der „Quellen:“‑Liste
    tail = info.split("Quellen:", 1)[-1]
    return DATE_PAT.findall(tail)

def _date_str_to_dt(s: str):
    for fmt in ("%Y-%m-%d", "%d.%m.%Y"):
        try:
            return datetime.strptime(s, fmt).date()
        except Exception:
            pass
    return None

def _within_days(d: date, days: int) -> bool:
    try:
        return (date.today() - d).days <= days
    except Exception:
        return False

def _parse_csv_like(spieler_in: str, verein_in: str, txt: str, prefer_input_club: bool) -> list:
    header_join = ";".join(CSV_HEADER)
    if not txt or ";" not in txt:
        return [spieler_in, verein_in or "", "Unklar", "0.0", "0.0", "0.0"]

    lines = [ln.strip() for ln in txt.splitlines() if ln.strip()]
    if not lines:
        return [spieler_in, verein_in or "", "Unklar", "0.0", "0.0", "0.0"]

    data = lines[1] if lines[0] == header_join and len(lines) >= 2 else lines[0]
    parts = [p.strip() for p in data.split(";")]
    if len(parts) < 6:
        parts = (parts + [""] * 6)[:6]

    sp_model, ve_model = parts[0] or "", parts[1] or ""
    info = _norm_spaces(" ".join(parts[2:-3])) if len(parts) > 5 else "Unklar"
    n1, n2, n3 = parts[-3], parts[-2], parts[-1]

    sp_out = spieler_in or sp_model or "Unbekannt"
    if prefer_input_club and verein_in:
        ve_out = verein_in
    else:
        ve_out = ve_model if ve_model else (verein_in or "")

    return [
        sp_out,
        ve_out,
        _norm_spaces(info) if info else "Unklar",
        _clamp_num(n1),
        _clamp_num(n2),
        _clamp_num(n3),
    ]

# ---------- Prompt ----------

def _build_prompt(spieler: str, verein: str, today_iso: str, days_limit: int | None,
                  allow_player_only: bool, allow_infer_club: bool, stricter: bool=False) -> str:
    scope = ""
    if days_limit:
        base = f"Bevorzuge strikt Inhalte aus den letzten {days_limit} Tagen. "
        add  = "Verwerfe ältere Inhalte, wenn frische verfügbar sind. "
        scope = base + add

    if verein:
        search = (f"Führe eine gezielte Websuche zu '{spieler} {verein}' durch. ")
        if allow_player_only:
            search += f"Falls keine verwertbaren Treffer erscheinen, erweitere auf '{spieler}'. "
    else:
        search = f"Führe eine gezielte Websuche zu '{spieler}' durch. "
        if allow_infer_club:
            search += "Ermittele den aktuellen Verein und trage ihn in 'Verein' ein. "

    citations = (
        "Verwende KEINE Markdown-Links. "
        "Schließe jeden faktischen Satz mit einer durchnummerierten Referenz in eckigen Klammern ab, z. B. [1], [2]. "
        "Am Ende der Info füge eine neue Zeile 'Quellen:' hinzu, gefolgt von einer nummerierten Liste aller vollständigen URLs "
        "in der Reihenfolge der Referenzen. Verwende nach Möglichkeit mindestens zwei unterschiedliche Quellen. "
        "Hänge an jede URL ein Veröffentlichungsdatum an, z. B. ' – veröffentlicht am YYYY-MM-DD'. "
    )

    strict_tail = ""
    if stricter:
        # explizite, harte Anforderungen wenn der erste Durchlauf nicht genügt
        strict_tail = (
            "Wenn du keine Quelle innerhalb des Zeitfensters findest, markiere dies klar durch das Datum in der Quellenliste. "
            "Gib niemals veraltete Aussagen ohne Datum aus. "
        )

    return (
        f"Gib eine CSV mit genau diesen Spalten zurück:\n{';'.join(CSV_HEADER)}\n"
        f"Erzeuge genau EINE Datenzeile für: {spieler} ({verein}). "
        f"{search}{scope}"
        "Info = ausführliche deutsche Kurzprosa (max. 200 Wörter), ohne Semikolons. "
        "Beantworte explizit: 1) Rolle/Status (Stamm, Rotation, Joker), "
        "2) Einsatzprognose nächste 4–6 Wochen, 3) Trainerstimme/Einordnung, "
        "4) Verletzungslage, 5) Wechselwahrscheinlichkeit. "
        f"{citations}"
        "Zahlen nur in den letzten drei Spalten, Werte 0–3 (Dezimal erlaubt). "
        f"Stand: {today_iso}. "
        "Wenn gar keine verwertbaren Infos gefunden werden: Info='Unklar'; alle Zahlen=0. "
        "Antworte NUR mit Kopfzeile + EINER Datenzeile, sonst nichts. "
        f"{strict_tail}"
    )

def _ask_gpt(prompt: str, model: str) -> str:
    tools = [{
        "type": "web_search_preview",
        "user_location": {"type": "approximate", "country": "DE"},
        "search_context_size": "high"
    }]
    try:
        resp = client.responses.create(
            model=model,
            input=prompt,
            tools=tools,
            tool_choice={"type": "web_search_preview"},
            max_output_tokens=3200,
            store=False
        )
        return (resp.output_text or "").strip()
    except Exception:
        return ""

# ---------- Public API ----------

def query_player(spieler: str, verein: str, model: str = None):
    mdl = model or DEFAULT_MODEL
    today_iso = date.today().isoformat()
    header = ";".join(CSV_HEADER)
    verein_clean = (verein or "").strip()

    # Pass 1: 30 Tage, Spieler+Verein, Spieler-only erlaubt
    txt = _ask_gpt(
        _build_prompt(spieler, verein_clean, today_iso, days_limit=30, allow_player_only=True, allow_infer_club=True),
        mdl
    )

    # Validierung. Wenn fehlende Endnoten/Refs → strenger Re-Prompt mit gleichen Parametern
    def needs_retry(t: str, days: int | None) -> bool:
        if not t or ";" not in t: return True
        parts = _parse_csv_like(spieler, verein_clean, t, prefer_input_club=bool(verein_clean))
        info = parts[2]
        if not _has_refs_and_sources(info): return True
        if days:
            ds = [_date_str_to_dt(s) for s in _dates_in_sources(info)]
            if ds and any(d and _within_days(d, days) for d in ds):
                return False  # ok, hat mind. ein frisches Datum
            # keine frischen Datumsangaben erkannt -> retry
            return True
        return False

    if needs_retry(txt, 30):
        txt = _ask_gpt(
            _build_prompt(spieler, verein_clean, today_iso, days_limit=30, allow_player_only=True, allow_infer_club=True, stricter=True),
            mdl
        )

    # Pass 2: 180 Tage, wenn immer noch unbrauchbar
    if needs_retry(txt, 30) or "Unklar" in txt:
        txt = _ask_gpt(
            _build_prompt(spieler, verein_clean, today_iso, days_limit=180, allow_player_only=True, allow_infer_club=True),
            mdl
        )
        if needs_retry(txt, 180):
            txt = _ask_gpt(
                _build_prompt(spieler, verein_clean, today_iso, days_limit=180, allow_player_only=True, allow_infer_club=True, stricter=True),
                mdl
            )

    if not txt or ";" not in txt:
        txt = f"{header}\n{spieler};{verein_clean};Unklar;0.0;0.0;0.0"

    parts = _parse_csv_like(spieler, verein_clean, txt, prefer_input_club=bool(verein_clean))

    return {
        "Spieler": parts[0],
        "Verein": parts[1],
        "Info": parts[2],
        "Stammplatzwahrscheinlichkeit": parts[3],
        "Potenzial": parts[4],
        "Bundesliga_2025_26": parts[5]
    }

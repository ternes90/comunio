# gpt_player.py – Websuche-only, frische & vertrauenswürdige Quellen
# Liefert entweder strukturierte CSV-ähnliche Dicts (query_player)
# oder ein frei formatiertes Text-Ergebnis für Shiny (query_player_text)

import re
from datetime import date, datetime
from openai import OpenAI

client = OpenAI()

DEFAULT_MODEL = "gpt-4.1"
CSV_HEADER = ["Spieler", "Verein", "Info", "Stammplatzwahrscheinlichkeit", "Potenzial", "Bundesliga_2025_26"]

# ---- Trust / Filter ----
TRUSTED_DOMAINS = {
    "bundesliga.com", "dfb.de", "dfl.de", "kicker.de", "transfermarkt.de",
    "ruhrnachrichten.de", "waz.de", "bild.de",
    "bvb.de", "fc-bayern.com", "rbleipzig.com", "svw.de", "werder.de", "tsg-hoffenheim.de",
    "eintracht.de", "fcn.de", "vfb.de", "herthabsc.com", "borussia.de", "schalke04.de",
    "f95.de", "fcaugsburg.de", "bochum1848.de", "sv98.de", "svd98.de", "hsv.de", "stpauli.com",
}
DISCOURAGED = {"twitter.com", "x.com", "instagram.com", "tiktok.com", "facebook.com", "reddit.com", "youtube.com", "wikipedia.org"}

# ---------- Regex ----------
DATE_PAT = re.compile(r"(\d{4}-\d{2}-\d{2}|\d{1,2}\.\d{1,2}\.\d{4})")
MD_LINK_PAT = re.compile(r"\[[^\]]{1,80}\]\(https?://[^)]+?\)")

# ---------- Utils ----------
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
    if not re.search(r"\[\d+\]", info): return False
    if not re.search(r"https?://", info): return False
    return True

def _contains_markdown_link(info: str) -> bool:
    return bool(MD_LINK_PAT.search(info or ""))

def _dates_in_sources(info: str):
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

def _extract_urls(info: str):
    tail = info.split("Quellen:", 1)[-1] if "Quellen:" in (info or "") else info or ""
    return re.findall(r"https?://[^\s\]]+", tail)

def _domain(u: str) -> str:
    try:
        host = re.sub(r"^https?://", "", u).split("/")[0].lower()
        if host.startswith("www."): host = host[4:]
        return host
    except Exception:
        return ""

def _trusted_mix(urls: list[str]) -> bool:
    if not urls: return False
    hosts = {_domain(u) for u in urls}
    discouraged = hosts & DISCOURAGED
    trusted = any(any(td in h for td in TRUSTED_DOMAINS) for h in hosts)
    if trusted: return True
    return False if hosts and hosts == discouraged else trusted

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
    info_raw = _norm_spaces(" ".join(parts[2:-3])) if len(parts) > 5 else "Unklar"
    n1, n2, n3 = parts[-3], parts[-2], parts[-1]

    sp_out = spieler_in or sp_model or "Unbekannt"
    ve_out = verein_in if (prefer_input_club and verein_in) else (ve_model if ve_model else (verein_in or ""))
    info_out = info_raw if info_raw else "Unklar"

    if info_out.strip().lower() == "unklar":
        n1, n2, n3 = "0.0", "0.0", "0.0"

    return [sp_out, ve_out, info_out, _clamp_num(n1), _clamp_num(n2), _clamp_num(n3)]

def _score_floor(info: str, n1: str, n2: str, n3: str) -> tuple[str, str, str]:
    info_l = _norm_spaces(info or "").lower()
    if info_l == "unklar":
        return ("0.0", "0.0", "0.0")

    def _to_num(s, default):
        try:
            return max(0.0, min(3.0, float(str(s).replace(",", "."))))
        except:
            return default

    a = _to_num(n1, 0.0); b = _to_num(n2, 0.0); c = _to_num(n3, 0.0)
    if any(x > 0.0 for x in (a, b, c)):
        return (f"{a:.1f}", f"{b:.1f}", f"{c:.1f}")

    stamm = 1.0; pot = 1.0; bl = 1.0; keys = info_l
    if any(k in keys for k in ["klarer stamm","gesetzt","startelf","stammspieler","erste elf"]):
        stamm = 2.5; bl = max(bl, 2.0)
    elif any(k in keys for k in ["rotation","rotiert","konkurrenzkampf","teilt sich","wechselt mit"]):
        stamm = 1.5
    elif any(k in keys for k in ["joker","wechselspieler","kurzeinsätze","späte einsätze"]):
        stamm = 1.0

    if any(k in keys for k in ["verletzung","verletzt","ausfall","fehl","wochen","monate","rehabilitation","aufbautraining"]):
        stamm = min(stamm, 0.5); bl = min(bl, 0.5)
    if any(k in keys for k in ["trainiert wieder","kehrt zurück","voll im training","einsatzbereit","verletzungsfrei","fit"]):
        pot = max(pot, stamm + 0.5); bl = max(bl, stamm)

    if any(k in keys for k in ["trainer lobt","trainer betont","wichtiger spieler","plant als","schlüsselfigur"]):
        stamm = max(stamm, 2.0); pot = max(pot, 2.0)

    if any(k in keys for k in ["wechselwahrscheinlichkeit hoch","steht vor wechsel","abgang möglich"]):
        bl = min(bl, 1.0)
    if any(k in keys for k in ["verlängert vertrag","langfristig geplant"]):
        bl = max(bl, 2.0)

    return (f"{stamm:.1f}", f"{pot:.1f}", f"{bl:.1f}")

# ---------- Prompt (CSV-Modus, bestehend) ----------
PREFERRED_SOURCES_TEXT = (
    "Bevorzuge Vereinsseiten, bundesliga.com/de, dfb.de, kicker.de, transfermarkt.de sowie lokale Qualitätsmedien "
    "(z. B. Ruhr Nachrichten/WAZ in Dortmund). Meide Social-Media-Posts und reine Aggregatoren."
)

def _build_prompt_csv(spieler: str, verein: str, today_iso: str, days_limit: int | None,
                      allow_player_only: bool, allow_infer_club: bool, stricter: bool=False) -> str:
    scope = ""
    if days_limit:
        scope = (
            f"Fokussiere Inhalte der letzten {days_limit} Tage. "
            "Wenn nur ältere, aber verlässliche Quellen existieren, nutze sie mit Datum. "
        )

    if verein:
        search = f"Führe eine Websuche zu '{spieler} {verein}' durch. "
        if allow_player_only:
            search += f"Falls nötig, erweitere auf '{spieler}'. "
    else:
        search = f"Führe eine Websuche zu '{spieler}' durch. "
        if allow_infer_club:
            search += "Ermittele den aktuellen Verein und trage ihn in 'Verein' ein. "

    citations = (
        "Verwende KEINE Markdown‑Links. "
        "Beende jeden faktischen Satz mit Quellenangaben …. "
        "Schließe die Info mit einer neuen Zeile 'Quellen:' ab und liste die URLs in Referenzreihenfolge, "
        "jeweils mit Datum 'YYYY-MM-DD' (oder 'zuletzt aktualisiert'). Mindestens zwei unterschiedliche Domains, wenn möglich. "
    )

    scale = (
        "Bewerte differenziert in 0.5‑Schritten. "
        "Stammplatz: 0=no data, 0.5=kaum Chance, 1.0=Joker/Backup, 1.5=Rotation selten, 2.0=Rotation häufig, "
        "2.5=wahrscheinlicher Starter, 3.0=klarer Starter. "
        "Potenzial analog für 6–8 Wochen. "
        "Bundesliga_2025_26: 0=keine Einsätze, 1=vereinzelte Kurzeinsätze, 2=regelmäßige Einsätze, 3=Stamm. "
        "Vermeide drei identische Werte ohne starke Evidenz."
    )

    guard = (
        "Gib 'Unklar' NUR aus, wenn keine verwertbaren Quellen auffindbar sind; dann müssen die drei Werte 0.0/0.0/0.0 sein. "
        "Info ohne Semikolons, max. 500 Wörter, deutsch."
    )

    strict_tail = "Auch wenn nur ältere Inhalte vorliegen: gib sie mit Datum an und bewerte trotzdem. " if stricter else ""

    return (
        f"Gib eine CSV mit genau diesen Spalten zurück:\n{';'.join(CSV_HEADER)}\n"
        f"Erzeuge genau EINE Datenzeile für: {spieler} ({verein}). "
        f"{search}{scope}{PREFERRED_SOURCES_TEXT} "
        "Beantworte in der Info: 1) Rolle/Status, 2) Einsatzprognose 4–6 Wochen, 3) Trainerstimme/Einordnung, "
        "4) Verletzungslage, 5) Wechselwahrscheinlichkeit. "
        f"{citations}{scale}{guard} "
        f"Stand: {today_iso}. Antworte NUR mit Kopfzeile + EINER Datenzeile. {strict_tail}"
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
            temperature=0.25,
            max_output_tokens=5000,
            store=False
        )
        return (resp.output_text or "").strip()
    except Exception:
        return ""

# ---------- Öffentliche API (CSV-ähnlich) ----------
def query_player(spieler: str, verein: str, model: str = None):
    mdl = model or DEFAULT_MODEL
    today_iso = date.today().isoformat()
    header = ";".join(CSV_HEADER)
    verein_clean = (verein or "").strip()

    def fresh_enough(info: str, days: int) -> bool:
        ds = [_date_str_to_dt(s) for s in _dates_in_sources(info)]
        return any(d and _within_days(d, days) for d in ds) if ds else False

    def needs_retry(t: str, fresh_days: int) -> bool:
        if not t or ";" not in t:
            return True
        parts = _parse_csv_like(spieler, verein_clean, t, prefer_input_club=bool(verein_clean))
        info = parts[2]
        if info.strip().lower() == "unklar":
            return True
        if not _has_refs_and_sources(info):
            return True
        if _contains_markdown_link(info):
            return True
        urls = _extract_urls(info)
        if not _trusted_mix(urls):
            return True
        if not fresh_enough(info, fresh_days):
            return True
        return False

    # Pass 1: 60 Tage
    txt = _ask_gpt(
        _build_prompt_csv(spieler, verein_clean, today_iso, days_limit=60, allow_player_only=True, allow_infer_club=True),
        mdl
    )
    if not needs_retry(txt, 60):
        parts = _parse_csv_like(spieler, verein_clean, txt, prefer_input_club=bool(verein_clean))
    else:
        # Pass 2: 270 Tage
        txt = _ask_gpt(
            _build_prompt_csv(spieler, verein_clean, today_iso, days_limit=270, allow_player_only=True, allow_infer_club=True, stricter=True),
            mdl
        )
        if not txt or ";" not in txt:
            txt = f"{header}\n{spieler};{verein_clean};Unklar;0.0;0.0;0.0"
        parts = _parse_csv_like(spieler, verein_clean, txt, prefer_input_club=bool(verein_clean))

    s1, s2, s3 = _score_floor(parts[2], parts[3], parts[4], parts[5])

    return {
        "Spieler": parts[0],
        "Verein": parts[1],
        "Info": parts[2],
        "Stammplatzwahrscheinlichkeit": s1,
        "Potenzial": s2,
        "Bundesliga_2025_26": s3
    }

# ---------- Textmodus für Shiny ----------
def _build_prompt_text(spieler: str, verein: str) -> str:
    today = date.today().isoformat()
    header = "Spieler\nVerein\n"
    return (
        "Gib GENAU dieses Format zurück:\n"
        f"{header}\n"
        f">>> Die erste Zeile muss mit '{spieler}' beginnen. "
        f">>> Die zweite Zeile muss mit '{verein}' beginnen. "
        "Falls der Verein leer/unsicher ist, trage den korrekt ermittelten aktuellen Verein dort ein. <<<\n"
        "In 'Info' kurz und faktenbasiert: Rolle/Status; Einsatz 4–6 Wochen; Trainerstimme; Verletzung; Wechsel. "
        "Keine Semikolons in 'Info'. KEINE Markdown-Links. Jede Tatsachen-Aussage mit [1], [2] … belegen. "
        "Am Ende von 'Info' eine neue Zeile 'Quellen:' und die URLs mit Datum YYYY-MM-DD, mind. 2 Domains bevorzugt "
        "(Vereinsseiten, bundesliga.com, dfb.de, kicker.de, transfermarkt.de, lokale Qualitätsmedien). "
        "Meide Social Media.\n\n"
        "Danach GENAU drei neue Zeilen:\n"
        "Stammplatz: <Zahl 0.0–3.0 in 0.5-Schritten>\n"
        "Potenzial: <Zahl 0.0–3.0 in 0.5-Schritten>\n"
        "Wechselwahrscheinlichkeit: <Zahl 0.0–3.0 in 0.5-Schritten>\n\n"
        "Suchfenster fokussiert die letzten 60 Tage, sonst ältere verlässliche Quellen mit Datum. "
        f"Stand: {today}. Antworte NUR in diesem Format."
    )

def _basic_text_sanity(sp: str, ve: str, txt: str) -> str:
    t = (txt or "").strip()
    if not t:
        return f"{sp}\n{ve}\nInfo: Unklar\nQuellen:\nStammplatz: 0.0\nPotenzial: 0.0\nWechselwahrscheinlichkeit: 0.0"
    return t

def query_player_text(spieler: str, verein: str, model: str = None, prompt_override: str | None = None):
    """
    Textantwort für Shiny-UI.
    Nutzt die gleiche Websuche wie query_player.
    """
    mdl = model or DEFAULT_MODEL
    prompt = prompt_override if prompt_override else _build_prompt_text(spieler, verein)

    tools = [{
        "type": "web_search_preview",
        "user_location": {"type": "approximate", "country": "DE"},
        "search_context_size": "high"
    }]
    try:
        resp = client.responses.create(
            model=mdl,
            input=prompt,
            tools=tools,
            tool_choice={"type": "web_search_preview"},
            temperature=0.25,
            max_output_tokens=5000,
            store=False
        )
        out = (resp.output_text or "")
    except Exception:
        out = ""

    return _basic_text_sanity(spieler, verein, out)

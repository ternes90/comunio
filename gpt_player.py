# gpt_player.py
# Single-Player-Research für R Shiny via reticulate
# Voraussetzungen:
#   pip install --upgrade openai
# Umgebung:
#   OPENAI_API_KEY=<dein Key>

import re
from datetime import date
from openai import OpenAI, APIStatusError, RateLimitError

client = OpenAI()

DEFAULT_MODEL = "gpt-4.1"  # wie im funktionierenden Minimaltest
CSV_HEADER = ["Spieler", "Verein", "Info", "Stammplatzwahrscheinlichkeit", "Potenzial", "Bundesliga_2025_26"]
ALLOWED_DOMAINS = ["kicker.de", "transfermarkt.de", "bundesliga.com", "ligainsider.de"]
ALLOWED_TAGS = {"kicker", "transfermarkt", "bundesliga", "ligainsider"}  # Kurzformen für [tags]

# -------------------- Prompt-Bausteine --------------------

def _build_system():
    today_iso = date.today().isoformat()
    txt = (
        "Du bist ein akribischer deutschsprachiger Research-Assistent. "
        f"Heute ist {today_iso}. "
        "Halte dich strikt an die Ausgabevorgaben (CSV, Semikolon-getrennt). "
        "Keine zusätzlichen Texte, keine Codeblöcke, keine Erklärungen."
    )
    return {"role": "system", "content": [{"type": "input_text", "text": txt}]}

def _build_user(spieler: str, verein: str):
    today_iso = date.today().isoformat()
    header = ";".join(CSV_HEADER)
    domains = ", ".join(ALLOWED_DOMAINS)
    rules = (
        f"Für DIESEN Bundesliga-Spieler (mit Verein): {spieler} ({verein})\n"
        "- Führe eine gezielte Websuche durch.\n"
        f"- Nutze vorrangig folgende Domains: {domains}\n"
        "- Verwende möglichst Treffer aus mehreren dieser Domains.\n"
        f"- Gib eine CSV mit GENAU diesen Spalten zurück:\n{header}\n"
        "- 'Info' = komprimierte, sachliche Zusammenfassung in deutscher Kurzprosa (max. 200 Wörter), "
        "fokussiert auf aktuelle Leistungen (Vorbereitung/Saison, Stand: "
        f"{today_iso}), aktuelle Entwicklung, Verletzungsanfälligkeit, Haltung des aktuellen Trainers, mögliche Wechselabsichten.\n"
        "- Quellenangaben in 'Info' nur in Kurzform in eckigen Klammern am Satzende, z. B. [kicker], [transfermarkt], [bundesliga], [ligainsider].\n"
        "- Keine Semikolons im Infotext.\n"
        "- Zahlenfelder = Dezimalzahlen im Bereich 0–3.\n"
        "- 'Bundesliga_2025_26' = Wahrscheinlichkeit (0–3), dass der Spieler in der Saison 2025/26 in der Bundesliga spielt "
        "(egal bei welchem Verein; 0 = sehr unwahrscheinlich, 3 = sicher).\n"
        "- Wenn trotz Websuche keine verwertbaren Infos: Info='Unklar'; alle Zahlen = 0.\n"
        "- Namen und Verein exakt wie vorgegeben übernehmen.\n"
        "- Antworte NUR mit Kopfzeile + genau EINER Datenzeile, sonst nichts. Keine Codefences."
    )
    return {"role": "user", "content": [{"type": "input_text", "text": rules}]}

# -------------------- Utils --------------------

def _clamp_num(x: str) -> str:
    try:
        v = float(str(x).replace(",", "."))
        v = max(0.0, min(3.0, v))
        return f"{v:.1f}"
    except Exception:
        return "0"

def _filter_citations(info: str) -> str:
    # Erlaube nur [kicker], [transfermarkt], [bundesliga], [ligainsider]
    def repl(m):
        tag = m.group(1).lower()
        return f"[{tag}]" if tag in ALLOWED_TAGS else ""
    info = re.sub(r"\[([A-Za-z0-9_.\-]+)\]", repl, info)
    return " ".join(info.split())

def _parse_csv_like(spieler: str, verein: str, txt: str):
    if not txt or ";" not in txt:
        return [spieler, verein, "Unklar", "0", "0", "0"]
    lines = [ln.strip() for ln in txt.splitlines() if ln.strip()]
    if not lines:
        return [spieler, verein, "Unklar", "0", "0", "0"]
    if lines[0] == ";".join(CSV_HEADER) and len(lines) >= 2:
        data = lines[1]
    else:
        data = lines[0]
    parts = [p.strip() for p in data.split(";")]
    if len(parts) >= 6:
        tail_nums = parts[-3:]
        info_parts = parts[2:-3]
        info = " ".join([p for p in info_parts if p]).strip()
        parts = [parts[0] or spieler, parts[1] or verein, info] + tail_nums
    elif len(parts) >= 5:
        head = parts[:2]
        info = " ".join(parts[2:-2]).strip()
        nums = parts[-2:] + ["0"]
        parts = head + [info] + nums
    else:
        parts = [spieler, verein, "Unklar", "0", "0", "0"]
    parts[3] = _clamp_num(parts[3])
    parts[4] = _clamp_num(parts[4])
    parts[5] = _clamp_num(parts[5])
    parts[2] = _filter_citations(parts[2])
    return parts

# -------------------- Hauptfunktion --------------------

def query_player(spieler: str, verein: str, model: str = None, use_web: bool = True):
    """
    Minimalversion: identisch zum funktionierenden web_search-Test.
    - Ein String-Prompt
    - tools + tool_choice auf Top-Level
    - kein temperature/top_p/max_output_tokens
    """
    mdl = model or DEFAULT_MODEL
    header = ";".join(CSV_HEADER)

    prompt = (
        f"Gib eine CSV mit genau diesen Spalten zurück:\n{header}\n"
        f"Erzeuge genau EINE Datenzeile für: {spieler} ({verein}). "
        "Info = knappe deutsche Kurzprosa, max. 200 Wörter, keine Semikolons. "
        "Am Ende jedes faktischen Satzes eine Kurzquelle in eckigen Klammern mit Domain, z. B. [kicker.de], [bundesliga.com]. "
        "Zahlen nur in den letzten drei Spalten, Werte 0–3 (Dezimal erlaubt). "
        "Wenn keine verwertbaren Infos: Info='Unklar'; alle Zahlen=0. "
        "Antworte NUR mit Kopfzeile + EINER Datenzeile, sonst nichts."
    )

    tools = [{
        "type": "web_search_preview",
        "user_location": {"type": "approximate", "country": "DE"},
        "search_context_size": "high"
    }] if use_web else None

    websearch_used = False
    citations = []

    try:
        resp = client.responses.create(
            model=mdl,
            input=prompt,
            tools=tools,
            tool_choice={"type": "web_search_preview"} if use_web else None,  # erzwinge Suche
            store=False
        )

        # Websuche + Zitate wie im Minimaltest erkennen
        for it in (getattr(resp, "output", []) or []):
            if getattr(it, "type", "") == "web_search_call":
                websearch_used = True
                print("🔍 web_search_call.action:", getattr(it, "action", None))
            if getattr(it, "type", "") == "message":
                for ctn in (getattr(it, "content", []) or []):
                    for ann in (getattr(ctn, "annotations", []) or []):
                        if getattr(ann, "type", "") == "url_citation":
                            citations.append((getattr(ann, "title", ""), getattr(ann, "url", "")))

        txt = (resp.output_text or "").strip()

    except Exception:
        txt = ""

    # Fallback auf minimale CSV-Struktur
    if not txt or ";" not in txt:
        txt = f"{header}\n{spieler};{verein};Unklar;0;0;0"

    parts = _parse_csv_like(spieler, verein, txt)

    # Bekannte Domains als Kurz-Tags an Info anhängen
    def _tag_from_url(u: str) -> str:
        if "kicker.de" in u: return "kicker"
        if "transfermarkt.de" in u: return "transfermarkt"
        if "bundesliga.com" in u: return "bundesliga"
        if "ligainsider.de" in u: return "ligainsider"
        return ""
    tags = []
    for _, u in citations:
        t = _tag_from_url(u)
        if t and f"[{t}]" not in parts[2]:
            tags.append(f"[{t}]")
    if tags:
        parts[2] = (parts[2].rstrip() + " " + " ".join(sorted(set(tags)))).strip()
        parts[2] = _filter_citations(parts[2])

    print("model_used:", mdl, "| web_search_used:", websearch_used, "| citations:", len(citations))

    return {
        "Spieler": parts[0],
        "Verein": parts[1],
        "Info": parts[2],
        "Stammplatzwahrscheinlichkeit": parts[3],
        "Potenzial": parts[4],
        "Bundesliga_2025_26": parts[5],
        "model_used": mdl,
        "websearch_used": bool(websearch_used)
    }

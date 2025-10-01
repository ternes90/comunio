import json
from datetime import date
from openai import OpenAI

client = OpenAI()
DEFAULT_MODEL = "gpt-4o-mini"

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
        return (
            f"{sp}\n{ve}\n"
            "Info: Unklar\n"
            "Quellen:\n"
            "Stammplatz: 0.0\n"
            "Potenzial: 0.0\n"
            "Wechselwahrscheinlichkeit: 0.0"
        )
    return t

def query_player_text(spieler: str, verein: str, model: str = None, prompt_override: str | None = None):
    """
    Textantwort für Shiny-UI mit Websuche.
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
        out = resp.output_text or ""
        # JSON-escaped Unicode (\uXXXX) in echte Zeichen umwandeln
        try:
            out = json.loads(f'"{out}"')
        except Exception:
            pass
    except Exception:
        out = ""

    return _basic_text_sanity(spieler, verein, out)

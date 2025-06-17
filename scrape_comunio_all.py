# COMUNIO KOMBI-SCRAPER (Stand: 16.06.2025)
# Kombiniert Login, Consent und vier Scraping-Prozesse in einem Durchlauf

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
import pandas as pd
import os
import time
import re
import sys
import math
from datetime import datetime, date, timedelta

USERNAME = "Dr. Bier"
PASSWORD = "123Hase"
URL = "https://www.comunio.de/"
CSV_PATH = "TRANSFERMARKT.csv"
CSV_PATH_2 = "ALL_PLAYERS.csv"
TODAY = date.today().strftime("%d.%m.%Y")

# Managerliste (Name + URL)
MANAGER_URLS = {
    "Thomas": "https://www.comunio.de/users/13023580",
    "Alfons": "https://www.comunio.de/users/13024246",
    "Christoph": "https://www.comunio.de/users/13022574",
    "Pascal": "https://www.comunio.de/users/13778221",
    "Andreas": "https://www.comunio.de/users/13718087",
    "Dominik": "https://www.comunio.de/users/13720358",
    "Nico": "https://www.comunio.de/users/13280827",
    "Christian": "https://www.comunio.de/users/13778645"
}

options = webdriver.ChromeOptions()
options.add_argument("--headless=new")
options.add_argument("--disable-gpu")
options.add_argument("--window-size=1920,1080")
options.add_argument("--log-level=3")           # Nur FATAL anzeigen
options.add_argument("--disable-logging")        # Logging vollständig deaktivieren
options.add_experimental_option("excludeSwitches", ["enable-automation"])
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)
wait = WebDriverWait(driver, 30)

def click_consent_if_present():
    try:
        zustimmen_button = wait.until(
            EC.element_to_be_clickable((By.XPATH, "//button[translate(normalize-space(), 'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')='ZUSTIMMEN']"))
        )
        driver.execute_script("arguments[0].click();", zustimmen_button)
        time.sleep(1)
    except:
        pass

def login():
    driver.get(URL)
    time.sleep(2)
    click_consent_if_present()

    login_button_open = wait.until(EC.element_to_be_clickable((By.XPATH, "//a[contains(@class, 'login-btn')]")))
    driver.execute_script("arguments[0].click();", login_button_open)
    time.sleep(2)

    username_input = wait.until(EC.presence_of_element_located((By.XPATH, "//input[@placeholder='Benutzername']")))
    password_input = wait.until(EC.presence_of_element_located((By.XPATH, "//input[@placeholder='Passwort']")))

    username_input.send_keys(USERNAME + " ")
    username_input.send_keys(Keys.BACKSPACE)
    password_input.send_keys(PASSWORD + " ")
    password_input.send_keys(Keys.BACKSPACE)

    login_button = wait.until(EC.presence_of_element_located((By.ID, "login-btn-modal")))
    while "enabled" not in login_button.get_attribute("class"):
        time.sleep(0.2)
    driver.execute_script("arguments[0].click();", login_button)
    print("✅ Login erfolgreich")

def parse_date(raw):
    raw = raw.strip().lower()
    if "heute" in raw:
        return date.today()
    elif "gestern" in raw:
        return date.today() - timedelta(days=1)
    else:
        return datetime.strptime(raw, "%d.%m.%y").date()

def format_datum(dt):
    return dt.strftime("%d.%m.%Y")

def scrape_transfers():
    print("\n🗞️  Starte Scraping Transfers (News) …")
    wait.until(EC.url_contains("dashboard"))
    time.sleep(2)
    while True:
        try:
            last_dates = driver.find_elements(By.CLASS_NAME, "news_date")
            if last_dates and any("27.05.25" in el.text for el in last_dates):
                break
            btn = driver.find_element(By.ID, "btn_load_more_news")
            driver.execute_script("arguments[0].click();", btn)
            time.sleep(1.2)
        except:
            break

    news_groups = driver.find_elements(By.CLASS_NAME, "news_body_per_day")
    result = []

    for group in news_groups:
        try:
            datum_raw = group.find_element(By.CLASS_NAME, "news_date").text
            parsed_date = parse_date(datum_raw).strftime("%d.%m.%Y")
            transfers = group.find_elements(By.CSS_SELECTOR, ".news_text p")
            for transfer in transfers:
                text = re.sub(r"^\d{1,2}:\d{2} - ", "", transfer.text.strip())
                spieler_m = re.match(r"(.+?) wechselt für ([\d\.]+) von (.+?) zu (.+)", text)
                zweit_m = re.search(r"Das zweithöchste Angebot betrug ([\d\.]+) von (.+?)\.", text)
                if spieler_m:
                    spieler, betrag, von, zu = spieler_m.groups()
                    zweitgebot = zweitbietender = ""
                    if zweit_m:
                        zweitgebot = zweit_m.group(1).replace(".", "")
                        zweitbietender = zweit_m.group(2).strip()
                    result.append([parsed_date, spieler.strip(), von.strip(), betrag.replace(".", "").strip(), zu.strip(), zweitgebot, zweitbietender])
        except:
            continue

    df = pd.DataFrame(result, columns=["Datum", "Spieler", "Besitzer", "Hoechstgebot", "Hoechstbietender", "Zweitgebot", "Zweitbietender"])
    df.to_csv("TRANSFERS_all.csv", index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ {len(df)} Transfers seit 27.05.25 gespeichert.")

def load_existing_transactions():
    if os.path.exists("TRANSACTIONS.csv"):
        return pd.read_csv("TRANSACTIONS.csv", sep=";", encoding="utf-8-sig")
    return pd.DataFrame(columns=["Datum", "Spieler", "Transaktion", "Begründung"])

def scrape_transactions():
    print("\n💸  Starte Scraping Transaktionen …")
    wait.until(EC.url_contains("dashboard"))
    time.sleep(2)
    while True:
        try:
            last_dates = driver.find_elements(By.CLASS_NAME, "news_date")
            if last_dates and any("27.05.25" in el.text for el in last_dates):
                break
            btn = driver.find_element(By.ID, "btn_load_more_news")
            driver.execute_script("arguments[0].click();", btn)
            time.sleep(1.5)
            driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(0.5)
        except:
            break

    news_groups = driver.find_elements(By.CLASS_NAME, "news_body_per_day")
    existing = load_existing_transactions()
    new_rows = []

    for group in news_groups:
        try:
            datum_raw = group.find_element(By.CLASS_NAME, "news_date").text
            parsed_date = format_datum(parse_date(datum_raw))
            texts = group.find_elements(By.CLASS_NAME, "news_text")
            for text_el in texts:
                text = text_el.text.strip()
                if not ("Disziplinarstrafe" in text or "Gutschrift" in text):
                    continue
                m2 = re.match(r"Gutschrift: ([\d\.]+) wurden (.+?) vom Communityleiter .*? gutgeschrieben\. Begründung: (.+)", text)
                if m2:
                    betrag = int(m2.group(1).replace(".", ""))
                    spieler = m2.group(2)
                    begruendung = f"Bonus: {m2.group(3)}"
                    new_rows.append([parsed_date, spieler, betrag, begruendung])
                    continue
                m3 = re.match(r"Disziplinarstrafe: ([\d\.]+) wurden (.+?) .*?abgezogen\. Begründung: (.+)", text)
                if m3:
                    betrag = -int(m3.group(1).replace(".", ""))
                    spieler = m3.group(2)
                    begruendung = f"Disziplinarstrafe: {m3.group(3)}"
                    new_rows.append([parsed_date, spieler, betrag, begruendung])
        except Exception as e:
            print("⚠️ Fehler bei Nachricht:", e)
            continue

    df_new = pd.DataFrame(new_rows, columns=["Datum", "Spieler", "Transaktion", "Begründung"])
    combined = pd.concat([existing, df_new]).drop_duplicates()
    combined.to_csv("TRANSACTIONS.csv", index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ {len(df_new)} neue Transaktionen gespeichert. Gesamt: {len(combined)}")

def scrape_transfermarkt():
    print("\n📊  Starte Scraping Transfermarkt …")
    driver.get("https://www.comunio.de/exchangemarket")
    print("🌐  Aufruf Transfermarkt …")
    try:
        wait.until(EC.presence_of_element_located((By.CLASS_NAME, "csspt-row")))
    except:
        print("❌ csspt-row nicht gefunden.")
        return

    rows = driver.find_elements(By.CLASS_NAME, "csspt-row")
    result = []
    tm_datum = date.today().strftime("%d.%m.%Y")

    for row in rows:
        try:
            name = row.find_element(By.CLASS_NAME, "text-to-slide").text.strip()
        except:
            name = ""
        try:
            marktwert = row.find_element(By.CLASS_NAME, "csspt-marketvalue").text.strip()
        except:
            marktwert = ""
        try:
            owner_block = row.find_element(By.CLASS_NAME, "csspt-owner__text").text.strip()
            if "(Du)" in owner_block:
                besitzer = "Dominik"
            elif "Computer" in owner_block:
                besitzer = "Computer"
            else:
                besitzer = owner_block.split("\n")[0].split(" ")[0].strip()
        except:
            besitzer = ""
        result.append([name, marktwert, besitzer, tm_datum])

    df_new = pd.DataFrame(result, columns=["Spieler", "Marktwert", "Besitzer", "TM_Stand"])
    if os.path.exists(CSV_PATH):
        df_existing = pd.read_csv(CSV_PATH, sep=";", encoding="utf-8-sig", dtype={"Marktwert": str})
        merge_cols = ["Spieler", "Marktwert", "Besitzer", "TM_Stand"]
        df_merged = pd.merge(df_new, df_existing, on=merge_cols, how="left", indicator=True)
        df_unique = df_merged[df_merged["_merge"] == "left_only"].drop(columns=["_merge"])
        df_append = df_unique[df_new.columns]
        df_final = pd.concat([df_existing, df_append], ignore_index=True)
        print(f"➕ {len(df_append)} neue Spieler auf dem Transfermarkt gefunden.")
    else:
        df_final = df_new
        print(f"🆕 Erstmaliger Export – {len(df_final)} Spieler gespeichert.")

    df_final.to_csv(CSV_PATH, index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ Transfermarkt gespeichert mit insgesamt {len(df_final)} Einträgen.")

def lade_standings_csv(pfad="STANDINGS.csv"):
    print("\n🏆 Starte Scraping Standings …")
    if not os.path.exists(pfad):
        return pd.DataFrame(columns=["Manager", "Teamwert", "Datum"])
    try:
        df = pd.read_csv(pfad, sep=";", encoding="utf-8-sig")
        if not {"Manager", "Teamwert", "Datum"}.issubset(df.columns):
            return pd.DataFrame(columns=["Manager", "Teamwert", "Datum"])
        return df
    except:
        return pd.DataFrame(columns=["Manager", "Teamwert", "Datum"])

def scrape_standings():
    driver.get("https://www.comunio.de/standings/season/total")
    wait.until(EC.presence_of_element_located((By.CLASS_NAME, "standingstable")))
    time.sleep(3)
    rows = driver.find_elements(By.CSS_SELECTOR, ".standingstable .standingstable_row")

    result = []
    for row in rows:
        try:
            name_full = row.find_element(By.CSS_SELECTOR, ".name").text.strip()
            vorname = name_full.split(" ")[0]
            teamwert = int(row.find_element(By.CSS_SELECTOR, ".teamvalue").text.strip().replace(".", "").replace(" ", ""))
            result.append([vorname, teamwert])
        except:
            continue

    df_new = pd.DataFrame(result, columns=["Manager", "Teamwert"])
    df_new["Datum"] = datetime.now().strftime("%d.%m.%Y")
    pfad = "STANDINGS.csv"
    df_alt = lade_standings_csv(pfad)
    df_gesamt = pd.concat([df_alt, df_new], ignore_index=True).drop_duplicates(subset=["Manager", "Datum"], keep="last")
    df_gesamt.to_csv(pfad, index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ Standings gespeichert: {len(df_new)} neue, insgesamt {len(df_gesamt)}")
    
    # ---- Scraper für alle Spieler ----

import math
import sys

def show_progress(current, total):
    try:
        percent = int(current / total * 100)
        bar = "█" * (percent // 5) + "-" * (20 - percent // 5)
        sys.stdout.write(f"\r⏳ Lade Spieler: [{bar}] {percent}%")
        sys.stdout.flush()
    except:
        dots = "." * (current % 20)
        sys.stdout.write(f"\r⏳ Lade weitere Spieler{dots}   ")
        sys.stdout.flush()

def scrape_all_players():
    print("\n📋 Starte Scraping aller Spieler …")
    driver.get("https://www.comunio.de/players/search?limit=40")
    time.sleep(2)

    total_players = None
    try:
        headline = wait.until(EC.presence_of_element_located((By.CLASS_NAME, "headline")))
        total_text = headline.text.split()[0]
        print(f"🔍 Spieler gefunden: {headline.text}")
        total_players = int(total_text.replace(".", ""))
        print(f"🔢 Gesamtanzahl Spieler: {total_players}")
    except Exception as e:
        print("❌ Spieleranzahl konnte nicht ermittelt werden. Fehler:", e)

    players_per_page = 40
    expected_clicks = math.ceil((total_players - players_per_page) / players_per_page) if total_players else None
    clicks_done = 0

    while True:
        try:
            driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            time.sleep(2)

            btn = wait.until(EC.presence_of_element_located((By.ID, "btn_load_more_news")))
            if btn.is_displayed():
                driver.execute_script("arguments[0].click();", btn)
                clicks_done += 1
                show_progress(clicks_done, expected_clicks if expected_clicks else 999)
                time.sleep(1)
            else:
                print("\n🛑 Keine weiteren Spieler gefunden – fertig.")
                break
        except Exception as e:
            print(f"\n⚠️ Kein Button gefunden oder klickbar – möglicherweise fertig. Fehler: {e}")
            break

    print("📦 Extrahiere Spielerblöcke …")
    player_blocks = driver.find_elements(By.CLASS_NAME, "player-list-item")
    print(f"📦 Anzahl geladener Spieler: {len(player_blocks)}")

    result = []
    for player in player_blocks:
        try:
            name = player.find_element(By.CLASS_NAME, "tradable-name").text.strip()
        except:
            name = ""
        try:
            mw = player.find_element(By.CLASS_NAME, "market-value").text.strip().replace(".", "").replace("€", "")
        except:
            mw = ""
        result.append([name, mw, TODAY])

    df_new = pd.DataFrame(result, columns=["Spieler", "Marktwert", "Datum"])
    if os.path.exists(CSV_PATH_2):
        df_existing = pd.read_csv(CSV_PATH_2, sep=";", encoding="utf-8-sig")
        df_combined = pd.concat([df_existing, df_new], ignore_index=True)
    else:
        df_combined = df_new

    df_final = df_combined.drop_duplicates(subset=["Spieler", "Datum"], keep="last")
    df_final.to_csv(CSV_PATH_2, index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ Spieler gespeichert: {len(df_new)} neue Einträge, Gesamt: {len(df_final)}")


def scrape_kader():
    print("\n👥 Starte Scraping aller Kader …")
    result = []

    for manager, url in MANAGER_URLS.items():
        print(f"🔍 Lade Kader von {manager} …")
        driver.get(url)
        time.sleep(3)

        try:
            wait.until(EC.presence_of_element_located((By.ID, "current-squad-user-info")))
            time.sleep(1)

            spieler_container = driver.find_elements(By.CLASS_NAME, "tradable-name-container")
            for container in spieler_container:
                try:
                    name_elem = container.find_element(By.CSS_SELECTOR, "a.tradable-name")
                    spielername = name_elem.text.strip()

                    # Versuche Position im Elterncontainer zu finden
                    parent = container.find_element(By.XPATH, "..")  # Direkt übergeordneter Container
                    try:
                        position_elem = parent.find_element(By.CLASS_NAME, "position-name")
                        position = position_elem.text.strip()
                    except:
                        position = "?"

                    if spielername:
                        result.append([manager, spielername, position])
                except:
                    continue
        except Exception as e:
            print(f"⚠️ Fehler bei {manager}: {e}")

    df = pd.DataFrame(result, columns=["Manager", "Spieler", "Position"])
    df.to_csv("TEAMS_all.csv", index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ {len(df)} Spieler gespeichert.")

try:
    login()
    scrape_transfers()
    scrape_transactions()
    scrape_transfermarkt()
    scrape_standings()
    scrape_all_players()
    scrape_kader()
finally:
    driver.quit()

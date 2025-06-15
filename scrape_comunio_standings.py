from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
import pandas as pd
import time
import os
from datetime import datetime

USERNAME = "Dr. Bier"
PASSWORD = "123Hase"
URL = "https://www.comunio.de/"

options = webdriver.ChromeOptions()
options.add_argument("--headless=new")  # Headless-Modus
options.add_experimental_option("excludeSwitches", ["enable-automation"])
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)
wait = WebDriverWait(driver, 25)

def click_consent_if_present():
    try:
        zustimmen_button = wait.until(
            EC.element_to_be_clickable((By.XPATH, "//button[translate(normalize-space(), 'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')='ZUSTIMMEN']"))
        )
        driver.execute_script("arguments[0].click();", zustimmen_button)
        time.sleep(1)
    except Exception:
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

def lade_standings_csv(pfad="STANDINGS.csv"):
    if not os.path.exists(pfad):
        print(f"Datei {pfad} existiert nicht, erzeuge leeres DataFrame.")
        return pd.DataFrame(columns=["Manager", "Teamwert", "Datum"])
    try:
        df = pd.read_csv(pfad, sep=";", encoding="utf-8-sig")
        required_cols = {"Manager", "Teamwert", "Datum"}
        if not required_cols.issubset(df.columns):
            print(f"Fehler: Fehlende Spalten in CSV. Gefunden: {df.columns.tolist()}")
            return pd.DataFrame(columns=["Manager", "Teamwert", "Datum"])
        return df
    except Exception as e:
        print(f"Fehler beim Laden von CSV: {e}")
        return pd.DataFrame(columns=["Manager", "Teamwert", "Datum"])

def scrape_standings():
    wait.until(EC.url_contains("dashboard"))
    driver.get("https://www.comunio.de/standings/season/total")
    wait.until(EC.presence_of_element_located((By.CLASS_NAME, "standingstable")))

    time.sleep(3)
    rows = driver.find_elements(By.CSS_SELECTOR, ".standingstable .standingstable_row")

    result = []
    for row in rows:
        try:
            name_full = row.find_element(By.CSS_SELECTOR, ".name").text.strip()
            # Nur ersten Vornamen extrahieren (alles vor erstem Leerzeichen)
            vorname = name_full.split(" ")[0]
            teamwert_text = row.find_element(By.CSS_SELECTOR, ".teamvalue").text.strip()
            teamwert_clean = int(teamwert_text.replace(".", "").replace(" ", ""))
            result.append([vorname, teamwert_clean])
        except Exception as e:
            print(f"Fehler beim Verarbeiten einer Zeile: {e}")
            continue

    df_new = pd.DataFrame(result, columns=["Manager", "Teamwert"])

    # Aktuelles Datum im Format TT.MM.JJJJ
    heute = datetime.now().strftime("%d.%m.%Y")
    df_new["Datum"] = heute

    pfad = "STANDINGS.csv"
    df_alt = lade_standings_csv(pfad)

    print(f"Alt: {len(df_alt)} Einträge")
    print(f"Neu: {len(df_new)} Einträge")

    df_gesamt = pd.concat([df_alt, df_new], ignore_index=True)
    print(f"Kombiniert vor Duplikate: {len(df_gesamt)}")

    # Duplikate nach Manager und Datum entfernen, letzte behalten (neueste Einträge)
    df_gesamt.drop_duplicates(subset=["Manager", "Datum"], keep="last", inplace=True)
    print(f"Kombiniert nach Duplikate entfernen: {len(df_gesamt)}")

    df_gesamt.to_csv(pfad, index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ {len(df_new)} Manager neu hinzugefügt, insgesamt {len(df_gesamt)} Einträge in {pfad}.")

try:
    login()
    scrape_standings()
finally:
    driver.quit()

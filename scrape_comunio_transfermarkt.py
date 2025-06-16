from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
from datetime import date
import pandas as pd
import os
import time

USERNAME = "Dr. Bier"
PASSWORD = "123Hase"
URL = "https://www.comunio.de/exchangemarket"
CSV_PATH = "TRANSFERMARKT.csv"

options = webdriver.ChromeOptions()
options.add_argument("--headless=new")  # Erzwinge Headless-Modus für Batch
options.add_argument("--disable-gpu")  # Oft nötig bei Windows
options.add_argument("--window-size=1920,1080")
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)
wait = WebDriverWait(driver, 30)

def click_consent_if_present():
    try:
        print("🔘 Consent-Button wird gesucht …")
        zustimmen_button = wait.until(
            EC.element_to_be_clickable((By.XPATH, "//button[translate(normalize-space(), 'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')='ZUSTIMMEN']"))
        )
        driver.execute_script("arguments[0].click();", zustimmen_button)
        print("✅ Consent akzeptiert")
        time.sleep(1)
    except:
        print("ℹ️ Kein Consent angezeigt.")

def login():
    print("🔐 Login startet …")
    driver.get("https://www.comunio.de/")
    time.sleep(2)
    click_consent_if_present()

    login_button_open = wait.until(EC.element_to_be_clickable((By.XPATH, "//a[contains(@class, 'login-btn')]")))
    driver.execute_script("arguments[0].click();", login_button_open)
    time.sleep(2)

    username_input = wait.until(EC.presence_of_element_located((By.XPATH, "//input[@placeholder='Benutzername']")))
    password_input = wait.until(EC.presence_of_element_located((By.XPATH, "//input[@placeholder='Passwort']")))

    username_input.send_keys(USERNAME + " ")
    username_input.send_keys("\b")
    password_input.send_keys(PASSWORD + " ")
    password_input.send_keys("\b")

    login_button = wait.until(EC.presence_of_element_located((By.ID, "login-btn-modal")))
    while "enabled" not in login_button.get_attribute("class"):
        time.sleep(0.2)
    driver.execute_script("arguments[0].click();", login_button)
    print("✅ Login erfolgreich")

def scrape_transfermarkt():
    print("🌐 Aufruf Transfermarkt …")
    wait.until(EC.url_contains("dashboard"))
    driver.get(URL)

    try:
        wait.until(EC.presence_of_element_located((By.CLASS_NAME, "csspt-row")))
    except:
        print("❌ csspt-row nicht gefunden – Seite evtl. nicht korrekt geladen.")
        driver.save_screenshot("fehler_transfermarkt.png")
        return

    print("📥 Transfermarkt geladen, Scraping startet …")
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
    print(f"📊 {len(df_new)} Spieler gefunden am Transfermarkt.")

    if os.path.exists(CSV_PATH):
        df_existing = pd.read_csv(CSV_PATH, sep=";", encoding="utf-8-sig", dtype={"Marktwert": str})
        merge_cols = ["Spieler", "Marktwert", "Besitzer", "TM_Stand"]
        df_merged = pd.merge(df_new, df_existing, on=merge_cols, how="left", indicator=True)
        df_unique = df_merged[df_merged["_merge"] == "left_only"].drop(columns=["_merge"])
        df_append = df_unique[df_new.columns]
        df_final = pd.concat([df_existing, df_append], ignore_index=True)
        print(f"➕ Neue Einträge: {len(df_append)}")
    else:
        df_final = df_new
        print(f"➕ Erstmaliger Export: {len(df_new)} Einträge")

    df_final.to_csv(CSV_PATH, index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ Transfermarkt gespeichert mit insgesamt {len(df_final)} Einträgen.")

try:
    login()
    scrape_transfermarkt()
finally:
    driver.quit()

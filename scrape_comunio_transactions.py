from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
from datetime import date, timedelta, datetime
import pandas as pd
import time
import re
import os

USERNAME = "Dr. Bier"
PASSWORD = "123Hase"
URL = "https://www.comunio.de/"

options = webdriver.ChromeOptions()
options.add_argument("--headless=new")
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

def load_existing_transactions():
    if os.path.exists("TRANSACTIONS.csv"):
        return pd.read_csv("TRANSACTIONS.csv", sep=";", encoding="utf-8-sig")
    return pd.DataFrame(columns=["Datum", "Spieler", "Transaktion", "Begründung"])

def scrape_transactions():
    wait.until(EC.url_contains("dashboard"))
    time.sleep(2)

    # Lade News bis 27.05.25 sichtbar
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
        except:
            continue

        texts = group.find_elements(By.CLASS_NAME, "news_text")

        for text_el in texts:
            try:
                driver.execute_script("arguments[0].scrollIntoView(true);", text_el)
                time.sleep(0.1)

                text = text_el.text.strip()

                if not ("Disziplinarstrafe" in text or "Gutschrift" in text):
                    continue

                print(f"💥 Treffer: {parsed_date} → {text}")

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
                    continue

            except Exception as e:
                print("⚠️ Fehler bei Nachricht:", e)
                continue

    df_new = pd.DataFrame(new_rows, columns=["Datum", "Spieler", "Transaktion", "Begründung"])
    combined = pd.concat([existing, df_new]).drop_duplicates()
    combined.to_csv("TRANSACTIONS.csv", index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ {len(df_new)} neue Transaktionen gespeichert. Gesamt: {len(combined)}")

# Main
try:
    login()
    scrape_transactions()
finally:
    driver.quit()

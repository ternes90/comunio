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

def parse_date(raw_date):
    raw = raw_date.strip().lower()
    if "heute" in raw:
        return date.today()
    elif "gestern" in raw:
        return date.today() - timedelta(days=1)
    else:
        return datetime.strptime(raw, "%d.%m.%y").date()

def scrape_transfers():
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
        except:
            continue

        try:
            transfers = group.find_elements(By.CSS_SELECTOR, ".news_text p")
            for transfer in transfers:
                text = transfer.text.strip()
                text = re.sub(r"^\d{1,2}:\d{2} - ", "", text)

                spieler_m = re.match(r"(.+?) wechselt für ([\d\.]+) von (.+?) zu (.+)", text)
                zweit_m = re.search(r"Das zweithöchste Angebot betrug ([\d\.]+) von (.+?)\.", text)

                if spieler_m:
                    spieler, betrag, von, zu = spieler_m.groups()
                    zweitgebot, zweitbietender = "", ""
                    if zweit_m:
                        zweitgebot = zweit_m.group(1).replace(".", "")
                        zweitbietender = zweit_m.group(2).strip()
                    result.append([
                        parsed_date,
                        spieler.strip(),
                        von.strip(),
                        betrag.replace(".", "").strip(),
                        zu.strip(),
                        zweitgebot,
                        zweitbietender
                    ])
        except:
            continue

        df = pd.DataFrame(result, columns=["Datum", "Spieler", "Besitzer", "Hoechstgebot", "Hoechstbietender", "Zweitgebot", "Zweitbietender"])
    df.to_csv("TRANSFERS_all.csv", index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ {len(df)} Transfers seit 27.05.25 gespeichert.")


try:
    login()
    scrape_transfers()
finally:
    driver.quit()

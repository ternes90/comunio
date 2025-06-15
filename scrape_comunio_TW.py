from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
import pandas as pd
import time
import re

USERNAME = "Dr. Bier"
PASSWORD = "123Hase"
URL = "https://www.comunio.de/"

options = webdriver.ChromeOptions()
options.add_argument("--headless=new")  # Headless-Betrieb
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

def scrape_standings():
    wait.until(EC.url_contains("dashboard"))
    driver.get("https://www.comunio.de/standings/season/total")
    wait.until(EC.presence_of_element_located((By.CLASS_NAME, "standingstable")))

    time.sleep(3)
    rows = driver.find_elements(By.CSS_SELECTOR, ".standingstable .standingstable_row")

    result = []
    for row in rows:
        try:
            name = row.find_element(By.CSS_SELECTOR, ".name").text.strip()
            teamwert_text = row.find_element(By.CSS_SELECTOR, ".teamvalue").text.strip()
            teamwert_clean = int(teamwert_text.replace(".", "").replace(" ", ""))
            result.append([name, teamwert_clean])
        except:
            continue

    df = pd.DataFrame(result, columns=["Manager", "Teamwert"])
    df.to_csv("STANDINGS.csv", index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ {len(df)} Manager gespeichert.")

try:
    login()
    scrape_standings()
finally:
    driver.quit()

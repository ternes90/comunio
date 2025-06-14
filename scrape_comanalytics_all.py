from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
import pandas as pd
import time

USERNAME = "Dr. Bier"
PASSWORD = "123Hase"
URL_LOGIN = "https://www.com-analytics.de/login"
URL_PREDICTIONS = "https://www.com-analytics.de/predictions"

options = webdriver.ChromeOptions()
options.add_argument("--headless=new")  # Für Debug am besten offen lassen!
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)

try:
    wait = WebDriverWait(driver, 20)
    driver.get(URL_LOGIN)
    time.sleep(2)  # kurze Wartezeit zum Laden

    # Cookie-Consent klicken, falls nötig
    try:
        cookie_btn = wait.until(EC.element_to_be_clickable(
            (By.XPATH, "//button[contains(text(), 'Cookies zulassen') or contains(text(), 'Alle akzeptieren') or contains(text(), 'Zustimmen')]")
        ))
        cookie_btn.click()
        print("✅ Consent geklickt")
        time.sleep(1)
    except Exception:
        print("ℹ️ Kein Cookie-Consent sichtbar.")

    # Login
    username_field = wait.until(EC.presence_of_element_located((By.NAME, "_username")))
    password_field = wait.until(EC.presence_of_element_located((By.NAME, "_password")))
    username_field.clear()
    username_field.send_keys(USERNAME)
    password_field.clear()
    password_field.send_keys(PASSWORD)
    driver.find_element(By.XPATH, "//button[contains(., 'Login')]").click()
    print("✅ Eingeloggt")
    wait.until(EC.url_contains("/my_team"))

    # Predictions öffnen
    driver.get(URL_PREDICTIONS)
    wait.until(EC.presence_of_element_located((By.TAG_NAME, "table")))

    # --- ECHTES Dropdown auf 100 setzen ---
    try:
        select_elem = wait.until(EC.presence_of_element_located((By.NAME, "dataTable_length")))
        select = Select(select_elem)
        select.select_by_value("100")
        print("✅ 100 Zeilen pro Seite ausgewählt")
        time.sleep(2)  # Tabelle lädt neu!
    except Exception as e:
        print("⚠️ Konnte '100' nicht auswählen:", e)

    # --- Durch alle Seiten blättern & sammeln ---
    all_data = []
    page_num = 1
    while True:
        table = wait.until(EC.presence_of_element_located((By.TAG_NAME, "table")))
        rows = table.find_elements(By.TAG_NAME, "tr")
        headers = [th.text.strip() for th in rows[0].find_elements(By.TAG_NAME, "th")]
        for row in rows[1:]:
            cols = [td.text.strip() for td in row.find_elements(By.TAG_NAME, "td")]
            if cols:
                all_data.append(cols)
        print(f"📄 Seite {page_num} gesammelt ({len(all_data)} gesamt)")

        # Suche „Nächste“-Button (enabled)
        try:
            next_btn = driver.find_element(By.XPATH, "//a[contains(., 'Nächste') and not(contains(@class, 'disabled'))]")
            if next_btn.is_displayed() and next_btn.is_enabled():
                next_btn.click()
                page_num += 1
                time.sleep(1.8)
            else:
                break
        except Exception:
            break  # keine weitere Seite

    df = pd.DataFrame(all_data, columns=headers)
    df.to_csv("com_analytics_predictions_full.csv", index=False, encoding="utf-8-sig")
    print(f"✅ ALLE Predictions exportiert! {len(df)} Zeilen gespeichert.")

except Exception as e:
    print("❌ Fehler beim Scraping:", e)
    driver.save_screenshot("fehler_screenshot_predictions.png")
finally:
    driver.quit()

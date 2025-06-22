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
URL_TOPPLAYERS = "https://www.com-analytics.de/topplayers"

options = webdriver.ChromeOptions()
options.add_argument("--headless=new")  # Für Debug evtl. auskommentieren!
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)

try:
    wait = WebDriverWait(driver, 20)
    driver.get(URL_LOGIN)
    time.sleep(2)

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

    # Gehe zu Topplayers
    driver.get(URL_TOPPLAYERS)
    wait.until(EC.presence_of_element_located((By.TAG_NAME, "table")))
    time.sleep(1.5)  # Für Sicherheit, ggf. anpassen

    # Mapping: Tabellenname, Dropdown-Name, Positionslabel
    tables_info = [
        ("topplayerskeeper", "topplayerskeeper_length", "Torhüter"),
        ("topplayersdefender", "topplayersdefender_length", "Abwehr"),
        ("topplayersmidfielder", "topplayersmidfielder_length", "Mittelfeld"),
        ("topplayersstriker", "topplayersstriker_length", "Sturm"),
    ]
    all_data = []
    for table_id, select_name, pos_label in tables_info:
        print(f"▶️ Bearbeite {pos_label} ...")

        # 1. "Alle" auswählen (Dropdown: value="-1")
        try:
            select_elem = wait.until(EC.presence_of_element_located((By.NAME, select_name)))
            select = Select(select_elem)
            select.select_by_value("-1")
            print(f"   ✅ 'Alle' ausgewählt ({pos_label})")
            time.sleep(2.5)  # Tabelle lädt neu
        except Exception as e:
            print(f"   ⚠️ Konnte 'Alle' für {pos_label} nicht auswählen: {e}")

        # 2. Tabelle parsen
        try:
            table = wait.until(EC.presence_of_element_located((By.ID, table_id)))
            rows = table.find_elements(By.TAG_NAME, "tr")
            headers = [th.text.strip() for th in rows[0].find_elements(By.TAG_NAME, "th")]
            # Wir hängen die Position an, damit später klar ist, was was ist
            for row in rows[1:]:
                cols = [td.text.strip() for td in row.find_elements(By.TAG_NAME, "td")]
                if cols:
                    all_data.append(cols + [pos_label])
            print(f"   📄 {len(rows)-1} Zeilen extrahiert ({pos_label})")
        except Exception as e:
            print(f"   ❌ Fehler beim Parsen der Tabelle {pos_label}: {e}")

    # 3. Gesamtheaders um "Positionstyp" ergänzen
    if all_data:
        headers.append("Positionsgruppe")
        df = pd.DataFrame(all_data, columns=headers)
        df.to_csv("com_analytics_all_players.csv", index=False, encoding="utf-8-sig", sep=";")
        print(f"✅ Alle Spieler gespeichert: {len(df)} Zeilen.")
    else:
        print("❌ Keine Daten gefunden!")

except Exception as e:
    print("❌ Fehler beim Scraping:", e)
    driver.save_screenshot("fehler_screenshot_topplayers.png")
finally:
    driver.quit()

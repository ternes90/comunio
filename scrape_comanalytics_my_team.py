from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
import pandas as pd
import time

USERNAME = "Dr. Bier"
PASSWORD = "123Hase"
URL_LOGIN = "https://www.com-analytics.de/login"
URL_MY_TEAM = "https://www.com-analytics.de/my_team"

options = webdriver.ChromeOptions()
options.add_argument("--headless=new")  # Headless AUS für Debug, AN für Unsichtbar

driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)

def click_all_consents(driver):
    # 1. Suche alle Consent-/Cookie-Buttons (z.B. "Cookies zulassen")
    try:
        all_buttons = driver.find_elements(By.XPATH, "//button|//a|//span")
        consent_clicked = False
        for btn in all_buttons:
            txt = btn.text.strip().lower()
            if any(x in txt for x in [
                "akzeptieren", "zustimmen", "einverstanden", "alle akzeptieren",
                "ich akzeptiere", "cookies zulassen", "accept", "agree", "allow all", "consent"
            ]):
                if btn.is_displayed() and btn.is_enabled():
                    btn.click()
                    print(f"✅ Consent/Overlay geklickt: [{btn.text.strip()}]")
                    consent_clicked = True
                    time.sleep(1)
                    break
        if not consent_clicked:
            print("ℹ️ Kein Consent/Overlay-Button gefunden, mache normal weiter.")
        # 2. Suche und schließe bekannte Overlay-Container per Button oder JS
        overlays = driver.find_elements(By.CLASS_NAME, "qc-cmp-cleanslate")
        for overlay in overlays:
            if overlay.is_displayed():
                btns = overlay.find_elements(By.XPATH, ".//button | .//a | .//span")
                for btn in btns:
                    txt = btn.text.strip().lower()
                    if any(x in txt for x in [
                        "akzeptieren", "zustimmen", "einverstanden", "alle akzeptieren", "ich akzeptiere",
                        "cookies zulassen", "accept", "agree", "allow all", "consent"
                    ]):
                        try:
                            btn.click()
                            print(f"✅ 2. Consent-Overlay geklickt: [{btn.text.strip()}]")
                            time.sleep(1)
                            break
                        except Exception as e:
                            print("⚠️ Overlay-Button nicht klickbar:", e)
                # Im Notfall per JS entfernen
                if overlay.is_displayed():
                    driver.execute_script("arguments[0].style.display='none';", overlay)
                    print("✅ Overlay per JS entfernt.")
        # Warten bis alle Overlays weg
        try:
            WebDriverWait(driver, 7).until_not(
                lambda d: any(el.is_displayed() for el in d.find_elements(By.CLASS_NAME, "qc-cmp-cleanslate"))
            )
            print("✅ Consent-Overlay ist jetzt wirklich verschwunden.")
        except Exception:
            print("ℹ️ Kein weiteres Consent-Overlay mehr gefunden.")
        time.sleep(0.8)
    except Exception as e:
        print("ℹ️ Consent/Overlay-Fehler (ignoriert):", e)

try:
    wait = WebDriverWait(driver, 20)
    driver.get(URL_LOGIN)
    time.sleep(2)  # Zeit für Consent

    click_all_consents(driver)

    # Login
    username_field = wait.until(EC.presence_of_element_located((By.NAME, "_username")))
    password_field = wait.until(EC.presence_of_element_located((By.NAME, "_password")))

    username_field.clear()
    username_field.send_keys(USERNAME)
    password_field.clear()
    password_field.send_keys(PASSWORD)

    driver.find_element(By.XPATH, "//button[contains(., 'Login')]").click()
    wait.until(EC.url_contains("/my_team"))

    driver.get(URL_MY_TEAM)
    time.sleep(2)

    # Nochmal Consent prüfen (manchmal kommt nochmal ein Overlay nach Redirect)
    click_all_consents(driver)

    # --- "Alle" Zeilen auswählen (statt nur 10) ---
    try:
        # 1. Dropdown öffnen
        btn = wait.until(EC.element_to_be_clickable(
            (By.XPATH, "//button[contains(@class,'dropdown-toggle') and contains(@class,'btn-default')]")
        ))
        btn.click()
        time.sleep(0.6)
        # 2. "All"-Eintrag anklicken (englisch ODER deutsch, je nach UI!)
        all_option = driver.find_element(By.XPATH, "//a[contains(text(), 'All') or contains(text(), 'Alle')]")
        all_option.click()
        print("✅ 'All' ausgewählt!")
        time.sleep(1.5)
    except Exception as e:
        print("⚠️ Konnte 'All'-Button nicht finden oder klicken:", e)

    # --- Tabelle auslesen ---
    wait.until(EC.presence_of_element_located((By.TAG_NAME, "table")))
    table = driver.find_element(By.TAG_NAME, "table")
    rows = table.find_elements(By.TAG_NAME, "tr")
    data = []
    headers = [th.text.strip() for th in rows[0].find_elements(By.TAG_NAME, "th")]
    for row in rows[1:]:
        cols = [td.text.strip() for td in row.find_elements(By.TAG_NAME, "td")]
        if cols:
            data.append(cols)
    df = pd.DataFrame(data, columns=headers)
    df.to_csv("com_analytics_my_team.csv", index=False, encoding="utf-8-sig")
    print(f"✅ My Team Tabelle erfolgreich exportiert! {len(df)} Zeilen gespeichert.")

except Exception as e:
    print("❌ Fehler beim Scraping:", e)
    driver.save_screenshot("fehler_screenshot.png")
finally:
    driver.quit()

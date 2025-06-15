from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from webdriver_manager.chrome import ChromeDriverManager
import time
import pandas as pd

# Login-Daten
USERNAME = "Dr. Bier"
PASSWORD = "123Hase"
URL = "https://www.comunio.de/"

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

# Headless Browser Setup
options = webdriver.ChromeOptions()
options.add_argument("--headless=new")
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)
wait = WebDriverWait(driver, 20)

def click_consent_if_present():
    try:
        zustimmen = wait.until(EC.element_to_be_clickable(
            (By.XPATH, "//button[contains(translate(text(), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'zustimmen')]")))
        zustimmen.click()
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
        time.sleep(0.1)
    driver.execute_script("arguments[0].click();", login_button)

def scrape_kader():
    result = []

    for manager, url in MANAGER_URLS.items():
        print(f"🔍 Lade Kader von {manager} …")
        driver.get(url)
        time.sleep(3)

        try:
            # Warte auf das Laden des Kaderbereichs
            wait.until(EC.presence_of_element_located((By.ID, "current-squad-user-info")))
            time.sleep(1)

            # Spielername auslesen
            spieler_elems = driver.find_elements(By.CSS_SELECTOR, ".tradable-name-container a.tradable-name")
            for elem in spieler_elems:
                spielername = elem.text.strip()
                if spielername:
                    result.append([manager, spielername])
        except Exception as e:
            print(f"⚠️ Fehler bei {manager}: {e}")

    # Speichern
    df = pd.DataFrame(result, columns=["Manager", "Spieler"])
    df.to_csv("TEAMS_all.csv", index=False, sep=";", encoding="utf-8-sig")
    print(f"✅ {len(df)} Spieler gespeichert.")

try:
    login()
    scrape_kader()
finally:
    driver.quit()

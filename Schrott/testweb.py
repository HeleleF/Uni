# -*- coding: utf-8 -*-

from time import sleep
import time
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
import os
import re

WARTEZEIT_IN_SEC = 1

PATTERN = "Treskowallee+Geb%C3%A4ude"

PATH_TO_CHROME_EXE = "C:/Python27/Scripts/chromedriver.exe"
PATH_TO_DOWNLOAD = "F:/Testprojekte"

URL_RAUM = "https://lsf.htw-berlin.de/qisserver/rds?state=wplan&act=Raum&raum.rgid="

URL_PLAN_OHNE_INAKTIVE = "https://lsf.htw-berlin.de/qisserver/rds?state=change&type=6" \
                         "&moduleParameter=veranstraumSelect&nextdir=change&next=SearchSelect.vm&target=raumSearch" \
                         "&subdir=raum&init=y&source=state%3Dchange%26type%3D5%26moduleParameter%3DraumPlanSearch%26" \
                         "nextdir%3Dchange%26next%3Dsearch.vm%26subdir%3Draum%26topitem%3D%26subitem%3D%26field%3Dk" \
                         "txt&targetfield=ktxt"

LISTE_IDS = []


def alles_machen():
    # Raumplan ohne Inaktive öffnen und Links holen
    browser.get(URL_PLAN_OHNE_INAKTIVE)
    links = browser.find_elements_by_css_selector(".klein:nth-child(3) .regular")
    for link in links:
        # unnötiges abschneiden
        teststr = link.get_attribute("href")[227:]

        try:
            raum_desc = re.search("&text=(.+?)$", teststr).group(1)
            raum_id = re.search("^(\d{3,4})", teststr).group(1)

            if PATTERN in raum_desc:
                LISTE_IDS.append(raum_id)

        except AttributeError:
            print("Fehler beim Einlesen von RaumID und/oder RaumBeschreibung! Ignorieren und naechstes!")
            pass


def runterzocken():

    for r_id in LISTE_IDS:

        browser.get(URL_RAUM + r_id)

        try:
            element = browser.find_element_by_css_selector("a.tree")
            print("Gefunden! Lade runter...")

            element.click()
            sleep(2)

            for filename in os.listdir(os.path.dirname(os.path.abspath(__file__))):

                base_file, ext = os.path.splitext(filename)
                # falls der ChromeDriver aus irgendwelchen Gründen rumbuggt und tmp Dateien erstellt hat
                if ext == ".ics" or ext == ".tmp":
                    timestr = time.strftime("%Y%m%d-%H%M%S")
                    os.rename(filename, timestr + "_lcs_" + r_id + ".txt")

        except NoSuchElementException:
            print("Nichts anklickbares gefunden... Ignorieren und naechstes!")

    print("fertig")


# Download Ordner aendern
chromeOptions = webdriver.ChromeOptions()
prefs = {"download.default_directory": PATH_TO_DOWNLOAD}
chromeOptions.add_experimental_option("prefs", prefs)

# create a new Chrome session
browser = webdriver.Chrome(PATH_TO_CHROME_EXE, chrome_options=chromeOptions)
browser.implicitly_wait(WARTEZEIT_IN_SEC)
browser.set_window_size(400, 400)

alles_machen()
runterzocken()

# damits nich rumbuggt
browser.get("http://www.google.com")

# close the browser window
browser.quit()
exit()

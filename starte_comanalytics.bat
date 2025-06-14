@echo off
REM Aktiviert das virtuelle Python-Environment (Pfad ggf. anpassen)
call venv_comunio\Scripts\activate

REM Erstes Skript (my_team)
python scrape_comanalytics_my_team.py

REM Zweites Skript (predictions)
python scrape_comanalytics_all.py

pause

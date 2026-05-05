# SleepTempFinder

Ein R-basiertes Analyseprojekt zur Auswertung von Garmin-Schlafdaten und Raumtemperatursensoren.

## Überblick

Dieses Projekt liest Schlaf-CSV-Dateien aus Garmin und Raumtemperatur-/Feuchtesensor-CSV-Dateien ein, ordnet die Messwerte den Schlafperioden zu, berechnet nächtliche Mittelwerte und führt eine Analyse mit optionalen Filterungen durch.

Die Hauptlogik liegt in:
- `RScript/SleepTempFinder.R` – Hauptskript für Datenimport, Bereinigung, Zuordnung, Filterung und Visualisierung
- `RScript/studio_commands.R` – interaktive Helferfunktionen für RStudio, insbesondere `run_analysis()`
- `RScript/config.yaml` – zentrale Konfiguration für Dateiformate, Sensoren, Kalender und Plot-Einstellungen

## Voraussetzungen

- R installiert
- Die benötigten Pakete werden beim ersten Lauf automatisch installiert:
  - `tidyverse`, `lubridate`, `yaml`, `broom`, `GGally`, `gridExtra`, `grid`, `scales`, `httpgd` (bei browser-Plotmodus)

## Verzeichnisstruktur

- `data/` – Datumsgesteuerte Ordner mit CSV-Exports
- `PlotOutput/` – optionaler Ausgabeordner für gespeicherte Plot-Bilder
- `RScript/` – Skripte und Konfigurationsdateien
- `RScript/config.yaml` – zentrale Projektkonfiguration
- `RScript/config.private.yaml` – optionaler lokaler Override für sensible oder benutzerdefinierte Einstellungen

## Konfiguration

Die wichtigste Datei ist `RScript/config.yaml`.

Wichtige Abschnitte:
- `data_directory` – Basisverzeichnis für deine Datensätze
- `parse_orders` – Datum-/Uhrzeitformate für Garmin- und Sensor-CSV-Dateien
- `locale` – insbesondere `decimal_mark: ","` für Sensor-CSV-Dateien mit Komma als Dezimaltrennzeichen
- `sensor_files` – Sensor-Definitionen mit Name, Dateipfad, Spaltennamen und optionalen Nicknames
- `calendar_default_sensor` – Standard-Sensor, falls im Kalender kein Sensor genannt wird
- `analysis_filter` – default-Werte für Filterungen, die beim Start geladen werden
- `plot.output_mode` – `rstudio`, `browser` oder `both`
- `plot.export.enabled` – wenn `true`, werden Grafiken nach `plot.export.output_dir` geschrieben

Wenn du persönliche Einstellungen brauchst, lege `RScript/config.private.yaml` an. Dort kannst du Konfigurationswerte überschreiben, ohne das Repo-Tracking zu ändern.

## Ausführen des Hauptskripts

### Von der Kommandozeile

Aus dem Projektordner:

```sh
Rscript RScript/SleepTempFinder.R
```

Optional mit Filter:

```sh
Rscript RScript/SleepTempFinder.R --filter="2026;Sensors=WohnwagenSensor"
```

Mit Dry-Run (plots werden unterdrückt):

```sh
Rscript RScript/SleepTempFinder.R --dry-run
```

Mit Verbose-Ausgabe (zusätzliche Debug-Informationen):

```sh
Rscript RScript/SleepTempFinder.R --verbose
```

### Filter-Syntax

Die Filter werden als semikolon-getrennte Zeichenkette angegeben.

Mögliche Bestandteile:
- Datumsauswahl: `YYYY`, `qN.YYYY`, `MM.YYYY`, `YYYY.MM`, `DD.MM.YYYY`, `DD.MM.YYYY,DD.MM.YYYY`
- Sensoren: `Sensors=Name` (mehrere Sensoren mit `,` oder `|`)
- Tags: `Tags=Tag1|Tag2` (OR) oder `Tags=Tag1,Tag2` (AND)
- Komplexe Tag-Ausdrücke: `TagsExpr=` mit `!`, `,`, `&`, `*`, Klammern
- Numerische Ausdrücke: z.B. `temp>18`, `SleepScore>80`, `18<temp<22`

Beispiele:

```sh
Rscript RScript/SleepTempFinder.R --filter="q1.2026;Tags=Hochlitten"
Rscript RScript/SleepTempFinder.R --filter="01.2026;Sensors=WohnwagenSensor"
Rscript RScript/SleepTempFinder.R --filter="temp>18"
Rscript RScript/SleepTempFinder.R --filter="SleepScore>80"
Rscript RScript/SleepTempFinder.R --filter="TagsExpr=(Urlaub, Wohnmobil) & !Hochlitten"
```

Hinweis: Die Ausreißererkennung wird über `RScript/config.yaml` konfiguriert. Setze `outlier_filter.mode` auf `false`, `manual` oder `value_interval`.
Zusätzlich gibt es eine `outlier_detection`-Sektion mit allen Metriken, die auf `true`, `false` oder `self` gesetzt werden können.

## Interaktive Nutzung in RStudio

1. Öffne das Projekt in RStudio.
2. Source das Hilfsskript:

```r
source("RScript/studio_commands.R")
```

3. Starte die Analyse direkt mit `run_analysis()`.

### Beispiele für `run_analysis()`

```r
run_analysis("2026")
run_analysis(tags = "Hochlitten")
run_analysis(sensors = "WohnwagenSensor")
run_analysis(filter = "temp>18")
run_analysis(filter = "SleepScore>80")
run_analysis(tags = "(Urlaub, Wohnmobil) & !Hochlitten")
run_analysis(tags != 'Urlaub')
```

### `run_analysis()`-Argumente

- `date` – optionaler Datumsfilter oder Datumsbereich (z. B. `"2026"`, `"01.2026"`, `"02.02.2026,04.03.2026"`)
- `tags` – Filter auf Kalendertags
- `sensors` – Filter auf Sensoren
- `dry_run` – `TRUE` unterdrückt Plot-Ausgabe
- `filter` – roher Filterstring im selben Format wie `--filter`

Wenn `filter` gesetzt ist, hat es Vorrang vor den anderen Parametern.

## Komplexe Tag-Filter

Die neue Logik unterstützt komplexe boolesche Ausdrücke für Tags.

Operatoren:
- `,` → OR
- `&` → AND
- `!` → NOT
- `*` → XOR
- Klammern für Gruppierung

Beispiele:

```r
run_analysis(tags = "Urlaub, Wohnmobil")
run_analysis(tags = "Urlaub & !Hochlitten")
run_analysis(tags = "(Hochlitten, Trainingslager) & !Urlaub")
run_analysis(tags = "Urlaub * Wohnmobil")
```

Du kannst auch normale R-Vergleiche verwenden; sie werden automatisch in `TagsExpr=` konvertiert:

```r
run_analysis(tags != 'Hochlitten')
run_analysis(tags == 'Urlaub')
run_analysis(tags %in% c('Urlaub','Wohnmobil'))
```

## Sensor- und Kalenderdaten

Sensoren werden in `RScript/config.yaml` über `sensor_files` definiert. Jeder Eintrag enthält:
- `nickname` – alternative Bezeichnungen für Kalenderzuordnung
- `path` – Sensor-CSV-Datei unter `data/` oder in einem Unterordner
- `col_time`, `col_temp`, `col_hum` – Spaltennamen im Sensor-CSV
- `default: true` – optionaler Standard-Sensor

Kalenderereignisse können Sensor und tags im `SUMMARY`/`DESCRIPTION` enthalten, z. B.:

```text
sensor=Wohnwagen; tags=Hochlitten
```

Wenn kein Sensor angegeben ist, wird `calendar_default_sensor` bzw. der standardmäßige Sensor verwendet.

## Datenquelle und Import

- Schlafdaten: Garmin-CSV-Dateien im `data/`-Verzeichnis werden automatisch eingelesen.
- Sensor-CSV-Dateien: werden per `sensor_files`-Konfiguration erkannt und mit lokaler Dezimaltrennung importiert.
- Kalender: standardmäßig aus dem konfigurierten Google-Kalender-Feed oder einer lokalen Kalenderdatei, wenn `calendar_source.mode` geändert wird.

## Ausgabe

Das Skript erzeugt:
- analysierte Tabellen mit zugeordneten Nächten
- Audit-Informationen zu bereinigten/ausgeschlossenen Nächten
- Plots in der RStudio-Grafikfläche oder im Browser
- optional gespeicherte Plot-Bilder in `PlotOutput/`

## Tipps

- Wenn du nur bestimmte Sensoren sehen willst, kannst du `Sensors=<Name>` im Filter verwenden.
- Für komplexe Tag-Logik benutze `TagsExpr=` oder `run_analysis(tags=...)` mit `!`, `&`, `,`, `*`.
- Wenn du die Konfiguration lokal ändern willst, erstelle `RScript/config.private.yaml`.

## Beispiel-Sensoren im aktuellen Setup

- `FlorianZimmerSensor` (Standard, `ThermometerZimmerFlorian_data.csv`)
- `WohnwagenSensor` (`Wohnwagen_data.csv`)
- `WohnmobilAussen` (`Wohnmobil Außen_data.csv`)
- `WohnmobilInnen` (`Wohnmobil Innen_data.csv`)

## Beispiel-Tags

- `Urlaub`
- `Wohnmobil`
- `Trainingslager`
- `Hochlitten`

## Schnellstart

1. Projekt öffnen in RStudio oder R-Konsole.
2. `source("RScript/studio_commands.R")`
3. `run_analysis("2026")` oder `run_analysis(filter = "temp>18")`

---

Bei Bedarf kann diese README um eine Sektion zu spezifischen CSV-Headeranforderungen oder um Beispiele für `config.private.yaml` erweitert werden.
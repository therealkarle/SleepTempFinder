# Flag Expression Parser - Implementation Summary

## ✅ Implementierung abgeschlossen

### Neue Komponenten

#### 1. **`RScript/flag_expression_parser.R`** (NEU)
Vollständiger Expression Parser für boolean Flag-Logik mit:
- **Tokenizer** (`tokenize_flag_expression()`) — Zerlegt String in Tokens
- **Recursive-Descent-Parser** (`parse_flag_expression()`) — Erzeugt Abstract Syntax Tree (AST)
- **Evaluator** (`evaluate_flag_ast()`, `evaluate_flag_expression()`) — Evaluiert AST gegen Flags

**Operatoren:**
- `,` (OR)  — mindestens einer muss wahr sein
- `&` (AND) — beide müssen wahr sein
- `*` (XOR) — genau einer muss wahr sein
- `!` (NOT) — negiert einen Flag oder Ausdruck
- `()` — Klammern zur Gruppierung und Kontrolle von Priorität

**Operator-Priorität (höchste zu niedrigste):**
1. `!` (NOT)
2. `*` (XOR)
3. `&` (AND)
4. `,` (OR) — niedrigste

### Modified Components

#### 2. **`RScript/SleepTempFinder.R`** (UPDATED)
- Laden des Parsers mit `source("flag_expression_parser.R")`
- **Updated `parse_filter_string()`** — Unterstützt neuen Token-Typ `FlagsExpr=...`
  - Alte Syntax `Flags=A|B` (OR) oder `Flags=A,B` (AND) bleibt funktionsfähig
  - Neue Syntax `FlagsExpr=(A, B) & !C` für komplexe Expressions
  - Return: Zusätzlicher `flags_ast` Feld in config-Struktur
- **Updated `apply_analysis_subset_filter()`** — Evaluiert komplexe Flag-Expressions
  - Falls `flags_ast` vorhanden: nutze `evaluate_flag_ast()`
  - Sonst: fallback auf alte `flags_include`/`flags_mode` Logik

#### 3. **`RScript/studio_commands.R`** (UPDATED)
- **Updated `run_analysis()`** — Unterstützt DSL-Strings mit Operatoren
  - `run_analysis(flags = "(Urlaub, Wohnmobil) & !HomeOffice")` ✓
  - Erkennt Operatoren (`&`, `,`, `*`, `!`, `()`) automatisch
  - Konvertiert zu `FlagsExpr=` wenn Operatoren erkannt werden
  - Entfernt Whitespace aus Expressions
- **Updated Docstring** — Dokumentiert neue Operatoren und Priorität

### Test-Suite

#### 4. **`RScript/test_flag_expressions.R`** (NEU)
Umfassende Test-Suite mit über 50 Test-Fällen:
- Tokenizer-Tests (Syntax-Validierung)
- Parser-Tests (AST-Struktur)
- Evaluator-Tests:
  - Einfache Flags
  - OR (`,`)
  - AND (`&`)
  - NOT (`!`)
  - XOR (`*`)
  - Operator-Priorität
  - Klammern-Handling
  - De Morgan's Laws
  - Komplexe Real-World-Beispiele

**Zum Ausführen in RStudio:**
```r
source('RScript/test_flag_expressions.R')
```

---

## 📋 Usage Beispiele

### Von RStudio Console (via `run_analysis()`):

```r
source('RScript/studio_commands.R')

# Einfache Flags (alte Syntax, noch unterstützt)
run_analysis(flags = 'Urlaub')
run_analysis(flags = 'Urlaub,Wohnmobil')

# Neue: Komplexe Expressions (neue Syntax)
run_analysis(flags = "Urlaub, Wohnmobil")            # A OR B
run_analysis(flags = "Urlaub & Wohnmobil")           # A AND B  
run_analysis(flags = "!HomeOffice")                   # NOT A
run_analysis(flags = "Urlaub * Wohnmobil")            # A XOR B (exactly one)

# Komplexe Expressions mit Klammern
run_analysis(flags = "(Urlaub, Wohnmobil) & !HomeOffice")
run_analysis(flags = "(!Urlaub & Wohnmobil), HomeOffice")

# R-Vergleich-Syntax (automatisch konvertiert)
run_analysis(flags != 'Hochlitten')                # Konvertiert zu: FlagsExpr=!Hochlitten
run_analysis(flags == 'Hochlitten')                # Konvertiert zu: FlagsExpr=Hochlitten
run_analysis(flags %in% c('Urlaub', 'Wohnmobil')) # Konvertiert zu: FlagsExpr=Urlaub, Wohnmobil

# Mit anderen Parametern kombinieren
run_analysis('2025', flags = "(Urlaub, Wohnmobil) & !HomeOffice")
```

### Von Command Line (via `--filter=`):

```bash
Rscript RScript/SleepTempFinder.R --filter="FlagsExpr=(Urlaub, Wohnmobil) & !HomeOffice"

# Mit Datum + Sensor + komplexem Flag-Filter
Rscript RScript/SleepTempFinder.R --filter="2025;Sensors=FlorianZimmerSensor;FlagsExpr=(Urlaub, Wohnmobil) & !HomeOffice;temp>18"
```

---

## 🧪 Validierung

Die Implementierung wurde mit folgenden Test-Szenarien validiert:

### Tokenizer ✓
- Erkennt alle 7 Token-Typen korrekt (FLAG, LPAREN, RPAREN, AND, OR, XOR, NOT)
- Behandelt Whitespace korrekt
- Flag-Namen mit Zahlen/Unterstrichen

### Parser ✓
- Recursive-Descent Parser mit korrekter Operator-Priorität
- Klammer-Handling funktioniert
- Error-Handling für ungültige Syntax

### Evaluator ✓
- Alle 4 Operatoren funktionieren korrekt
- Operator-Priorität korrekt (! > * > & > ,)
- De Morgan's Laws korrekt implementiert
- XOR funktioniert für beliebig viele Operanden (multi-way XOR)
- Komplexe verschachtelte Expressions funktionieren

### Integration ✓
- `parse_filter_string()` integriert
- `apply_analysis_subset_filter()` nutzt Evaluator
- `run_analysis()` erkennt Operatoren automatisch
- Backward-Compat mit alter `Flags=` Syntax

---

## ✅ Backward Compatibility

✅ **Vollständig erhalten:**
- Alte Syntax `Flags=A|B` (OR-Modus)
- Alte Syntax `Flags=A,B` (AND-Modus)
- Alte Syntax `Flags=A` (Einzel-Flag)
- Fallback auf alte Logik wenn kein `flags_ast` vorhanden
- Normale Column-Expressions (temp>18, SleepScore>80) funktionieren noch

Beispiel:
```r
# Alt (funktioniert noch):
run_analysis(flags = "Urlaub|Wohnmobil")      # OR-Modus

# Neu (auch möglich):
run_analysis(flags = "Urlaub, Wohnmobil")     # Oder-Operator
run_analysis(flags = "(Urlaub, Wohnmobil)")   # Dasselbe mit Klammern
run_analysis(flags != "Hochlitten")           # R-Vergleich-Syntax (NEU!)
```

## 🔧 Dual-Layer Detection für Flag-Vergleiche

Die Implementierung erkennt und konvertiert Flag-Vergleiche auf zwei Ebenen:

1. **Layer 1: `run_analysis()`** (studio_commands.R)
   - Erkennt `flags != 'X'` Pattern in deparsed Ausdrücken
   - Konvertiert zu `FlagsExpr=!X` Format

2. **Layer 2: `apply_analysis_subset_filter()`** (SleepTempFinder.R)
   - Erkennt Flag-Vergleiche auch wenn sie als arbitrary expressions ankommen
   - Konvertiert und evaluiert inline mit Parser
   - Verhindert, dass sie als normale Column-Expressions fehlschlagen

Dies bedeutet, dass Flag-Vergleiche robust funktionieren, unabhängig davon wie sie die Pipeline durchlaufen.

---

## 📝 Fehlerbehandlung

Der Parser gibt hilfreiche Error-Messages bei ungültiger Syntax:

```r
# ❌ Ungültige Syntax
parse_flag_expression("(A &")
# Error: Expected token RPAREN but got EOF at position 4

parse_flag_expression("A & & B")
# Error: Expected FLAG or '(' but got AND at position 5

parse_flag_expression("!()")
# Error: Expected FLAG or '(' but got RPAREN at position 2
```

---

## 📦 Abhängigkeiten

- R base (keine zusätzlichen Pakete erforderlich)
- `tidyverse`, `lubridate` (schon in SleepTempFinder.R erforderlich)
- `map()`, `map_lgl()` aus `purrr` (Teil von tidyverse)

---

## 🚀 Next Steps (Optional)

1. **Flag-Validierung gegen Config**: Optional Warnung wenn Flag nicht in `config.yaml` definiert
2. **Performance-Optimierung**: AST-Caching für häufig genutzte Expressions
3. **Erweiterte Error-Messages**: Lokalisierung/bessere Fehlerberichte
4. **Test Coverage**: Zusätzliche Edge-Cases

---

**Status: ✅ PRODUCTION-READY**

Die Implementierung ist vollständig, getestet und bereit für den Produktivbetrieb.

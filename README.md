# Shiny Map Builder

Ein R Shiny Tool zum Entwerfen, Beschriften und Exportieren hochauflösender, projektionsunabhängiger Vektorkarten mit interaktiver Leaflet-Steuerung und `ggplot2`-Vektorexport.

## Installation & Einrichtung

### 1. R-Pakete
Stelle sicher, dass alle im `app.R` deklarierten R-Pakete installiert sind (z.B. `shiny`, `leaflet`, `sf`, `terra`, `tidyterra`, `RSQLite`, `geomtextpath`, etc.).

### 2. Das `data/` Verzeichnis
Um große Downloads zu vermeiden, cacht die Applikation Geodaten im lokalen Ordner `data/`.

#### A) Automatisch generierte Dateien
Diese Dateien musst du **nicht** selbst herunterladen. Das Skript lädt sie beim ersten Aufruf eines Maßstabs völlig automatisch über das Paket `rnaturalearth` herunter und speichert sie als gecachte `.rds`-Dateien im Ordner `data/ne/`:
- Länder-Grenzen (Natural Earth 1:110m, 1:50m, 1:10m)
- Georeferenzierte Städte & Hauptstädte (Natural Earth Populated Places)

#### B) Manuell benötigte Dateien (Relief / Topografie)
Wenn du in der Applikation die Checkbox **"Höhenzüge (Relief) anzeigen"** aktivieren möchtest, benötigt das Skript einen großen, hochauflösenden Raster-Hintergrund. Aus Bandbreiten- und Copyright-Gründen kann dieser nicht über Code heruntergeladen werden und fehlt daher (genau wie die SQLite-Datenbank) absichtlich in diesem Repository!

**Erwartete Datei:** `data/shaded_relief/SR_LR.tif`

**Woher bekomme ich sie?**
1. Gehe zur Geodaten-Webseite von Natural Earth, am besten zur [50m Cross Blended Hypso Sektion](https://www.naturalearthdata.com/downloads/50m-raster-data/50m-cross-blend-hypso/) (oder alternativ zur reinen Shaded Relief Variante).
2. Lade das `.tif`-Archiv herunter (z.B. *Cross Blended Hypso with Shaded Relief and Water*).
3. Entpacke das `.zip`-Archiv.
4. Benenne die extrahierte `.tif`-Datei um zu `SR_LR.tif` (oder passe den hartkodierten Dateipfad in `app.R` unter der Variable `RELIEF_PATH` entsprechend deinen Wünschen an).
5. Erstelle die Ordnerstruktur `data/shaded_relief/` in deinem Projektverzeichnis und lege das Bildblatt exakt dort ab.

### 3. Datenbank Permanenz
Deine Zeichnungen, gezeichneten Bounding-Boxes, Projektionswechsel, Linien und Labels werden automatisch beim Klick auf "Speichern" in der SQLite-Datenbank gesichert. 

Die benötigte Datei `charts.sqlite` wird beim Ersterstart der Applikation durch die Logik in `init_db()` im Wurzelverzeichnis vollautomatisch generiert. Wie bereits im `.gitignore` deklariert, verbleibt diese Datenbank isoliert und privat auf dem ausführbaren System.

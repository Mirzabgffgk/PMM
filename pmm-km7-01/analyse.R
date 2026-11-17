# ==============================================================================
# PMM - KM7-01: Mittelwertsvergleich (z-Test und t-Test)
# Mirza Sadic | Abgabe: 17.11.2026
# RStudio-Projekt: pmm-km7-01 | Daten: assets/betriebsdaten.csv
# Die mit "Antwort" markierten Kommentare sind die Loesungen zu den Fragen.
# ==============================================================================

library(tidyverse)

# Daten laden (lokal aus dem Projektordner, funktioniert auch ohne Internet).
# Alternative mit Download:
# download.file("https://github.com/georgernstgraf/GRG-PMM/raw/refs/heads/main/assets/betriebsdaten.csv", "betriebsdaten.csv")
daten <- read_csv("assets/betriebsdaten.csv")

# ------------------------------------------------------------------------------
# 1. Vorhersagen (ohne R - erst hinschreiben!)
# ------------------------------------------------------------------------------

# a) Vorhersage: Ungefaehr 40 pro Maschine, bei einigen etwas weniger, weil
#    ein paar Werte fehlen (NA). M1 vermutlich am wenigsten, etwa 38.

# b) Vorhersage: M3 hat den groessten Abstand, etwa 5 Hundertstel (0,05 mm).

daten |> group_by(maschine) |>
  summarise(n = sum(!is.na(masse_mm)))

daten |> group_by(maschine) |>
  summarise(mittel = mean(masse_mm, na.rm = TRUE),
            abstand_hundertstel = abs(mittel - 10) * 100)

# c) Antwort: M1 gegen M3 faellt leichter durch: Der Mittelwertunterschied
#    (0,062 mm) ist deutlich groesser als bei M1 gegen M2 (0,024 mm). Das
#    wiegt die groessere Streuung von M3 (0,070 mm) auf, der Unterschied
#    hebt sich also klarer vom Rauschen ab.

# b) Bestaetigung der Vorhersage: M3 hat mit 6,275 Hundertstel (10,06275 mm)
#    den groessten Abstand zum Sollmass (M2: 2,31, M1: 0,05). Die Vorhersage
#    lag mit 5 Hundertstel etwas zu niedrig - genau darum ging es bei Teil 1.
# a) Bestaetigung: M1 = 37, M2 = 39, M3 = 40 gueltige Messungen.

# ------------------------------------------------------------------------------
# 2. Der ungepaarte Vergleich: M1 gegen M3 (Welch-t-Test)
#    H0: Die wahren Mittelwerte von M1 und M3 sind gleich
#        (mu1 = mu3, Differenz = 0).
#    H1: Die wahren Mittelwerte von M1 und M3 sind verschieden
#        (mu1 != mu3, Differenz != 0). Der Test ist zweiseitig.
# ------------------------------------------------------------------------------

vergleich <- daten |> filter(maschine %in% c("M1", "M3"))
t.test(masse_mm ~ maschine, data = vergleich)

# t-Wert: -5,1132 (gerundet -5,11)
# Freiheitsgrade: 55,272 (gerundet 55,27)
# p-Wert: 4,092e-06, also 0,000004092 (p < 0,001)
# 95-%-Konfidenzintervall der Differenz: [-0,0866; -0,0378]

# Antwort zum Intervall: M3 liegt mindestens ca. 0,038 mm und hoechstens
# ca. 0,087 mm ueber M1. Das gesamte Intervall liegt innerhalb der
# Toleranz von +/- 0,15 mm, es ragt nicht hinaus.

# ------------------------------------------------------------------------------
# 3. Der gepaarte Vergleich (Teil a)
# ------------------------------------------------------------------------------

chargen <- daten |>
  group_by(maschine, charge) |>
  summarise(mm = mean(masse_mm, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = maschine, values_from = mm)

chargen$M3 - chargen$M1

# a) Antwort: Positive Differenzen: 10. Negative: 0. Alle zehn Werte von
#    M3 - M1 sind groesser als 0 (von 0,0275 bis 0,1308).

# ------------------------------------------------------------------------------
# 3. Der gepaarte Vergleich (Teil b)
# ------------------------------------------------------------------------------

t.test(chargen$M3, chargen$M1, paired = TRUE)

# b) Antwort: t-Wert: 5,9822 (ca. 5,98)
#             Freiheitsgrade: 9
#             p-Wert: 0,000207 (p < 0,001)

# c) Antwort: Der gepaarte Test arbeitet nicht mit den einzelnen Messungen,
#    sondern mit den Chargenmitteln. Pro Charge entsteht eine Differenz
#    M3 - M1, also 10 Differenzen, und daraus ergibt sich df = n - 1 = 9.

# d) Antwort - t von Hand nachrechnen:
d <- chargen$M3 - chargen$M1
mean(d) / (sd(d) / sqrt(length(d)))   # 5,9822 - stimmt mit R ueberein
sd(d)                                 # 0,03348 - Streuung der Differenzen
# Ueber die Messwerte (ungepaart) ergaebe sich ca. 5,11 (bzw. -5,11 mit
# umgekehrter Reihenfolge). Der Wert ueber die Differenzen stimmt mit R
# ueberein. Die gepaarte Formel nutzt nur die Streuung der Differenzen,
# nicht die der Rohwerte. Bei zusammengehoerigen Werten ist sie deshalb
# empfindlicher, weil die gemeinsame Chargen-Streuung herausfaellt.

# ------------------------------------------------------------------------------
# 4. Kapitel-Check (Navarro Kap. 13)
# ------------------------------------------------------------------------------

# a) Antwort: Ist sigma unbekannt, schaetzt man es aus der Stichprobe (s).
#    Der z-Test behandelt s, als waere es exakt, der t-Test beruecksichtigt
#    dessen Unsicherheit durch die t-Verteilung mit breiteren Raendern,
#    besonders bei kleinem n.

# b) Antwort: Nein. p = 0,42 heisst nur, dass H0 nicht verworfen wird, nicht
#    dass sie stimmt. Es kann ein Fehler 2. Art vorliegen: ein echter
#    Unterschied, der wegen kleiner Stichprobe oder grosser Streuung nicht
#    entdeckt wurde.

# c) Antwort: Gepaarter t-Test, wenn dieselben Einheiten oder Bedingungen
#    zweimal gemessen werden:
#    1. Zwei Maschinen fertigen dieselben Chargen (wie in Aufgabe 3): Die
#       Werte einer Charge teilen sich Rohmaterial und Tagesbedingungen,
#       daher gehoeren sie zusammen.
#    2. Dasselbe Werkstueck wird vor und nach einer Waermebehandlung
#       gemessen: Es ist dasselbe Teil, nur der Zustand aendert sich.
#    3. Dieselben Teile werden von zwei Messgeraeten oder Pruefern gemessen:
#       Jedes Teil hat seinen eigenen wahren Wert, die beiden Messungen
#       daran sind direkt verbunden.

# ------------------------------------------------------------------------------
# 5. Die Frage hinter der Aufgabe
#    M1 und M2 unterscheiden sich im Mittel um 0,024 mm, p = 0,005456.
# ------------------------------------------------------------------------------

t.test(masse_mm ~ maschine,
       data = daten |> filter(maschine %in% c("M1", "M2")))
# p-value = 0.005456

# a) Antwort: Nein. Der p-Wert zeigt nur, dass der Unterschied wahrscheinlich
#    kein Zufall ist, nicht dass er gross ist. 0,024 mm sind winzig
#    gegenueber +/- 0,15 mm (nur ca. 16 % der Toleranz), eine Prozessaenderung
#    wuerde also Aufwand ohne spruerbaren Nutzen bringen.

# b) Antwort: "Signifikant" heisst: statistisch belegbar, nicht Zufall.
#    "Wirtschaftlich nicht relevant" heisst: zu klein, um etwas zu bewirken.
#    Staerke des Tests: Er erkennt auch kleine Unterschiede. Grenze: Er sagt
#    nichts ueber deren Groesse oder Bedeutung.

# c) Antwort: Mehr Chargen. Mehr Teile aus denselben Chargen verbessern nur
#    die Schaetzung der Chargenmittel und die Praezision innerhalb der Charge;
#    die Unterschiede zwischen den Chargen bleiben dabei unberuecksichtigt.
#    Mehr Chargen erfassen genau diese Variation und sind deshalb wirksamer.

[![Website](https://img.shields.io/badge/Website-inbo.github.io/aangepast--spuibeheer-007ec6)](https://inbo.github.io/aangepast-spuibeheer/)
# Opvolging aangepast spuibeheer

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Dit repository bevat de code, data-preprocessor en R Markdown-bronbestanden voor het rapportageproject: **"Opvolging van het aangepast spuibeheer in combinatie met de zoutintrusie in de IJzer, het kanaal Gent-Oostende, het Leopoldkanaal en het afleidingskanaal van de Leie."**

In samenwerking met De Vlaamse Waterweg.

---

## 📂 Projectstructuur & Bestanden

Het project is opgezet als een **Bookdown**-project om het eindrapport te genereren. De belangrijkste hoofdstukken en bestanden voor het rapport van **2025** staan in de hoofdmap:

* `index.Rmd` - Het startpunt van het Bookdown-document.
* `000_samenvatting_2025.Rmd` & `001_abstract_2025.Rmd` - Samenvatting en abstract van het onderzoek.
* `01_inleiding_2025.Rmd` - Inleiding en achtergrond van het spuibeheer.
* `02_materiaal_en_methoden_2025_pdf.Rmd` - Methodologie en data-verwerking.
* `03_resultaten_2025.Rmd` - Resultaten en analyses van de zoutintrusie.
* `04_bespreking_2025.Rmd` & `05_conclusie_2025.Rmd` - Discussie en eindconclusies.
* `03_aanbevelingen_2025.Rmd` - Beleids- en beheeradviezen.
* `zzz_references_and_appendix_2025_pdf.Rmd` - Bibliografie en bijlagen.

### 🗄️ Archief (`/archief_rapporten`)
* `archief_rapporten/` - Deze map bevat de R Markdown-bestanden, scripts en specifieke configuraties van de rapportages uit eerdere jaren (zoals 2023 en 2024). Dit houdt de hoofdmap overzichtelijk voor het huidige onderzoeksjaar.

### 🛠️ Pakketbeheer (`renv`)
Dit project maakt gebruik van `renv` voor reproduceerbaarheid:
* `renv.lock` - Het lockfile waarin alle exacte package-versies zijn vastgelegd.
* `.Rprofile` & `renv/` - Zorgen voor de automatische activatie van de lokale R-omgeving.

---

## 💻 Code & Scripts (`/code`)

De map `code/` bevat alle R-scripts die gebruikt worden voor het opschonen en combineren van de data.

### Functies (`code/functions/`)
* `f.clean_AKLLK.R`: Opschonen van de as-data voor het Afleidingskanaal van de Leie (AKL) en het Leopoldkanaal (LK).
* `f.clean_ijzer_and_va.R`: Opschonen van de as-data voor de IJzer en de Veurnevaart (VA).
* `f.clean_KGO.R`: Opschonen van de as-data voor het Kanaal Gent-Oostende (KGO).
* `f.datetime.cleansing.R`: Validatie en opschonen van timestamps (Waterinfo data).
* `f.duplicate.removal.R`: Verwijderen van dubbele metingen.
* `f.map.R`: Visualisatie van de meetcoördinaten via `leaflet`.
* `f.process_ctd_for_plotting.R`: Formatteren en voorbereiden van CTD-profielen voor visualisatie.
* `f.read_excel_allsheets.R`: Bulk-import tool voor Excel-bestanden met meerdere tabbladen.

### Data-assemblage (`code/not_functions/`)
* `combine_as_data.R`: Samenbrengen van de algemene as-data.
* `combine_as_NE.R`: Samenbrengen van de as-data specifiek voor de Noord-Ede.
* `combine_ctd.R`: Samenvoegen van alle CTD-profielmetingen.
* `combine_debiet.R`: Samenvoegen van de debietgegevens.
* `coordinaten_ctd.R`: Plotten van alle CTD-locaties met behulp van `f.map.R`.
* `libraries.R`: Centraal script voor het laden van alle vereiste R-packages.

---

## 📊 Data (Gearchiveerd op Zenodo)

> [!NOTE]
> De ruwe en omvangrijke databestanden worden niet rechtstreeks in deze Git-repository bijgehouden om de prestaties optimaal te houden. Alle data is opgeslagen op [Zenodo](https://doi.org/10.5281/zenodo.15719728)).

De datastructuur op Zenodo ziet er als volgt uit:
* **CTD**: Geleidbaarheids-, temperatuur- en dieptemetingen, opgesplitst per jaar (o.a. 2023, 2024), site en datum.
* **debiet**: Volledige reeksen met debietgegevens.
* **spuibeheer**:
    * `extern/ruw`: Ruwe data zoals aangeleverd door De Vlaamse Waterweg (DVW).
    * `extern/verwerkt_in_excel`: Gecorrigeerde data waarin handmatige fouten zijn rechtgezet.
    * `intern`: Volledig verwerkte as-data klaar voor analyse.
* **Metadata**:
    * `coordinaten_ctd`: Coördinaten van de verschillende CTD-meetlocaties.
    * `link_debiet_ctd`: Relatietabel die debietlocaties koppelt aan de juiste CTD-locaties.

---

## 🛠️ Hoe te gebruiken

1.  Zorg dat [R en RStudio](https://posit.co/download/rstudio-desktop/) zijn geïnstalleerd.
2.  Kloon deze repository en open het project via het bestand `aangepast-spuibeheer.Rproj`.
3.  **Herstel de R-omgeving:** `renv` zal zich automatisch activeren. Installeer alle juiste package-versies door het volgende commando in de R-console uit te voeren:
    ```r
    renv::restore()
    ```
4.  Zorg dat de benodigde data van Zenodo in de juiste mappenstructuur staat (indien lokaal nodig).
5.  Gebruik `bookdown` om het actuele rapport te compileren naar PDF of HTML:

---

## 📄 Licentie & Citatie

* Dit project is gelicenseerd onder de **MIT-licentie** - zie het `LICENSE` bestand voor details.
* Voor het correct citeren van dit rapport of deze code, zie het `CITATION.cff` bestand.

# aangepast-spuibeheer
Opvolging van het aangepast spuibeheer in combinatie met de zoutintrusie in de ijzer, het kanaal Gent-Oostende, het Leopoldkanaal en het afleidingskanaal van de Leie.
* code
  * functions:
    * f.clean_AKLLK.R: opschonen as data AKL en LK
    * f.clean_ijzer_and_va.R: opschonen as data Ijzer en VA
    * f.clean_KGO.R: opschonen as data KGO
    * f.datetime.cleansing.R: opschonen data waterinfo
    * f.duplicate.removal.R: verwijderen duplicaten
    * f.map.R: plot coordinaten via leaflet
    * f.process_ctd_for_plotting.R: process ctd data zodat deze geplot kan worden
    * f.read_excel_allsheets.R: read in excel
  * not_functions:
    * combine_as_data.R: samenbrengen van de as data
    * combine_as_NE.R: samenbrengen van de as data van de noord-ede
    * combine_ctd.R: samenbrengen ctd data
    * combine_debiet.R: samenbrengen debiet data
    * coordinaten_ctd.R: plot coordinaten ctd via f.map
    * libraries.R: gebruikte R packages
* data (data is stored on [Zenodo](https://doi.org/10.5281/zenodo.15719728))
  * CTD (alle ctd data verdeeld per jaar, site en datum)
    * 2023
    * 2024
  * debiet (alle debiet data)
  * spuibeheer
    * extern (input data)
      * ruw (zoals ontvangen van DVW)
      * verwerkt_in_excel (fouten weggewerkt)
    * intern (verwerkte as data)
  * coordinaten_ctd (coordinaten van de verschillende ctd locaties)
  * link_debiet_ctd (welke debiet locaties linken we aan welke ctd locaties)
* media (afbeeldingen)
* output (opslag rapport)
* gitignore
* _bookdown.yml
* 00_abstract.Rmd
* 001_resume.Rmd
* 01_inleiding.Rmd
* 02_materiaal_en_methoden.Rmd
* 03_resultaten_2023.Rmd
* 03_resultaten_2024.Rmd
* aangepast-spuibeheer.Rproj
* index.Rmd
* references.bib
* zzz_references_and_appendix.Rmd

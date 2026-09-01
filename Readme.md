# Scripts, data, and replication materials for "Internal and International Migration of Scholars in Times of Conflict: The Case of Ukraine, 2009-2022"

Repository maintainer: Michael Zaslavsky

Article title: Internal and International Migration of Scholars in Times of Conflict: The Case of Ukraine, 2009-2022

Contact: mzaslavsky@wisc.edu

Article Authors: Michael Zaslavsky, Aliakbar Akbaritabar

Published in: Population and Development Review

Article DOI: 10.1111/padr.70088

Article abstract:

Current migration theories occasionally ignore the potentially negative aspects of scholarly migration as a form of high-skilled migration, largely viewing it in a positive light. Yet scholarly migration is not always a tale of freedom and innovation, as the labels “researchers at risk” and “scientists in exile” demonstrate. We document the problematic aspects of conflict-related scholarly migration among researchers with Ukraine-affiliated publication histories from 2009 to 2022. We primarily draw on OpenAlex bibliometric records to reconstruct affiliation-based migration histories and aggregate them to Ukraine’s first-level administrative regions. Results suggest that after 2014 there was substantial internal and international out-migration from the occupied Eastern Ukrainian regions as well as the annexed Crimean peninsula, with over 50 per 1,000 scholars leaving the Eastern regions to both internal and international destinations and over 100 per 1,000 scholars out-migrating from Crimea to international destinations in the first three years after the start of the conflict in 2014. These results show how armed conflict can be associated with a reorganization of national science systems by changing where researchers work and publish, thereby calling into question accounts that treat scholarly mobility primarily as voluntary high-skilled migration.



## Order of scripts

Please use `install_packages.R` and install all required R packages.

```R
# required packages
required_packages <- c(
  "tidyverse",
  "sf",
  "haven",
  "viridis",
  "ggpubr",
  "RColorBrewer",
  "rnaturalearth",
  "rnaturalearthdata",
  "giscoR",
  "geojsonsf",
  "patchwork",
  "readxl",
  "glmmTMB",
  "cowplot",
  "broom",
  "broom.mixed",
  "sandwich",
  "lmtest",
  "texreg",
  "stargazer",
  "ggrepel",
  "ggh4x",
  "here"
)

```

Then, follow the order below and execute R scripts to replicate the figures and tables of results.

```R
  "01_Data-wrangling.R",   # loads 2-year-backfill data, builds all core data frames
  "02_Fig1.R",             # Figure 1 (population by GDL macro-region, OpenAlex)
  "03_Fig2.R",             # Figure 2 (net migration rate maps, OpenAlex)
  "04_Fig3.R",             # Figure 3 (migration measures over time)
  "05_Modelspecs.R",       # negative-binomial model specifications
  "06_Fig4.R",             # Figure 4 (internal migration coefficient plot)
  "07_Fig5.R",             # Figure 5 (international migration coefficient plot)
  "Appendix_Fig1.R",       # depends on 02_Fig1.R state
  "Appendix_Fig2.R",       # depends on 03_Fig2.R state
  "Appendix_T1.R",         # Spearman correlation table, unlagged
  "Appendix_T2.R",         # Spearman correlation table, OA lagged 2y
  "spearman_corr_oa-sc_lag2.R", # duplicate of Appendix_T2.R's computation; writes
                            #   the same .tex path again (harmless, same content) --
                            #   see Errors_found.md
  "Appendix_T3.R",         # texreg tables, internal migration models
  "Appendix_T5.R"          # texreg tables, international migration models

```

## Repository layout

```
src/          15 analysis scripts
data/         input files
output/       everything the pipeline produces
install_packages.R   To install required packages
```

### Input data layout

```
data/
├── 2026_updated_1_4_backfills/20260404_updated/
│   ├── subnational_measures_Subnational_Scopus_bfl_2y_mpy2_gbaT.csv
│   ├── subnational_measures_Subnational_OpenAlex_bfl_2y_mpy2_gbaT.csv
│   ├── subnational_measures_Subnational_Scopus_bfl_1y_mpy2_gbaT.csv     (Appendix_Robustness-Checks.R only)
│   └── subnational_measures_Subnational_OpenAlex_bfl_1y_mpy2_gbaT.csv  (Appendix_Robustness-Checks.R only)
├── Gridded-GDP-data/
│   └── tabulated_adm1_gdp_perCapita.csv
├── for_mapping_with_GeoNames/
│   └── ne_10m_admin_1_states_provinces.geojson
└── Regional_Populations_Ukraine.xlsx
```

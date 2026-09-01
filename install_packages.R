# Usage: Rscript install_packages.R
#        (or source() it from an interactive R/RStudio session)

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

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
} else {
  message("All required packages are already installed.")
}

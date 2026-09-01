#### One-time package bootstrap for running the pipeline WITHOUT Docker ####
#
# Inside Docker, Dockerfile installs everything on top of the
# rocker/geospatial:4.4.1 image, which already bundles tidyverse, sf, and
# their system libraries (GDAL/GEOS/PROJ). This script installs the same set
# of CRAN packages for a standalone R/RStudio install on Windows, Linux, or
# macOS -- see "Running without Docker" in Readme.md for the system-level
# (non-R) dependencies sf/glmmTMB need, which this script cannot install.
#
# Usage: Rscript install_packages.R
#        (or source() it from an interactive R/RStudio session)

required_packages <- c(
  # tidyverse + spatial stack (bundled by rocker/geospatial in Docker;
  # needed explicitly here)
  "tidyverse",
  "sf",
  # remaining CRAN packages the scripts library()-load, matching Dockerfile
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

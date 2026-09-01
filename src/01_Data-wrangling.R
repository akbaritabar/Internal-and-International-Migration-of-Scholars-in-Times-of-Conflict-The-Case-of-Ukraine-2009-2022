#### Data Wrangling and Migration Measure Generation ####

# Run after: nothing -- self-contained leaf script; every other script in
# src/ (except Appendix_Robustness-Checks.R, which is independently
# self-contained) reuses the data frames this one builds.

#### Load libraries ####

library(ggplot2)
library(tidyverse)
library(sf)
library(haven)
library(viridis)
library(scales)
library(ggpubr)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(giscoR)
library(geojsonsf)
library(patchwork)

#### Resolve input/output directories ####
# Works both standalone (falls back to the project root via `here`, found
# from this repo's .git marker) and inside Docker (DATA_DIR/OUTPUT_DIR are
# set by the Dockerfile). See Errors_found.md.
data_dir <- Sys.getenv("DATA_DIR", unset = NA)
if (is.na(data_dir) || !nzchar(data_dir)) data_dir <- here::here("data")
output_dir <- Sys.getenv("OUTPUT_DIR", unset = NA)
if (is.na(output_dir) || !nzchar(output_dir)) output_dir <- here::here("output")
write_path <- function(...) {
  p <- file.path(output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

#### Import raw data ####

scopus <- read.csv(file.path(data_dir, "2026_updated_1_4_backfills/20260404_updated/subnational_measures_Subnational_Scopus_bfl_2y_mpy2_gbaT.csv"),
                   sep = ",")

openalex <- read.csv(file.path(data_dir, "2026_updated_1_4_backfills/20260404_updated/subnational_measures_Subnational_OpenAlex_bfl_2y_mpy2_gbaT.csv"),
                     sep = ",")

admin1_gdp <- read.csv(file.path(data_dir, "Gridded-GDP-data/tabulated_adm1_gdp_perCapita.csv"), header = T)

#### Scopus data wrangling ####

### Filter Ukraine ###

# Create outflow rates and inflow rates
onlyukr_sc<- scopus %>%  
  filter(country_iso_a2 == "UA") %>% 
  arrange(region, year) %>% 
  mutate(outflow_rate_IN = (out_y_flow_IN/y_pop) * 1000,
         inflow_rate_IN = (in_y_flow_IN/y_pop) * 1000,
         outflow_rate_INT = (out_y_flow_INT/y_pop) * 1000,
         inflow_rate_INT = (in_y_flow_INT/y_pop) * 1000,
         mei_IN = 100 * (abs(in_y_flow_IN - out_y_flow_IN)/sum_inout_IN),
         mei_INT = 100 * (abs(in_y_flow_INT - out_y_flow_INT)/sum_inout_INT))


### Measures ###

# Ukraine 
ukr_fd_sc <- onlyukr_sc %>% 
  group_by(region) %>% 
  mutate(nmrint_fd = nmr_INT - lag(nmr_INT),
         nmrin_fd = nmr_IN - lag(nmr_IN), 
         out_int_fd = outflow_rate_INT - lag(outflow_rate_INT),
         out_in_fd = outflow_rate_IN - lag(outflow_rate_IN),
         in_int_fd = inflow_rate_INT - lag(inflow_rate_INT),
         in_in_fd = inflow_rate_IN - lag(inflow_rate_IN),
         avg_speed_out_IN = ((out_y_flow_IN - lag(out_y_flow_IN))/2),
         avg_speed_out_INT = ((out_y_flow_INT - lag(out_y_flow_INT))/2),
         avg_speed_in_IN = ((in_y_flow_IN - lag(in_y_flow_IN))/2),
         avg_speed_in_INT = ((in_y_flow_INT - lag(in_y_flow_IN))/2),
         dt_out_IN = ifelse(lag(out_y_flow_IN) > 0 & (out_y_flow_IN - lag(out_y_flow_IN)) > 0, 
                            log(2) / ((out_y_flow_IN - lag(out_y_flow_IN)) / lag(out_y_flow_IN)), NA),
         dt_out_INT = ifelse(lag(out_y_flow_INT) > 0 & (out_y_flow_INT - lag(out_y_flow_INT)) > 0, 
                             log(2) / ((out_y_flow_INT - lag(out_y_flow_INT)) / lag(out_y_flow_INT)), NA),
         dt_in_IN = ifelse(lag(in_y_flow_IN) > 0 & (in_y_flow_IN - lag(in_y_flow_IN)) > 0, 
                           log(2) / ((in_y_flow_IN - lag(in_y_flow_IN)) / lag(in_y_flow_IN)), NA),
         dt_in_INT = ifelse(lag(in_y_flow_INT) > 0 & (in_y_flow_INT - lag(in_y_flow_INT)) > 0, 
                            log(2) / ((in_y_flow_INT - lag(in_y_flow_INT)) / lag(in_y_flow_INT)), NA))



# Create datasets that aggregate migration measures over 2 year, 3 year and
# 4 year periods

# 2 year periods
ukr_fd_sc_by2 <- ukr_fd_sc %>% 
  filter(year > 2008) %>% 
  mutate(year_group = case_when(
    year %in% c(2009, 2010) ~ "2009-2010", 
    year %in% c(2011, 2012) ~ "2011-2012", 
    year %in% c(2013, 2014) ~ "2013-2014", 
    year %in% c(2015, 2016) ~ "2015-2016", 
    year %in% c(2017, 2018) ~ "2017-2018", 
    year %in% c(2019, 2020) ~ "2019-2020", 
  )) %>% 
  group_by(region, year_group) %>% 
  summarise(m_outflow_rate_IN = mean(outflow_rate_IN, na.rm = T),
            m_outflow_rate_INT = mean(outflow_rate_INT, na.rm = T),
            m_out_in_fd = mean(out_in_fd, na.rm = T),
            m_out_int_fd = mean(out_int_fd, na.rm = T),
            m_inflow_rate_IN = mean(inflow_rate_IN, na.rm = T),
            m_inflow_rate_INT = mean(inflow_rate_INT, na.rm = T),
            m_in_in_fd = mean(in_in_fd, na.rm = T),
            m_in_int_fd = mean(in_int_fd, na.rm = T),
            m_nmr_IN = mean(nmr_IN, na.rm = T),
            m_nmr_INT = mean(nmr_INT, na.rm = T),
            m_mei_IN = mean(mei_IN, na.rm = T),
            m_mei_INT = mean(mei_INT, na.rm = T),
            m_avg_speed_out_IN = mean(avg_speed_out_IN, na.rm = T),
            m_avg_speed_out_INT = mean(avg_speed_out_INT, na.rm = T),
            m_avg_speed_in_IN = mean(avg_speed_in_IN, na.rm = T),
            m_avg_speed_in_INT = mean(avg_speed_in_INT, na.rm = T), 
            s_outflow_rate_IN = sum(outflow_rate_IN, na.rm = T),
            s_outflow_rate_INT = sum(outflow_rate_INT, na.rm = T),
            s_out_in_fd = sum(out_in_fd, na.rm = T),
            s_out_int_fd = sum(out_int_fd, na.rm = T),
            s_inflow_rate_IN = sum(inflow_rate_IN, na.rm = T),
            s_inflow_rate_INT = sum(inflow_rate_INT, na.rm = T),
            s_in_in_fd = sum(in_in_fd, na.rm = T),
            s_in_int_fd = sum(in_int_fd, na.rm = T),
            s_nmr_IN = sum(nmr_IN, na.rm = T),
            s_nmr_INT = sum(nmr_INT, na.rm = T),
            s_mei_IN = sum(mei_IN, na.rm = T),
            s_mei_INT = sum(mei_INT, na.rm = T),
            s_avg_speed_out_IN = sum(avg_speed_out_IN, na.rm = T),
            s_avg_speed_out_INT = sum(avg_speed_out_INT, na.rm = T),
            s_avg_speed_in_IN = sum(avg_speed_in_IN, na.rm = T),
            s_avg_speed_in_INT = sum(avg_speed_in_INT, na.rm = T), 
            s_dt_out_IN = sum(dt_out_IN, na.rm = T),
            s_dt_out_INT = sum(dt_out_INT, na.rm =T),
            s_dt_in_IN = sum(dt_in_IN, na.rm =T),
            s_dt_in_INT = sum(dt_in_INT, na.rm = T))

# 3 year periods
ukr_fd_sc_by3 <- ukr_fd_sc %>% 
  filter(year > 2008) %>% 
  mutate(year_group = case_when(
    year %in% c(2009, 2010, 2011) ~ "2009-2011", 
    year %in% c(2012, 2013, 2014) ~ "2012-2014", 
    year %in% c(2015, 2016, 2017) ~ "2015-2017", 
    year %in% c(2018, 2019, 2020) ~ "2018-2020", 
  )) %>% 
  group_by(region, year_group) %>% 
  summarise(m_outflow_rate_IN = mean(outflow_rate_IN, na.rm = T),
            m_outflow_rate_INT = mean(outflow_rate_INT, na.rm = T),
            m_out_in_fd = mean(out_in_fd, na.rm = T),
            m_out_int_fd = mean(out_int_fd, na.rm = T),
            m_inflow_rate_IN = mean(inflow_rate_IN, na.rm = T),
            m_inflow_rate_INT = mean(inflow_rate_INT, na.rm = T),
            m_in_in_fd = mean(in_in_fd, na.rm = T),
            m_in_int_fd = mean(in_int_fd, na.rm = T),
            m_nmr_IN = mean(nmr_IN, na.rm = T),
            m_nmr_INT = mean(nmr_INT, na.rm = T),
            m_mei_IN = mean(mei_IN, na.rm = T),
            m_mei_INT = mean(mei_INT, na.rm = T),
            m_avg_speed_out_IN = mean(avg_speed_out_IN, na.rm = T),
            m_avg_speed_out_INT = mean(avg_speed_out_INT, na.rm = T),
            m_avg_speed_in_IN = mean(avg_speed_in_IN, na.rm = T),
            m_avg_speed_in_INT = mean(avg_speed_in_INT, na.rm = T), 
            s_outflow_rate_IN = sum(outflow_rate_IN, na.rm = T),
            s_outflow_rate_INT = sum(outflow_rate_INT, na.rm = T),
            s_out_in_fd = sum(out_in_fd, na.rm = T),
            s_out_int_fd = sum(out_int_fd, na.rm = T),
            s_inflow_rate_IN = sum(inflow_rate_IN, na.rm = T),
            s_inflow_rate_INT = sum(inflow_rate_INT, na.rm = T),
            s_in_in_fd = sum(in_in_fd, na.rm = T),
            s_in_int_fd = sum(in_int_fd, na.rm = T),
            s_nmr_IN = sum(nmr_IN, na.rm = T),
            s_nmr_INT = sum(nmr_INT, na.rm = T),
            s_mei_IN = sum(mei_IN, na.rm = T),
            s_mei_INT = sum(mei_INT, na.rm = T),
            s_avg_speed_out_IN = sum(avg_speed_out_IN, na.rm = T),
            s_avg_speed_out_INT = sum(avg_speed_out_INT, na.rm = T),
            s_avg_speed_in_IN = sum(avg_speed_in_IN, na.rm = T),
            s_avg_speed_in_INT = sum(avg_speed_in_INT, na.rm = T), 
            s_dt_out_IN = sum(dt_out_IN, na.rm = T),
            s_dt_out_INT = sum(dt_out_INT, na.rm =T),
            s_dt_in_IN = sum(dt_in_IN, na.rm =T),
            s_dt_in_INT = sum(dt_in_INT, na.rm = T))

# 4 year periods
ukr_fd_sc_by4 <- ukr_fd_sc %>% 
  filter(year > 2008) %>% 
  mutate(year_group = case_when(
    year %in% c(2009, 2010, 2011, 2012) ~ "2009-2012", 
    year %in% c(2013, 2014, 2015, 2016) ~ "2013-2016", 
    year %in% c(2017, 2018, 2019, 2020) ~ "2017-2020", 
  )) %>% 
  group_by(region, year_group) %>% 
  summarise(m_outflow_rate_IN = mean(outflow_rate_IN, na.rm = T),
            m_outflow_rate_INT = mean(outflow_rate_INT, na.rm = T),
            m_out_in_fd = mean(out_in_fd, na.rm = T),
            m_out_int_fd = mean(out_int_fd, na.rm = T),
            m_inflow_rate_IN = mean(inflow_rate_IN, na.rm = T),
            m_inflow_rate_INT = mean(inflow_rate_INT, na.rm = T),
            m_in_in_fd = mean(in_in_fd, na.rm = T),
            m_in_int_fd = mean(in_int_fd, na.rm = T),
            m_nmr_IN = mean(nmr_IN, na.rm = T),
            m_nmr_INT = mean(nmr_INT, na.rm = T), 
            m_mei_IN = mean(mei_IN, na.rm = T),
            m_mei_INT = mean(mei_INT, na.rm = T),
            m_avg_speed_out_IN = mean(avg_speed_out_IN, na.rm = T),
            m_avg_speed_out_INT = mean(avg_speed_out_INT, na.rm = T),
            m_avg_speed_in_IN = mean(avg_speed_in_IN, na.rm = T),
            m_avg_speed_in_INT = mean(avg_speed_in_INT, na.rm = T),
            s_outflow_rate_IN = sum(outflow_rate_IN, na.rm = T),
            s_outflow_rate_INT = sum(outflow_rate_INT, na.rm = T),
            s_out_in_fd = sum(out_in_fd, na.rm = T),
            s_out_int_fd = sum(out_int_fd, na.rm = T),
            s_inflow_rate_IN = sum(inflow_rate_IN, na.rm = T),
            s_inflow_rate_INT = sum(inflow_rate_INT, na.rm = T),
            s_in_in_fd = sum(in_in_fd, na.rm = T),
            s_in_int_fd = sum(in_int_fd, na.rm = T),
            s_nmr_IN = sum(nmr_IN, na.rm = T),
            s_nmr_INT = sum(nmr_INT, na.rm = T),
            s_mei_IN = sum(mei_IN, na.rm = T),
            s_mei_INT = sum(mei_INT, na.rm = T),
            s_avg_speed_out_IN = sum(avg_speed_out_IN, na.rm = T),
            s_avg_speed_out_INT = sum(avg_speed_out_INT, na.rm = T),
            s_avg_speed_in_IN = sum(avg_speed_in_IN, na.rm = T),
            s_avg_speed_in_INT = sum(avg_speed_in_INT, na.rm = T), 
            s_dt_out_IN = sum(dt_out_IN, na.rm = T),
            s_dt_out_INT = sum(dt_out_INT, na.rm =T),
            s_dt_in_IN = sum(dt_in_IN, na.rm =T),
            s_dt_in_INT = sum(dt_in_INT, na.rm = T))

#### Preparation for mapping ####

# Create individual datasets from 2009 to 2020

ukr_fd_sc_list <- list()

for(y in 2009:2020){
  ukr_fd_sc_list[[paste0("ukr_fd_sc", y)]] <- ukr_fd_sc %>% 
    filter(year == y)
}

for(y in 2009:2020){
  assign(paste0("ukr_fd_sc", y), ukr_fd_sc_list[[paste0("ukr_fd_sc", y)]])
}

#### OpenAlex ###
### Filter Ukraine ###

# Create outflow rates and inflow rates 
onlyukr_oa <- openalex %>%  
  filter(country_iso_a2 == "UA") %>% 
  arrange(region, year) %>% 
  mutate(outflow_rate_IN = (out_y_flow_IN/y_pop) * 1000,
         inflow_rate_IN = (in_y_flow_IN/y_pop) * 1000,
         outflow_rate_INT = (out_y_flow_INT/y_pop) * 1000,
         inflow_rate_INT = (in_y_flow_INT/y_pop) * 1000,
         mei_IN = 100 * (abs(in_y_flow_IN - out_y_flow_IN)/sum_inout_IN),
         mei_INT = 100 * (abs(in_y_flow_INT - out_y_flow_INT)/sum_inout_INT))

#### Measures ####

# Ukraine 
ukr_fd_oa <- onlyukr_oa %>% 
  group_by(region) %>% 
  mutate(nmrint_fd = nmr_INT - lag(nmr_INT), 
         nmrin_fd = nmr_IN - lag(nmr_IN), 
         out_int_fd = out_y_flow_INT - lag(out_y_flow_INT), 
         out_in_fd = out_y_flow_IN - lag(out_y_flow_IN),
         in_int_fd = in_y_flow_INT - lag(in_y_flow_INT),
         in_in_fd = in_y_flow_IN - lag(in_y_flow_IN),
         avg_speed_out_IN = ((out_y_flow_IN - lag(out_y_flow_IN))/2),
         avg_speed_out_INT = ((out_y_flow_INT - lag(out_y_flow_INT))/2),
         avg_speed_in_IN = ((in_y_flow_IN - lag(in_y_flow_IN))/2),
         avg_speed_in_INT = ((in_y_flow_INT - lag(in_y_flow_IN))/2),
         dt_out_IN = ifelse(lag(out_y_flow_IN) > 0 & (out_y_flow_IN - lag(out_y_flow_IN)) > 0, 
                            log(2) / ((out_y_flow_IN - lag(out_y_flow_IN)) / lag(out_y_flow_IN)), NA),
         dt_out_INT = ifelse(lag(out_y_flow_INT) > 0 & (out_y_flow_INT - lag(out_y_flow_INT)) > 0, 
                             log(2) / ((out_y_flow_INT - lag(out_y_flow_INT)) / lag(out_y_flow_INT)), NA),
         dt_in_IN = ifelse(lag(in_y_flow_IN) > 0 & (in_y_flow_IN - lag(in_y_flow_IN)) > 0, 
                           log(2) / ((in_y_flow_IN - lag(in_y_flow_IN)) / lag(in_y_flow_IN)), NA),
         dt_in_INT = ifelse(lag(in_y_flow_INT) > 0 & (in_y_flow_INT - lag(in_y_flow_INT)) > 0, 
                            log(2) / ((in_y_flow_INT - lag(in_y_flow_INT)) / lag(in_y_flow_INT)), NA))



# Create datasets that aggregate migration measures over 2 year, 3 year and
# 4 year periods

# 2 year periods
ukr_fd_oa_by2 <- ukr_fd_oa %>% 
  filter(year > 2008) %>% 
  mutate(year_group = case_when(
    year %in% c(2009, 2010) ~ "2009-2010", 
    year %in% c(2011, 2012) ~ "2011-2012", 
    year %in% c(2013, 2014) ~ "2013-2014", 
    year %in% c(2015, 2016) ~ "2015-2016", 
    year %in% c(2017, 2018) ~ "2017-2018", 
    year %in% c(2019, 2020) ~ "2019-2020", 
    year %in% c(2021, 2022) ~ "2021-2022"
  )) %>% 
  group_by(region, year_group) %>% 
  summarise(m_outflow_rate_IN = mean(outflow_rate_IN, na.rm = T),
            m_outflow_rate_INT = mean(outflow_rate_INT, na.rm = T),
            m_out_in_fd = mean(out_in_fd, na.rm = T),
            m_out_int_fd = mean(out_int_fd, na.rm = T),
            m_inflow_rate_IN = mean(inflow_rate_IN, na.rm = T),
            m_inflow_rate_INT = mean(inflow_rate_INT, na.rm = T),
            m_in_in_fd = mean(in_in_fd, na.rm = T),
            m_in_int_fd = mean(in_int_fd, na.rm = T),
            m_nmr_IN = mean(nmr_IN, na.rm = T),
            m_nmr_INT = mean(nmr_INT, na.rm = T),
            m_mei_IN = mean(mei_IN, na.rm = T),
            m_mei_INT = mean(mei_INT, na.rm = T),
            m_avg_speed_out_IN = mean(avg_speed_out_IN, na.rm = T),
            m_avg_speed_out_INT = mean(avg_speed_out_INT, na.rm = T),
            m_avg_speed_in_IN = mean(avg_speed_in_IN, na.rm = T),
            m_avg_speed_in_INT = mean(avg_speed_in_INT, na.rm = T), 
            m_dt_out_IN = mean(dt_out_IN, na.rm = T),
            m_dt_out_INT = mean(dt_out_INT, na.rm = T),
            m_dt_in_IN = mean(dt_in_IN, na.rm = T),
            m_dt_in_INT = mean(dt_in_INT, na.rm = T),
            s_outflow_rate_IN = sum(outflow_rate_IN, na.rm = T),
            s_outflow_rate_INT = sum(outflow_rate_INT, na.rm = T),
            s_out_in_fd = sum(out_in_fd, na.rm = T),
            s_out_int_fd = sum(out_int_fd, na.rm = T),
            s_inflow_rate_IN = sum(inflow_rate_IN, na.rm = T),
            s_inflow_rate_INT = sum(inflow_rate_INT, na.rm = T),
            s_in_in_fd = sum(in_in_fd, na.rm = T),
            s_in_int_fd = sum(in_int_fd, na.rm = T),
            s_nmr_IN = sum(nmr_IN, na.rm = T),
            s_nmr_INT = sum(nmr_INT, na.rm = T),
            s_mei_IN = sum(mei_IN, na.rm = T),
            s_mei_INT = sum(mei_INT, na.rm = T),
            s_avg_speed_out_IN = sum(avg_speed_out_IN, na.rm = T),
            s_avg_speed_out_INT = sum(avg_speed_out_INT, na.rm = T),
            s_avg_speed_in_IN = sum(avg_speed_in_IN, na.rm = T),
            s_avg_speed_in_INT = sum(avg_speed_in_INT, na.rm = T),
            s_dt_out_IN = sum(dt_out_IN, na.rm = T),
            s_dt_out_INT = sum(dt_out_INT, na.rm =T),
            s_dt_in_IN = sum(dt_in_IN, na.rm =T),
            s_dt_in_INT = sum(dt_in_INT, na.rm = T))

# 3 year periods
ukr_fd_oa_by3 <- ukr_fd_oa %>% 
  filter(year > 2008) %>% 
  mutate(year_group = case_when(
    year %in% c(2009, 2010, 2011) ~ "2009-2011", 
    year %in% c(2012, 2013, 2014) ~ "2012-2014", 
    year %in% c(2015, 2016, 2017) ~ "2015-2017", 
    year %in% c(2018, 2019, 2020) ~ "2018-2020", 
    year %in% c(2021, 2022) ~ "2021-2022"
  )) %>% 
  group_by(region, year_group) %>% 
  summarise(m_outflow_rate_IN = mean(outflow_rate_IN, na.rm = T),
            m_outflow_rate_INT = mean(outflow_rate_INT, na.rm = T),
            m_out_in_fd = mean(out_in_fd, na.rm = T),
            m_out_int_fd = mean(out_int_fd, na.rm = T),
            m_inflow_rate_IN = mean(inflow_rate_IN, na.rm = T),
            m_inflow_rate_INT = mean(inflow_rate_INT, na.rm = T),
            m_in_in_fd = mean(in_in_fd, na.rm = T),
            m_in_int_fd = mean(in_int_fd, na.rm = T),
            m_nmr_IN = mean(nmr_IN, na.rm = T),
            m_nmr_INT = mean(nmr_INT, na.rm = T),
            m_mei_IN = mean(mei_IN, na.rm = T),
            m_mei_INT = mean(mei_INT, na.rm = T),
            m_avg_speed_out_IN = mean(avg_speed_out_IN, na.rm = T),
            m_avg_speed_out_INT = mean(avg_speed_out_INT, na.rm = T),
            m_avg_speed_in_IN = mean(avg_speed_in_IN, na.rm = T),
            m_avg_speed_in_INT = mean(avg_speed_in_INT, na.rm = T), 
            m_dt_out_IN = mean(dt_out_IN, na.rm = T),
            m_dt_out_INT = mean(dt_out_INT, na.rm = T),
            m_dt_in_IN = mean(dt_in_IN, na.rm = T),
            m_dt_in_INT = mean(dt_in_INT, na.rm = T),
            s_outflow_rate_IN = sum(outflow_rate_IN, na.rm = T),
            s_outflow_rate_INT = sum(outflow_rate_INT, na.rm = T),
            s_out_in_fd = sum(out_in_fd, na.rm = T),
            s_out_int_fd = sum(out_int_fd, na.rm = T),
            s_inflow_rate_IN = sum(inflow_rate_IN, na.rm = T),
            s_inflow_rate_INT = sum(inflow_rate_INT, na.rm = T),
            s_in_in_fd = sum(in_in_fd, na.rm = T),
            s_in_int_fd = sum(in_int_fd, na.rm = T),
            s_nmr_IN = sum(nmr_IN, na.rm = T),
            s_nmr_INT = sum(nmr_INT, na.rm = T),
            s_mei_IN = sum(mei_IN, na.rm = T),
            s_mei_INT = sum(mei_INT, na.rm = T),
            s_avg_speed_out_IN = sum(avg_speed_out_IN, na.rm = T),
            s_avg_speed_out_INT = sum(avg_speed_out_INT, na.rm = T),
            s_avg_speed_in_IN = sum(avg_speed_in_IN, na.rm = T),
            s_avg_speed_in_INT = sum(avg_speed_in_INT, na.rm = T),
            s_dt_out_IN = sum(dt_out_IN, na.rm = T),
            s_dt_out_INT = sum(dt_out_INT, na.rm =T),
            s_dt_in_IN = sum(dt_in_IN, na.rm =T),
            s_dt_in_INT = sum(dt_in_INT, na.rm = T))

# 4 year periods
ukr_fd_oa_by4 <- ukr_fd_oa %>% 
  filter(year > 2008) %>% 
  mutate(year_group = case_when(
    year %in% c(2009, 2010, 2011, 2012) ~ "2009-2012", 
    year %in% c(2013, 2014, 2015, 2016) ~ "2013-2016", 
    year %in% c(2017, 2018, 2019, 2020) ~ "2017-2020", 
    year %in% c(2021, 2022) ~ "2021-2022"
  )) %>% 
  group_by(region, year_group) %>% 
  summarise(m_outflow_rate_IN = mean(outflow_rate_IN, na.rm = T),
            m_outflow_rate_INT = mean(outflow_rate_INT, na.rm = T),
            m_out_in_fd = mean(out_in_fd, na.rm = T),
            m_out_int_fd = mean(out_int_fd, na.rm = T),
            m_inflow_rate_IN = mean(inflow_rate_IN, na.rm = T),
            m_inflow_rate_INT = mean(inflow_rate_INT, na.rm = T),
            m_in_in_fd = mean(in_in_fd, na.rm = T),
            m_in_int_fd = mean(in_int_fd, na.rm = T),
            m_nmr_IN = mean(nmr_IN, na.rm = T),
            m_nmr_INT = mean(nmr_INT, na.rm = T),
            m_mei_IN = mean(mei_IN, na.rm = T),
            m_mei_INT = mean(mei_INT, na.rm = T),
            m_avg_speed_out_IN = mean(avg_speed_out_IN, na.rm = T),
            m_avg_speed_out_INT = mean(avg_speed_out_INT, na.rm = T),
            m_avg_speed_in_IN = mean(avg_speed_in_IN, na.rm = T),
            m_avg_speed_in_INT = mean(avg_speed_in_INT, na.rm = T), 
            m_dt_out_IN = mean(dt_out_IN, na.rm = T),
            m_dt_out_INT = mean(dt_out_INT, na.rm = T),
            m_dt_in_IN = mean(dt_in_IN, na.rm = T),
            m_dt_in_INT = mean(dt_in_INT, na.rm = T),
            s_outflow_rate_IN = sum(outflow_rate_IN, na.rm = T),
            s_outflow_rate_INT = sum(outflow_rate_INT, na.rm = T),
            s_out_in_fd = sum(out_in_fd, na.rm = T),
            s_out_int_fd = sum(out_int_fd, na.rm = T),
            s_inflow_rate_IN = sum(inflow_rate_IN, na.rm = T),
            s_inflow_rate_INT = sum(inflow_rate_INT, na.rm = T),
            s_in_in_fd = sum(in_in_fd, na.rm = T),
            s_in_int_fd = sum(in_int_fd, na.rm = T),
            s_nmr_IN = sum(nmr_IN, na.rm = T),
            s_nmr_INT = sum(nmr_INT, na.rm = T),
            s_mei_IN = sum(mei_IN, na.rm = T),
            s_mei_INT = sum(mei_INT, na.rm = T),
            s_avg_speed_out_IN = sum(avg_speed_out_IN, na.rm = T),
            s_avg_speed_out_INT = sum(avg_speed_out_INT, na.rm = T),
            s_avg_speed_in_IN = sum(avg_speed_in_IN, na.rm = T),
            s_avg_speed_in_INT = sum(avg_speed_in_INT, na.rm = T),
            s_dt_out_IN = sum(dt_out_IN, na.rm = T),
            s_dt_out_INT = sum(dt_out_INT, na.rm =T),
            s_dt_in_IN = sum(dt_in_IN, na.rm =T),
            s_dt_in_INT = sum(dt_in_INT, na.rm = T))

#### Preparation for mapping ####

# Create individual datasets from 2009 to 2022

ukr_fd_list <- list()

for(y in 2009:2022){
  ukr_fd_list[[paste0("ukr_fd_oa", y)]] <- ukr_fd_oa %>% 
    filter(year == y)
}

for(y in 2009:2022){
  assign(paste0("ukr_fd_oa", y), ukr_fd_list[[paste0("ukr_fd_oa", y)]])
}

# Ali's preferred palette
RdYlGn_4maps <- c('#d7d7d2', # for missing (NAs)
                  '#d73027', # for lowest negative value (darkred)
                  '#f46d43',
                  '#fdae61',
                  '#fee08b',
                  '#ffffbf', # for 0 or balanced flow (yellow)
                  '#d9ef8b',
                  '#a6d96a',
                  '#66bd63',
                  '#1a9850') # for the highest positive value (darkgreen)

pal_7cat_div <- c('#f46d43',
                  '#fdae61',
                  '#fee08b',
                  '#ffffbf',
                  '#d9ef8b',
                  '#a6d96a',
                  '#66bd63',
                  '#d7d7d2')

pal_5cat_div <- c('#d73027',
                  '#fdae61',
                  '#ffffbf',
                  '#a6d96a',
                  '#1a9850')

#### Mapping ####

# Read in NaturalEarth JSON file (passed the full path directly rather than
# setwd()-ing into its folder first, so this doesn't change the working
# directory for the rest of the session -- see Errors_found.md)

geo_world <- geojson_sf(file.path(data_dir, "for_mapping_with_GeoNames/ne_10m_admin_1_states_provinces.geojson"))

# Crimea listed as part of Russia, connect it back to Ukraine

geo_ukr_nocrim <- geo_world %>% 
  filter(admin == "Ukraine") %>% 
  select(gn_a1_code, name, latitude, longitude, geometry) %>% 
  arrange(gn_a1_code) %>% 
  mutate(region = gn_a1_code)

geo_crimea <- geo_world %>% 
  filter(name == "Crimea") %>% 
  select(gn_a1_code, name, latitude, longitude, geometry) %>% 
  mutate(region = "UA.11")

geo_sevastopol <- geo_world %>% 
  filter(name == "Sevastopol") %>% 
  select(gn_a1_code, name, latitude, longitude, geometry) %>% 
  mutate(region = "UA.20")

ukr <- rbind(geo_ukr_nocrim, geo_crimea, geo_sevastopol)

# Create different areas of Ukraine

# Kyiv International Institute of Sociology's 4-region division
# https://www.kiis.com.ua/?lang=eng&cat=reports&id=236&page=1 (footnote 1)


ukr <- ukr %>% 
  mutate(area = factor(region),
         cardinal_area = fct_collapse(area, 
                                      "Western Ukraine" = c("UA.03", "UA.06", 
                                                            "UA.15", "UA.22",
                                                            "UA.25", "UA.19", 
                                                            "UA.09",
                                                            "UA.24"),
                                      "Central Ukraine" = c("UA.23", "UA.27", 
                                                            "UA.21", "UA.02", 
                                                            "UA.18", "UA.10", 
                                                            "UA.01", "UA.12", 
                                                            "UA.13"),
                                      "Eastern Ukraine" = c("UA.05", "UA.14",
                                                            "UA.07"),
                                      "Southern Ukraine" = c("UA.04", "UA.26",
                                                             "UA.16", "UA.08",
                                                             "UA.17", "UA.11", 
                                                             "UA.20")),
         cardinal_area2 = fct_collapse(area, 
                                       "Western Ukraine" = c("UA.03", "UA.06", 
                                                             "UA.15", "UA.22",
                                                             "UA.19", 
                                                             "UA.25", "UA.09",
                                                             "UA.24"),
                                       "Central Ukraine" = c("UA.23", "UA.27", 
                                                             "UA.21", "UA.02", 
                                                             "UA.18", "UA.10", 
                                                             "UA.01", "UA.12", 
                                                             "UA.13"),
                                       "Eastern Ukraine + Crimea" = c("UA.05", "UA.14",
                                                                      "UA.07", "UA.11", "UA.20"),
                                       "Southern Ukraine" = c("UA.04", "UA.26",
                                                              "UA.16", "UA.08",
                                                              "UA.17")),
         gdl_area = fct_collapse(area, 
                                 "Northern Ukraine" = c("UA.02", "UA.21", 
                                                        "UA.12", "UA.13", "UA.27"),
                                 "Central Ukraine" = c("UA.01", "UA.10", "UA.18", 
                                                       "UA.23"),
                                 "Eastern Ukraine" = c("UA.04", "UA.05", "UA.07",
                                                       "UA.14", "UA.26"),
                                 "Southern Ukraine" = c("UA.08", "UA.11","UA.16", 
                                                        "UA.17", "UA.20"),
                                 "Western Ukraine" = c("UA.03", "UA.06", 
                                                       "UA.09", "UA.15", "UA.19",
                                                       "UA.22", "UA.24", "UA.25" 
                                 )),
         gdl_kyiv = fct_collapse(area, 
                                 "Northern Ukraine" = c("UA.02", "UA.21", 
                                                        "UA.13", "UA.27"),
                                 "Central Ukraine" = c("UA.01", "UA.10", "UA.18", 
                                                       "UA.23"),
                                 "Eastern Ukraine" = c("UA.04", "UA.05", "UA.07",
                                                       "UA.14", "UA.26"),
                                 "Southern Ukraine" = c("UA.08", "UA.11","UA.16", 
                                                        "UA.17", "UA.20"),
                                 "Western Ukraine" = c("UA.03", "UA.06", 
                                                       "UA.09", "UA.15", "UA.19",
                                                       "UA.22", "UA.24", "UA.25"),
                                 "Kyiv" = c("UA.12")))

# Test basic map -- it works.

ggplot(ukr) + 
  geom_sf() +
  geom_sf_text(data = ukr, aes(label = name), size = 3) +
  theme_void()

#### Regional population counts ####

library(readxl)

regpop_ukr <- read_excel(file.path(data_dir, "Regional_Populations_Ukraine.xlsx"))

regpop_ukr_long_20 <- regpop_ukr %>% 
  select(region, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, 
         `2017`, `2018`, `2019`, `2020`) %>% 
  pivot_longer(cols = c(`2009`, `2010`, `2011`, `2012`, 
                        `2013`, `2014`, `2015`, `2016`, 
                        `2017`, `2018`, `2019`, `2020`), 
               names_to = "year", values_to = "regpop")

regpop_ukr_long_22 <- regpop_ukr %>% 
  select(region, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, 
         `2017`, `2018`, `2019`, `2020`, `2021`, `2022`) %>% 
  pivot_longer(cols = c(`2009`, `2010`, `2011`, `2012`, 
                        `2013`, `2014`, `2015`, `2016`, 
                        `2017`, `2018`, `2019`, `2020`, 
                        `2021`, `2022`), 
               names_to = "year", values_to = "regpop")

#### Admin1 GDP data ####
ukr_gdp <- admin1_gdp %>% 
  filter(iso3 == "UKR") %>% 
  rename_with(~ sub("^X", "", .x), starts_with("X")) %>% 
  select(-c(1:3, 5:24)) %>% 
  # Duplicate the Crimea & Sevastopol row
  {
    crimea_row <- filter(., Subnat == "Crimea & Sevastopol")
    bind_rows(., crimea_row)
  } %>%
  # Add a row identifier to distinguish the two Crimea & Sevastopol rows
  group_by(Subnat) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  mutate(name = case_when(
    Subnat == "Cherkasy" ~ "Cherkasy",
    Subnat == "Chernihiv" ~ "Chernihiv",
    Subnat == "Chernivtsi" ~ "Chernivtsi",
    Subnat == "Crimea & Sevastopol" & row_id == 1 ~ "Crimea",
    Subnat == "Crimea & Sevastopol" & row_id == 2 ~ "Sevastopol",  # Fixed typo
    Subnat == "Dnipropetrovsk" ~ "Dnipropetrovs'k",
    Subnat == "Donetsk" ~ "Donets'k",
    Subnat == "Ivano-Frankivsk" ~ "Ivano-Frankivs'k",
    Subnat == "Kharkiv" ~ "Kharkiv",
    Subnat == "Kherson" ~ "Kherson",
    Subnat == "Khmelnytskiy" ~ "Khmel'nyts'kyy",
    Subnat == "Kirovohrad" ~ "Kirovohrad",
    Subnat == "Kyiv_sub" ~ "Kiev",
    Subnat == "Kyiv_city" ~ "Kiev City",
    Subnat == "Lviv" ~ "L'viv",
    Subnat == "Luhansk" ~ "Luhans'k",
    Subnat == "Mykolayiv" ~ "Mykolayiv",
    Subnat == "Odesa" ~ "Odessa",
    Subnat == "Poltava" ~ "Poltava",
    Subnat == "Rivne" ~ "Rivne",
    Subnat == "Sumy" ~ "Sumy",
    Subnat == "Ternopil" ~ "Ternopil'",
    Subnat == "Vinnytsya" ~ "Vinnytsya",
    Subnat == "Volyn" ~ "Volyn",
    Subnat == "Zakarpattya" ~ "Transcarpathia",
    Subnat == "Zaporizhzhya" ~ "Zaporizhzhya",
    Subnat == "Zhytomyr" ~ "Zhytomyr",
    TRUE ~ Subnat
  )) %>% 
  select(-row_id) %>%  # Remove the helper column
  pivot_longer(cols = c(2:15), 
               names_to = "year", values_to = "gdppc")


#### Scopus ####

# Merge Ukraine first-difference datasets with spatial data

for(y in 2009:2020) {
  merged_data_sc <- merge(get(paste0("ukr_fd_sc", y)), ukr, by = "region")
  assign(paste0("ukrmapmerge_sc", y), merged_data_sc)
}

# Merge moving average 2 years dataset

merged_data2_sc <- merge(ukr_fd_sc_by2, ukr, by = "region")

# Merge moving average 3 years dataset

merged_data3_sc <- merge(ukr_fd_sc_by3, ukr, by = "region")

# Merge moving average 4 years dataset

merged_data4_sc <- merge(ukr_fd_sc_by4, ukr, by = "region")

for(y in 2009:2020) {
  merged_data_sc <- merge(get(paste0("ukr_fd_sc", y)), ukr, by = "region")
  assign(paste0("ukrmapmerge_sc", y), merged_data_sc)
}

ukr_sc_2009.2020 <- do.call("rbind", list(ukrmapmerge_sc2009,
                                          ukrmapmerge_sc2010, ukrmapmerge_sc2011, ukrmapmerge_sc2012,
                                          ukrmapmerge_sc2013, ukrmapmerge_sc2014, ukrmapmerge_sc2015, 
                                          ukrmapmerge_sc2016, ukrmapmerge_sc2017, ukrmapmerge_sc2018, 
                                          ukrmapmerge_sc2019, ukrmapmerge_sc2020))

# Merge in regional population counts # 

ukr_sc_2009.2020 <- merge(ukr_sc_2009.2020, regpop_ukr_long_20, by = c("region", "year"))

# Merge in gdp per capita # 

ukr_sc_2009.2020 <- merge(ukr_sc_2009.2020, ukr_gdp, by = c("name", "year"))
# Create post-2014 and treated regions variables # 

ukr_sc_2009.2020 <- ukr_sc_2009.2020 %>% 
  mutate(post_2014 = ifelse(year > 2014, 1, 0),
         treated_regions = ifelse(name == "Donets'k" | name == "Luhans'k" | 
                                    name == "Crimea" | name == "Sevastopol", 1, 0))

# Exclude region-years with scholarly population less than 25 #
ukr_sc_2009.2020 <- ukr_sc_2009.2020 %>% 
  filter(y_pop > 25)

#### OpenAlex ####

# Merge Ukraine first-difference datasets with spatial data

for(y in 2009:2022) {
  merged_data_oa <- merge(get(paste0("ukr_fd_oa", y)), ukr, by = "region")
  assign(paste0("ukrmapmerge_oa", y), merged_data_oa)
}

# Merge moving average 2 years dataset

merged_data2_oa <- merge(ukr_fd_oa_by2, ukr, by = "region")

# Merge moving average 3 years dataset

merged_data3_oa <- merge(ukr_fd_oa_by3, ukr, by = "region")

# Merge moving average 4 years dataset

merged_data4_oa <- merge(ukr_fd_oa_by4, ukr, by = "region")

for(y in 2009:2022) {
  merged_data_oa <- merge(get(paste0("ukr_fd_oa", y)), ukr, by = "region")
  assign(paste0("ukrmapmerge_oa", y), merged_data_oa)
}

ukr_oa_2009.2022 <- do.call("rbind", list(ukrmapmerge_oa2009,
                                          ukrmapmerge_oa2010, ukrmapmerge_oa2011, ukrmapmerge_oa2012,
                                          ukrmapmerge_oa2013, ukrmapmerge_oa2014, ukrmapmerge_oa2015, 
                                          ukrmapmerge_oa2016, ukrmapmerge_oa2017, ukrmapmerge_oa2018, 
                                          ukrmapmerge_oa2019, ukrmapmerge_oa2020, ukrmapmerge_oa2021,
                                          ukrmapmerge_oa2022))

# Merge in regional population counts #

ukr_oa_2009.2022 <- merge(ukr_oa_2009.2022, regpop_ukr_long_22, by = c("region", "year"))

# Merge in gdp per capita # 

ukr_oa_2009.2022 <- merge(ukr_oa_2009.2022, ukr_gdp, by = c("name", "year"))

# Create post-2014 and treated regions variables # 

ukr_oa_2009.2022 <- ukr_oa_2009.2022 %>% 
  mutate(post_2014 = ifelse(year > 2014, 1, 0),
         treated_regions = ifelse(name == "Donets'k" | name == "Luhans'k" |  
                                    name == "Crimea", 1, 0))

# Exclude region-years with scholarly population less than 25 #
ukr_oa_2009.2022 <- ukr_oa_2009.2022 %>% 
  filter(y_pop > 25) 

ukr_oa_2009.2020 <- do.call("rbind", list(ukrmapmerge_oa2009,
                                          ukrmapmerge_oa2010, ukrmapmerge_oa2011, ukrmapmerge_oa2012,
                                          ukrmapmerge_oa2013, ukrmapmerge_oa2014, ukrmapmerge_oa2015, 
                                          ukrmapmerge_oa2016, ukrmapmerge_oa2017, ukrmapmerge_oa2018, 
                                          ukrmapmerge_oa2019, ukrmapmerge_oa2020))

# Merge in regional population counts #

ukr_oa_2009.2020 <- merge(ukr_oa_2009.2020, regpop_ukr_long_20, by = c("region", "year"))

# Merge in gdp per capita # 

ukr_oa_2009.2020 <- merge(ukr_oa_2009.2020, ukr_gdp, by = c("name", "year"))

# Create post-2014 and treated regions variables # 

ukr_oa_2009.2020 <- ukr_oa_2009.2020 %>% 
  mutate(post_2014 = ifelse(year > 2014, 1, 0),
         treated_regions = ifelse(name == "Donets'k" | name == "Luhans'k" | 
                                    name == "Crimea", 1, 0))

ukr_oa_2009.2020 <- ukr_oa_2009.2020 %>% 
  filter(y_pop > 25)

## Save plot function

# Define a function to save plots
save_plot <- function(plot, filename, scale = 1, width = 12.28, height = 10,
                      dpi = 300, units = "cm", limitsize = FALSE) {
  # device was hardcoded to cairo_pdf regardless of `filename`'s extension,
  # so a "*.svg" call silently wrote PDF bytes into a .svg-named file (see
  # Errors_found.md). NULL lets ggsave() pick the device from the filename
  # extension instead; .pdf calls still get cairo_pdf explicitly, since its
  # fontconfig-based text rendering is what makes the Times New Roman
  # theme() calls elsewhere in this codebase work at all.
  dev <- if (grepl("\\.svg$", filename, ignore.case = TRUE)) NULL else cairo_pdf
  ggsave(filename = filename, plot = plot, scale = scale,
         width = width, height = height, units = units,
         dpi = dpi, limitsize = limitsize, device = dev)
}









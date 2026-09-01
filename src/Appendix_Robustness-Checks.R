#### Appendix Robustness Checks: F3 and associated T4 (Negative Binomial internal ####
#### 1-year backfill models) and F4 and associated T6 (Negative Binomial international ####
#### 1-year backfill models) ####

#### Code same as 01_Data-wrangling.R but with the correct one-year backfill dataset ####

# Run after: nothing -- self-contained (own data load, own model specs);
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

scopus <- read.csv(file.path(data_dir, "2026_updated_1_4_backfills/20260404_updated/subnational_measures_Subnational_Scopus_bfl_1y_mpy2_gbaT.csv"),
                   sep = ",")

openalex <- read.csv(file.path(data_dir, "2026_updated_1_4_backfills/20260404_updated/subnational_measures_Subnational_OpenAlex_bfl_1y_mpy2_gbaT.csv"),
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

# One way to measure the "speed" of outgoing and ingoing scholars is to take 
# a first difference approach: simply subtract the out/inflow rate in one year 
# from the previous year for all years

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

# One potential measure: acceleration (how in/outflow rate in 1 year compares
# to the prior year)

# Another way to measure the "speed" of outgoing and ingoing scholars is to take 
# a first difference approach: simply subtract the out/inflow rate in one year 
# from the previous year for all years. For now I use counts instead of rates

# Finally, doubling time (the amount of years it takes for number of scholars 
# entering/leaving to double) may also be useful

# Ukraine 
ukr_fd_oa <- onlyukr_oa %>% 
  group_by(region) %>% 
  mutate(nmrint_fd = nmr_INT - lag(nmr_INT), #should it not be counts though, rather than rates?
         nmrin_fd = nmr_IN - lag(nmr_IN), 
         out_int_fd = out_y_flow_INT - lag(out_y_flow_INT), # I changed the next four rows into count measures on 01/30/24
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
# setwd()-ing into its folder first -- see Errors_found.md)

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
save_plot <- function(plot, filename, scale = 1, width = 8, height = 4, dpi = 300, limitsize = FALSE) {
  # see the matching note in 01_Data-wrangling.R / Errors_found.md
  dev <- if (grepl("\\.svg$", filename, ignore.case = TRUE)) NULL else cairo_pdf
  ggsave(filename = filename, plot = plot, scale = scale, width = width, height = height, dpi = dpi, limitsize = F,
         device = dev)
}

#### Model specifications, same as 05_Modelspecs.R ####
library(glmmTMB)

# Set reference category
ukr_oa_2009.2022 <- ukr_oa_2009.2022 %>% 
  mutate(name = factor(name),  # Ensure it's a factor
         name = relevel(name, ref = "L'viv"),
         year = factor(year),
         year = relevel(year, ref = "2014"))

# Define which regions annexed and which occupied 2014 onwards
annexed  <- c("Crimea", "Sevastopol")
occupied <- c("Donets'k", "Luhans'k")

# Robustness check: extrapolate pre-2014 GDP trends for occupied regions, 
# include as a model specification

gdppc_data <- ukr_oa_2009.2022 %>%
  mutate(
    year   = as.numeric(as.character(year)),
    region = case_when(
      name %in% annexed  ~ "Annexed Regions",
      name %in% occupied ~ "Occupied Regions",
      TRUE               ~ "Rest of Ukraine"
    )
  )

# Get counterfactual predictions
cf_predictions <- gdppc_data %>%
  filter(name %in% occupied) %>%
  group_by(name) %>%
  group_modify(~ {
    fit <- lm(gdppc ~ year, data = filter(.x, year < 2014))
    tibble(
      year        = .x$year,
      gdppc_cf_temp = predict(fit, newdata = tibble(year = .x$year))
    )
  }) %>%
  ungroup()

# Join and build gdppc_cf cleanly 
ukr_oa_2009.2022 <- ukr_oa_2009.2022 %>%
  mutate(year = as.numeric(as.character(year))) %>%
  left_join(cf_predictions, by = c("name", "year")) %>%
  mutate(
    gdppc_cf = case_when(
      name %in% occupied & year >= 2014 ~ gdppc_cf_temp,
      TRUE                              ~ gdppc
    )
  ) %>%
  select(-gdppc_cf_temp)  # drop the temp column

# Test: thresholds 

regions_to_keep <- ukr_oa_2009.2022 %>% 
  filter(year < 2014) %>% 
  group_by(name) %>% 
  summarise(
    cond1 = any(sum_inout_INT > 2),
    cond2 = any(sum_inout_IN > 2)
  ) %>% 
  filter(cond1 & cond2) %>% 
  pull(name)

regions_to_keep2 <- ukr_oa_2009.2022 %>% 
  filter(year < 2014) %>% 
  group_by(name) %>% 
  summarise(
    qualifying_obs = sum(sum_inout_IN >= 1 & sum_inout_INT >= 1)
  ) %>% 
  filter(qualifying_obs >= 2, 
         name != c("Zhytomyr")) %>% 
  pull(name) 

regions_to_keep3 <- ukr_oa_2009.2022 %>% 
  filter(year < 2014, y_pop > 20) %>% 
  group_by(name) %>% 
  summarise(
    qualifying_obs = sum(sum_inout_IN >= 1 & sum_inout_INT >= 1)
  ) %>% 
  filter(qualifying_obs >= 2,
         name != c("Zhytomyr")) %>% 
  pull(name) 

regions_to_keep4 <- ukr_oa_2009.2022 %>% 
  filter(year < 2014, y_pop > 30) %>% 
  group_by(name) %>% 
  summarise(
    qualifying_obs = sum(sum_inout_IN >= 1 & sum_inout_INT >= 1)
  ) %>% 
  filter(qualifying_obs >= 2,
         name != c("Zhytomyr")) %>% 
  pull(name) 


regions_to_keep5 <- ukr_oa_2009.2022 %>% 
  filter(year < 2014) %>% 
  group_by(name) %>% 
  summarise(
    total_obs = n()
  ) %>% 
  filter(total_obs >= 4) %>% 
  pull(name)

ukr_oa_2009.2022.cut <- ukr_oa_2009.2022 %>% 
  filter(name %in% regions_to_keep2)


## STEP 1a: First, include all model objects -- INTERNAL migration ##

# OpenAlex 2009-2022#

# Negbins with counts as outcomes #
#1a
count_wpop_wgdp_IN_oa_out <- glmmTMB(out_y_flow_IN ~ log(gdppc) + log(regpop) +
                                       offset(log(y_pop)) + factor(name) + post_2014, 
                                     family = nbinom2,
                                     data = ukr_oa_2009.2022.cut)
#1b
count_wpop_wgdp_IN_oa_in <- glmmTMB(in_y_flow_IN ~ log(gdppc) + log(regpop) +
                                      offset(log(y_pop)) + factor(name) + post_2014, 
                                    family = nbinom2,
                                    data = ukr_oa_2009.2022.cut)

#1c
count_wpop_wgdpcf_IN_oa_out <- glmmTMB(out_y_flow_IN ~ log(gdppc_cf) + log(regpop) +
                                         offset(log(y_pop)) + factor(name) + post_2014, 
                                       family = nbinom2,
                                       data = ukr_oa_2009.2022.cut)
#1d
count_wpop_wgdpcf_IN_oa_in <- glmmTMB(in_y_flow_IN ~ log(gdppc_cf) + log(regpop) +
                                        offset(log(y_pop)) + factor(name) + post_2014, 
                                      family = nbinom2,
                                      data = ukr_oa_2009.2022.cut)
#2a
count_wpop_nogdp_IN_oa_out <- glmmTMB(out_y_flow_IN ~ log(regpop) + 
                                        offset(log(y_pop)) + factor(name) + post_2014, 
                                      family = nbinom2,
                                      data = ukr_oa_2009.2022.cut)
#2b
count_wpop_nogdp_IN_oa_in <- glmmTMB(out_y_flow_IN ~ log(regpop) + 
                                       offset(log(y_pop)) + factor(name) + post_2014, 
                                     family = nbinom2,
                                     data = ukr_oa_2009.2022.cut)
#3a
count_nopop_wgdp_IN_oa_out <- glmmTMB(out_y_flow_IN ~ log(gdppc) + 
                                        offset(log(y_pop)) + factor(name) + post_2014, 
                                      family = nbinom2,
                                      data = ukr_oa_2009.2022.cut)

#3b
count_nopop_wgdp_IN_oa_in <- glmmTMB(in_y_flow_IN ~ log(gdppc) + 
                                       offset(log(y_pop)) + factor(name) + post_2014, 
                                     family = nbinom2,
                                     data = ukr_oa_2009.2022.cut)

#3c
count_nopop_wgdpcf_IN_oa_out <- glmmTMB(out_y_flow_IN ~ log(gdppc_cf) + 
                                          offset(log(y_pop)) + factor(name) + post_2014, 
                                        family = nbinom2,
                                        data = ukr_oa_2009.2022.cut)

#3d
count_nopop_wgdpcf_IN_oa_in <- glmmTMB(in_y_flow_IN ~ log(gdppc_cf) + 
                                         offset(log(y_pop)) + factor(name) + post_2014, 
                                       family = nbinom2,
                                       data = ukr_oa_2009.2022.cut)

#4a
count_nopop_nogdp_IN_oa_out <- glmmTMB(out_y_flow_IN ~ 
                                         offset(log(y_pop)) + factor(name) + post_2014, 
                                       family = nbinom2,
                                       data = ukr_oa_2009.2022.cut)

#4b
count_nopop_nogdp_IN_oa_in <- glmmTMB(in_y_flow_IN ~ 
                                        offset(log(y_pop)) + factor(name) + post_2014, 
                                      family = nbinom2, 
                                      data = ukr_oa_2009.2022.cut)

#5a
count_nopop_wgdp_IN_interac_oa_out <- glmmTMB(out_y_flow_IN ~ log(gdppc) + 
                                                offset(log(y_pop)) + post_2014*factor(name), 
                                              family = nbinom2,
                                              data = ukr_oa_2009.2022.cut)

#5b
count_nopop_wgdp_IN_interac_oa_in <- glmmTMB(in_y_flow_IN ~ log(gdppc) + 
                                               offset(log(y_pop)) + post_2014*factor(name), 
                                             family = nbinom2,
                                             data = ukr_oa_2009.2022.cut)

#5c
count_nopop_wgdpcf_IN_interac_oa_out <- glmmTMB(out_y_flow_IN ~ log(gdppc_cf) + 
                                                  offset(log(y_pop)) + post_2014*factor(name), 
                                                family = nbinom2,
                                                data = ukr_oa_2009.2022.cut)

#5d
count_nopop_wgdpcf_IN_interac_oa_in <- glmmTMB(in_y_flow_IN ~ log(gdppc_cf) + 
                                                 offset(log(y_pop)) + post_2014*factor(name), 
                                               family = nbinom2,
                                               data = ukr_oa_2009.2022.cut)

#6a
count_nopop_nogdp_IN_interac_oa_out <- glmmTMB(out_y_flow_IN ~ 
                                                 offset(log(y_pop)) + post_2014*factor(name), 
                                               family = nbinom2,
                                               data = ukr_oa_2009.2022.cut)

#6b
count_nopop_nogdp_IN_interac_oa_in <- glmmTMB(in_y_flow_IN ~ 
                                                offset(log(y_pop)) + post_2014*factor(name), 
                                              family = nbinom2,
                                              data = ukr_oa_2009.2022.cut)

#7a 
count_wpop_nogdp_IN_interac_oa_out <- glmmTMB(out_y_flow_IN ~ log(regpop) +
                                                offset(log(y_pop)) + post_2014*factor(name), 
                                              family = nbinom2,
                                              data = ukr_oa_2009.2022.cut)
#7b
count_wpop_nogdp_IN_interac_oa_in <- glmmTMB(in_y_flow_IN ~ log(regpop) +
                                               offset(log(y_pop)) + post_2014*factor(name), 
                                             family = nbinom2,
                                             data = ukr_oa_2009.2022.cut)
#8a
count_wpop_wgdp_IN_interac_oa_out <- glmmTMB(out_y_flow_IN ~ log(regpop) + log(gdppc) +
                                               offset(log(y_pop)) + post_2014*factor(name), 
                                             family = nbinom2,
                                             data = ukr_oa_2009.2022.cut)
#8b 
count_wpop_wgdp_IN_interac_oa_in <- glmmTMB(in_y_flow_IN ~ log(regpop) + log(gdppc) +
                                              offset(log(y_pop)) + post_2014*factor(name), 
                                            family = nbinom2,
                                            data = ukr_oa_2009.2022.cut)

#8c
count_wpop_wgdpcf_IN_interac_oa_out <- glmmTMB(out_y_flow_IN ~ log(regpop) + log(gdppc_cf) +
                                                 offset(log(y_pop)) + post_2014*factor(name), 
                                               family = nbinom2,
                                               data = ukr_oa_2009.2022.cut)
#8d 
count_wpop_wgdpcf_IN_interac_oa_in <- glmmTMB(in_y_flow_IN ~ log(regpop) + log(gdppc_cf) +
                                                offset(log(y_pop)) + post_2014*factor(name), 
                                              family = nbinom2,
                                              data = ukr_oa_2009.2022.cut)


## STEP 1b: Include all model objects -- INTERNATIONAL migration ##

#1a
count_wpop_wgdp_INT_oa_out <- glmmTMB(out_y_flow_INT ~ log(gdppc) + log(regpop) +
                                        offset(log(y_pop)) + factor(name) + post_2014, 
                                      family = nbinom2,
                                      data = ukr_oa_2009.2022.cut)
#1b
count_wpop_wgdp_INT_oa_in <- glmmTMB(in_y_flow_INT ~ log(gdppc) + log(regpop) +
                                       offset(log(y_pop)) + factor(name) + post_2014, 
                                     family = nbinom2,
                                     data = ukr_oa_2009.2022.cut)

#1c
count_wpop_wgdpcf_INT_oa_out <- glmmTMB(out_y_flow_INT ~ log(gdppc_cf) + log(regpop) +
                                          offset(log(y_pop)) + factor(name) + post_2014, 
                                        family = nbinom2,
                                        data = ukr_oa_2009.2022.cut)
#1d
count_wpop_wgdpcf_INT_oa_in <- glmmTMB(in_y_flow_INT ~ log(gdppc_cf) + log(regpop) +
                                         offset(log(y_pop)) + factor(name) + post_2014, 
                                       family = nbinom2,
                                       data = ukr_oa_2009.2022.cut)
#2a
count_wpop_nogdp_INT_oa_out <- glmmTMB(out_y_flow_INT ~ log(regpop) + 
                                         offset(log(y_pop)) + factor(name) + post_2014, 
                                       family = nbinom2,
                                       data = ukr_oa_2009.2022.cut)
#2b
count_wpop_nogdp_INT_oa_in <- glmmTMB(out_y_flow_INT ~ log(regpop) + 
                                        offset(log(y_pop)) + factor(name) + post_2014, 
                                      family = nbinom2,
                                      data = ukr_oa_2009.2022.cut)
#3a
count_nopop_wgdp_INT_oa_out <- glmmTMB(out_y_flow_INT ~ log(gdppc) + 
                                         offset(log(y_pop)) + factor(name) + post_2014, 
                                       family = nbinom2,
                                       data = ukr_oa_2009.2022.cut)

#3b
count_nopop_wgdp_INT_oa_in <- glmmTMB(in_y_flow_INT ~ log(gdppc) + 
                                        offset(log(y_pop)) + factor(name) + post_2014, 
                                      family = nbinom2,
                                      data = ukr_oa_2009.2022.cut)

#3c
count_nopop_wgdpcf_INT_oa_out <- glmmTMB(out_y_flow_INT ~ log(gdppc_cf) + 
                                           offset(log(y_pop)) + factor(name) + post_2014, 
                                         family = nbinom2,
                                         data = ukr_oa_2009.2022.cut)

#3d
count_nopop_wgdpcf_INT_oa_in <- glmmTMB(in_y_flow_INT ~ log(gdppc_cf) + 
                                          offset(log(y_pop)) + factor(name) + post_2014, 
                                        family = nbinom2,
                                        data = ukr_oa_2009.2022.cut)

#4a
count_nopop_nogdp_INT_oa_out <- glmmTMB(out_y_flow_INT ~ 
                                          offset(log(y_pop)) + factor(name) + post_2014, 
                                        family = nbinom2,
                                        data = ukr_oa_2009.2022.cut)

#4b
count_nopop_nogdp_INT_oa_in <- glmmTMB(in_y_flow_INT ~ 
                                         offset(log(y_pop)) + factor(name) + post_2014, 
                                       family = nbinom2, 
                                       data = ukr_oa_2009.2022.cut)

#5a
count_nopop_wgdp_INT_interac_oa_out <- glmmTMB(out_y_flow_INT ~ log(gdppc) + 
                                                 offset(log(y_pop)) + post_2014*factor(name), 
                                               family = nbinom2,
                                               data = ukr_oa_2009.2022.cut)

#5b
count_nopop_wgdp_INT_interac_oa_in <- glmmTMB(in_y_flow_INT ~ log(gdppc) + 
                                                offset(log(y_pop)) + post_2014*factor(name), 
                                              family = nbinom2,
                                              data = ukr_oa_2009.2022.cut)

#5c
count_nopop_wgdpcf_INT_interac_oa_out <- glmmTMB(out_y_flow_INT ~ log(gdppc_cf) + 
                                                   offset(log(y_pop)) + post_2014*factor(name), 
                                                 family = nbinom2,
                                                 data = ukr_oa_2009.2022.cut)

#5d
count_nopop_wgdpcf_INT_interac_oa_in <- glmmTMB(in_y_flow_INT ~ log(gdppc_cf) + 
                                                  offset(log(y_pop)) + post_2014*factor(name), 
                                                family = nbinom2,
                                                data = ukr_oa_2009.2022.cut)

#6a
count_nopop_nogdp_INT_interac_oa_out <- glmmTMB(out_y_flow_INT ~ 
                                                  offset(log(y_pop)) + post_2014*factor(name), 
                                                family = nbinom2,
                                                data = ukr_oa_2009.2022.cut)

#6b
count_nopop_nogdp_INT_interac_oa_in <- glmmTMB(in_y_flow_INT ~ 
                                                 offset(log(y_pop)) + post_2014*factor(name), 
                                               family = nbinom2,
                                               data = ukr_oa_2009.2022.cut)

#7a 
count_wpop_nogdp_INT_interac_oa_out <- glmmTMB(out_y_flow_INT ~ log(regpop) +
                                                 offset(log(y_pop)) + post_2014*factor(name), 
                                               family = nbinom2,
                                               data = ukr_oa_2009.2022.cut)
#7b
count_wpop_nogdp_INT_interac_oa_in <- glmmTMB(in_y_flow_INT ~ log(regpop) +
                                                offset(log(y_pop)) + post_2014*factor(name), 
                                              family = nbinom2,
                                              data = ukr_oa_2009.2022.cut)
#8a
count_wpop_wgdp_INT_interac_oa_out <- glmmTMB(out_y_flow_INT ~ log(regpop) + log(gdppc) +
                                                offset(log(y_pop)) + post_2014*factor(name), 
                                              family = nbinom2,
                                              data = ukr_oa_2009.2022.cut)
#8b 
count_wpop_wgdp_INT_interac_oa_in <- glmmTMB(in_y_flow_INT ~ log(regpop) + log(gdppc) +
                                               offset(log(y_pop)) + post_2014*factor(name), 
                                             family = nbinom2,
                                             data = ukr_oa_2009.2022.cut)

#8c
count_wpop_wgdpcf_INT_interac_oa_out <- glmmTMB(out_y_flow_INT ~ log(regpop) + log(gdppc_cf) +
                                                  offset(log(y_pop)) + post_2014*factor(name), 
                                                family = nbinom2,
                                                data = ukr_oa_2009.2022.cut)
#8d 
count_wpop_wgdpcf_INT_interac_oa_in <- glmmTMB(in_y_flow_INT ~ log(regpop) + log(gdppc_cf) +
                                                 offset(log(y_pop)) + post_2014*factor(name), 
                                               family = nbinom2,
                                               data = ukr_oa_2009.2022.cut)

## STEP 2A: Model list internal negbin models ##

model_list_IN_oa_count_out <- list(
  "With pop with gdp" = count_wpop_wgdp_IN_oa_out,
  "With pop with counterfactual gdp" = count_wpop_wgdpcf_IN_oa_out,
  "With pop no gdp" = count_wpop_nogdp_IN_oa_out, 
  "No pop with gdp" = count_nopop_wgdp_IN_oa_out,
  "No pop with counterfactual gdp" = count_nopop_wgdpcf_IN_oa_out,
  "No pop no gdp" = count_nopop_nogdp_IN_oa_out,
  "No pop with gdp interaction" = count_nopop_wgdp_IN_interac_oa_out,
  "No pop with counterfactual gdp interaction" = count_nopop_wgdpcf_IN_interac_oa_out,
  "No pop no gdp interaction" = count_nopop_nogdp_IN_interac_oa_out,
  "With pop no gdp interaction" = count_wpop_nogdp_IN_interac_oa_out,
  "With pop with gdp interaction" = count_wpop_wgdp_IN_interac_oa_out,
  "With pop with counterfactual gdp interaction" = count_wpop_wgdpcf_IN_interac_oa_out)

# In-migration

model_list_IN_oa_count_in <- list(
  "With pop with gdp" = count_wpop_wgdp_IN_oa_in,
  "With pop with counterfactual gdp" = count_wpop_wgdpcf_IN_oa_in,
  "With pop no gdp" = count_wpop_nogdp_IN_oa_in, 
  "No pop with gdp" = count_nopop_wgdp_IN_oa_in,
  "No pop with counterfactual gdp" = count_nopop_wgdpcf_IN_oa_in,
  "No pop no gdp" = count_nopop_nogdp_IN_oa_in,
  "No pop with gdp interaction" = count_nopop_wgdp_IN_interac_oa_in,
  "No pop with counterfactual gdp interaction" = count_nopop_wgdpcf_IN_interac_oa_in,
  "No pop no gdp interaction" = count_nopop_nogdp_IN_interac_oa_in,
  "With pop no gdp interaction" = count_wpop_nogdp_IN_interac_oa_in,
  "With pop with gdp interaction" = count_wpop_wgdp_IN_interac_oa_in,
  "With pop with counterfactual gdp interaction" = count_wpop_wgdpcf_IN_interac_oa_in)

## STEP 2B: Model list international negbin models ##

model_list_INT_oa_count_out <- list(
  "With pop with gdp" = count_wpop_wgdp_INT_oa_out,
  "With pop with counterfactual gdp" = count_wpop_wgdpcf_INT_oa_out,
  "With pop no gdp" = count_wpop_nogdp_INT_oa_out, 
  "No pop with gdp" = count_nopop_wgdp_INT_oa_out,
  "No pop with counterfactual gdp" = count_nopop_wgdpcf_INT_oa_out,
  "No pop no gdp" = count_nopop_nogdp_INT_oa_out,
  "No pop with gdp interaction" = count_nopop_wgdp_INT_interac_oa_out,
  "No pop with counterfactual gdp interaction" = count_nopop_wgdpcf_INT_interac_oa_out,
  "No pop no gdp interaction" = count_nopop_nogdp_INT_interac_oa_out,
  "With pop no gdp interaction" = count_wpop_nogdp_INT_interac_oa_out,
  "With pop with gdp interaction" = count_wpop_wgdp_INT_interac_oa_out,
  "With pop with counterfactual gdp interaction" = count_wpop_wgdpcf_INT_interac_oa_out)

# In-migration

model_list_INT_oa_count_in <- list(
  "With pop with gdp" = count_wpop_wgdp_INT_oa_in,
  "With pop with counterfactual gdp" = count_wpop_wgdpcf_INT_oa_in,
  "With pop no gdp" = count_wpop_nogdp_INT_oa_in, 
  "No pop with gdp" = count_nopop_wgdp_INT_oa_in,
  "No pop with counterfactual gdp" = count_nopop_wgdpcf_INT_oa_in,
  "No pop no gdp" = count_nopop_nogdp_INT_oa_in,
  "No pop with gdp interaction" = count_nopop_wgdp_INT_interac_oa_in,
  "No pop with counterfactual gdp interaction" = count_nopop_wgdpcf_INT_interac_oa_in,
  "No pop no gdp interaction" = count_nopop_nogdp_INT_interac_oa_in,
  "With pop no gdp interaction" = count_wpop_nogdp_INT_interac_oa_in,
  "With pop with gdp interaction" = count_wpop_wgdp_INT_interac_oa_in,
  "With pop with counterfactual gdp interaction" = count_wpop_wgdpcf_INT_interac_oa_in)

## STEP 3: create function to rename terms ##

# The first paragraph (ending before tidy_result) I put in to deal with 
# wonky confidence intervals

tidy_terms <- function(model_results) {
  # Try with conf.int first, fall back without it if it fails
  tidy_result <- tryCatch(
    tidy(model_results, conf.int = TRUE, exponentiate = FALSE),
    error = function(e) {
      warning("Could not compute confidence intervals, proceeding without them")
      tidy(model_results, conf.int = FALSE, exponentiate = FALSE) %>%
        mutate(conf.low = NA_real_, conf.high = NA_real_)
    }
  )
  tidy_result %>% 
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "log(gdppc)" ~ "Log Regional GDPpc",
        term == "log(gdppc_cf)" ~ "Log Regional CF GDPpc",
        term == "sdhi" ~ "Subnational HDI (SHDI)", 
        term == "log(regpop)" ~ "Log Regional Population", 
        term == "factor(name)Cherkasy" ~ "C: Cherkasy",
        term == "post_2014:factor(name)Cherkasy" ~ "C: P-2014 x Cherkasy",
        term == "factor(name)Chernihiv" ~ "N: Chernihiv",
        term == "post_2014:factor(name)Chernihiv" ~ "N: P-2014 x Chernihiv",
        term == "factor(name)Chernivtsi" ~ "W: Chernivtsi",
        term == "post_2014:factor(name)Chernivtsi" ~ "W: P-2014 x Chernivtsi",
        term == "factor(name)Crimea" ~ "S: Crimea (A)",
        term == "post_2014:factor(name)Crimea" ~ "S: P-2014 x Crimea (A)",
        term == "factor(name)Dnipropetrovs'k" ~ "E: Dnipropetrovsk",
        term == "post_2014:factor(name)Dnipropetrovs'k" ~ "E: P-2014 x Dnipropetrovsk",
        term == "factor(name)Donets'k" ~ "E: Donetsk (O)",
        term == "post_2014:factor(name)Donets'k" ~ "E: P-2014 x Donetsk (O)",
        term == "factor(name)Ivano-Frankivs'k" ~ "W: Ivano-Frankivsk",
        term == "post_2014:factor(name)Ivano-Frankivs'k" ~ "W: P-2014 x Ivano-Frankivsk",
        term == "factor(name)Kharkiv" ~ "E: Kharkiv",
        term == "post_2014:factor(name)Kharkiv" ~ "E: P-2014 x Kharkiv",
        term == "factor(name)Kherson" ~ "S: Kherson",
        term == "post_2014:factor(name)Kherson" ~ "S: P-2014 x Kherson",
        term == "factor(name)Khmel'nyts'kyy" ~ "W: Khmelnytskyi",
        term == "post_2014:factor(name)Khmel'nyts'kyy" ~ "W: P-2014 x Khmelnytskyi",
        term == "factor(name)Kiev" ~ "N: Kyiv",
        term == "post_2014:factor(name)Kiev" ~ "N: P-2014 x Kyiv",
        term == "factor(name)Kiev City" ~ "K: Kyiv City",
        term == "post_2014:factor(name)Kiev City" ~ "K: P-2014 x Kyiv City",
        term == "factor(name)Kirovohrad" ~ "C: Kirovohrad",
        term == "post_2014:factor(name)Kirovohrad" ~ "C: P-2014 x Kirovohrad",
        term == "factor(name)L'viv" ~ "W: Lviv",
        term == "post_2014:factor(name)L'viv" ~ "W: P-2014 x Lviv",
        term == "factor(name)Luhans'k" ~ "E: Luhansk (O)",
        term == "post_2014:factor(name)Luhans'k" ~ "E: P-2014 x Luhansk (O)",
        term == "factor(name)Mykolayiv" ~ "S: Mykolaiv",
        term == "post_2014:factor(name)Mykolayiv" ~ "S: P-2014 x Mykolaiv",
        term == "factor(name)Odessa" ~ "S: Odesa",
        term == "post_2014:factor(name)Odessa" ~ "S: P-2014 x Odesa",
        term == "factor(name)Poltava" ~ "C: Poltava",
        term == "post_2014:factor(name)Poltava" ~ "C: P-2014 x Poltava",
        term == "factor(name)Rivne" ~ "W: Rivne",
        term == "post_2014:factor(name)Rivne" ~ "W: P-2014 x Rivne",
        term == "factor(name)Sevastopol" ~ "S: Sevastopol (A)",
        term == "post_2014:factor(name)Sevastopol" ~ "S: P-2014 x Sevastopol (A)",
        term == "factor(name)Sumy" ~ "N: Sumy",
        term == "post_2014:factor(name)Sumy" ~ "N: P-2014 x Sumy",
        term == "factor(name)Ternopil'" ~ "W: Ternopil",
        term == "post_2014:factor(name)Ternopil'" ~ "W: P-2014 x Ternopil",
        term == "factor(name)Transcarpathia" ~ "W: Zakarpattia",
        term == "post_2014:factor(name)Transcarpathia" ~ "W: P-2014 x Zakarpattia",
        term == "factor(name)Vinnytsya" ~ "C: Vinnytsia",
        term == "post_2014:factor(name)Vinnytsya" ~ "C: P-2014 x Vinnytsia",
        term == "factor(name)Volyn" ~ "W: Volyn",
        term == "post_2014:factor(name)Volyn" ~ "W: P-2014 x Volyn",
        term == "factor(name)Zaporizhzhya" ~ "E: Zaporizhzhia",
        term == "post_2014:factor(name)Zaporizhzhya" ~ "E: P-2014 x Zaporizhzhia",
        term == "factor(name)Zhytomyr" ~ "N: Zhytomyr",
        term == "post_2014:factor(name)Zhytomyr" ~ "N: P-2014 x Zhytomyr",
        term == "nameCentral Ukraine" ~ "Central Ukraine", 
        term == "nameNorthern Ukraine" ~ "Northern Ukraine", 
        term == "nameEastern Ukraine" ~ "Eastern Ukraine", 
        term == "nameSouthern Ukraine" ~ "Southern Ukraine", 
        term == "factor(name)Central Ukraine" ~ "Central Ukraine", 
        term == "factor(name)Northern Ukraine" ~ "Northern Ukraine", 
        term == "factor(name)Eastern Ukraine" ~ "Eastern Ukraine", 
        term == "factor(name)Southern Ukraine" ~ "Southern Ukraine",
        term == "post_2014" ~ "Post-2014", 
        term == "post_2014:nameCentral Ukraine" ~ "Post-2014 x Central Ukraine", 
        term == "post_2014:nameNorthern Ukraine" ~ "Post-2014 x Northern Ukraine", 
        term == "post_2014:nameEastern Ukraine" ~ "Post-2014 x Eastern Ukraine", 
        term == "post_2014:nameSouthern Ukraine" ~ "Post-2014 x Southern Ukraine", 
        term == "factor(year)2009" ~ "2009",
        term == "factor(year)2010" ~ "2010", 
        term == "factor(year)2011" ~ "2011", 
        term == "factor(year)2012" ~ "2012", 
        term == "factor(year)2013" ~ "2013", 
        term == "factor(year)2014" ~ "2014", 
        term == "factor(year)2015" ~ "2015", 
        term == "factor(year)2016" ~ "2016", 
        term == "factor(year)2017" ~ "2017", 
        term == "factor(year)2018" ~ "2018", 
        term == "factor(year)2019" ~ "2019", 
        term == "factor(year)2020" ~ "2020", 
        term == "factor(year)2021" ~ "2021", 
        term == "factor(year)2022" ~ "2022",
        TRUE ~ term 
      ),  
      term = factor(term, levels = c(
        "2009",
        "2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019",
        "2020",
        "2021", 
        "2022",
        "Subnational HDI (SHDI)",
        "Log Regional Population",
        "Log Regional GDPpc",
        "Log Regional CF GDPpc",
        "Central Ukraine",
        "Eastern Ukraine",
        "Northern Ukraine",
        "Southern Ukraine",
        "C: Cherkasy",
        "C: Kirovohrad",
        "C: Poltava",
        "C: Vinnytsia",
        "E: Dnipropetrovsk",
        "E: Donetsk (O)",
        "E: Kharkiv",
        "E: Luhansk (O)",
        "E: Zaporizhzhia",
        "K: Kyiv City",
        "N: Chernihiv",
        "N: Kyiv",
        "N: Sumy",
        "N: Zhytomyr",
        "S: Crimea (A)",
        "S: Kherson",
        "S: Mykolaiv",
        "S: Odesa",
        "S: Sevastopol (A)",
        "W: Chernivtsi",
        "W: Ivano-Frankivsk",
        "W: Khmelnytskyi",
        "W: Lviv",
        "W: Rivne",
        "W: Ternopil",
        "W: Volyn",
        "W: Zakarpattia",
        "Post-2014",
        "Post-2014 x Central Ukraine",
        "Post-2014 x Eastern Ukraine",
        "Post-2014 x Northern Ukraine",
        "Post-2014 x Southern Ukraine",
        "C: P-2014 x Cherkasy",
        "C: P-2014 x Kirovohrad",
        "C: P-2014 x Poltava",
        "C: P-2014 x Vinnytsia",
        "E: P-2014 x Dnipropetrovsk",
        "E: P-2014 x Donetsk (O)",
        "E: P-2014 x Kharkiv",
        "E: P-2014 x Luhansk (O)",
        "E: P-2014 x Zaporizhzhia",
        "K: P-2014 x Kyiv City",
        "N: P-2014 x Chernihiv",
        "N: P-2014 x Kyiv",
        "N: P-2014 x Sumy",
        "N: P-2014 x Zhytomyr",
        "S: P-2014 x Crimea (A)",
        "S: P-2014 x Kherson",
        "S: P-2014 x Mykolaiv",
        "S: P-2014 x Odesa",
        "S: P-2014 x Sevastopol (A)",
        "W: P-2014 x Chernivtsi",
        "W: P-2014 x Ivano-Frankivsk",
        "W: P-2014 x Khmelnytskyi",
        "W: P-2014 x Lviv",
        "W: P-2014 x Rivne",
        "W: P-2014 x Ternopil",
        "W: P-2014 x Volyn",
        "W: P-2014 x Zakarpattia"
      )),
      significant = if_else(p.value  <= 0.05, "Yes", "No")
    ) %>% 
    filter(term != "Intercept") 
}

#### Appendix F3 and associated T4 ####

#### F3 ####
library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)
library(patchwork)
library(broom)
library(broom.mixed)
library(sandwich)
library(lmtest)

# 1. Define standardized color palette (only interaction models)

model_colors <- c(
  "No pop no gdp interaction"          = "#B8DE29",
  "No pop with gdp interaction"        = "#1F9E89",
  "With pop no gdp interaction"        = "#6A00A8",
  "Full model"                         = "#FCA636",
  "No pop with CF gdp interaction"     = "#45B8A0",
  "Full model with CF gdp" = "#F0756A"
)

# 2. Update model lists to ONLY include interaction models

# For Negative Binomial models - ONLY interaction models
model_list_IN_oa_count_out <- list(
  "No pop with gdp interaction" = count_nopop_wgdp_IN_interac_oa_out,
  "No pop with CF gdp interaction" = count_nopop_wgdpcf_IN_interac_oa_out,
  "No pop no gdp interaction" = count_nopop_nogdp_IN_interac_oa_out,
  "With pop no gdp interaction" = count_wpop_nogdp_IN_interac_oa_out,
  "Full model" = count_wpop_wgdp_IN_interac_oa_out,
  "Full model with CF gdp" = count_wpop_wgdpcf_IN_interac_oa_out
)

model_list_IN_oa_count_in <- list(
  "No pop with gdp interaction" = count_nopop_wgdp_IN_interac_oa_in,
  "No pop with CF gdp interaction" = count_nopop_wgdpcf_IN_interac_oa_in,
  "No pop no gdp interaction" = count_nopop_nogdp_IN_interac_oa_in,
  "With pop no gdp interaction" = count_wpop_nogdp_IN_interac_oa_in,
  "Full model" = count_wpop_wgdp_IN_interac_oa_in,
  "Full model with CF gdp" = count_wpop_wgdpcf_IN_interac_oa_in
)

# 3. Generate combined results (NOW only from interaction models)

combined_results_IN_oa_count_out <- map_df(model_list_IN_oa_count_out, tidy_terms, .id = "model")
combined_results_IN_oa_count_in <- map_df(model_list_IN_oa_count_in, tidy_terms, .id = "model")

# 4. Combine NegBin for IN-migration

combined_in_negbin <- combined_results_IN_oa_count_in %>%
  mutate(model_type = "Negative Binomial",
         migration_type = "In-migration",
         significant = if_else(p.value <= 0.05, "Yes", "No"),
         significant = factor(significant, levels = c("Yes", "No")))

# 5. Combine NegBin for OUT-migration
combined_out_negbin <- combined_results_IN_oa_count_out %>%
  mutate(model_type = "Negative Binomial",
         migration_type = "Out-migration",
         significant = if_else(p.value <= 0.05, "Yes", "No"),
         significant = factor(significant, levels = c("Yes", "No")))

# 6. Combine all data and filter terms

combined_all <- bind_rows(combined_in_negbin, combined_out_negbin) %>%
  mutate(
    model_type = factor(model_type, 
                        levels = c("Negative Binomial", "Difference-in-Differences")),
    migration_type = factor(migration_type,
                            levels = c("In-migration", "Out-migration"))
  ) %>%
  # Filter to only include specified terms (excluding region fixed effects)
  filter(
    term %in% c("Log Regional GDPpc", "Log Regional CF GDPpc",
                "Log Regional Population", "Post-2014") |
      grepl("P-2014 x ", term)  # Only interaction terms
  ) %>% 
  filter(!is.na(estimate))

# 7. Compute x-axis limits based on confidence intervals

x_min_all <- min(combined_all$conf.low, na.rm = TRUE)
x_max_all <- max(combined_all$conf.high, na.rm = TRUE)

# 8. Create faceted plot by migration type

figure_combined <- combined_all %>%
  ggplot(aes(estimate, term, color = model, shape = significant)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 3, 
             position = position_dodge(width = 0.6)) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = c("Yes" = 16, "No" = 1),
                     name = "Significance (p <= 0.05)") +
  scale_x_continuous(limits = c(x_min_all, x_max_all)) +
  facet_wrap(~ migration_type, ncol = 2) +
  guides(color = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Negative Binomial Internal Migration Models by Migration Type \n OpenAlex 2009-2022",
    y = NULL,
    x = "Estimates",
    color = "Model"
  )

figure_combined <- combined_all %>%
  ggplot(aes(estimate, term, color = model, shape = significant)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 3, 
             position = position_dodge(width = 0.6)) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = c("Yes" = 16, "No" = 1),
                     name = "Significance (p <= 0.05)") +
  scale_x_continuous(limits = c(x_min_all, x_max_all)) +
  facet_wrap(~ migration_type, ncol = 2,
             labeller = label_wrap_gen(width = 15)) +
  guides(
    color = guide_legend(order = 1, nrow = 2),
    shape = guide_legend(order = 2, nrow = 2)
  ) +
  theme_minimal() +
  theme(
    # Title: 9pt Times New Roman, 10pt leading
    plot.title = element_text(
      family = "Times New Roman", size = 9, face = "bold",
      hjust = 0.5, lineheight = 10/9
    ),
    # Axes: 8pt Times New Roman, 9pt leading
    axis.title = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8
    ),
    axis.text = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8
    ),
    # Facet strip labels: axes category (8pt)
    strip.text = element_text(
      family = "Times New Roman", size = 8, face = "bold",
      lineheight = 9/8, margin = margin(t = 3, b = 3, unit = "pt")
    ),
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    # Legend title: axes category (8pt)
    legend.title = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8
    ),
    # Legend text: labels category (6pt)
    legend.text = element_text(
      family = "Times New Roman", size = 6, lineheight = 7/6
    ),
    legend.position = "bottom"
  ) +
  labs(
    title = "Negative Binomial Internal Migration Models by Migration Type \n OpenAlex 2009-2022",
    y = NULL,
    x = "Estimates",
    color = "Model"
  )

# 9. Output

figure_combined

#save_plot(figure_combined,
#          write_path("updated-data/twoyearbackfill/nbins-faceted-IN.pdf"),
#          width = 12,
#          height = 12)

#save_plot(figure_combined,
#          write_path("updated-data/twoyearbackfill/nbins-faceted-IN.svg"),
#          width = 12, height = 12)

save_plot(figure_combined,
          write_path("nbins-faceted-IN.pdf"),
          width = 12,
          height = 12)

#### T4 ####
library(texreg)

# ======================================================
# Create custom coefficient names mapping (must be a LIST, not vector)
# ======================================================
custom_coef_names <- list(
  "(Intercept)" = "Intercept",
  "log(gdppc)" = "Log Regional GDPpc",
  "log(gdppc_cf)" = "Log Regional CF GDPpc",
  "log(regpop)" = "Log Regional Population",
  "post_2014" = "Post-2014",
  "factor(name)Cherkasy" = "C: Cherkasy",
  "post_2014:factor(name)Cherkasy" = "C: P-2014 x Cherkasy",
  "factor(name)Chernihiv" = "N: Chernihiv",
  "post_2014:factor(name)Chernihiv" = "N: P-2014 x Chernihiv",
  "factor(name)Chernivtsi" = "W: Chernivtsi",
  "post_2014:factor(name)Chernivtsi" = "W: P-2014 x Chernivtsi",
  "factor(name)Crimea" = "S: Crimea (A)",
  "post_2014:factor(name)Crimea" = "S: P-2014 x Crimea (A)",
  "factor(name)Dnipropetrovs'k" = "E: Dnipropetrovsk",
  "post_2014:factor(name)Dnipropetrovs'k" = "E: P-2014 x Dnipropetrovsk",
  "factor(name)Donets'k" = "E: Donetsk (O)",
  "post_2014:factor(name)Donets'k" = "E: P-2014 x Donetsk (O)",
  "factor(name)Ivano-Frankivs'k" = "W: Ivano-Frankivsk",
  "post_2014:factor(name)Ivano-Frankivs'k" = "W: P-2014 x Ivano-Frankivsk",
  "factor(name)Kharkiv" = "E: Kharkiv",
  "post_2014:factor(name)Kharkiv" = "E: P-2014 x Kharkiv",
  "factor(name)Kherson" = "S: Kherson",
  "post_2014:factor(name)Kherson" = "S: P-2014 x Kherson",
  "factor(name)Khmel'nyts'kyy" = "W: Khmelnytskyi",
  "post_2014:factor(name)Khmel'nyts'kyy" = "W: P-2014 x Khmelnytskyi",
  "factor(name)Kiev" = "N: Kyiv",
  "post_2014:factor(name)Kiev" = "N: P-2014 x Kyiv",
  "factor(name)Kiev City" = "K: Kyiv City",
  "post_2014:factor(name)Kiev City" = "K: P-2014 x Kyiv City",
  "factor(name)Kirovohrad" = "C: Kirovohrad",
  "post_2014:factor(name)Kirovohrad" = "C: P-2014 x Kirovohrad",
  "factor(name)L'viv" = "W: Lviv",
  "post_2014:factor(name)L'viv" = "W: P-2014 x Lviv",
  "factor(name)Luhans'k" = "E: Luhansk (O)",
  "post_2014:factor(name)Luhans'k" = "E: P-2014 x Luhansk (O)",
  "factor(name)Mykolayiv" = "S: Mykolaiv",
  "post_2014:factor(name)Mykolayiv" = "S: P-2014 x Mykolaiv",
  "factor(name)Odessa" = "S: Odesa",
  "post_2014:factor(name)Odessa" = "S: P-2014 x Odesa",
  "factor(name)Poltava" = "C: Poltava",
  "post_2014:factor(name)Poltava" = "C: P-2014 x Poltava",
  "factor(name)Rivne" = "W: Rivne",
  "post_2014:factor(name)Rivne" = "W: P-2014 x Rivne",
  "factor(name)Sevastopol" = "S: Sevastopol (A)",
  "post_2014:factor(name)Sevastopol" = "S: P-2014 x Sevastopol (A)",
  "factor(name)Sumy" = "N: Sumy",
  "post_2014:factor(name)Sumy" = "N: P-2014 x Sumy",
  "factor(name)Ternopil'" = "W: Ternopil",
  "post_2014:factor(name)Ternopil'" = "W: P-2014 x Ternopil",
  "factor(name)Transcarpathia" = "W: Zakarpattia",
  "post_2014:factor(name)Transcarpathia" = "W: P-2014 x Zakarpattia",
  "factor(name)Vinnytsya" = "C: Vinnytsia",
  "post_2014:factor(name)Vinnytsya" = "C: P-2014 x Vinnytsia",
  "factor(name)Volyn" = "W: Volyn",
  "post_2014:factor(name)Volyn" = "W: P-2014 x Volyn",
  "factor(name)Zaporizhzhya" = "E: Zaporizhzhia",
  "post_2014:factor(name)Zaporizhzhya" = "E: P-2014 x Zaporizhzhia",
  "factor(name)Zhytomyr" = "N: Zhytomyr",
  "post_2014:factor(name)Zhytomyr" = "N: P-2014 x Zhytomyr"
)

# ======================================================
# OUT-migration table with custom labels
# ======================================================
texreg(
  list(count_nopop_nogdp_IN_interac_oa_out,
       count_nopop_wgdp_IN_interac_oa_out,
       count_nopop_wgdpcf_IN_interac_oa_out,
       count_wpop_nogdp_IN_interac_oa_out,
       count_wpop_wgdp_IN_interac_oa_out,
       count_wpop_wgdpcf_IN_interac_oa_out),
  file = write_path("updated-data/oneyearbackfill/negbin_outmigration_models_IN_texreg.tex"),
  custom.model.names = c("No pop\nno GDP", "No pop\nw/ GDP", "No pop \nw/CF GDP",
                         "W/ pop\nno GDP", "Full model", "Full model w/CF GDP"),
  custom.coef.map = custom_coef_names,
  caption = "Negative Binomial Models: Internal Out-migration (2009-2022)",
  label = "tab:out_migration",
  stars = c(0.001, 0.01, 0.05),
  custom.note = "Standard errors in parentheses. All models include offset for Log(Population). %stars.",
  fontsize = "small"
)

# ======================================================
# IN-migration table with custom labels
# ======================================================
texreg(
  list(count_nopop_nogdp_IN_interac_oa_in,
       count_nopop_wgdp_IN_interac_oa_in,
       count_nopop_wgdpcf_IN_interac_oa_in,
       count_wpop_nogdp_IN_interac_oa_in,
       count_wpop_wgdp_IN_interac_oa_in,
       count_wpop_wgdpcf_IN_interac_oa_in),
  file = write_path("updated-data/oneyearbackfill/negbin_inmigration_models_IN_texreg.tex"),
  custom.model.names = c("No pop\nno GDP", "No pop\nw/ GDP", "No pop \nw/CF GDP",
                         "W/ pop\nno GDP", "Full model", "Full model w/CF GDP"),
  custom.coef.map = custom_coef_names,
  caption = "Negative Binomial Models: Internal In-migration (2009-2022)",
  label = "tab:in_migration",
  stars = c(0.001, 0.01, 0.05),
  custom.note = "Standard errors in parentheses. All models include offset for Log(Population). %stars.",
  fontsize = "small"
)

# ======================================================
# Combined table with custom labels
# ======================================================
texreg(
  list(count_nopop_nogdp_IN_interac_oa_in,
       count_nopop_wgdp_IN_interac_oa_in,
       count_nopop_wgdpcf_IN_interac_oa_in,
       count_wpop_nogdp_IN_interac_oa_in,
       count_wpop_wgdp_IN_interac_oa_in,
       count_wpop_wgdpcf_IN_interac_oa_in,
       count_nopop_nogdp_IN_interac_oa_out,
       count_nopop_wgdp_IN_interac_oa_out,
       count_nopop_wgdpcf_IN_interac_oa_out,
       count_wpop_nogdp_IN_interac_oa_out,
       count_wpop_wgdp_IN_interac_oa_out,
       count_wpop_wgdpcf_IN_interac_oa_out),
  file = write_path("updated-data/oneyearbackfill/negbin_combined_models_IN_texreg.tex"),
  custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)"),
  custom.coef.map = custom_coef_names,
  caption = "Negative Binomial Models: Internal In-migration and Out-migration (2009-2022)",
  label = "tab:combined_migration",
  stars = c(0.001, 0.01, 0.05),
  custom.note = "Standard errors in parentheses. All models include offset for Log(Population of scholars). Models (1)-(6): In-migration. Models (7)-(12): Out-migration. %stars.",
  fontsize = "tiny"
)

#### Appendix F4 and associated T6 ####

#### F4 ####
#### Libraries ####

library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)
library(patchwork)
library(broom)
library(broom.mixed)

# 1. Define standardized color palette (only interaction models)

model_colors <- c(
  "No pop no gdp interaction"          = "#B8DE29",
  "No pop with gdp interaction"        = "#1F9E89",
  "With pop no gdp interaction"        = "#6A00A8",
  "Full model"                         = "#FCA636",
  "No pop with CF gdp interaction"     = "#45B8A0",
  "Full model with CF gdp" = "#F0756A"
)

# 2. Update model lists to ONLY include interaction models

# For Negative Binomial models - ONLY interaction models
model_list_INT_oa_count_out <- list(
  "No pop with gdp interaction" = count_nopop_wgdp_INT_interac_oa_out,
  "No pop with CF gdp interaction" = count_nopop_wgdpcf_INT_interac_oa_out,
  "No pop no gdp interaction" = count_nopop_nogdp_INT_interac_oa_out,
  "With pop no gdp interaction" = count_wpop_nogdp_INT_interac_oa_out,
  "Full model" = count_wpop_wgdp_INT_interac_oa_out,
  "Full model with CF gdp" = count_wpop_wgdpcf_INT_interac_oa_out
)

model_list_INT_oa_count_in <- list(
  "No pop with gdp interaction" = count_nopop_wgdp_INT_interac_oa_in,
  "No pop with CF gdp interaction" = count_nopop_wgdpcf_INT_interac_oa_in,
  "No pop no gdp interaction" = count_nopop_nogdp_INT_interac_oa_in,
  "With pop no gdp interaction" = count_wpop_nogdp_INT_interac_oa_in,
  "Full model" = count_wpop_wgdp_INT_interac_oa_in,
  "Full model with CF gdp" = count_wpop_wgdpcf_INT_interac_oa_in
)


# 3. Generate combined results 

combined_results_INT_oa_count_out <- map_df(model_list_INT_oa_count_out, tidy_terms, .id = "model")
combined_results_INT_oa_count_in <- map_df(model_list_INT_oa_count_in, tidy_terms, .id = "model")


# 4. Combine NegBin for IN-migration

combined_in_negbin <- combined_results_INT_oa_count_in %>%
  mutate(model_type = "Negative Binomial",
         migration_type = "In-migration",
         significant = if_else(p.value <= 0.05, "Yes", "No"),
         significant = factor(significant, levels = c("Yes", "No")))


# 5. Combine NegBin for OUT-migration

combined_out_negbin <- combined_results_INT_oa_count_out %>%
  mutate(model_type = "Negative Binomial",
         migration_type = "Out-migration",
         significant = if_else(p.value <= 0.05, "Yes", "No"),
         significant = factor(significant, levels = c("Yes", "No")))


# 6. Combine all data and filter terms

combined_all <- bind_rows(combined_in_negbin, combined_out_negbin) %>%
  mutate(
    model_type = factor(model_type, 
                        levels = c("Negative Binomial", "Difference-in-Differences")),
    migration_type = factor(migration_type,
                            levels = c("In-migration", "Out-migration"))
  ) %>%
  # Filter to only include specified terms (excluding region fixed effects)
  filter(
    term %in% c("Log Regional GDPpc", "Log Regional CF GDPpc",
                "Log Regional Population", "Post-2014") |
      grepl("P-2014 x ", term)  # Only interaction terms
  ) %>% 
  filter(!is.na(estimate))


# 7. Compute x-axis limits based on confidence intervals

x_min_all <- min(combined_all$conf.low, na.rm = TRUE)
x_max_all <- max(combined_all$conf.high, na.rm = TRUE)


# 8. Create faceted plot by migration type

figure_combined <- combined_all %>%
  ggplot(aes(estimate, term, color = model, shape = significant)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 3, 
             position = position_dodge(width = 0.6)) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = c("Yes" = 16, "No" = 1),
                     name = "Significance (p <= 0.05)") +
  scale_x_continuous(limits = c(x_min_all, x_max_all)) +
  facet_wrap(~ migration_type, ncol = 2,
             labeller = label_wrap_gen(width = 15)) +
  guides(
    color = guide_legend(order = 1, nrow = 2),
    shape = guide_legend(order = 2, nrow = 2)
  ) +
  theme_minimal() +
  theme(
    # Title: 9pt Times New Roman, 10pt leading
    plot.title = element_text(
      family = "Times New Roman", size = 9, face = "bold",
      hjust = 0.5, lineheight = 10/9
    ),
    # Axes: 8pt Times New Roman, 9pt leading
    axis.title = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8
    ),
    axis.text = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8
    ),
    # Facet strip labels: axes category (8pt)
    strip.text = element_text(
      family = "Times New Roman", size = 8, face = "bold",
      lineheight = 9/8, margin = margin(t = 3, b = 3, unit = "pt")
    ),
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    # Legend title: axes category (8pt)
    legend.title = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8
    ),
    # Legend text: labels category (6pt)
    legend.text = element_text(
      family = "Times New Roman", size = 6, lineheight = 7/6
    ),
    legend.position = "bottom"
  ) +
  labs(
    title = "Negative Binomial International Migration Models by Migration Type \n OpenAlex 2009-2022",
    y = NULL,
    x = "Estimates",
    color = "Model"
  )


# 9. Output

figure_combined

#save_plot(figure_combined,
#          write_path("updated-data/twoyearbackfill/nbins-faceted-INT.pdf"),
#          width = 12,
#          height = 12)

#save_plot(figure_combined,
#          write_path("updated-data/twoyearbackfill/nbins-faceted-INT.svg"),
#          width = 12,
#          height = 12)

save_plot(figure_combined,
          write_path("nbins-faceted-INT.pdf"),
          width = 12,
          height = 12)


#### T6 ####
library(texreg)

# ======================================================
# Create custom coefficient names mapping (must be a LIST, not vector)
# ======================================================
custom_coef_names <- list(
  "(Intercept)" = "Intercept",
  "log(gdppc)" = "Log Regional GDPpc",
  "log(gdppc_cf)" = "Log Regional CF GDPpc",
  "log(regpop)" = "Log Regional Population",
  "post_2014" = "Post-2014",
  "factor(name)Cherkasy" = "C: Cherkasy",
  "post_2014:factor(name)Cherkasy" = "C: P-2014 x Cherkasy",
  "factor(name)Chernihiv" = "N: Chernihiv",
  "post_2014:factor(name)Chernihiv" = "N: P-2014 x Chernihiv",
  "factor(name)Chernivtsi" = "W: Chernivtsi",
  "post_2014:factor(name)Chernivtsi" = "W: P-2014 x Chernivtsi",
  "factor(name)Crimea" = "S: Crimea (A)",
  "post_2014:factor(name)Crimea" = "S: P-2014 x Crimea (A)",
  "factor(name)Dnipropetrovs'k" = "E: Dnipropetrovsk",
  "post_2014:factor(name)Dnipropetrovs'k" = "E: P-2014 x Dnipropetrovsk",
  "factor(name)Donets'k" = "E: Donetsk (O)",
  "post_2014:factor(name)Donets'k" = "E: P-2014 x Donetsk (O)",
  "factor(name)Ivano-Frankivs'k" = "W: Ivano-Frankivsk",
  "post_2014:factor(name)Ivano-Frankivs'k" = "W: P-2014 x Ivano-Frankivsk",
  "factor(name)Kharkiv" = "E: Kharkiv",
  "post_2014:factor(name)Kharkiv" = "E: P-2014 x Kharkiv",
  "factor(name)Kherson" = "S: Kherson",
  "post_2014:factor(name)Kherson" = "S: P-2014 x Kherson",
  "factor(name)Khmel'nyts'kyy" = "W: Khmelnytskyi",
  "post_2014:factor(name)Khmel'nyts'kyy" = "W: P-2014 x Khmelnytskyi",
  "factor(name)Kiev" = "N: Kyiv",
  "post_2014:factor(name)Kiev" = "N: P-2014 x Kyiv",
  "factor(name)Kiev City" = "K: Kyiv City",
  "post_2014:factor(name)Kiev City" = "K: P-2014 x Kyiv City",
  "factor(name)Kirovohrad" = "C: Kirovohrad",
  "post_2014:factor(name)Kirovohrad" = "C: P-2014 x Kirovohrad",
  "factor(name)L'viv" = "W: Lviv",
  "post_2014:factor(name)L'viv" = "W: P-2014 x Lviv",
  "factor(name)Luhans'k" = "E: Luhansk (O)",
  "post_2014:factor(name)Luhans'k" = "E: P-2014 x Luhansk (O)",
  "factor(name)Mykolayiv" = "S: Mykolaiv",
  "post_2014:factor(name)Mykolayiv" = "S: P-2014 x Mykolaiv",
  "factor(name)Odessa" = "S: Odesa",
  "post_2014:factor(name)Odessa" = "S: P-2014 x Odesa",
  "factor(name)Poltava" = "C: Poltava",
  "post_2014:factor(name)Poltava" = "C: P-2014 x Poltava",
  "factor(name)Rivne" = "W: Rivne",
  "post_2014:factor(name)Rivne" = "W: P-2014 x Rivne",
  "factor(name)Sevastopol" = "S: Sevastopol (A)",
  "post_2014:factor(name)Sevastopol" = "S: P-2014 x Sevastopol (A)",
  "factor(name)Sumy" = "N: Sumy",
  "post_2014:factor(name)Sumy" = "N: P-2014 x Sumy",
  "factor(name)Ternopil'" = "W: Ternopil",
  "post_2014:factor(name)Ternopil'" = "W: P-2014 x Ternopil",
  "factor(name)Transcarpathia" = "W: Zakarpattia",
  "post_2014:factor(name)Transcarpathia" = "W: P-2014 x Zakarpattia",
  "factor(name)Vinnytsya" = "C: Vinnytsia",
  "post_2014:factor(name)Vinnytsya" = "C: P-2014 x Vinnytsia",
  "factor(name)Volyn" = "W: Volyn",
  "post_2014:factor(name)Volyn" = "W: P-2014 x Volyn",
  "factor(name)Zaporizhzhya" = "E: Zaporizhzhia",
  "post_2014:factor(name)Zaporizhzhya" = "E: P-2014 x Zaporizhzhia",
  "factor(name)Zhytomyr" = "N: Zhytomyr",
  "post_2014:factor(name)Zhytomyr" = "N: P-2014 x Zhytomyr"
)

# ======================================================
# OUT-migration table with custom labels
# ======================================================
texreg(
  list(count_nopop_nogdp_INT_interac_oa_out,
       count_nopop_wgdp_INT_interac_oa_out,
       count_nopop_wgdpcf_INT_interac_oa_out,
       count_wpop_nogdp_INT_interac_oa_out,
       count_wpop_wgdp_INT_interac_oa_out,
       count_wpop_wgdpcf_INT_interac_oa_out),
  file = write_path("updated-data/oneyearbackfill/negbin_outmigration_models_INT_texreg.tex"),
  custom.model.names = c("No pop\nno GDP", "No pop\nw/ GDP", "No pop \nw/CF GDP",
                         "W/ pop\nno GDP", "Full model", "Full model w/CF GDP"),
  custom.coef.map = custom_coef_names,
  caption = "Negative Binomial Models: Out-migration (2009-2022)",
  label = "tab:out_migration",
  stars = c(0.001, 0.01, 0.05),
  custom.note = "Standard errors in parentheses. All models include offset for Log(Population). %stars.",
  fontsize = "small"
)

# ======================================================
# IN-migration table with custom labels
# ======================================================
texreg(
  list(count_nopop_nogdp_INT_interac_oa_in,
       count_nopop_wgdp_INT_interac_oa_in,
       count_nopop_wgdpcf_INT_interac_oa_in,
       count_wpop_nogdp_INT_interac_oa_in,
       count_wpop_wgdp_INT_interac_oa_in,
       count_wpop_wgdpcf_INT_interac_oa_in),
  file = write_path("updated-data/oneyearbackfill/negbin_inmigration_models_INT_texreg.tex"),
  custom.model.names = c("No pop\nno GDP", "No pop\nw/ GDP", "No pop \nw/CF GDP",
                         "W/ pop\nno GDP", "Full model", "Full model w/CF GDP"),
  custom.coef.map = custom_coef_names,
  caption = "Negative Binomial Models: In-migration (2009-2022)",
  label = "tab:in_migration",
  stars = c(0.001, 0.01, 0.05),
  custom.note = "Standard errors in parentheses. All models include offset for Log(Population). %stars.",
  fontsize = "small"
)

# ======================================================
# Combined table with custom labels
# ======================================================
texreg(
  list(count_nopop_nogdp_INT_interac_oa_in,
       count_nopop_wgdp_INT_interac_oa_in,
       count_nopop_wgdpcf_INT_interac_oa_in,
       count_wpop_nogdp_INT_interac_oa_in,
       count_wpop_wgdp_INT_interac_oa_in,
       count_wpop_wgdpcf_INT_interac_oa_in,
       count_nopop_nogdp_INT_interac_oa_out,
       count_nopop_wgdp_INT_interac_oa_out,
       count_nopop_wgdpcf_INT_interac_oa_out,
       count_wpop_nogdp_INT_interac_oa_out,
       count_wpop_wgdp_INT_interac_oa_out,
       count_wpop_wgdpcf_INT_interac_oa_out),
  file = write_path("updated-data/oneyearbackfill/negbin_combined_models_INT_texreg.tex"),
  custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)"),
  custom.coef.map = custom_coef_names,
  caption = "Negative Binomial Models: International In-migration and Out-migration (2009-2022)",
  label = "tab:combined_migration",
  stars = c(0.001, 0.01, 0.05),
  custom.note = "Standard errors in parentheses. All models include offset for Log(Population of scholars). Models (1)-(6): In-migration. Models (7)-(12): Out-migration. %stars.",
  fontsize = "tiny"
)










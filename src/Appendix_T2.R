# Spearman rank correlations between migration datasets
# with ukr_oa lagged by 2 years (OA data appears 2 years after SC data)

# Run after: 01_Data-wrangling.R (reuses its data frames in this session)

library(dplyr)
library(stargazer)

#### Resolve output directory ####
output_dir <- Sys.getenv("OUTPUT_DIR", unset = NA)
if (is.na(output_dir) || !nzchar(output_dir)) output_dir <- here::here("output")
write_path <- function(...) {
  p <- file.path(output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

# Create datasets -- must have same regions and years #
#ukr_oa_2009.2020.cut <- ukr_oa_2009.2022.cut %>% 
#  mutate(year = as.numeric(as.character(year))) %>% 
#  filter(year < 2021)

ukr_2020oa_sp <- ukr_oa_2009.2020 %>% 
  group_by(name) %>% 
  mutate(n_years = sum(n_distinct(year)), .groups = "drop") %>% 
  filter(n_years > 6)

# --- Lag ukr_oa by 2 years BEFORE joining ---
# By subtracting 2 from the OA year, we align OA(t) with SC(t-1),
# i.e. the SC signal that preceded the OA observation.

ukr_oa_lagged <- ukr_2020oa_sp %>%
  mutate(year = year - 2)

ukr_2020sc_sp <- ukr_sc_2009.2020 %>% 
  group_by(name) %>% 
  mutate(n_years = sum(n_distinct(year)), .groups = "drop") %>% 
  filter(n_years > 6)

# Join on name + (lagged) year
#ukr_joined_df <- inner_join(ukr_sc_2009.2020, ukr_oa_2009.2020,
#                            by = c("name", "year"))

ukr_joined_df <- inner_join(ukr_oa_lagged, ukr_2020sc_sp,
                            by = c("name", "year"))
# --- Correlation function (unchanged) ---
compute_regional_correlations <- function(joined_data,
                                          flow_type = "INT") {
  
  var_out_oa <- paste0("out_y_flow_", flow_type, ".y")
  var_out_sc <- paste0("out_y_flow_", flow_type, ".x")
  var_in_oa  <- paste0("in_y_flow_",  flow_type, ".y")
  var_in_sc  <- paste0("in_y_flow_",  flow_type, ".x")
  
  joined_data %>%
    group_by(name) %>%
    summarise(
      n_obs = sum(
        complete.cases(
          .data[[var_out_oa]],
          .data[[var_out_sc]]
        )
      ),
      
      spearman_out = cor(
        .data[[var_out_oa]],
        .data[[var_out_sc]],
        method = "spearman",
        use = "complete.obs"
      ),
      
      spearman_in = cor(
        .data[[var_in_oa]],
        .data[[var_in_sc]],
        method = "spearman",
        use = "complete.obs"
      ),
      
      .groups = "drop"
    )
}

correlations_IN  <- compute_regional_correlations(ukr_joined_df, flow_type = "IN")
correlations_INT <- compute_regional_correlations(ukr_joined_df, flow_type = "INT")

# --- Build combined results table ---
region_labels <- data.frame(
  Region = c("Cherkasy", "Chernivtsi", "Dnipropetrovs'k", "Donets'k",
             "Kharkiv", "Kherson", "Kiev City", "L'viv", "Luhans'k", 
             "Mykolayiv", "Odessa", "Poltava", "Rivne", "Sumy", "Ternopil'", 
             "Vinnytsya", "Volyn", "Zaporizhzhya"),
  Label = c("Cherkasy", "Chernivtsi", "Dnipropetrovsk", "Donetsk", 
            "Kharkiv", "Kherson", "Kyiv City", "Lviv", "Luhansk", 
            "Mykolaiv", "Odesa", "Poltava", "Rivne", "Sumy", "Ternopil",  
            "Vinnytsia", "Volyn", "Zaporizhzhia")
)


combined_results <- correlations_IN %>%
  select(name, n_obs, spearman_out, spearman_in) %>%
  rename(
    Region     = name,
    N          = n_obs,
    `Out (IN)` = spearman_out,
    `In (IN)`  = spearman_in
  ) %>%
  left_join(
    correlations_INT %>%
      select(name, spearman_out, spearman_in) %>%
      rename(
        Region      = name,
        `Out (INT)` = spearman_out,
        `In (INT)`  = spearman_in
      ),
    by = "Region"
  ) %>%
  left_join(region_labels, by = "Region") %>%
  select(Label, N, `Out (IN)`, `In (IN)`, `Out (INT)`, `In (INT)`) %>%
  arrange(Label) %>%
  mutate(across(where(is.numeric) & !N, ~round(., 2))) %>%
  rename(Region = Label)

# --- Stargazer output ---
stargazer(combined_results,
          type    = "latex",
          summary = FALSE,
          rownames = FALSE,
          title = "Spearman Rank Correlations Between OA (Lagged 2 Years) and SC Migration Data by Region",
          label = "tab:spearman_correlations_lagged",
          digits = 2,
          digits.extra = 0,
          column.sep.width = "3pt",
          font.size = "small",
          header = FALSE,
          out = write_path("updated-data/twoyearbackfill/upd_spearman_twoyrbf_2l.tex"),
          notes = c("Note: Correlations computed across years 2009--2020. Only regions who have non-missing observations for over half of the observation period are considered.",
                    "NA spearman correlations reflect cases where a region has zero variance on a particular migration outcome.",
                    "IN = Internal migration flows; INT = International migration flows.",
                    "Out = Outmigration; In = Inmigration."),
          notes.align = "l")

#### Model specifications ####

# Run after: 01_Data-wrangling.R (reuses its data frames in this session)

# Generalized and linear mixed models package of choice #
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
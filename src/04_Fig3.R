#### Figure 3: temporal view with various migration measures, by year ####

# Run after: 01_Data-wrangling.R (reuses its data frames in this session)

# facet_grid2() below is from ggh4x; loaded here (rather than only via
# run_pipeline.R) so this script also runs standalone, e.g. opened directly
# in RStudio.
library(ggh4x)

#### Resolve output directory ####
output_dir <- Sys.getenv("OUTPUT_DIR", unset = NA)
if (is.na(output_dir) || !nzchar(output_dir)) output_dir <- here::here("output")
write_path <- function(...) {
  p <- file.path(output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

## In-migration ##

# Compute yearly internal in-migration by macro-regions, OpenAlex

ukr_agg_inmig_IN2_oa <- ukr_oa_2009.2022 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(inflow_rate_IN, na.rm = T), .groups = "drop")

# Compute yearly international in-migration by macro-regions, OpenAlex

ukr_agg_inmig_INT2_oa <- ukr_oa_2009.2022 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(inflow_rate_INT, na.rm = T), .groups = "drop")

# Compute yearly internal in-migration by macro-regions, Scopus

ukr_agg_inmig_IN2_sc <- ukr_sc_2009.2020 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(inflow_rate_IN, na.rm = T), .groups = "drop")

# Compute yearly international in-migration by macro-regions, Scopus

ukr_agg_inmig_INT2_sc <- ukr_sc_2009.2020 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(inflow_rate_INT, na.rm = T), .groups = "drop")

## Out-migration ##

# Compute yearly internal out-migration by macro-regions, OpenAlex

ukr_agg_outmig_IN2_oa <- ukr_oa_2009.2022 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(outflow_rate_IN, na.rm = T), .groups = "drop")

# Compute yearly international out-migration by macro-regions, OpenAlex

ukr_agg_outmig_INT2_oa <- ukr_oa_2009.2022 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(outflow_rate_INT, na.rm = T), .groups = "drop")

# Compute yearly internal out-migration by macro-regions, Scopus

ukr_agg_outmig_IN2_sc <- ukr_sc_2009.2020 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(outflow_rate_IN, na.rm = T), .groups = "drop")

# Compute yearly international out-migration by macro-regions, Scopus

ukr_agg_outmig_INT2_sc <- ukr_sc_2009.2020 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(outflow_rate_INT, na.rm = T), .groups = "drop")

# Compute yearly internal in-migration by macro-regions, OpenAlex

## NMR ## 

# Compute yearly internal nmr by macro-regions, OpenAlex

ukr_agg_nmr_IN2_oa <- ukr_oa_2009.2022 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(nmr_IN, na.rm = T), .groups = "drop")

# Compute yearly international nmr by macro-regions, OpenAlex

ukr_agg_nmr_INT2_oa <- ukr_oa_2009.2022 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(nmr_INT, na.rm = T), .groups = "drop")

# Compute yearly internal nmr by macro-regions, Scopus

ukr_agg_nmr_IN2_sc <- ukr_sc_2009.2020 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(nmr_IN, na.rm = T), .groups = "drop")

# Compute yearly international nmr by macro-regions, Scopus

ukr_agg_nmr_INT2_sc <- ukr_sc_2009.2020 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(nmr_INT, na.rm = T), .groups = "drop")

## MEI ## 

# Compute yearly internal mei by macro-regions, OpenAlex

ukr_agg_mei_IN2_oa <- ukr_oa_2009.2022 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(mei_IN, na.rm = T), .groups = "drop")

# Compute yearly international mei by macro-regions, OpenAlex

ukr_agg_mei_INT2_oa <- ukr_oa_2009.2022 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(mei_INT, na.rm = T), .groups = "drop")

# Compute yearly internal mei by macro-regions, Scopus

ukr_agg_mei_IN2_sc <- ukr_sc_2009.2020 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(mei_IN, na.rm = T), .groups = "drop")

# Compute yearly international mei by macro-regions, Scopus

ukr_agg_mei_INT2_sc <- ukr_sc_2009.2020 %>% 
  group_by(year, gdl_area) %>% 
  summarise(value = mean(mei_INT, na.rm = T), .groups = "drop")


# Combine datasets into long format
ukr_combined_measures <- bind_rows(
  ukr_agg_nmr_IN2_oa %>% mutate(migration_measure = "NMR - Internal", dataset = "OpenAlex"),
  ukr_agg_nmr_INT2_oa %>% mutate(migration_measure = "NMR - International", dataset = "OpenAlex"),
  ukr_agg_nmr_IN2_sc %>% mutate(migration_measure = "NMR - Internal", dataset = "Scopus"),
  ukr_agg_nmr_INT2_sc %>% mutate(migration_measure = "NMR - International", dataset = "Scopus"),
  ukr_agg_inmig_IN2_oa %>% mutate(migration_measure = "IMR - Internal", dataset = "OpenAlex"),
  ukr_agg_inmig_INT2_oa %>% mutate(migration_measure = "IMR - International", dataset = "OpenAlex"),
  ukr_agg_inmig_IN2_sc %>% mutate(migration_measure = "IMR - Internal", dataset = "Scopus"),
  ukr_agg_inmig_INT2_sc %>% mutate(migration_measure = "IMR - International", dataset = "Scopus"),
  ukr_agg_outmig_IN2_oa %>% mutate(migration_measure = "OMR - Internal", dataset = "OpenAlex"),
  ukr_agg_outmig_INT2_oa %>% mutate(migration_measure = "OMR - International", dataset = "OpenAlex"),
  ukr_agg_outmig_IN2_sc %>% mutate(migration_measure = "OMR - Internal", dataset = "Scopus"),
  ukr_agg_outmig_INT2_sc %>% mutate(migration_measure = "OMR - International", dataset = "Scopus"),
  ukr_agg_mei_IN2_oa %>% mutate(migration_measure = "MEI - Internal", dataset = "OpenAlex"),
  ukr_agg_mei_INT2_oa %>% mutate(migration_measure = "MEI - International", dataset = "OpenAlex"),
  ukr_agg_mei_IN2_sc %>% mutate(migration_measure = "MEI - Internal", dataset = "Scopus"),
  ukr_agg_mei_INT2_sc %>% mutate(migration_measure = "MEI - International", dataset = "Scopus"))

# Define lineplot function
create_migration_lineplot <- function(data) {
  ggplot(data, aes(x = year, y = value, color = gdl_area, linetype = migration_type)) +
    geom_line(linewidth = 1) + 
    facet_grid(rows = vars(migration_measure_type), cols = vars(dataset), scales = "free_y") +
    scale_linetype_manual(values = c("Internal" = "dotted", "International" = "solid")) +  # Ensure these match exactly
    scale_color_brewer(palette = "Set1") + 
    theme_minimal() +
    labs(title = "Migration Measures Over Time by Region",
         x = "Year",
         y = "Value",
         color = "Region",
         linetype = "Migration Type") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
}

# Modify dataset to ensure migration_type is properly set
ukr_combined_measures <- ukr_combined_measures %>%
  mutate(migration_measure_type = case_when(
    grepl("IMR", migration_measure) ~ "In-Migration Rate (IMR)",
    grepl("OMR", migration_measure) ~ "Out-Migration Rate (OMR)",
    grepl("NMR", migration_measure) ~ "Net Migration Rate (NMR)",
    grepl("MEI", migration_measure) ~ "Migration Effectiveness Index (MEI)"
  ),
  migration_type = case_when(
    grepl("Internal", migration_measure) ~ "Internal",
    grepl("International", migration_measure) ~ "International"
  ))

# Print unique values to debug if needed
print(unique(ukr_combined_measures$migration_type))  # Should print "Internal" and "International"

# Create the plot
migration_lineplot <- create_migration_lineplot(ukr_combined_measures)

# Print the plot
print(migration_lineplot)

################################################################################################

# Combine OpenAlex datasets into long format
ukr_combined_measures_oa <- bind_rows(
  ukr_agg_nmr_IN2_oa %>% mutate(migration_measure = "NMR - Internal", dataset = "OpenAlex"),
  ukr_agg_nmr_INT2_oa %>% mutate(migration_measure = "NMR - International", dataset = "OpenAlex"),
  ukr_agg_inmig_IN2_oa %>% mutate(migration_measure = "IMR - Internal", dataset = "OpenAlex"),
  ukr_agg_inmig_INT2_oa %>% mutate(migration_measure = "IMR - International", dataset = "OpenAlex"),
  ukr_agg_outmig_IN2_oa %>% mutate(migration_measure = "OMR - Internal", dataset = "OpenAlex"),
  ukr_agg_outmig_INT2_oa %>% mutate(migration_measure = "OMR - International", dataset = "OpenAlex"),
  ukr_agg_mei_IN2_oa %>% mutate(migration_measure = "MEI - Internal", dataset = "OpenAlex"),
  ukr_agg_mei_INT2_oa %>% mutate(migration_measure = "MEI - International", dataset = "OpenAlex"),
)

# Modify dataset to ensure migration_type is properly set
ukr_combined_measures_oa <- ukr_combined_measures_oa %>%
  mutate(migration_measure_type = case_when(
    grepl("IMR doubling time", migration_measure) ~ "IMR doubling time",
    grepl("OMR doubling time", migration_measure) ~ "OMR doubling time",
    grepl("IMR", migration_measure) ~ "In-Migration Rate (IMR)",
    grepl("OMR", migration_measure) ~ "Out-Migration Rate (OMR)",
    grepl("NMR", migration_measure) ~ "Net Migration Rate (NMR)",
    grepl("MEI", migration_measure) ~ "Migration Effectiveness Index (MEI)"
  ),
  migration_type = case_when(
    grepl("Internal", migration_measure) ~ "Internal",
    grepl("International", migration_measure) ~ "International"
  ))

# Print unique values to debug if needed
print(unique(ukr_combined_measures_oa$migration_measure_type))
print(unique(ukr_combined_measures_oa$migration_type))  # Should print "Internal" and "International"

# Get the order correct

ukr_combined_measures_oa <- ukr_combined_measures_oa %>%
  mutate(migration_measure_type = factor(migration_measure_type, 
                                         levels = c("In-Migration Rate (IMR)",
                                                    "Out-Migration Rate (OMR)",
                                                    "Net Migration Rate (NMR)", 
                                                    "Migration Effectiveness Index (MEI)")))

# Create dataset to help make y-axis scales the same on reach row:

df2_1 <- data.frame(year = c(2009:2022),
                    gdl_area = rep("Northern Ukraine", 14),
                    migration_measure_type = rep("In-Migration Rate (IMR)", 14),
                    migration_type = rep("Internal", 14),
                    value = rep(50, 14))

df2_2 <- data.frame(year = c(2009:2022),
                    gdl_area = rep("Northern Ukraine", 14),
                    migration_measure_type = rep("In-Migration Rate (IMR)", 14),
                    migration_type = rep("International", 14),
                    value = rep(50, 14))

df2_3 <- data.frame(year = c(2009:2022),
                    gdl_area = rep("Northern Ukraine", 14),
                    migration_measure_type = rep("Out-Migration Rate (OMR)", 14),
                    migration_type = rep("Internal", 14),
                    value = rep(50, 14))

df2_4 <- data.frame(year = c(2009:2022),
                    gdl_area = rep("Northern Ukraine", 14),
                    migration_measure_type = rep("Out-Migration Rate (OMR)", 14),
                    migration_type = rep("International", 14),
                    value = rep(50, 14))

df2_9 <- data.frame(year = c(2009:2022),
                    gdl_area = rep("Northern Ukraine", 14),
                    migration_measure_type = rep("Net Migration Rate (NMR)", 14),
                    migration_type = rep("Internal", 14),
                    value = rep(30, 14))

df2_10 <- data.frame(year = c(2009:2022),
                     gdl_area = rep("Northern Ukraine", 14),
                     migration_measure_type = rep("Net Migration Rate (NMR)", 14),
                     migration_type = rep("International", 14),
                     value = rep(30, 14))

df2_11 <- data.frame(year = c(2009:2022),
                     gdl_area = rep("Northern Ukraine", 14),
                     migration_measure_type = rep("Migration Effectiveness Index (MEI)", 14),
                     migration_type = rep("Internal", 14),
                     value = rep(50, 14))

df2_12 <- data.frame(year = c(2009:2022),
                     gdl_area = rep("Northern Ukraine", 14),
                     migration_measure_type = rep("Migration Effectiveness Index (MEI)", 14),
                     migration_type = rep("International", 14),
                     value = rep(50, 14))

df2 <- bind_rows(df2_1, df2_2, df2_3, df2_4, 
                 df2_9, df2_10, 
                 df2_11, df2_12)

df2 <- df2 %>%
  mutate(migration_measure_type = factor(migration_measure_type, 
                                         levels = c("In-Migration Rate (IMR)",
                                                    "Out-Migration Rate (OMR)",
                                                    "Net Migration Rate (NMR)", 
                                                    "Migration Effectiveness Index (MEI)")))


ukr_combined_measures_oa$gdl_area <- factor(ukr_combined_measures_oa$gdl_area)
levels(ukr_combined_measures_oa$gdl_area) <- gsub(" Ukraine", "", levels(ukr_combined_measures_oa$gdl_area))


create_migration_lineplot_oa <- function(data) {
  ggplot(data, aes(x = year, y = value, color = gdl_area, linetype = migration_type)) +
    geom_smooth(method = "loess", se = FALSE, size = 1) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "red", size = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) + 
    facet_wrap(~migration_measure_type, ncol = 2, scales = "free_y") +
    scale_x_continuous(breaks = seq(min(ukr_combined_measures_oa$year),
                                    max(ukr_combined_measures_oa$year), 1)) +
    scale_linetype_manual(
      values = c("Internal" = "dashed", "International" = "solid"),
      guide = guide_legend(
        order = 1,
        nrow = 1,  # one row for clarity
        override.aes = list(size = 2.5, color = "black"),  # thicker lines in the legend
        keywidth = unit(1, "cm"),         # wider key
        keyheight = unit(0.5, "cm")         # taller key if needed
      )
    ) +  
    scale_color_manual(values = c(
      "Central" = "#F8766D",
      "Eastern" = "#B79F00",
      "Northern" = "#00BA38",
      "Southern" = "#00BFC4",
      "Western" = "#F564E3"
    )) + 
    theme_minimal() +
    labs(title = "Migration Measures Over Time by Region, OpenAlex",
         x = "Year",
         y = "Value",
         color = "Region",
         linetype = "Migration Type") +
    theme(legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          aspect.ratio = 0.8) +
    guides(
      color = guide_legend(order = 2, nrow = 2)
    )
}


# Create and print the plot for OpenAlex only
migration_lineplot_oa <- create_migration_lineplot_oa(ukr_combined_measures_oa)
print(migration_lineplot_oa)

#save_plot(migration_lineplot_oa,
#          write_path("twoyearbackfill/fig3_migmeasures_OA_gdl_updated.pdf"),
#          height = 12,
#          width = 10)

create_migration_lineplot_oa_v1 <- function(data) {
  ggplot(data, aes(x = year, y = value, color = gdl_area, linetype = migration_type)) +
    geom_smooth(method = "loess", se = FALSE, size = 1) +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "red", size = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) + 
    facet_grid(migration_type ~ migration_measure_type) +  # removed scales = "free_y"
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), 2)) +
    scale_y_continuous(breaks = seq(-20, 50, 10)) +
    scale_linetype_manual(
      values = c("Internal" = "dashed", "International" = "solid")
    ) +
    scale_color_manual(values = c(
      "Central" = "#F8766D",
      "Eastern" = "#B79F00",
      "Northern" = "#00BA38",
      "Southern" = "#00BFC4",
      "Western" = "#F564E3"
    )) + 
    theme_minimal() +
    labs(title = "Migration Measures Over Time by Region, OpenAlex",
         x = "Year",
         y = "Value",
         color = "Region",
         linetype = "Migration Type") +
    theme(legend.position = "bottom",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          strip.text = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          aspect.ratio = 1) +
    guides(
      color = guide_legend(order = 1, nrow = 1),
      linetype = guide_legend(order = 2, nrow = 1)
    )
}

migration_lineplot_oa_v1 <- create_migration_lineplot_oa_v1(ukr_combined_measures_oa)
print(migration_lineplot_oa_v1)

#save_plot(migration_lineplot_oa_v1,
#          write_path("twoyearbackfill/fig3_migmeasures_OA_gdl_updated_v1.pdf"),
#          width = 11,
#          height = 8)


#### Now quantify uncertainty using supersmoother function ####

create_migration_lineplot_oa_v3_supsmu_ci <- function(data, n_boot = 1000, seed = 42) {
  
  set.seed(seed)
  
  smoothed_data <- data %>% 
    dplyr::group_by(gdl_area, migration_type, migration_measure_type) %>% 
    dplyr::group_modify(~ {
      
      years <- .x$year
      vals  <- .x$value
      
      # Point estimate on original data
      sm_fit <- stats::supsmu(x = years, y = vals)
      fitted  <- approx(sm_fit$x, sm_fit$y, xout = years)$y
      
      # Bootstrap: resample rows with replacement, smooth, interpolate back
      boot_mat <- replicate(n_boot, {
        idx    <- sample(nrow(.x), replace = TRUE)
        sm_b   <- stats::supsmu(x = years[idx], y = vals[idx])
        approx(sm_b$x, sm_b$y, xout = years, rule = 2)$y
      })
      # boot_mat is (n_years x n_boot); take row-wise percentiles
      ymin <- apply(boot_mat, 1, quantile, probs = 0.025, na.rm = TRUE)
      ymax <- apply(boot_mat, 1, quantile, probs = 0.975, na.rm = TRUE)
      
      dplyr::tibble(year = years, value = fitted, ymin = ymin, ymax = ymax)
    }) %>% 
    dplyr::ungroup()
  
  ggplot(smoothed_data, aes(x = year, color = gdl_area,
                            fill  = gdl_area, linetype = migration_type)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax),
                alpha = 0.15, color = NA) +
    geom_line(aes(y = value), size = 1) +
    geom_vline(xintercept = 2014, linetype = "dashed",
               color = "red", size = 0.5) +
    geom_hline(yintercept = 0, linetype = "solid",
               color = "black", size = 0.5) +
    facet_grid2(migration_type ~ migration_measure_type,
                scales = "free_y",
                independent = "y",
                axes = "all") +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), 2)) +
    scale_linetype_manual(
      values = c("Internal" = "dashed", "International" = "solid")
    ) +
    scale_color_manual(values = c(
      "Central"  = "#F8766D", "Eastern"  = "#B79F00",
      "Northern" = "#00BA38", "Southern" = "#00BFC4",
      "Western"  = "#F564E3"
    )) +
    scale_fill_manual(values = c(
      "Central"  = "#F8766D", "Eastern"  = "#B79F00",
      "Northern" = "#00BA38", "Southern" = "#00BFC4",
      "Western"  = "#F564E3"
    )) +
    theme_minimal() +
    labs(title    = "Migration Measures Over Time by Region, OpenAlex",
         x        = "Year", y = "Value",
         color    = "Region", fill = "Region",
         linetype = "Migration Type") +
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
      axis.text.x = element_text(
        family = "Times New Roman", size = 8, lineheight = 9/8,
        angle = 90, hjust = 1
      ),
      # Facet strip labels: axes category (8pt)
      strip.text = element_text(
        family = "Times New Roman", size = 8, face = "bold",
        lineheight = 9/8
      ),
      # Legend title: axes category (8pt)
      legend.title = element_text(
        family = "Times New Roman", size = 8, lineheight = 9/8
      ),
      # Legend text: labels category (6pt)
      legend.text = element_text(
        family = "Times New Roman", size = 6, lineheight = 7/6
      ),
      legend.position = "bottom",
      legend.key.size = unit(0.5, "cm"),
      plot.margin = margin(1, 1, 1, 1, "mm")
    ) +
    guides(
      color    = guide_legend(order = 1, nrow = 1),
      fill     = guide_legend(order = 1, nrow = 1),
      linetype = guide_legend(order = 2, nrow = 1)
    )
}

migration_lineplot_oa_v3 <- create_migration_lineplot_oa_v3_supsmu_ci(ukr_combined_measures_oa)
print(migration_lineplot_oa_v3)

save_plot(migration_lineplot_oa_v3,
          write_path("twoyearbackfill/fig3_migmeasures_OA_gdl_v3_supsmu.pdf"),
          width = 11,
          height = 8)


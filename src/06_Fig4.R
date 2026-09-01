#### Figure 4 #####

# Run after: 01_Data-wrangling.R, 05_Modelspecs.R (reuses their objects in this session)

#### Resolve output directory ####
output_dir <- Sys.getenv("OUTPUT_DIR", unset = NA)
if (is.na(output_dir) || !nzchar(output_dir)) output_dir <- here::here("output")
write_path <- function(...) {
  p <- file.path(output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

# Libraries

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

save_plot(figure_combined,
          write_path("updated-data/twoyearbackfill/nbins-faceted-IN.pdf"),
          width = 12,
          height = 12)

save_plot(figure_combined,
          write_path("updated-data/twoyearbackfill/nbins-faceted-IN.svg"),
          width = 12, height = 12)


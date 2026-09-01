#### Appendix Figure 1 ####

# Run after: 01_Data-wrangling.R, 02_Fig1.R (reuses their objects in this session)

#### Resolve output directory ####
output_dir <- Sys.getenv("OUTPUT_DIR", unset = NA)
if (is.na(output_dir) || !nzchar(output_dir)) output_dir <- here::here("output")
write_path <- function(...) {
  p <- file.path(output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

# Run 02_Fig1.R before running this

## Create Appendix Figure 1 ##

fig1_gdl <- ggplot(gdl_pops_combined, aes(x = year, y = pop, color = gdl_area, group = gdl_area)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = custom_colors) +
  facet_wrap(vars(type), labeller = label_wrap_gen(width = 15)) +
  labs(
    title = "Population of Ukrainian scholars, total and by region",  
    x = "Year",
    y = "Population of scholars",
    color = "Ukrainian macro-regions"
  ) +
  scale_x_continuous(breaks = seq(min(gdl_pops_combined$year), max(gdl_pops_combined$year), 1)) +
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
      lineheight = 9/8, margin = margin(t = 3, b = 3, unit = "pt")
    ),
    # Legend title: axes category (8pt)
    legend.title = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8
    ),
    # Legend text: labels category (6pt)
    legend.text = element_text(
      family = "Times New Roman", size = 6, lineheight = 7/6
    ),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.key.size = unit(0.1, "cm")
  ) +
  guides(color = guide_legend(ncol = 1))

save_plot(fig1_gdl,
          write_path("updated-data/twoyearbackfill/fig1_sampledesc_gdl.svg"))
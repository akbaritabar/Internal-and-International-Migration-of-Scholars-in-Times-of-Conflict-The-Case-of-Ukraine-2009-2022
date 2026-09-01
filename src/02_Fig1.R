#### Figure 1: Population of scholars in Ukraine ####

# Run after: 01_Data-wrangling.R (reuses its data frames in this session)

#### Resolve output directory ####
output_dir <- Sys.getenv("OUTPUT_DIR", unset = NA)
if (is.na(output_dir) || !nzchar(output_dir)) output_dir <- here::here("output")
write_path <- function(...) {
  p <- file.path(output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

## Data wrangling ##

# Total population of Ukrainian scholars by year OpenAlex #

totalpop_oa <- ukr_oa_2009.2022 %>% 
  group_by(year) %>% 
  summarise(pop = sum(y_pop, na.rm = T), .groups = "drop") %>% 
  mutate(type = "OpenAlex") 

# Total population of Ukrainian scholars by year Scopus #

totalpop_sc <- ukr_sc_2009.2020 %>% 
  group_by(year) %>% 
  summarise(pop = sum(y_pop, na.rm = T), .groups = "drop") %>% 
  mutate(type = "Scopus") 

# Combined

totalpop <- bind_rows(totalpop_oa, totalpop_sc) %>% 
  mutate(gdl_area = "Ukraine (total)")

# Combined (to use with Kyiv separated)

totalpop2 <- bind_rows(totalpop_oa, totalpop_sc) %>% 
  mutate(gdl_kyiv = "Ukraine (total)")

# Check labels for regions #
ukr_oa_2009.2022 %>% 
  count(cardinal_area2)

ukr_sc_2009.2020 %>% 
  count(cardinal_area2)

ukr_oa_2009.2022 %>% 
  count(gdl_area)

ukr_sc_2009.2020 %>% 
  count(gdl_area)

# Total population of Ukrainian scholars by macro-region and year# 

# GDL macro-regions

gdl_pops_oa <- ukr_oa_2009.2022 %>% 
  group_by(year, gdl_area) %>% 
  summarise(pop = sum(y_pop, na.rm = T), .groups = "drop") %>% 
  mutate(type = "OpenAlex")

gdl_pops_sc <- ukr_sc_2009.2020 %>% 
  group_by(year, gdl_area) %>% 
  summarise(pop = sum(y_pop, na.rm = T), .groups = "drop") %>% 
  mutate(type = "Scopus")

gdl_pops_combined <- bind_rows(totalpop, gdl_pops_oa, gdl_pops_sc)

# Label #
gdl_pops_combined <- gdl_pops_combined %>% 
  mutate(gdl_area = factor(gdl_area, 
                           levels = c("Ukraine (total)",
                                      "Northern Ukraine",
                                      "Southern Ukraine",
                                      "Eastern Ukraine",
                                      "Western Ukraine",
                                      "Central Ukraine")))


"#B79F00"
"#619CFF"
"#00BA38"
"#F564E3"
"#00BFC4"
"#F8766D"


# Create color palette #

gdl_colors <- scales::hue_pal()(length(unique(gdl_pops_combined$gdl_area)))
names(gdl_colors) <- unique(gdl_pops_combined$gdl_area)


# Reorder the colors manually (put "Ukraine (total)" last)
desired_order <- c("Central Ukraine", "Eastern Ukraine", "Northern Ukraine", 
                   "Southern Ukraine", "Western Ukraine", "Ukraine (total)")

# Reorder colors
gdl_colors <- gdl_colors[desired_order]



## Figure 1: population of scholars by GDL macro-region (OpenAlex) ##

# A 4-region + total variant of this figure (cardinal_area2, with an
# "Eastern Ukraine + Crimea" category) used to live here, built from
# centrukrpop_oa/westukrpop_oa/southukrpop_oa/eastcrimukrpop_oa. Those
# objects were never defined anywhere in this codebase and cardinal_area2
# is otherwise unused, so the block was removed -- see Errors_found.md.

# Combine GDL data for OpenAlex only
gdl_pops_oa_combined <- bind_rows(totalpop_oa %>% mutate(gdl_area = "Ukraine (total)"), 
                                  gdl_pops_oa)

gdl_pops_oa_combined <- gdl_pops_oa_combined %>% 
  mutate(gdl_area = factor(gdl_area, 
                           levels = c("Ukraine (total)",
                                      "Northern Ukraine",
                                      "Southern Ukraine",
                                      "Eastern Ukraine",
                                      "Western Ukraine",
                                      "Central Ukraine")))

custom_colors <- c(
  "Ukraine (total)" = "#619CFF",
  "Central Ukraine" = "#F8766D",
  "Eastern Ukraine" = "#B79F00",
  "Northern Ukraine" = "#00BA38",
  "Southern Ukraine" = "#00BFC4",
  "Western Ukraine" = "#F564E3"
)

# Create figure for GDL regions (OpenAlex only)
fig1_gdl_oa <- ggplot(gdl_pops_oa_combined, aes(x = year, y = pop, color = gdl_area, group = gdl_area)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Population of Ukrainian scholars, total and by region",
    x = "Year",
    y = "Population of scholars",
    color = "Ukrainian macro-regions"
  ) +
  scale_x_continuous(breaks = seq(2009, 2022, 1)) +
  theme(
    # Title: 9pt Times New Roman, 10pt leading
    plot.title = element_text(
      family = "Times New Roman", size = 9, face = "bold",
      hjust = 0.5, lineheight = 10/9
    ),
    # Axes: 8pt Times New Roman, 9pt leading
    axis.title = element_text(family = "Times New Roman", size = 8),
    axis.text.x = element_text(
      family = "Times New Roman", size = 8,
      angle = 90, hjust = 1, lineheight = 9/8
    ),
    axis.text.y = element_text(family = "Times New Roman", size = 8),
    # Legend (labels): 6pt Times New Roman, 7pt leading
    legend.title = element_text(
      family = "Times New Roman", size = 6, lineheight = 7/6
    ),
    legend.text = element_text(
      family = "Times New Roman", size = 6, lineheight = 7/6
    ),
    legend.position = c(0.25, 1),
    legend.justification = c(1, 1),
    legend.key.size = unit(0.1, "cm"),
    plot.margin = margin(1, 1, 1, 1, "mm")
  ) +
  guides(color = guide_legend(ncol = 1))


save_plot(fig1_gdl_oa,
          write_path("twoyearbackfill/fig1_gdl_openalex_only.svg"))


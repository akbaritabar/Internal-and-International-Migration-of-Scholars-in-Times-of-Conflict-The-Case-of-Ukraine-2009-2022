## Figure 2: Spatial view. Net migration rate map, 3 years ##

# Run after: 01_Data-wrangling.R (reuses its data frames in this session)

#### Resolve output directory ####
output_dir <- Sys.getenv("OUTPUT_DIR", unset = NA)
if (is.na(output_dir) || !nzchar(output_dir)) output_dir <- here::here("output")
write_path <- function(...) {
  p <- file.path(output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

### Internal netmigration ###

### Define functions ###

# Define the binning function
bin_nmr_rate <- function(data, variable, new_var_name) {
  data[[new_var_name]] <- cut(
    data[[variable]],
    breaks = c(-1000, -50, -10, -0.00000001, 0, 10, 50, 1000),
    labels = c("< -100", "[-100, -10)", "[-10, 0)", "0", "[0, 10)", "(10, 100]", "> 100")
  )
  return(data)
}

# List of datasets and their corresponding variables to bin (both internal and international)
datasets_nmr <- list(
  ukr_sc_2009.2020 = list(data = ukr_sc_2009.2020, vars = c("nmr_IN" = "nmr_IN_binned", "nmr_INT" = "nmr_INT_binned")),
  merged_data2_sc = list(data = merged_data2_sc, vars = c("s_nmr_IN" = "s_nmr_IN_binned", "s_nmr_INT" = "s_nmr_INT_binned")),
  merged_data3_sc = list(data = merged_data3_sc, vars = c("s_nmr_IN" = "s_nmr_IN_binned", "s_nmr_INT" = "s_nmr_INT_binned")),
  merged_data4_sc = list(data = merged_data4_sc, vars = c("s_nmr_IN" = "s_nmr_IN_binned", "s_nmr_INT" = "s_nmr_INT_binned")),
  
  ukr_oa_2009.2022 = list(data = ukr_oa_2009.2022, vars = c("nmr_IN" = "nmr_IN_binned", "nmr_INT" = "nmr_INT_binned")),
  merged_data2_oa = list(data = merged_data2_oa, vars = c("s_nmr_IN" = "s_nmr_IN_binned", "s_nmr_INT" = "s_nmr_INT_binned")),
  merged_data3_oa = list(data = merged_data3_oa, vars = c("s_nmr_IN" = "s_nmr_IN_binned", "s_nmr_INT" = "s_nmr_INT_binned")),
  merged_data4_oa = list(data = merged_data4_oa, vars = c("s_nmr_IN" = "s_nmr_IN_binned", "s_nmr_INT" = "s_nmr_INT_binned"))
)

# Loop through each dataset and apply the binning function for both internal and international migration rates
for (name in names(datasets_nmr)) {
  for (var in names(datasets_nmr[[name]]$vars)) {
    new_var <- datasets_nmr[[name]]$vars[[var]]
    datasets_nmr[[name]]$data <- bin_nmr_rate(datasets_nmr[[name]]$data, var, new_var)
  }
}

# Reassign updated data back to original objects
ukr_sc_2009.2020 <- datasets_nmr$ukr_sc_2009.2020$data
merged_data2_sc <- datasets_nmr$merged_data2_sc$data
merged_data3_sc <- datasets_nmr$merged_data3_sc$data
merged_data4_sc <- datasets_nmr$merged_data4_sc$data

ukr_oa_2009.2022 <- datasets_nmr$ukr_oa_2009.2022$data
merged_data2_oa <- datasets_nmr$merged_data2_oa$data
merged_data3_oa <- datasets_nmr$merged_data3_oa$data
merged_data4_oa <- datasets_nmr$merged_data4_oa$data

# Create complete grid of all region-year combinations for both datasets
# Get all unique regions and year_groups from BOTH datasets
all_regions_sc <- unique(merged_data3_sc$region)
all_regions_oa <- unique(merged_data3_oa$region)
all_regions <- unique(c(all_regions_sc, all_regions_oa))

all_year_groups_sc <- unique(merged_data3_sc$year_group)
all_year_groups_oa <- unique(merged_data3_oa$year_group)
all_year_groups <- unique(c(all_year_groups_sc, all_year_groups_oa))

# Create a reference table with geometry and name for each region from Scopus data
region_geometry_ref <- merged_data3_sc %>%
  select(region, geometry, name) %>%
  distinct(region, .keep_all = TRUE)

region_geometry_ref1 <- merged_data3_oa %>%
  select(region, geometry, name) %>%
  distinct(region, .keep_all = TRUE)

# Create complete grid
complete_grid <- expand.grid(
  region = all_regions,
  year_group = all_year_groups,
  stringsAsFactors = FALSE
) %>% 
  arrange(region, year_group)

# Fill in missing combinations for Scopus data
merged_data3_sc_complete <- complete_grid %>%
  left_join(merged_data3_sc, by = c("region", "year_group")) %>%
  # Add geometry and name from reference table
  select(-geometry, -name) %>%
  left_join(region_geometry_ref1, by = "region")

# Fill in missing combinations for OpenAlex data
merged_data3_oa_complete <- complete_grid %>%
  left_join(merged_data3_oa, by = c("region", "year_group")) %>%
  # Add geometry and name from reference table
  select(-geometry, -name) %>%
  left_join(region_geometry_ref1, by = "region")

# Merge Scopus and OpenAlex datasets for visualization
merged_data_3combined <- bind_rows(
  merged_data3_sc_complete %>% mutate(dataset = "Scopus"), 
  merged_data3_oa_complete %>% mutate(dataset = "OpenAlex")
) %>% 
  arrange(region, year_group)

# Reshape data: Gather internal and international net-migration rates into a single column
merged_data_long <- merged_data_3combined %>%
  select(region, year_group, s_nmr_IN_binned, s_nmr_INT_binned, dataset, name, geometry) %>% 
  pivot_longer(
    cols = c(s_nmr_IN_binned, s_nmr_INT_binned),
    names_to = "Migration_Type",
    values_to = "binned_rate"
  ) %>% 
  filter(!is.na(year_group))

# Prepare data for the two separate maps
# Filter out Scopus data for 2021-2022 (which would be all NAs)
scopus_year_groups <- unique(merged_data3_sc$year_group)

merged_data_IN_by3 <- merged_data_3combined %>%
  filter(!(dataset == "Scopus" & !year_group %in% scopus_year_groups)) %>%
  select(region, year_group, s_nmr_IN_binned, dataset, name, geometry)

merged_data_INT_by3 <- merged_data_3combined %>%
  filter(!(dataset == "Scopus" & !year_group %in% scopus_year_groups)) %>%
  select(region, year_group, s_nmr_INT_binned, dataset, name, geometry)

# Create reference maps with labels
# Place them in the Scopus position for the years where Scopus has no data
openalex_only_years <- setdiff(all_year_groups, scopus_year_groups)

# Create macro-region categorization based on gdl_kyiv
region_macroregion <- data.frame(
  region = c("UA.02", "UA.21", "UA.13", "UA.27",  # Northern Ukraine
             "UA.01", "UA.10", "UA.18", "UA.23",  # Central Ukraine
             "UA.04", "UA.05", "UA.07", "UA.14", "UA.26",  # Eastern Ukraine
             "UA.08", "UA.11", "UA.16", "UA.17", "UA.20",  # Southern Ukraine
             "UA.03", "UA.06", "UA.09", "UA.15", "UA.19", "UA.22", "UA.24", "UA.25",  # Western Ukraine
             "UA.12"),  # Kyiv
  macro_region = c(rep("Northern Ukraine", 4),
                   rep("Central Ukraine", 4),
                   rep("Eastern Ukraine", 5),
                   rep("Southern Ukraine", 5),
                   rep("Western Ukraine", 8),
                   "Kyiv"),
  stringsAsFactors = FALSE
)

# Create reference data for the OpenAlex-only year
# For Internal migration: show region labels
reference_data_IN <- merged_data3_sc_complete %>%
  filter(year_group == all_year_groups[1]) %>%
  mutate(dataset = "Scopus",
         year_group = openalex_only_years[1],
         s_nmr_IN_binned = factor(NA, levels = levels(merged_data3_sc_complete$s_nmr_IN_binned))) %>%
  select(region, year_group, s_nmr_IN_binned, dataset, name, geometry)

# For International migration: show macro-region labels
# Combine s_nmr_INT_binned and macro_region into a single fill variable
reference_data_INT <- merged_data3_sc_complete %>%
  filter(year_group == all_year_groups[1]) %>%
  left_join(region_macroregion, by = "region") %>%
  mutate(dataset = "Scopus",
         year_group = openalex_only_years[1],
         s_nmr_INT_binned = factor(macro_region, 
                                   levels = c(levels(merged_data3_sc_complete$s_nmr_INT_binned),
                                              "Northern Ukraine", "Central Ukraine", "Eastern Ukraine",
                                              "Southern Ukraine", "Western Ukraine", "Kyiv")),
         name = macro_region) %>%
  select(region, year_group, s_nmr_INT_binned, dataset, name, geometry)

# Calculate centroids for each macro-region to place labels only once
macro_region_labels <- reference_data_INT %>%
  group_by(name) %>%
  summarize(geometry = st_union(geometry)) %>%
  mutate(centroid = st_centroid(geometry),
         dataset = "Scopus",
         year_group = openalex_only_years[1]) %>%
  ungroup()

# Create color palette for macro-regions
macro_region_colors <- c(
  "Northern Ukraine" = "#00BA38",
  "Central Ukraine" = "#F8766D",
  "Eastern Ukraine" = "#A3A500",
  "Southern Ukraine" = "#00BF7D",
  "Western Ukraine" = "#00B0F6",
  "Kyiv" = "#7f0000"
)

# Name the migration rate palette to match factor levels
migration_rate_names <- c("< -100", "[-100, -10)", "[-10, 0)", "0", "[0, 10)", "(10, 100]", "> 100")
names(pal_7cat_div) <- migration_rate_names

# Combine palettes
combined_palette <- c(pal_7cat_div, macro_region_colors)

# Add reference data to the main datasets
merged_data_IN_by3_with_ref <- bind_rows(merged_data_IN_by3, reference_data_IN)

# For INT, bind first then expand factor levels
merged_data_INT_by3_with_ref <- merged_data_INT_by3 %>%
  bind_rows(reference_data_INT) %>%
  mutate(s_nmr_INT_binned = factor(s_nmr_INT_binned,
                                   levels = c(levels(merged_data3_sc_complete$s_nmr_INT_binned),
                                              "Northern Ukraine", "Central Ukraine", "Eastern Ukraine",
                                              "Southern Ukraine", "Western Ukraine", "Kyiv")))

# Standardize labels to reflect romanized Ukrainian 
merged_data_IN_by3_with_ref <- merged_data_IN_by3_with_ref %>% 
  mutate(name2 = case_match(name,
                            "Rivne" ~	"Rivne",
                            "Vinnytsya"	~ "Vinnytsia",
                            "Chernivtsi" ~ "Chernivtsi",
                            "L'viv" ~	"Lviv",
                            "Zhytomyr"	~ "Zhytomyr",
                            "Zaporizhzhya"	~ "Zaporizhzhia",
                            "Sumy" ~	"Sumy",
                            "Kharkiv" ~ "Kharkiv",
                            "Mykolayiv"	~ "Mykolaiv",
                            "Dnipropetrovs'k" ~ "Dnipropetrovsk",
                            "Luhans'k"	~ "Luhansk",
                            "Khmel'nyts'kyy"	~ "Khmelnytskyi",
                            "Ivano-Frankivs'k"	~ "Ivano-Frankivsk",
                            "Volyn"	~ "Volyn",
                            "Kiev City" ~	"Kyiv City",
                            "Cherkasy" ~ "Cherkasy",
                            "Kherson" ~	"Kherson",
                            "Ternopil'" ~ "Ternopil",
                            "Odessa" ~ "Odesa",
                            "Transcarpathia" ~ "Zakarpattia",
                            "Kirovohrad" ~ "Kirovohrad",
                            "Donets'k" ~ "Donetsk",
                            "Kiev" ~ "Kyiv",
                            "Chernihiv"	~ "Chernihiv",
                            "Poltava"	~ "Poltava",
                            "Crimea" ~ "Crimea",
                            "Sevastopol" ~ "Sevastopol"))

## OpenAlex Only Migration Maps with Reference Maps as Legends ##

# Fix labels of regions, put in standard romanized Ukrainian 

merged_data3_oa_complete <- merged_data3_oa_complete %>% 
  mutate(name2 = case_match(name, 
                            "Rivne" ~	"Rivne",
                            "Vinnytsya"	~ "Vinnytsia",
                            "Chernivtsi" ~ "Chernivtsi",
                            "L'viv" ~	"Lviv",
                            "Zhytomyr"	~ "Zhytomyr",
                            "Zaporizhzhya"	~ "Zaporizhzhia",
                            "Sumy" ~	"Sumy",
                            "Kharkiv" ~ "Kharkiv",
                            "Mykolayiv"	~ "Mykolaiv",
                            "Dnipropetrovs'k" ~ "Dnipropetrovsk",
                            "Luhans'k"	~ "Luhansk",
                            "Khmel'nyts'kyy"	~ "Khmelnytskyi",
                            "Ivano-Frankivs'k"	~ "Ivano-Frankivsk",
                            "Volyn"	~ "Volyn",
                            "Kiev City" ~	"Kyiv City",
                            "Cherkasy" ~ "Cherkasy",
                            "Kherson" ~	"Kherson",
                            "Ternopil'" ~ "Ternopil",
                            "Odessa" ~ "Odesa",
                            "Transcarpathia" ~ "Zakarpattia",
                            "Kirovohrad" ~ "Kirovohrad",
                            "Donets'k" ~ "Donetsk",
                            "Kiev" ~ "Kyiv",
                            "Chernihiv"	~ "Chernihiv",
                            "Poltava"	~ "Poltava",
                            "Crimea" ~ "Crimea",
                            "Sevastopol" ~ "Sevastopol"))

# Prepare data for OpenAlex only maps
merged_data_IN_oa <- merged_data_3combined %>%
  filter(dataset == "OpenAlex") %>%
  select(region, year_group, s_nmr_IN_binned, dataset, name, geometry)

merged_data_INT_oa <- merged_data_3combined %>%
  filter(dataset == "OpenAlex") %>%
  select(region, year_group, s_nmr_INT_binned, dataset, name, geometry)

# Combine Internal and International into one dataset
merged_data_oa_combined <- bind_rows(
  merged_data_IN_oa %>% mutate(migration_type = "Internal"),
  merged_data_INT_oa %>% 
    rename(s_nmr_IN_binned = s_nmr_INT_binned) %>%
    mutate(migration_type = "International")
) %>% 
  filter(!is.na(year_group))

# Create reference maps
# Reference map 1: Region labels
reference_map_regions <- merged_data3_oa_complete %>%
  filter(year_group == all_year_groups[1]) %>%
  mutate(centroid = st_centroid(geometry),
         lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry), fill = "lightgray", color = "black") +
  ggrepel::geom_label_repel(
    aes(x = lon, y = lat, label = name2), 
    size = 6 / .pt,            # 6pt labels
    family = "Times New Roman",
    box.padding = 0.35,
    point.padding = 0.3,
    force = 2,
    max.overlaps = Inf,
    min.segment.length = 0,
    fill = "white"
  ) +
  theme_void() +
  labs(title = "Region Labels") +
  theme(
    plot.title = element_text(
      family = "Times New Roman", size = 9, face = "bold",
      hjust = 0.5, lineheight = 10/9
    )
  )

# Reference map 2: Macro-regions with colors
reference_data_macro <- merged_data3_oa_complete %>%
  filter(year_group == all_year_groups[1]) %>%
  left_join(region_macroregion, by = "region")

macro_region_centroids <- reference_data_macro %>%
  group_by(macro_region) %>%
  summarize(geometry = st_union(geometry)) %>%
  mutate(centroid = st_centroid(geometry),
         lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  ungroup()

reference_map_macro <- ggplot(reference_data_macro) +
  geom_sf(aes(geometry = geometry, fill = macro_region), color = "black") +
  ggrepel::geom_label_repel(
    data = macro_region_centroids,
    aes(x = lon, y = lat, label = macro_region), 
    size = 6 / .pt,            # 6pt labels
    family = "Times New Roman",
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.3,
    force = 1.5,
    max.overlaps = Inf,
    min.segment.length = 0,
    fill = "white"
  ) +
  scale_fill_manual(values = macro_region_colors) +
  theme_void() +
  labs(title = "Macro-regions") +
  theme(
    plot.title = element_text(
      family = "Times New Roman", size = 9, face = "bold",
      hjust = 0.5, lineheight = 10/9
    ),
    legend.position = "none"
  )

# Create main migration map
main_map <- ggplot(merged_data_oa_combined) +
  geom_sf(aes(geometry = geometry, fill = s_nmr_IN_binned)) +
  facet_grid(year_group ~ migration_type, switch = "y") +
  scale_fill_manual(values = pal_7cat_div, na.value = '#d7d7d2', drop = FALSE) +
  theme_void() +
  labs(fill = "Net Migration Rate") +
  theme(
    # Facet strip labels: axes category (8pt)
    strip.text = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8
    ),
    strip.text.y.left = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8,
      angle = 0, hjust = 1
    ),
    # Legend title: axes category (8pt)
    legend.title = element_text(
      family = "Times New Roman", size = 8, lineheight = 9/8
    ),
    # Legend text: labels category (6pt)
    legend.text = element_text(
      family = "Times New Roman", size = 6, lineheight = 7/6
    ),
    legend.position = "bottom"
  )
# Combine: main map on left, reference maps stacked on right
combined_nmr_map_oa <- ggarrange(
  main_map,
  ggarrange(reference_map_regions, reference_map_macro, 
            ncol = 1, heights = c(1, 1)),
  ncol = 2,
  widths = c(1.2, 1.2)
)

# Add overall title
combined_nmr_map_oa <- annotate_figure(
  combined_nmr_map_oa,
  top = text_grob("Net migration rates across Ukraine regions, OpenAlex data", 
                  face = "bold", size = 10)
)

print(combined_nmr_map_oa)

save_plot(combined_nmr_map_oa,
          write_path("updated-data/twoyearbackfill/fig2_spatialview_openalex.svg"),
          width = 12, height = 8)

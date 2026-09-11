#### Appendix Figure 2 ####

# Run after: 01_Data-wrangling.R, 03_Fig2.R (reuses their objects in this session)

#### Resolve output directory ####
output_dir <- Sys.getenv("OUTPUT_DIR", unset = NA)
if (is.na(output_dir) || !nzchar(output_dir)) output_dir <- here::here("output")
write_path <- function(...) {
  p <- file.path(output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

# Run 03_Fig2.R before running this

## Create Appendix Figure 2 ##

# Updated mapping function for Internal migration (region labels)
create_nmrmigration_map_IN <- function(data, fill_var, facet_var, title_text) {
  label_data <- data %>% 
    filter(dataset == "Scopus" & year_group %in% openalex_only_years) %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(lon = st_coordinates(centroid)[,1],
           lat = st_coordinates(centroid)[,2]) %>%
    st_drop_geometry()
  
  ggplot(data) +
    geom_sf(aes(geometry = geometry, fill = !!rlang::sym(fill_var))) +
    ggrepel::geom_label_repel(    # switched from geom_text_repel
      data = label_data,
      aes(x = lon, y = lat, label = name2), 
      size = 2 / .pt,
      family = "Times New Roman",
      box.padding = 0.1,
      point.padding = 0.1,
      force = 1,
      force_pull = 3,
      max.overlaps = Inf,
      min.segment.length = 0,     # restore segments
      segment.size = 0.2,
      segment.color = "gray50",
      fill = "white",             # white label background
      label.size = 0.1,           # thin border around label box
      label.padding = unit(0.1, "lines")  # tight padding inside box
    ) +
    facet_grid(as.formula(paste("year_group ~ dataset"))) +
    scale_fill_manual(values = pal_7cat_div, na.value = '#d7d7d2', drop = FALSE) +
    theme_void() +
    labs(title = title_text, fill = "Net Migration Rate") +
    theme(
      plot.title = element_text(
        family = "Times New Roman", size = 9, face = "bold",
        hjust = 0.5, lineheight = 10/9
      ),
      strip.text = element_text(
        family = "Times New Roman", size = 8, lineheight = 9/8
      ),
      legend.title = element_text(
        family = "Times New Roman", size = 8, lineheight = 9/8
      ),
      legend.text = element_text(
        family = "Times New Roman", size = 6, lineheight = 7/6
      ),
      legend.position = "bottom"
    )
}

create_nmrmigration_map_INT <- function(data, fill_var, facet_var, title_text) {
  macro_labels_data <- macro_region_labels %>%
    mutate(lon = st_coordinates(centroid)[,1],
           lat = st_coordinates(centroid)[,2]) %>%
    st_drop_geometry()
  
  ggplot(data) +
    geom_sf(aes(geometry = geometry, fill = !!rlang::sym(fill_var))) +
    ggrepel::geom_label_repel(    # switched from geom_text_repel
      data = macro_labels_data,
      aes(x = lon, y = lat, label = name), 
      size = 3 / .pt,
      family = "Times New Roman",
      fontface = "bold",
      box.padding = 0.1,
      point.padding = 0.1,
      force = 0.5,
      force_pull = 3,
      max.overlaps = Inf,
      min.segment.length = 0,     # restore segments
      segment.size = 0.2,
      segment.color = "gray50",
      fill = "white",             # white label background
      label.size = 0.1,           # thin border around label box
      label.padding = unit(0.1, "lines")  # tight padding inside box
    ) +
    facet_grid(as.formula(paste("year_group ~ dataset"))) +
    scale_fill_manual(values = combined_palette, na.value = '#d7d7d2', drop = FALSE) +
    theme_void() +
    labs(title = title_text, fill = "Net Migration Rate") +
    theme(
      plot.title = element_text(
        family = "Times New Roman", size = 9, face = "bold",
        hjust = 0.5, lineheight = 10/9
      ),
      strip.text = element_text(
        family = "Times New Roman", size = 8, lineheight = 9/8
      ),
      legend.title = element_text(
        family = "Times New Roman", size = 8, lineheight = 9/8
      ),
      legend.text = element_text(
        family = "Times New Roman", size = 6, lineheight = 7/6
      ),
      legend.position = "bottom"
    )
}

nmrmig_IN_by3_scoa <- create_nmrmigration_map_IN(
  merged_data_IN_by3_with_ref %>% filter(!is.na(year_group)), 
  "s_nmr_IN_binned", "year_group ~ dataset", 
  "Internal netmigration rates across Ukraine regions, \n selected years"
)

nmrmig_INT_by3_scoa <- create_nmrmigration_map_INT(
  merged_data_INT_by3_with_ref %>% filter(!is.na(year_group)), 
  "s_nmr_INT_binned", "year_group ~ dataset", 
  "International netmigration rates across Ukraine regions, \n selected years"
)

combined_nmr_map <- ggarrange(
  nmrmig_IN_by3_scoa, 
  nmrmig_INT_by3_scoa, 
  common.legend = TRUE,
  legend = "bottom"
)

print(combined_nmr_map)

save_plot(combined_nmr_map,
          write_path("updated-data/twoyearbackfill/fig2_spatialview_scoa_updated.pdf"),
          width = 18, height = 16)

save_plot(combined_nmr_map,
          write_path("updated-data/twoyearbackfill/fig2_spatialview_scoa_updated.pdf"),
          width = 18, height = 16)
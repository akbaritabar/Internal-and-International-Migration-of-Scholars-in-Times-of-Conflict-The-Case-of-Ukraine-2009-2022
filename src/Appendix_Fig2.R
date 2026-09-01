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
  # Prepare label data outside the plot
  label_data <- data %>% 
    filter(dataset == "Scopus" & year_group %in% openalex_only_years) %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(lon = st_coordinates(centroid)[,1],
           lat = st_coordinates(centroid)[,2]) %>%
    st_drop_geometry()
  
  ggplot(data) +
    geom_sf(aes(geometry = geometry, fill = !!rlang::sym(fill_var))) +
    ggrepel::geom_text_repel(
      data = label_data,
      aes(x = lon, y = lat, label = name2), 
      size = 6 / .pt,          # 6pt labels (convert to ggplot units)
      family = "Times New Roman",
      box.padding = 0.3,
      point.padding = 0.3,
      force = 2,
      force_pull = 2,
      max.overlaps = Inf,
      min.segment.length = 0,
      segment.size = 0.3,
      segment.color = "gray50"
    ) +
    facet_grid(as.formula(paste("year_group ~ dataset"))) +
    scale_fill_manual(values = pal_7cat_div, na.value = '#d7d7d2', drop = FALSE) +
    theme_void() +
    labs(title = title_text, fill = "Net Migration Rate") +
    theme(
      # Title: 9pt Times New Roman, 10pt leading
      plot.title = element_text(
        family = "Times New Roman", size = 9, face = "bold",
        hjust = 0.5, lineheight = 10/9
      ),
      # Facet strip labels: axes category (8pt)
      strip.text = element_text(
        family = "Times New Roman", size = 8, lineheight = 9/8
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
}

# Updated mapping function for International migration (macro-region labels)
create_nmrmigration_map_INT <- function(data, fill_var, facet_var, title_text) {
  # Prepare macro-region label data
  macro_labels_data <- macro_region_labels %>%
    mutate(lon = st_coordinates(centroid)[,1],
           lat = st_coordinates(centroid)[,2]) %>%
    st_drop_geometry()
  
  ggplot(data) +
    geom_sf(aes(geometry = geometry, fill = !!rlang::sym(fill_var))) +
    ggrepel::geom_text_repel(
      data = macro_labels_data,
      aes(x = lon, y = lat, label = name), 
      size = 6 / .pt,          # 6pt labels (convert to ggplot units)
      family = "Times New Roman",
      fontface = "bold",
      box.padding = 0.3,
      point.padding = 0.3,
      force = 2,
      max.overlaps = Inf
    ) +
    facet_grid(as.formula(paste("year_group ~ dataset"))) +
    scale_fill_manual(values = combined_palette, na.value = '#d7d7d2', drop = FALSE) +
    theme_void() +
    labs(title = title_text, fill = "Net Migration Rate") +
    theme(
      # Title: 9pt Times New Roman, 10pt leading
      plot.title = element_text(
        family = "Times New Roman", size = 9, face = "bold",
        hjust = 0.5, lineheight = 10/9
      ),
      # Facet strip labels: axes category (8pt)
      strip.text = element_text(
        family = "Times New Roman", size = 8, lineheight = 9/8
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
}

nmrmig_IN_by3_scoa <- create_nmrmigration_map_IN(merged_data_IN_by3_with_ref %>% filter(!is.na(year_group)), 
                                                 "s_nmr_IN_binned", "year_group ~ dataset", 
                                                 "Internal netmigration rates across Ukraine regions, \n selected years")

nmrmig_INT_by3_scoa <- create_nmrmigration_map_INT(merged_data_INT_by3_with_ref %>% filter(!is.na(year_group)), "s_nmr_INT_binned", "year_group ~ dataset", 
                                                   "International netmigration rates across Ukraine regions, \n selected years")

# Combine maps with shared legend
combined_nmr_map <- ggarrange(nmrmig_IN_by3_scoa, 
                              nmrmig_INT_by3_scoa, 
                              common.legend = TRUE,
                              legend = "bottom")

print(combined_nmr_map)

save_plot(combined_nmr_map,
          write_path("updated-data/twoyearbackfill/fig2_spatialview_scoa_updated.svg"),
          width = 12, height = 8)
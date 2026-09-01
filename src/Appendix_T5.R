#### Appendix Table 5: 2-year backward filling ####

# Run after: 01_Data-wrangling.R, 05_Modelspecs.R (reuses their objects in this session)

# libraries
library(texreg)

#### Resolve output directory ####
output_dir <- Sys.getenv("OUTPUT_DIR", unset = NA)
if (is.na(output_dir) || !nzchar(output_dir)) output_dir <- here::here("output")
write_path <- function(...) {
  p <- file.path(output_dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

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
  file = write_path("updated-data/twoyearbackfill/negbin_outmigration_models_INT_texreg.tex"),
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
  file = write_path("updated-data/twoyearbackfill/negbin_inmigration_models_INT_texreg.tex"),
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
  file = write_path("updated-data/twoyearbackfill/negbin_combined_models_INT_texreg.tex"),
  custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)", "(12)"),
  custom.coef.map = custom_coef_names,
  caption = "Negative Binomial Models: International In-migration and Out-migration (2009-2022)",
  label = "tab:combined_migration",
  stars = c(0.001, 0.01, 0.05),
  custom.note = "Standard errors in parentheses. All models include offset for Log(Population of scholars). Models (1)-(6): In-migration. Models (7)-(12): Out-migration. %stars.",
  fontsize = "tiny"
)


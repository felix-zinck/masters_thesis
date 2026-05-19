# =====================================================================
# Workflow 6: Results & Mapping
# =====================================================================

# -----------------------------
# 0. Load packages
# -----------------------------
library(terra)
library(sf)
library(ggplot2)
library(ggspatial)
library(viridis)
library(dplyr)
library(RColorBrewer)
library(classInt)
library(tidyr)
library(purrr)
library(e1071)
library(patchwork)
library(ggspatial)
library(broom)
library(tibble)
library(readr)
library(car)
library(spdep)
library(cowplot)

# -----------------------------
# 1. Load spatial data
# -----------------------------
berlin <- st_read("/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_borders/berlin.shp")
grid <- st_read("/home/fix/Documents/sustainable_development/master_thesis/data/geodata/zensus_berlin_grid/berlin_grid_final.gpkg")

lst_raster_file <- "/home/fix/Documents/sustainable_development/master_thesis/data/geodata/median_LST_final/median_LST_FINAL_4y_10cc.tif"
lst_raster <- rast(lst_raster_file)
lst_raster <- project(lst_raster, "EPSG:3035")

# -----------------------------
# 2. Define output paths
# -----------------------------

main_results_path <- "/home/fix/Documents/sustainable_development/master_thesis/results/main_results" 
appendix_path <- "/home/fix/Documents/sustainable_development/master_thesis/results/appendix"

# -----------------------------
# 3. Create summary statistics on created study measures
# -----------------------------

# Select relevant variables 
vars <- grid %>% st_drop_geometry() %>% select(
    lst_n_median, starts_with("sei_n_"), csi_n, starts_with("esi_n_"), cesi_n)

# Function for summary statistics
summary_stats <- function(x) {
  
  x_clean <- x[!is.na(x)]
  
  c(
    n_valid = length(x_clean),
    na_percent = 100 * sum(is.na(x)) / length(x),
    p10        = unname(quantile(x_clean, 0.10)),
    median     = median(x_clean),
    mean       = mean(x_clean),
    p90        = unname(quantile(x_clean, 0.90)),
    IQR        = IQR(x_clean),
    sd         = sd(x_clean),
    skewness   = skewness(x_clean)
  )
}

# Create summary table
summary_table <- as.data.frame(t(sapply(vars, summary_stats)))
summary_table$variable <- rownames(summary_table)
rownames(summary_table) <- NULL

summary_table <- summary_table %>%
  select(variable, n_valid, na_percent, p10, median, mean, p90, IQR, sd, skewness) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Preview
print(summary_table)

# Save summary table
write_csv(summary_table, file.path(appendix_path, "appendix_05_table_indicator_summary_statistics.csv"))

# -----------------------------
# 4. Density distribution plots for normalized indicators/indices
# -----------------------------

# 4.1 Density distribution plot for normalized LST, SEI, and CSI

# Prepare LST data
lst_df <- grid %>% st_drop_geometry() %>% select(lst_n_median) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>% filter(!is.na(value)) %>% mutate(variable = "LST")

# Prepare SEI data
sei_df <- grid %>% st_drop_geometry() %>% select(starts_with("sei_n_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>% filter(!is.na(value)) %>%
  mutate(variable = dplyr::recode(variable,
                           "sei_n_elderly"       = "Elderly (65+)",
                           "sei_n_children"      = "Children (<6)",
                           "sei_n_single_hh"     = "Single households",
                           "sei_n_single_parent" = "Single parents",
                           "sei_n_foreign_born"  = "Foreign-born",
                           "sei_n_non_citizen"   = "Non-citizens",
                           "sei_n_space"         = "Living space (inverse)",
                           "sei_n_rent"          = "Rent (inverse)",))

# Remove non-citizens
sei_df <- sei_df %>% filter(variable != "Non-citizens")

# Prepare CSI data
csi_df <- grid %>% st_drop_geometry() %>% select(csi_n) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>% filter(!is.na(value)) %>% mutate(variable = "CSI")

# Define desired legend order
legend_order <- c(
  "LST", "CSI", "Elderly (65+)", "Children (<6)", "Single households",
  "Single parents", "Foreign-born", "Living space (inverse)", "Rent (inverse)")

# Apply to all datasets
sei_df$variable <- factor(sei_df$variable, levels = legend_order)
csi_df$variable <- factor(csi_df$variable, levels = c("LST", "CSI"))
lst_df$variable <- factor(lst_df$variable, levels = c("LST", "CSI"))

# Define uniform figure theme for density plots 
plot_theme <- function(base_size = 10) { 
  theme_minimal(base_size = base_size) + 
    theme(
      legend.title     = element_text(size = rel(0.75), face = "bold"), 
      legend.text      = element_text(size = rel(0.75)), 
      axis.title       = element_text(size = rel(0.8)), 
      axis.text        = element_text(size = rel(0.75)), 
      panel.grid.minor = element_blank(), 
      plot.margin      = margin(0, 0, 0, 0),
      
      legend.position    = "right",
      legend.justification = c(1, 0),
      legend.box         = "vertical",
      legend.box.spacing = unit(2, "pt"),
      legend.spacing.y   = unit(1, "pt"),
      legend.key.height  = unit(10, "pt"),
      legend.key.width   = unit(14, "pt"),
      legend.margin      = margin(2, 2, 2, 2))}

# Plot
p_density_lst_sei_csi <- ggplot() +
    
    # SEI layer
    geom_density(data = sei_df, aes(x = value, color = variable),
      linewidth = 0.85, key_glyph = "path") +
    
    # CSI layer
    geom_density(data = csi_df, aes(x = value, linetype = variable),
      linewidth = 1.65, color = "black", key_glyph = "path") +
    
    # LST layer
    geom_density(data = lst_df, aes(x = value, linetype = variable),
      linewidth = 1.65, color = "red", key_glyph = "path") +
    
    scale_color_manual(values = c(
      "Elderly (65+)"          = "#E69F00",  
      "Children (<6)"          = "#F0E442",  
      "Single households"      = "#CC79A7",
      "Single parents"         = "#D55E00",  
      "Foreign-born"           = "#009E73",
      "Living space (inverse)" = "#7F7F7F",
      "Rent (inverse)"         = "#0072B2")) +
    scale_linetype_manual(
      values = c("LST" = "dashed", "CSI" = "solid"
      ), breaks = c("LST", "CSI")) +
    guides(linetype = guide_legend(order = 1, title = NULL),
           color = guide_legend(order = 2, title = "SEI:")) +
    labs(x = "Normalized value", y = "Density") +
    plot_theme()

# Preview
p_density_lst_sei_csi

# Export plot 
ggsave(filename = file.path(main_results_path, "5_2_01_figure_density_lst_sei_csi.png"), 
       plot = ggdraw(p_density_lst_sei_csi), width = 130, 
       height = 65, units = "mm", dpi = 300)

# 4.2 Density distribution plot for normalized ESI and CESI

# Prepare ESI data
esi_df <- grid %>% st_drop_geometry() %>% select(starts_with("esi_n_")) %>%
  pivot_longer(cols = everything(),names_to = "variable", values_to = "value"
  ) %>% filter(!is.na(value)) %>%
  mutate(variable = dplyr::recode(variable,
                           "esi_n_elderly"       = "Elderly (65+)",
                           "esi_n_children"      = "Children (<6)",
                           "esi_n_single_hh"     = "Single households",
                           "esi_n_single_parent" = "Single parents",
                           "esi_n_foreign_born"  = "Foreign-born",
                           "esi_n_rent"          = "Rent (inverse)",
                           "esi_n_space"         = "Living space (inverse)"))

# Prepare CESI data
cesi_df <- grid %>%
  st_drop_geometry() %>% select(cesi_n) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value"
  ) %>% filter(!is.na(value)) %>% mutate(variable = "CESI")

# Legend order
legend_order <- c("CESI", "Elderly (65+)", "Children (<6)", "Single households",
                  "Single parents", "Foreign-born","Living space (inverse)",
                  "Rent (inverse)")

esi_df$variable  <- factor(esi_df$variable, levels = legend_order)
cesi_df$variable <- factor(cesi_df$variable, levels = legend_order)

# Plot
p_density_esi_cesi <- ggplot() +
  
  # ESI layer
  geom_density(data = esi_df, aes(x = value, color = variable),
    linewidth = 0.85, key_glyph = "path") +
  
  # CESI layer
  geom_density(data = cesi_df, aes(x = value, linetype = variable),
    linewidth = 1.65, color = "black", key_glyph = "path") +
  scale_color_manual(values = c(
    "Elderly (65+)"          = "#E69F00",
    "Children (<6)"          = "#F0E442",
    "Single households"      = "#CC79A7",
    "Single parents"         = "#D55E00",
    "Foreign-born"           = "#009E73",
    "Rent (inverse)"         = "#0072B2",
    "Living space (inverse)" = "#7F7F7F")) +
   guides(linetype = guide_legend(order = 1, title = NULL),
         color = guide_legend(order = 2, title = "ESI:")) +
  labs( x = "Normalized value", y = "Density", color = "ESI", linetype = NULL) +
  plot_theme()

# Preview
p_density_esi_cesi

# Export plot 
ggsave(filename = file.path(main_results_path, "5_2_02_figure_density_esi_cesi.png"), 
       plot = ggdraw(p_density_esi_cesi), width = 130, 
       height = 65, units = "mm", dpi = 300)

# -----------------------------
# 4. Plot: Exposure layer
# -----------------------------

# Prepare raster dataframe
lst_berlin_df <- as.data.frame(lst_raster, xy = TRUE, na.rm = TRUE)
colnames(lst_berlin_df) <- c("x", "y", "lst")

# Define shared scale from aggregated grid LST
limits_grid <- range(grid$lst_median, na.rm = TRUE)
breaks_grid <- pretty(limits_grid, n = 5)

# Define uniform map theme for all map plots
map_theme <- function(base_size = 10) { 
  theme_minimal(base_size = base_size) + 
    theme(
      plot.title = element_text(size = rel(0.9), face = "bold"),
      legend.title = element_text(size = rel(0.75), face = "bold"), 
      legend.text  = element_text(size = rel(0.75)), 
      axis.text  = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(), 
      plot.margin      = margin(0, 0, 0, 0),
      
      legend.position    = "right",
      legend.justification = c(1, 0),
      legend.box         = "vertical",
      legend.box.spacing = unit(2, "pt"),
      legend.spacing.y   = unit(1, "pt"),
      legend.key.height  = unit(10, "pt"),
      legend.key.width   = unit(14, "pt"),
      legend.margin      = margin(2, 2, 2, 2))}

# Raster plot (with north arrow)
p_lst_raster <- ggplot() +
  geom_tile(data = lst_berlin_df, aes(x = x, y = y, fill = lst)) +
  geom_sf(data = berlin, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "LST (°C)",
    limits = limits_grid,
    oob = scales::squish,
    breaks = breaks_grid) +
  coord_sf(expand = FALSE) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering) +
  labs(title = "a) Raster-based median LST") +
  map_theme()

# Preview
p_lst_raster

# Grid plot (with scale bar)
p_lst_grid <- ggplot() +
  geom_sf(data = grid, aes(fill = lst_median), color = NA) +
  geom_sf(data = berlin, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "LST (°C)",
    limits = limits_grid,
    oob = scales::squish,
    breaks = breaks_grid) +
  coord_sf(expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.35) +
  labs(title = "b) Grid-based median LST") +
  map_theme()

# Preview
p_lst_grid

# Combine with shared legend
combined_exp_plot <- (p_lst_raster / p_lst_grid) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right", legend.justification = c(1, 0))

# Preview and export
combined_exp_plot
ggsave( filename = file.path(main_results_path, "5_3_figure_exposure_layer.png"), 
        plot = combined_exp_plot, width = 100, 
        height = 130, units = "mm", dpi = 300)

# -----------------------------
# 5. Plots: Sensitivity layer
# -----------------------------

# 5.1 Create quantile classes for SEI and CSI

cat_names <- c("Very low", "Low", "Medium", "High", "Very high")

sei_cols <- c(
  "sei_n_elderly", "sei_n_children", "sei_n_single_hh",
  "sei_n_single_parent", "sei_n_foreign_born",
  "sei_n_space", "sei_n_rent")

sei_cols_final <- c(sei_cols, "csi_n")

# Robust quantile classification function
quantile_class <- function(x, labels = cat_names) {
  brks <- quantile(x, probs = seq(0, 1, length.out = 6), na.rm = TRUE, type = 7)
  brks <- unique(brks)
  
  if (length(brks) < 2) {
    return(factor(rep(NA, length(x)), levels = labels))
  }
  labels_use <- labels[seq_len(length(brks) - 1)]
  cut(x, breaks = brks, include.lowest = TRUE, labels = labels_use)}

# Create quantile classes
for (col in sei_cols_final) {
  grid[[paste0(col, "_q_class")]] <- quantile_class(grid[[col]])}

# Ensure identical legend order
for (col in paste0(sei_cols_final, "_q_class")) {
  grid[[col]] <- factor(grid[[col]], levels = cat_names, ordered = TRUE)}

)# 5.2 Plot: CSI

p_csi <- ggplot() +
  geom_sf(
    data = grid %>% filter(!is.na(csi_n_q_class)),
    aes(fill = csi_n_q_class),
    color = NA) +
  geom_sf(data = berlin, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c(
      "Very low"  = "#000080",
      "Low"       = "#5ab0ff",
      "Medium"    = "#ffea00",
      "High"      = "#ff7a00",
      "Very high" = "#b10026"
    ),
    name = "CSI") +
  coord_sf(expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering) +
  map_theme()

# Preview
p_csi

# Export plot 
ggsave( filename = file.path(main_results_path, "5_4_02_figure_csi.png"), 
        plot = p_csi, width = 130, 
        height = 95, units = "mm", dpi = 300)

# 5.3 Plot: Individual SEI (facet map)

sei_class_cols <- paste0(sei_cols, "_q_class")

sei_labels <- c(
  "a) Elderly (65+)", "b) Children (<6)", "c) Single households",
  "d) Single parents", "e) Foreign-born", "f) Living space (inverse)",
  "g) Rent (inverse)")

grid_long_sei <- grid %>%
  select(grid_id, geom, all_of(sei_class_cols)) %>%
  pivot_longer(
    cols = all_of(sei_class_cols),
    names_to = "variable",
    values_to = "class") %>%
  mutate(
    variable = gsub("_q_class", "", variable),
    variable = factor(variable, levels = sei_cols, labels = sei_labels),
    class = factor(class, levels = cat_names, ordered = TRUE)) %>% 
  filter(!is.na(class)) %>% st_as_sf(sf_column_name = "geom")

p_sei_facet <- ggplot() +
  geom_sf(data = grid_long_sei, aes(fill = class), color = NA) +
  geom_sf(data = berlin, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Very low" = "#000080",
      "Low"      = "#5ab0ff",
      "Medium"   = "#ffea00",
      "High"     = "#ff7a00",
      "Very high"= "#b10026"
    ),
    name = "SEI") +
  facet_wrap(~ variable, ncol = 2) +
  coord_sf(expand = FALSE) +
  map_theme()

# Preview
p_sei_facet

# Plot without legend
p_sei_facet_no_legend <- p_sei_facet + theme(legend.position = "none")

# Extract legend
sei_legend <- cowplot::get_legend(p_sei_facet)

# Create north arrow
# Berlin bbox for spatial context
bb <- st_bbox(berlin)

north_arrow <- ggplot() +
  geom_sf(data = berlin, fill = NA, color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    crs = st_crs(berlin),
    expand = FALSE
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering(),
    height = unit(1.1, "cm"),
    width  = unit(1.1, "cm")
  ) +
  theme_void()

# Add legend and north arrow 
p_sei_facet_final <- p_sei_facet_no_legend + 
  patchwork::inset_element(sei_legend, left = 0.53, bottom = 0.06, right = 0.81, 
                           top = 0.24, align_to = "full" ) + 
  patchwork::inset_element(north_arrow, left = 0.53, bottom = 0.80, 
                           right = 0.97, top = 0.98, align_to = "full")
# Preview and export
p_sei_facet_final
ggsave( filename = file.path(main_results_path, "5_4_01_figure_sei_facet.png"), 
        plot = p_sei_facet_final, width = 130, 
        height = 200, units = "mm", dpi = 300)

# -----------------------------
# 6. Statistical Relationship Analysis
# -----------------------------

# 6.1 Correlation analysis between LST and SEI/CSI

# Prepare data
df <- grid %>% st_drop_geometry()

vars <- df %>% select(
    lst_n_median, csi_n, sei_n_elderly, sei_n_children, sei_n_single_hh,
    sei_n_single_parent, sei_n_foreign_born, sei_n_space, sei_n_rent)

# Spearman correlation: LST vs all SEI and CSI
lst_cor_mat <- cor(
  vars[["lst_n_median"]], vars %>% select(-lst_n_median),
  method = "spearman",use = "pairwise.complete.obs")

# Label map
label_map <- c(
  "sei_n_elderly"       = "Elderly (65+)",
  "sei_n_children"      = "Children (<6)",
  "sei_n_single_hh"     = "Single households",
  "sei_n_single_parent" = "Single parents",
  "sei_n_foreign_born"  = "Foreign-born",
  "sei_n_space"         = "Living space (inverse)",
  "sei_n_rent"          = "Rent (inverse)",
  "csi_n"               = "CSI")

# Output table
lst_cor_df <- as.data.frame(t(lst_cor_mat)) %>%
  rownames_to_column(var = "variable") %>%
  rename(spearman_rho = V1) %>%
  mutate(indicator = ifelse(variable %in% names(label_map),
                       label_map[variable], variable)) %>%
  select(indicator, spearman_rho) %>%
  mutate(indicator = factor(indicator,
      levels = c("Elderly (65+)", "Children (<6)", "Single households",
                 "Single parents","Foreign-born", "Living space (inverse)",
                 "Rent (inverse)", "CSI")),
  spearman_rho = round(spearman_rho, 3) ) %>% arrange(indicator)

# Preview and save correlation table
lst_cor_df
write_csv(lst_cor_df, file.path(main_results_path, "5_5_1_table_spearman_correlation.csv"))

# 6.2 Regression Analysis
# 6.2.1  Model fit incl. same-sample comparison

# Prepare regression data
df <- grid %>% st_drop_geometry() %>%
  select(lst_n_median, csi_n, sei_n_elderly, sei_n_children, sei_n_single_hh,
    sei_n_single_parent, sei_n_foreign_born, sei_n_space, sei_n_rent)

# Define label map
label_map <- c(
  "(Intercept)"         = "Intercept",
  "csi_n"               = "CSI",
  "sei_n_elderly"       = "Elderly (65+)",
  "sei_n_children"      = "Children (<6)",
  "sei_n_single_hh"     = "Single households",
  "sei_n_single_parent" = "Single parents",
  "sei_n_foreign_born"  = "Foreign-born",
  "sei_n_space"         = "Living space (inverse)",
  "sei_n_rent"          = "Rent (inverse)")

# Estimate main models
# Individual SEI model, complete-case sample across all SEI predictors
model_sei <- lm(
  lst_n_median ~ sei_n_elderly + sei_n_children + sei_n_single_hh +
    sei_n_single_parent + sei_n_foreign_born + sei_n_space + sei_n_rent,
  data = df)

# CSI model, full available sample
model_csi <- lm(lst_n_median ~ csi_n, data = df)

# CSI model on same sample as SEI model
# Extract the actual complete-case data used by the SEI model
df_sei_sample <- model.frame(model_sei)

# Estimate CSI model on the same rows as the SEI model
# (requires adding csi_n from the original df)
df_common <- df %>% mutate(row_id = row_number()) %>%
  filter(row_id %in% as.integer(rownames(df_sei_sample)),
    !is.na(csi_n))

model_csi_common <- lm(lst_n_median ~ csi_n, data = df_common)

# Create model fit table
model_fit_table <- bind_rows(
  glance(model_sei) %>%
    transmute(Model = "LST ~ individual SEI", n = nobs,
             `R2` = round(r.squared, 3),
             `Adj. R2` = round(adj.r.squared, 3)),
  
  glance(model_csi) %>%
    transmute(Model = "LST ~ CSI", n = nobs,
              `R2` = round(r.squared, 3),
              `Adj. R2` = round(adj.r.squared, 3) ),
  
  glance(model_csi_common) %>%
    transmute(Model = "LST ~ CSI (same sample as SEI model)", n = nobs,
              `R2` = round(r.squared, 3),
              `Adj. R2` = round(adj.r.squared, 3)))

# Preview
model_fit_table

# 6.2.2 Multivariable regression (LST ~ SEIs)

multivariable_sei_results <- tidy(model_sei, conf.int = TRUE) %>%
  mutate( term = ifelse(term %in% names(label_map), label_map[term], term)) %>%
  filter(term != "Intercept") %>%
  transmute(
    Predictor = term,
    Estimate = round(estimate, 3),
    `Std. Error` = round(std.error, 3),
    `95% CI` = paste0(
      "[", round(conf.low, 3), ", ", round(conf.high, 3),"]"),
    `p-value` = case_when(p.value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p.value, 3))))

# Preview
multivariable_sei_results

# 6.2.3 Bivariate regressions (LST ~ each SEI)

sei_vars <- c("sei_n_elderly", "sei_n_children", "sei_n_single_hh",
              "sei_n_single_parent", "sei_n_foreign_born",
              "sei_n_space", "sei_n_rent")

bivariate_sei_results <- lapply(sei_vars, function(var) {
  
  formula_i <- as.formula(paste("lst_n_median ~", var))
  model_i <- lm(formula_i, data = df)
  
  coef_i <- tidy(model_i, conf.int = TRUE) %>%
    filter(term == var) %>%
    mutate(term = ifelse(term %in% names(label_map), label_map[term], term)) %>%
    transmute(
      Predictor = term, n = nobs(model_i), Estimate = round(estimate, 3),
      `Std. Error` = round(std.error, 3),
      `95% CI` = paste0("[",round(conf.low, 3), ", ",round(conf.high, 3),"]"),
      `p-value` = case_when(
        p.value < 0.001 ~ "<0.001",TRUE ~ as.character(round(p.value, 3))),
      `R2` = round(glance(model_i)$r.squared, 3))
  
  return(coef_i)}) %>% bind_rows()

# Preview
bivariate_sei_results

# 6.3 Model diagnostics
# 6.3.1 Moran's I on residuals

# Function to calculate Moran's I for model residuals
calculate_residual_moran <- function(grid_data, model, k = 8) {
  coords <- st_coordinates(st_centroid(grid_data))
  nb <- knn2nb(knearneigh(coords, k = k))
  lw <- nb2listw(nb, style = "W")
  moran.test(residuals(model), lw)}

# Prepare spatial samples
# Individual SEI model sample
grid_sei <- grid %>%
  filter(!is.na(lst_n_median),
    if_all(c(sei_n_elderly, sei_n_children, sei_n_single_hh, sei_n_single_parent,
        sei_n_foreign_born, sei_n_space, sei_n_rent),~ !is.na(.)))

# CSI model sample
grid_csi <- grid %>% filter(!is.na(lst_n_median), !is.na(csi_n))

# CSI model on same sample as SEI model
grid_csi_common <- grid_sei %>% filter(!is.na(csi_n))

# Define models and samples
moran_models <- tibble(
  Model = c(
    "LST ~ individual SEI", "LST ~ CSI",
    "LST ~ CSI (same sample as SEI model)"),
  model = list(model_sei, model_csi, model_csi_common),
  grid_data = list(grid_sei, grid_csi, grid_csi_common))

# Calculate Moran's I
moran_diagnostics_table <- moran_models %>%
  mutate(moran = map2(grid_data, model, calculate_residual_moran),
    n = map_int(model, nobs),
    Morans_I = map_dbl(
      moran,
      ~ unname(.x$estimate["Moran I statistic"])),
    p_value = map_dbl(moran, ~ .x$p.value)) %>%
  transmute(
    Model, n, Morans_I = round(Morans_I, 3),
    `p-value` = if_else(p_value < 0.001, "<0.001",
      as.character(round(p_value, 3))))

# View table
moran_diagnostics_table

# 6.3.2 VIF diagnostic

vif_table <- tibble(
  Predictor = names(car::vif(model_sei)),
  VIF = as.numeric(car::vif(model_sei))) %>%
  mutate(Predictor = ifelse(Predictor %in% names(label_map), label_map[Predictor], 
                            Predictor), VIF = round(VIF, 2))

# Preview regression results and model diagnostics
print(model_fit_table)
print(multivariable_sei_results)
print(moran_diagnostics_table)

print(bivariate_sei_results)
print(vif_table)

# Save regression results and model diagnostics
write_csv(model_fit_table, file.path(main_results_path, "5_5_2_01_table_model_fit.csv"))
write_csv(multivariable_sei_results, file.path(main_results_path, "5_5_2_02_multivariable_regression_results.csv"))
write_csv(moran_diagnostics_table, file.path(main_results_path, "5_5_3_01_moran_residuals.csv"))

write_csv(bivariate_sei_results, file.path(appendix_path, "appendix_05_bivariate_regression_results.csv"))
write_csv(vif_table, file.path(appendix_path, "appendix_06_vif_diagnostic.csv"))

# -----------------------------
# 7. Plots: Exposure-sensitivity layer
# -----------------------------

# 7.1 Create quantile classes for ESI and CESI

esi_cols <- c(
  "esi_n_elderly", "esi_n_children", "esi_n_single_hh",
  "esi_n_single_parent", "esi_n_foreign_born", "esi_n_space", "esi_n_rent")

esi_cols_final <- c(esi_cols, "cesi_n")

# Create quantile classes
for (col in esi_cols_final) {
  grid[[paste0(col, "_q_class")]] <- quantile_class(grid[[col]])}

# Ensure identical legend order
for (col in paste0(esi_cols_final, "_q_class")) {
  grid[[col]] <- factor(grid[[col]], levels = cat_names, ordered = TRUE)}

# 7.2 Plot: CESI

p_cesi <- ggplot() +
  geom_sf(
    data = grid %>% filter(!is.na(cesi_n_q_class)),
    aes(fill = cesi_n_q_class), color = NA) +
  geom_sf(data = berlin, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_manual(
    values = c(
      "Very low"  = "#000080",
      "Low"       = "#5ab0ff",
      "Medium"    = "#ffea00",
      "High"      = "#ff7a00",
      "Very high" = "#b10026"),
    name = "CESI") +
  coord_sf(expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering) +
  map_theme()

# Preview
p_cesi

# Export plot 
ggsave( filename = file.path(main_results_path, "5_6_02_figure_cesi.png"), 
        plot = p_cesi, width = 130, 
        height = 95, units = "mm", dpi = 300)

# 7.3 Plot: Individual ESI (facet map)

esi_class_cols <- paste0(esi_cols, "_q_class")

esi_labels <- c("a) Elderly (65+)", "b) Children (<6)", "c) Single households",
  "d) Single parents", "e) Foreign-born", "f) Living space (inverse)",
  "g) Rent (inverse)")

grid_long_esi <- grid %>%
  select(grid_id, geom, all_of(esi_class_cols)) %>%
  pivot_longer(
    cols = all_of(esi_class_cols),
    names_to = "variable",
    values_to = "class") %>%
  mutate(
    variable = gsub("_q_class", "", variable),
    variable = factor(variable, levels = esi_cols, labels = esi_labels),
    class = factor(class, levels = cat_names, ordered = TRUE)) %>%
  filter(!is.na(class)) %>% st_as_sf(sf_column_name = "geom")

p_esi_facet <- ggplot() +
  geom_sf(data = grid_long_esi, aes(fill = class), color = NA) +
  geom_sf(data = berlin, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Very low"  = "#000080",
      "Low"       = "#5ab0ff",
      "Medium"    = "#ffea00",
      "High"      = "#ff7a00",
      "Very high" = "#b10026"),
    name = "ESI") +
  facet_wrap(~ variable, ncol = 2) +
  coord_sf(expand = FALSE) +
  map_theme()

# Preview
p_esi_facet

# Plot without legend
p_esi_facet_no_legend <- p_esi_facet + theme(legend.position = "none")

# Extract legend
esi_legend <- cowplot::get_legend(p_esi_facet)

# Add legend and north arrow 
p_esi_facet_final <- p_esi_facet_no_legend + 
  patchwork::inset_element(esi_legend, left = 0.53, bottom = 0.06, right = 0.81, 
                           top = 0.24, align_to = "full" ) + 
  patchwork::inset_element(north_arrow, left = 0.53, bottom = 0.80, 
                           right = 0.97, top = 0.98, align_to = "full")

# View and export plot 
p_esi_facet_final
ggsave(filename = file.path(main_results_path, "5_6_01_figure_esi_facet.png"), 
      plot = p_esi_facet_final, width = 130, 
      height = 200, units = "mm", dpi = 300)

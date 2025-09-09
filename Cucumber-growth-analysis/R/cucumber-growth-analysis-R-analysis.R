# ============================================================
# Title: Soil Properties vs Growth & Yield of Cucumis sativus
# Author: Wilson Brown Yomi
# Description: Statistical analysis of experimental data
# ============================================================

# -------------------------------
# 1. Load Required Packages
# -------------------------------
# Uncomment if packages are not installed
# install.packages(c("readxl", "janitor", "dplyr", "tidyr", "purrr"))

library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)

# -------------------------------
# 2. Import and Clean Data
# -------------------------------
data <- read_excel("data/Soil_growth_yield.xlsx")
data <- clean_names(data)
data <- na.omit(data)
data$week <- NULL

# -------------------------------
# 3. Correlation Analysis: Soil vs Growth
# -------------------------------
growth_data <- data[, c("sand", "clay", "silt", "p_h", "total_n", "tom", "toc", "total_p_mg_kg",
                        "na_cmol_kg", "k_cmol_kg", "ca2_cmol_kg", "mg2_cmol_kg", "ex_acidity_cmol_kg",
                        "cec_cmol_kg", "base_saturation", "fe_mg_kg", "mn_mg_kg",
                        "plant_height", "no_of_leaves", "stem_diameter")]

cor_growth <- cor(growth_data, method = "pearson", use = "complete.obs")
cor_growth_table <- as.data.frame(as.table(cor_growth))
colnames(cor_growth_table) <- c("Variable1", "Variable2", "Correlation")

soil_properties <- c("sand", "clay", "silt", "p_h", "total_n", "tom", "toc", "total_p_mg_kg",
                     "na_cmol_kg", "k_cmol_kg", "ca2_cmol_kg", "mg2_cmol_kg", "ex_acidity_cmol_kg",
                     "cec_cmol_kg", "base_saturation", "fe_mg_kg", "mn_mg_kg")
growth_parameters <- c("plant_height", "no_of_leaves", "stem_diameter")

filtered_growth <- cor_growth_table %>%
  filter(Variable1 %in% soil_properties & Variable2 %in% growth_parameters)

write.csv(filtered_growth, "results/correlation_soil_vs_growth.csv", row.names = FALSE)

# -------------------------------
# 4. Correlation Analysis: Soil vs Yield
# -------------------------------
yield_data <- data[, c("sand", "clay", "silt", "p_h", "total_n", "tom", "toc", "total_p_mg_kg",
                       "na_cmol_kg", "k_cmol_kg", "ca2_cmol_kg", "mg2_cmol_kg", "ex_acidity_cmol_kg",
                       "cec_cmol_kg", "base_saturation", "fe_mg_kg", "mn_mg_kg",
                       "days_to_flowering", "fruit_weight", "fruit_diameter", "fruit_length")]

cor_yield <- cor(yield_data, method = "pearson", use = "complete.obs")
cor_yield_table <- as.data.frame(as.table(cor_yield))
colnames(cor_yield_table) <- c("Variable1", "Variable2", "Correlation")

yield_metrics <- c("days_to_flowering", "fruit_weight", "fruit_diameter", "fruit_length")

filtered_yield <- cor_yield_table %>%
  filter(Variable1 %in% soil_properties & Variable2 %in% yield_metrics)

write.csv(filtered_yield, "results/correlation_soil_vs_yield.csv", row.names = FALSE)
# ============================================================
# Title: Visualization of Soil-Growth and Soil-Yield Correlations
# Author: Wilson Brown Yomi
# Description: Heatmaps, scatter plots, bar charts, and facet plots
# ============================================================

# -------------------------------
# 1. Load Required Packages
# -------------------------------
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)

# -------------------------------
# 2. Heatmap: Soil vs Growth Parameters
# -------------------------------
heatmap_data_growth <- filtered_growth %>%
  mutate(Correlation = as.numeric(gsub("\\*\\*", "", Correlation))) %>%
  pivot_wider(names_from = Variable2, values_from = Correlation)

melted_growth <- heatmap_data_growth %>%
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "value")

ggplot(melted_growth, aes(Variable2, Variable1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap: Soil vs Growth Parameters",
       x = "Growth Parameter", y = "Soil Property") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("results/heatmap_soil_vs_growth.png", width = 8, height = 6)

# -------------------------------
# 3. Heatmap: Soil vs Yield Parameters
# -------------------------------
heatmap_data_yield <- filtered_yield %>%
  pivot_wider(names_from = Variable2, values_from = Correlation)

melted_yield <- heatmap_data_yield %>%
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "value")

ggplot(melted_yield, aes(Variable2, Variable1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap: Soil vs Yield Parameters",
       x = "Yield Parameter", y = "Soil Property") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("results/heatmap_soil_vs_yield.png", width = 8, height = 6)

# -------------------------------
# 4. Scatter Plots: Strong Soil-Growth Correlations
# -------------------------------
strong_growth <- filtered_growth %>%
  filter(as.numeric(Correlation) >= 0.7)

for (i in seq_len(nrow(strong_growth))) {
  soil <- strong_growth$Variable1[i]
  growth <- strong_growth$Variable2[i]
  
  p <- ggplot(data, aes_string(x = soil, y = growth)) +
    geom_point(color = "blue", size = 3, alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = paste(soil, "vs", growth), x = soil, y = growth)
  
  ggsave(paste0("results/scatterplot_", soil, "_vs_", growth, ".png"), plot = p, width = 8, height = 6)
}

# -------------------------------
# 5. Bar Chart: Strongest Soil-Growth Correlations
# -------------------------------
filtered_growth %>%
  mutate(Abs_Correlation = abs(as.numeric(Correlation))) %>%
  arrange(desc(Abs_Correlation)) %>%
  ggplot(aes(x = reorder(Variable1, Abs_Correlation), y = Abs_Correlation, fill = Abs_Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "blue", high = "green") +
  theme_minimal() +
  labs(title = "Strongest Soil-Growth Correlations",
       x = "Soil Property", y = "Absolute Correlation")

ggsave("results/bar_chart_soil_vs_growth.png", width = 10, height = 8, dpi = 300)

# -------------------------------
# 6. Facet Plot: Soil vs Growth Parameters
# -------------------------------
scatter_data <- strong_growth %>%
  rowwise() %>%
  mutate(plot_data = list(data[, c(Variable1, Variable2)])) %>%
  unnest(plot_data)

ggplot(scatter_data, aes_string(x = "Variable1", y = "Variable2")) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  facet_wrap(~ Variable1 + Variable2, scales = "free") +
  theme_minimal() +
  labs(title = "Scatter Plots: Soil vs Growth Parameters",
       x = "Soil Property", y = "Growth Parameter")

ggsave("results/facet_plot_soil_vs_growth.png", width = 12, height = 8, dpi = 300)
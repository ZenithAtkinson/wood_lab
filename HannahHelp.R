#A: before starting, make sure these are all installed via the install.packages(" ") command
library(dplyr)
library(tidyr)
library(ggplot2)
library(umap)
library(ggpubr)
  a

#data <- read.csv("./Phrag_ICP_HBreinholt_Soil properties(Phrag_ICP_HBreinholt).csv")
data <- read.csv("./Phrag_ICP_HBreinholt_Soil properties (Phrag_ICP_HBreinholt).csv")

numeric_data <- data[sapply(data, is.numeric)]
data_scaled <- scale(numeric_data)

umap_results <- umap(data_scaled)
plot(umap_results$layout, main="UMAP Projection")

set.seed(123)  #seed for reproducibility
k <- 3  # choose the number of clusters
kmeans_results <- kmeans(umap_results$layout, centers = k)
plot(umap_results$layout, col = kmeans_results$cluster, pch = 19,
     main="UMAP Projection with k-means Clusters")

selected_data <- data[, c("Ca3179", "P_1782", "S_1820")]

selected_data_scaled <- scale(selected_data)

umap_results <- umap(selected_data_scaled)

set.seed(123)  # For reproducibility
k <- 3  # Choose your number of clusters
kmeans_results <- kmeans(umap_results$layout, centers = k)

selected_data$cluster <- kmeans_results$cluster

# To view data points in cluster 1:
cluster1 <- subset(selected_data, cluster == 1)
head(cluster1)

library(dplyr)
selected_data %>%
  group_by(cluster) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
#cluster Ca3179 P_1782 S_1820
#.    <int>  <dbl>  <dbl>  <dbl>
# 1     1    409.   5.00   25.8
# 2     2    177.   3.45   7.96
# 3     3    401.   2.93   5.16

plot(umap_results$layout, col = selected_data$cluster, pch = 19,
     main = "UMAP Projection Based on Three Variables")

library(ggplot2)

umap_df <- data.frame(
  UMAP1 = umap_results$layout[,1],
  UMAP2 = umap_results$layout[,2],
  cluster = factor(selected_data$cluster)
)

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "UMAP Projection with Clusters")

final_data <- data.frame(
  SampleID = data$sample,          # or the correct column for sample names
  Ca3179 = data$Ca3179,
  P_1782 = data$P_1782,
  S_1820 = data$S_1820,
  cluster = selected_data$cluster
)
#------- BOX PLOTS ---------------------------------

# Suppose your data is called 'selected_data' and has columns:
# Ca3179, P_1782, S_1820, and cluster

# Convert from "wide" to "long" format
df_long <- selected_data %>%
  pivot_longer(
    cols = c("Ca3179", "P_1782", "S_1820"),
    names_to = "variable",
    values_to = "value"
  )

# Make a boxplot, faceted by variable
ggplot(df_long, aes(x = factor(cluster), y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  labs(
    x = "Cluster",
    y = "Value",
    title = "Boxplots of Variables by Cluster"
  )
#########################. Regression Comparison Scatter Plots With Line #######################################

lm_model <- lm(P_1782 ~ Ca3179, data = data)
coefs <- coef(lm_model)
eq_text <- paste0("y = ", round(coefs[1], 2), " + ", round(coefs[2], 2), "x")

#Extract R-squared and P-value
model_summary <- summary(lm_model)
r_squared <- model_summary$r.squared
r2_text <- paste("R² =", round(r_squared, 2))
p_value <- model_summary$coefficients[2, 4]
p_text <- paste("p =", format.pval(p_value, digits = 3, scientific = TRUE))

#Combine into annotation text
annotation_text <- paste(eq_text, r2_text, p_text, sep = "\n")

#Scatterplot with Regression Line and Statistics
ggplot(data, aes(x = Ca3179, y = P_1782)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  annotate("text", 
           x = Inf, 
           y = Inf, 
           label = annotation_text,
           hjust = 1.1,  # adjust to shift the text slightly inside from the right
           vjust = 1.1,  # adjust to shift the text slightly inside from the top
           size = 5, 
           color = "blue") +
  labs(
    x = "Ca3179", 
    y = "P_1782",
    title = "Regression of P_1782 vs. Ca3179"
  ) +
  theme_minimal()
###### ----------------------------------------- ################
# Read in the phragmites dataset with category info
phrag_data <- read.csv("Phrag_ICP_HBreinholt_Soil+Elemental_data (1)(Phrag_ICP_HBreinholt_Soil+Eleme).csv")

# Ensure the 'sample' column is character in both datasets
phrag_elem_data <- phrag_data %>% mutate(sample = as.character(sample))
combined_data <- combined_data %>% mutate(sample = as.character(sample))

# Select only the 'sample' and 'Phrag Present' columns from the new dataset.
phrag_present_subset <- phrag_elem_data %>%
  select(sample, Phrag.Present)

# Now join with your existing combined_data based on the 'sample' column.
final_data <- left_join(combined_data, phrag_present_subset, by = "sample")

# View the first few rows of the final dataset
head(final_data)

# 1. Summary Statistics -----------------------------------------------------
summary_stats <- final_data %>%
  group_by(`Phrag.Present`) %>%
  summarise(across(c(Ca3179, P_1782, S_1820),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE))))
print(summary_stats)

# 2. Visualization: Boxplots by Phragmites Presence -------------------------
# Reshape data into long format for plotting
long_data <- final_data %>%
  pivot_longer(cols = c(Ca3179, P_1782, S_1820),
               names_to = "Element", values_to = "Concentration")

ggplot(long_data, aes(x = `Phrag.Present`, y = Concentration, fill = `Phrag.Present`)) +
  geom_boxplot() +
  facet_wrap(~ Element, scales = "free") +
  labs(title = "Elemental Concentrations by Phragmites Presence",
       x = "Phragmites Presence", y = "Concentration") +
  theme_minimal()

# 3. Hypothesis Testing -----------------------------------------------------
# Since "Phrag Present" has two groups ("Yes" and "No"), a t-test is appropriate.
t_test_results <- long_data %>%
  group_by(Element) %>%
  summarise(test = list(t.test(Concentration ~ `Phrag.Present`, data = cur_data())))

# To view the t-test results for each element:
t_test_results %>% 
  rowwise() %>% 
  mutate(result = list(test)) %>% 
  print()

# 4. Multivariate Analysis: MANOVA -----------------------------------------
# Test whether the combination of the three elements differs by Phragmites presence
manova_model <- manova(cbind(Ca3179, P_1782, S_1820) ~ `Phrag.Present`, data = final_data)
summary(manova_model, test = "Pillai")

# 5. Exploratory Analysis: PCA ---------------------------------------------
# Perform PCA on the three elements and color-code by Phragmites presence
pca_model <- prcomp(final_data %>% select(Ca3179, P_1782, S_1820), scale. = TRUE)
pca_data <- as.data.frame(pca_model$x)
pca_data$`Phrag.Present` <- final_data$`Phrag.Present`

ggplot(pca_data, aes(x = PC1, y = PC2, color = `Phrag.Present`)) +
  geom_point(size = 3) +
  labs(title = "PCA of Elemental Data", x = "PC1", y = "PC2") +
  theme_minimal()

##########################################################################################
# Plot
ggplot(df_long, aes(x = sample, y = Value, color = Element, group = Element)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(title = "Comparison of Ca3179, P_1782, and S_1820 across Samples",
       x = "Sample", y = "Concentration")

ggplot(df_long, aes(x = Element, y = Value, fill = Element)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Ca3179, P_1782, and S_1820",
       x = "Element", y = "Concentration")

ggplot(df_long, aes(x = sample, y = Value, fill = Element)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of Elements per Sample",
       x = "Sample", y = "Concentration")
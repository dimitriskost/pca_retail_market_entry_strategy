# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCA for Market Expansion Strategy
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Dependencies              ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Necessary Packages
# install.packages(c("eurostat", "tidyverse", "janitor", "psych", "corrplot", "ggrepel"))

library(eurostat)
library(tidyverse)
library(janitor)
library(psych)
library(corrplot)
library(ggrepel)

# Choose a recent year
year <- 2018

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Eurostat Urban Indicators ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Available datasets
search_results <- search_eurostat("city")
head(search_results)

# Population
df_pop <- get_eurostat("urb_cpop1", time_format = "num")

# Unemployment
df_unemp <- get_eurostat("urb_clma", time_format = "num")

# Education
df_educ <- get_eurostat("urb_ceduc", time_format = "num")

# Tourism (proxy for activity / attractiveness)
df_tourism <- get_eurostat("urb_ctour", time_format = "num")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Data Wrangling
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_pop <- df_pop %>% 
  filter(TIME_PERIOD == year & indic_ur == "DE1001V") %>%
  select(indic_ur, cities, values)

df_unemp <- df_unemp %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

df_educ <- df_educ %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

df_tourism <- df_tourism %>% 
  filter(TIME_PERIOD == year) %>%
  select(indic_ur, cities, values)

# Merge the dataset
df <- rbind(df_pop, df_unemp, df_educ, df_tourism)

# Update the labels
df <- label_eurostat(df)

# Remove indicators that are available for the male-female population
df <- df %>%
  filter(!str_detect(indic_ur, regex("male|female", ignore_case = TRUE)))

# Bring to tidy format
df <- pivot_wider(df,
                  names_from = indic_ur,
                  values_from = values)

# Keep only the variables with low missing values
df <- df[, colMeans(is.na(df)) < 0.30]

# Keep cities with no missing values
df <- df[rowMeans(is.na(df)) == 0, ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 1: Summary statistics and data visualization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

boxplot(df$`Population on the 1st of January, total`)
hist(df$`Population on the 1st of January, total`)
# Extreme outliers, our data might require further cleaning

max(df$`Population on the 1st of January, total`) 
# [1] 66732538, No city on earth has that many residents!

df %>%
  slice_max(`Population on the 1st of January, total`, n=2) %>%
  pull(cities)
# Returns France and Romania!. Now lets update our data frame

df <- df %>%
  arrange(desc(`Population on the 1st of January, total`)) %>%
  slice(-(1:2))

########~           Summary Statistics           ~########

df_numeric <- apply(X = df[, 2:28], MARGIN = 2, FUN = as.numeric)
# Turning df into a numeric data frame for computation purposes. Cities names 
# were also removed.

summary_stats <- data.frame(
  Mean = format(round(colMeans(df_numeric), 2), scientific = FALSE),
  SD = format(round(apply(df_numeric, 2, sd), 2), scientific = FALSE),
  Min = format(round(apply(df_numeric, 2, min), 2), scientific = FALSE),
  Max = format(round(apply(df_numeric, 2, max), 2), scientific = FALSE)
)

view(summary_stats)

# Numeric format of summary statistics 
sum_stats_numeric <- apply(X = summary_stats, MARGIN = 2, FUN = as.numeric)

########~                Corrplot               ~#######

cor_matrix <- cor(df_numeric) 
cor_matrix_plot <- cor_matrix

# Replacing the names of our variables with numbers for a cleaner representation
# and keeping them for the report's dictionary.
colnames(cor_matrix_plot) <- 1:ncol(cor_matrix_plot)
rownames(cor_matrix_plot) <- 1:nrow(cor_matrix_plot)

variable_dictionary <- data.frame(
  No = 1:ncol(cor_matrix),
  Indicator = colnames(cor_matrix)
)

corrplot(cor_matrix_plot, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.cex = 0.8,
         diag = FALSE
         )

# A quick look on the variables with the highest variance 
head(sort(sum_stats_numeric[,2], decreasing = TRUE))
# [1] 5596876.1  750336.5  393496.7  375088.7  333469.6  198553.9

# And lowest
head(sort(sum_stats_numeric[,2], decreasing = FALSE))
# [1]  4.05  6.99  9.38 13.18 16.67 34.27. Huge deviation


########~            Correlations Testing            ~########


# Bartlett test (Psych library) testing the null hypothesis of no correlation 
# between our variables (correlation matrix is an identity matrix) 

bartlett_test <- cortest.bartlett(cor_matrix, n = nrow(df_numeric))

bartlett_test$p.value

# p_value = 0 so we reject the null hypothesis on every statistical significance level

# =======================

# Kaiser, Meyer, Olkin (KMO) Measure of Sampling Adequacy

kmo <- KMO(cor_matrix)

kmo$MSA
# [1] 0.8850169, "Mertitourious", almost marvelous sampling adequacy


# Both tests indicate a strong correlation between our variables making the 
# implementation of PCA as an effective way of reducing the dimensions of
# our problem.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 2: PCA for standardized data           ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PCA on correlation matrix
m2 <- prcomp(df_numeric, center = TRUE, scale = TRUE)
m2
summary(m2) 

m2$sdev # square root of the eigenvalues
pca_var <- m2$sdev^2 

# The proportion of variance interpreted by each component is:
pve <- pca_var / sum(pca_var) 
format(pve, scientific = FALSE, digits = 3)

m2$rotation # eigenvector matrix (loadings)
m2$center

# Scree plot
bp <- barplot(pve, xlab = "Principal Component", 
              ylab = "Proportion of Variance Explained", 
              ylim = c(0, 1), xlim = c(0,15),
              col = "royalblue", lwd = 3,
              main = "Scree Plot", names.arg = 1:length(pve))

lines(bp, pve, type = "o", col = "black", lwd = 2, pch = 19)

text(x = bp, y = pve, label = paste0(round(pve * 100, 1), "%"), 
     pos = 3, cex = 0.8, col = "black")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 3: Components number and scoring       ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

########~      Choosing the appropriate number of PCs     ~#######

# Rule of thumb: Percentage of variance explained greater than 80%
eightyrule <- which(cumsum(pve) > 0.8)[1] 
# The first time the cumulative sum is > 0.8
eightyrule
#[1] 3

# Kaiser criterion: How many of these components have an eigenvalue > 1
kaisercomp <- sum(pca_var > 1)
kaisercomp
# [1] 4

########~               PC1 & PC2 Loadings               ~#######

loadings_pc1_pc3 <- round(m2$rotation[, 1:3], 3)

# Top 6 indicators behind PC1
head(loadings_pc1_pc3[order(-abs(loadings_pc1_pc2[, 1])), 1])

# Top 6 indicators behind PC2
head(loadings_pc1_pc3[order(-abs(loadings_pc1_pc2[, 2])), 2])

# Top 6 indicators behind PC3
head(loadings_pc1_pc3[order(-abs(loadings_pc1_pc3[, 3])), 3])

########~               Ranking each city                ~#######

city_scores <- data.frame(
  City = df$cities,
  PC1 = m2$x[, 1],
  PC2 = m2$x[, 2],
  PC3 = m2$x[, 3]
)

# Top 6 markets by size (PC1)
head(city_scores[order(city_scores$PC1), c("City", "PC1")])

# Top 6 markets by tourism (PC2)
head(city_scores[order(city_scores$PC2), c("City", "PC2")])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 5: Grouping by the PCA results         ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

key_cities <- c( # Top by PC1
                "Paris (greater city)", "Madrid (greater city)", "Berlin",
                 # Top by PC2
                "Benidorm", "Torremolinos (greater city)", "Fréjus (greater city)",
                 # Northern Cities
                "Frankfurt am Main", "Helsinki/Helsingfors", "Hamburg",
                 # Southern cities
                "Nice (greater city)", "Málaga", "Marseille (greater city)"
                ) 

city_scores$Label <- ifelse(city_scores$City %in% key_cities, as.character(city_scores$City), "")

# Scatter plot of the cities by the first 2 PC scores 
ggplot(city_scores, aes(x = PC1, y = PC2)) +
  # Scattered cities as points
  geom_point(color = "royalblue", size = 3, alpha = 0.7) +
  # Adding axis for directions
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey20") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray20") +
  # Text labels, ggrepel package 
  geom_text_repel(aes(label = Label), size = 3.5, fontface = "bold", 
                  color = "darkred", max.overlaps = Inf) +
  labs(title = "Where the European cities stand",
       x = "PC1: Market Size",
       y = "PC2: Tourism Intensity") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 6: Ranking using a composite index   ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pve
# We remind the proportion of explained variance by each component is stored in 
# pve and we concluded on using 3 components. Each one has a different proportion,
# so it is sound to introduce weights for each one:

w1 <- pve[1] 
w2 <- pve[2]
w3 <- pve[3]

city_scores$Composite_Score <- ((-1) * city_scores$PC1 * w1 +
                                (-1) * city_scores$PC2 * w2 + 
                                (-1) * city_scores$PC3 * w3)
# -1 because we decided earlier on that high score cities will be on the negative

ranked_cities <- city_scores[order(-city_scores$Composite_Score), ]

# Top 6 most attractive cities
head(ranked_cities[, c("City", "Composite_Score")])

# Bottom 6 attractive cities
tail(ranked_cities[, c("City", "Composite_Score")])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Task 7: PCA for non-standardized data     ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PCA on covariance matrix
m1 <- prcomp(df_numeric, center = TRUE, scale = FALSE)
options(scipen = 999)
print(summary(m1), digits = 3)


########~               PC1 & PC2 Loadings               ~#######

loads_pc1_pc2_tilde <- round(m1$rotation[, 1:2], 3)

# Top 6 indicators behind PC1
head(loads_pc1_pc2_tilde[order(-abs(loads_pc1_pc2_tilde[, 1])), 1])

# Top 6 indicators behind PC2
head(loads_pc1_pc2_tilde[order(-abs(loads_pc1_pc2_tilde[, 2])), 2])

city_scores_non_std <- data.frame(
  City = df$cities,  # Άλλαξε το 'df' με το όνομα του αρχικού σου dataset αν χρειάζεται
  PC1 = m1$x[, 1],
  PC2 = m1$x[, 2]
)

# Labels
city_scores_non_std$Label <- ifelse(city_scores_non_std$City %in% key_cities, 
                                    as.character(city_scores_non_std$City), "")

# Scatter plot for non-standardized data
ggplot(city_scores_non_std, aes(x = PC1, y = PC2)) +
  geom_point(color = "tomato", size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey20") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray20") +
  geom_text_repel(aes(label = Label), size = 3.5, fontface = "bold", 
                  color = "darkred", max.overlaps = Inf) +
  labs(title = "PCA on Non-Standardized Data",
       x = "PC1",
       y = "PC2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



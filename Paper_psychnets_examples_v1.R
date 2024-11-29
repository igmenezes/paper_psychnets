###############################################################################
# Paper: HRMJ - Psychological Networks: A Novel Methodology for Enhancing Insights and Practice in People Analytics
###############################################################################

# Loading required libraries
library(igraph)
library(sna)
library(mirt)
library(qgraph)
library(bootnet)
library(bnlearn)
library(tidyverse)
library(CliquePercolation)

###############################################################################
# Figure 1 - Comparison between Social Network Analysis (SNA) and a Psychological Network
###############################################################################

setwd("add your working directory")

# Set seed for reproducibility
set.seed(95)

# Generate and plot a random graph using SNA methods
gnp_Graph <- sample_gnp(10, 0.4, directed = FALSE, loops = FALSE) %>%
  set_vertex_attr("color", value = "lightgrey")
plot(gnp_Graph)

# Importing dataset for psychological network analysis
ATD <- read.csv("dataset_example.csv", sep = ",", header = TRUE)

# Select dimensions for psychological network
ATD_1 <- ATD[, -1]
ps_learn <- ATD_1[, 1:12]
ps_chal <- ATD_1[, 13:24]
ps_dif <- ATD_1[, 25:36]
connect_iden <- ATD_1[, 37:48]

# Step 1: Model calibration for each dimension using Graded Response Model (GRM)
ps_learn_mirt <- mirt(ps_learn, 1, 'graded')
ps_chal_mirt <- mirt(ps_chal, 1, 'graded')
ps_dif_mirt <- mirt(ps_dif, 1, 'graded')
connect_iden_mirt <- mirt(connect_iden, 1, 'graded')

# Step 2: Compute factor scores (FS) for each dimension
FS_ps_learn_mirt <- fscores(ps_learn_mirt, method = "EAP", full.scores = TRUE)
FS_ps_chal_mirt <- fscores(ps_chal_mirt, method = "EAP", full.scores = TRUE)
FS_ps_dif_mirt <- fscores(ps_dif_mirt, method = "EAP", full.scores = TRUE)
FS_connect_iden_mirt <- fscores(connect_iden_mirt, method = "EAP", full.scores = TRUE)

# Rename columns for ease of interpretation
colnames(FS_ps_learn_mirt) <- "Vision"
colnames(FS_ps_chal_mirt) <- "PartSafety"
colnames(FS_ps_dif_mirt) <- "TaskOrient"
colnames(FS_connect_iden_mirt) <- "Innovation"

# Combine factor scores into a single data frame
Team1All <- cbind(FS_ps_learn_mirt, FS_ps_chal_mirt, FS_ps_dif_mirt, FS_connect_iden_mirt)

# Compute correlation matrix and visualize the psychological network
corMat <- cor_auto(Team1All) 
Graph_lasso <- qgraph(
  corMat, graph = "glasso", layout = "spring", tuning = 0.25,
  borders = TRUE, posCol = 'darkblue', negCol = 'darkred',
  vsize = 14, esize = 14, legend.cex = 0.8, sampleSize = nrow(Team1All),
  edge.labels = TRUE, bidirectional = TRUE, directed = FALSE
)

###############################################################################
# Figures 2 and 3 - Psychological Network and Centrality Measure (Strength)
###############################################################################

# Import data and select dimensions for network analysis
ATD <- read.csv("dataset_example_Figure_2.csv", sep = ",", header = TRUE)
ATD_1 <- ATD[, 4:111]

# Define dimensions for the psychological network
ps_learn <- ATD_1[, 1:12]
ps_chal <- ATD_1[, 13:24]
ps_dif <- ATD_1[, 25:36]
# Other dimensions omitted for brevity

# Model calibration, factor score computation, and column renaming for all dimensions

Lern_data <- ATD_1[, 1:12]
Chl_data <- ATD_1[, 13:24]
Dff_data <- ATD_1[, 25:36]
Idn_data <- ATD_1[, 37:48]
Chs_data <- ATD_1[, 49:60]
Hlp_data <- ATD_1[, 61:72]
Lrnn_data <- ATD_1[, 73:84]
Ots_data <- ATD_1[, 85:96]
Rsl_data <- ATD_1[, 97:108]

# Step 2: List of all datasets
General <- list(
  Lern = Lern_data,
  Chl = Chl_data,
  Dff = Dff_data,
  Idn = Idn_data,
  Chs = Chs_data,
  Hlp = Hlp_data,
  Lrnn = Lrnn_data,
  Ots = Ots_data,
  Rsl = Rsl_data
)

# Step 3: Initialize lists to store models and factor scores
models <- list()
factor_scores <- list()

# Step 4: Loop through each dataset, calibrate the model, and compute factor scores
for (dim_name in names(General)) {
  # Fit GRM model
  model <- mirt(General[[dim_name]], 1, 'graded')
  models[[dim_name]] <- model
  
  # Compute factor scores
  scores <- fscores(model, method = "EAP", full.scores = TRUE)
  factor_scores[[dim_name]] <- scores
  
  # Rename the column to reflect the dimension
  colnames(factor_scores[[dim_name]]) <- paste0(dim_name, "")
}

# Step 5: Combine all factor scores into a single dataset
combined_factor_scores <- do.call(cbind, factor_scores)
  
# Step 6: Estimate and visualize psychological network with Spearman correlations

corMatSpearman <- cor(combined_factor_scores, method = "spearman")
Groups <- c(rep("Psychological safety", 3), rep("Connectedness", 3), rep("Growth Mindset", 3))

model1 <- qgraph(
  corMatSpearman, groups = Groups, graph = "pcor",
  posCol = 'darkblue', negCol = 'darkred', layout = 'spring',
  vsize = 7, sampleSize = nrow(General), directed = FALSE,
  edge.labels = TRUE
)

centralityPlot(
  model1, include = c("Strength", "Betweenness", "Closeness"),
  orderBy = "Strength", weighted = TRUE
)

###############################################################################
# Figure 4 - Bayesian Networks
###############################################################################

# Prepare data for Bayesian network analysis
df_bayesian <- as.data.frame(General)
df_bayesian <- df_bayesian[complete.cases(df_bayesian), ]

# Fit a Bayesian network using Hill-Climbing (HC) algorithm
res <- bnlearn::hc(df_bayesian)
fittedbn <- bn.fit(res, data = df_bayesian)

# Strength plot for Bayesian Network
pvalues <- arc.strength(res, df_bayesian)
ss <- strength.plot(res, strength = pvalues, main = "Bayesian Network", layout = "twopi", threshold = 1)

###############################################################################
# Figure 5 - Community Detection and Clique Percolation
###############################################################################

# Estimate network and visualize community structure
n1 <- bootnet::estimateNetwork(ATD[, c(10:65)], default = "EBICglasso")
g1 <- plot(
  n1, layout = "spring", vsize = 6, cut = 0,
  title = "Community Detection", legend.cex = 0.5
)

# Clique Percolation
W <- qgraph(n1$graph)
results <- cpAlgorithm(W, k = 3, method = "weighted", I = 0.12)
g3 <- cpColoredGraph(
  W, list.of.communities = results$list.of.communities.numbers,
  layout = g1$layout, theme = 'colorblind'
)

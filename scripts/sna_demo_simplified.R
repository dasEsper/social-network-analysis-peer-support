# Social Network Analysis Portfolio Demo
# Based on a simplified version of the original MRAD peer-support SNA workflow.
# This script uses synthetic data only. No original participant data are included.

# -------------------------------------------------------------------------
# 1. Setup
# -------------------------------------------------------------------------

library(dplyr)
library(readr)
library(igraph)
library(ggplot2)

set.seed(12345)

# If you run this file from the project root, these paths should work.
vertices_path <- "data/synthetic_vertices.csv"
academic_edges_path <- "data/synthetic_edges_academic_raw.csv"
nonacademic_edges_path <- "data/synthetic_edges_nonacademic_raw.csv"

dir.create("outputs", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# -------------------------------------------------------------------------
# 2. Load synthetic data
# -------------------------------------------------------------------------

vertices <- read_csv(vertices_path, show_col_types = FALSE)
edges_academic_raw <- read_csv(academic_edges_path, show_col_types = FALSE)
edges_nonacademic_raw <- read_csv(nonacademic_edges_path, show_col_types = FALSE)

glimpse(vertices)
glimpse(edges_academic_raw)
glimpse(edges_nonacademic_raw)

# -------------------------------------------------------------------------
# 3. Data cleaning
# -------------------------------------------------------------------------
# The original project cleaned invalid IDs such as 0, 106, and 107,
# and removed edges with zero total interactions.
# This synthetic version keeps the same logic in a simplified format.

valid_ids <- vertices$id

edges_academic_clean <- edges_academic_raw %>%
  filter(
    from %in% valid_ids,
    to %in% valid_ids,
    total_a > 0
  )

edges_nonacademic_clean <- edges_nonacademic_raw %>%
  filter(
    from %in% valid_ids,
    to %in% valid_ids,
    total_non > 0
  )

write_csv(edges_academic_clean, "outputs/synthetic_edges_academic_clean.csv")
write_csv(edges_nonacademic_clean, "outputs/synthetic_edges_nonacademic_clean.csv")

cat("Academic edges before cleaning:", nrow(edges_academic_raw), "\n")
cat("Academic edges after cleaning:", nrow(edges_academic_clean), "\n")
cat("Non-academic edges before cleaning:", nrow(edges_nonacademic_raw), "\n")
cat("Non-academic edges after cleaning:", nrow(edges_nonacademic_clean), "\n")

# -------------------------------------------------------------------------
# 4. Create igraph objects
# -------------------------------------------------------------------------
# The original script created igraph objects for academic and non-academic
# interaction networks. Here, both networks are treated as undirected weighted
# graphs for simplified portfolio demonstration.

g_academic <- graph_from_data_frame(
  d = edges_academic_clean %>% rename(weight = total_a),
  vertices = vertices,
  directed = FALSE
)

g_nonacademic <- graph_from_data_frame(
  d = edges_nonacademic_clean %>% rename(weight = total_non),
  vertices = vertices,
  directed = FALSE
)

is_weighted(g_academic)
is_weighted(g_nonacademic)

# -------------------------------------------------------------------------
# 5. Network-level descriptive analysis
# -------------------------------------------------------------------------

network_summary <- tibble(
  network = c("Academic", "Non-academic"),
  vertices = c(vcount(g_academic), vcount(g_nonacademic)),
  edges = c(gsize(g_academic), gsize(g_nonacademic)),
  density = c(edge_density(g_academic, loops = FALSE),
              edge_density(g_nonacademic, loops = FALSE)),
  diameter = c(diameter(g_academic, directed = FALSE),
               diameter(g_nonacademic, directed = FALSE)),
  transitivity = c(transitivity(g_academic, type = "global"),
                   transitivity(g_nonacademic, type = "global"))
)

print(network_summary)
write_csv(network_summary, "outputs/network_summary.csv")

# -------------------------------------------------------------------------
# 6. Node-level centrality analysis
# -------------------------------------------------------------------------
# The original project used degree centrality and normalized degree centrality.
# This version also includes weighted strength to reflect interaction frequency.

centrality_academic <- tibble(
  id = V(g_academic)$name,
  degree_academic = degree(g_academic, normalized = TRUE),
  strength_academic = strength(g_academic, weights = E(g_academic)$weight)
)

centrality_nonacademic <- tibble(
  id = V(g_nonacademic)$name,
  degree_nonacademic = degree(g_nonacademic, normalized = TRUE),
  strength_nonacademic = strength(g_nonacademic, weights = E(g_nonacademic)$weight)
)

node_metrics <- vertices %>%
  left_join(centrality_academic, by = "id") %>%
  left_join(centrality_nonacademic, by = "id") %>%
  mutate(
    degree_academic = ifelse(is.na(degree_academic), 0, degree_academic),
    degree_nonacademic = ifelse(is.na(degree_nonacademic), 0, degree_nonacademic),
    strength_academic = ifelse(is.na(strength_academic), 0, strength_academic),
    strength_nonacademic = ifelse(is.na(strength_nonacademic), 0, strength_nonacademic)
  )

print(head(node_metrics))
write_csv(node_metrics, "outputs/node_metrics.csv")

# Top 10 most central students in the academic network
top_academic <- node_metrics %>%
  arrange(desc(degree_academic)) %>%
  select(id, class, specialization, peer_support_frequency, degree_academic, strength_academic) %>%
  slice_head(n = 10)

print(top_academic)
write_csv(top_academic, "outputs/top_academic_centrality.csv")

# -------------------------------------------------------------------------
# 7. Clique analysis
# -------------------------------------------------------------------------
# Clique analysis identifies subgroups in which all members are connected.
# In igraph, clique calculations ignore edge direction, so this simplified
# version uses undirected graphs.

academic_cliques <- max_cliques(g_academic)
nonacademic_cliques <- max_cliques(g_nonacademic)

clique_summary <- tibble(
  network = c("Academic", "Non-academic"),
  number_of_maximal_cliques = c(length(academic_cliques), length(nonacademic_cliques)),
  largest_clique_size = c(
    max(lengths(academic_cliques)),
    max(lengths(nonacademic_cliques))
  )
)

print(clique_summary)
write_csv(clique_summary, "outputs/clique_summary.csv")

# -------------------------------------------------------------------------
# 8. Community detection
# -------------------------------------------------------------------------
# The original script used clustering/community analysis to identify groups.
# Here we use Louvain community detection because it is fast and commonly used
# for weighted undirected networks.

community_academic <- cluster_louvain(g_academic, weights = E(g_academic)$weight)
community_nonacademic <- cluster_louvain(g_nonacademic, weights = E(g_nonacademic)$weight)

community_summary <- tibble(
  network = c("Academic", "Non-academic"),
  number_of_communities = c(length(unique(membership(community_academic))),
                            length(unique(membership(community_nonacademic)))),
  modularity = c(modularity(community_academic),
                 modularity(community_nonacademic))
)

print(community_summary)
write_csv(community_summary, "outputs/community_summary.csv")

# Add academic community membership to node metrics
node_metrics <- node_metrics %>%
  mutate(
    community_academic = membership(community_academic)[id],
    community_nonacademic = membership(community_nonacademic)[id]
  )

write_csv(node_metrics, "outputs/node_metrics_with_communities.csv")

# -------------------------------------------------------------------------
# 9. Simple hypothesis-style analysis
# -------------------------------------------------------------------------
# Portfolio demonstration:
# Does peer-support discussion frequency relate to academic degree centrality?

model_academic <- lm(degree_academic ~ peer_support_frequency, data = node_metrics)
model_nonacademic <- lm(degree_nonacademic ~ peer_support_frequency, data = node_metrics)

summary(model_academic)
summary(model_nonacademic)

# Save regression coefficients without requiring extra packages
extract_lm <- function(model, model_name) {
  coefs <- summary(model)$coefficients
  tibble(
    model = model_name,
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    statistic = coefs[, "t value"],
    p_value = coefs[, "Pr(>|t|)"]
  )
}

regression_summary <- bind_rows(
  extract_lm(model_academic, "Academic degree centrality"),
  extract_lm(model_nonacademic, "Non-academic degree centrality")
)

write_csv(regression_summary, "outputs/regression_summary.csv")
print(regression_summary)

# -------------------------------------------------------------------------
# 10. Visualization
# -------------------------------------------------------------------------

# Node shape by sex, color by specialization, size by academic degree centrality
V(g_academic)$node_size <- 8 + (degree(g_academic, normalized = TRUE) * 30)
V(g_academic)$node_shape <- ifelse(V(g_academic)$sex == "Male", "circle", "square")

png("figures/academic_network.png", width = 1200, height = 900)
plot(
  g_academic,
  layout = layout_with_fr(g_academic),
  vertex.size = V(g_academic)$node_size,
  vertex.label = V(g_academic)$name,
  vertex.label.cex = 0.7,
  vertex.color = as.factor(V(g_academic)$specialization),
  vertex.shape = V(g_academic)$node_shape,
  edge.width = E(g_academic)$weight / 2,
  edge.color = "gray80",
  main = "Synthetic Academic Interaction Network"
)
dev.off()

png("figures/nonacademic_network.png", width = 1200, height = 900)
plot(
  g_nonacademic,
  layout = layout_with_fr(g_nonacademic),
  vertex.size = 8 + (degree(g_nonacademic, normalized = TRUE) * 30),
  vertex.label = V(g_nonacademic)$name,
  vertex.label.cex = 0.7,
  vertex.color = as.factor(V(g_nonacademic)$specialization),
  vertex.shape = ifelse(V(g_nonacademic)$sex == "Male", "circle", "square"),
  edge.width = E(g_nonacademic)$weight / 2,
  edge.color = "gray80",
  main = "Synthetic Non-Academic Interaction Network"
)
dev.off()

# Centrality bar plot
centrality_plot <- node_metrics %>%
  arrange(desc(degree_academic)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(id, degree_academic), y = degree_academic)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Students by Academic Degree Centrality",
    x = "Student ID",
    y = "Normalized degree centrality"
  ) +
  theme_minimal()

ggsave("figures/top_academic_degree_centrality.png", centrality_plot, width = 8, height = 5)

cat("\nAnalysis complete. Check the outputs/ and figures/ folders.\n")

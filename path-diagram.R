
library(ggplot2)
library(ggraph)
library(tidygraph)
library(tidyverse)

#########################################
# CCI and Depression
##########################################
# Read coefficients
coefs <- read_csv("../../Output/Dta25 Output/Statistics/clpm_cci_coefs.csv")

# Create nodes
nodes <- data.frame(
  id = 1:4,
  label = c("GDS-15 (t-1)", "CCI (t-1)", 
            "GDS-15 (t)", "CCI (t)"),
  x = c(1, 1, 3, 3),
  y = c(2, 1, 2, 1),
  time = c("t-1", "t-1", "t", "t")
)

# Create edges with coefficients
edges <- data.frame(
  from = c(1, 2, 2, 1, 1),
  to = c(3, 4, 3, 4, 2),
  type = c("auto", "auto", "cross", "cross", "cov"),
  label = c(
    coefs %>% filter(path == "Dep_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Cog") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Dep_to_Cog") %>% pull(label),
    ""  # Covariance - no label
  )
)

# reposition
edges$label_dx <- c(0, 0, -0.35, -0.15, 0)  # x nudges for each edge
edges$label_dy <- c(0.08, 0.08, 0, 0.12, 0)  # y nudges

# allow different colors
edges$subtype <- edges$type
edges$subtype[edges$type == "cross" & edges$from == 1] <- "cross_1"
edges$subtype[edges$type == "cross" & edges$from == 2] <- "cross_2"

# correlation by hand (from STATA)
covar <- 0.471

# Create plot
p <- ggplot() +
  # Add boxes for nodes
  geom_rect(data = nodes, 
            aes(xmin = x - 0.4, xmax = x + 0.4,
                ymin = y - 0.3, ymax = y + 0.3,
                fill = time),
            color = "black", size = 1) +
  # Add node labels
  geom_text(data = nodes, 
            aes(x = x, y = y, label = label),
            size = 5.5, fontface = "plain",
            family = "Times New Roman") +
  # Add arrows
  geom_segment(data = edges %>% filter(type != "cov"),
               aes(x = nodes$x[from] + 0.4, 
                   y = nodes$y[from],
                   xend = nodes$x[to] - 0.4, 
                   yend = nodes$y[to]),
               color = "black",
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1.2) +
  # Add covariance (curved line)
  geom_curve(data = edges %>% filter(type == "cov"),
             aes(x = nodes$x[from], 
                 y = nodes$y[from] - 0.3,
                 xend = nodes$x[to], 
                 yend = nodes$y[to] + 0.3),
             curvature = 0.3,
             linetype = "dashed",
             color = "black",
             arrow = arrow(length = unit(0.2, "cm"), 
                           ends = "both", type = "closed")) +
  # Add edge labels
  geom_text(data = edges %>% filter(type != "cov"),
            aes(x = (nodes$x[from] + nodes$x[to]) / 2 + label_dx,
                y = (nodes$y[from] + nodes$y[to]) / 2 + 
                  ifelse(type == "cross", 
                         ifelse(from == 1, 0.15, -0.15), 0) + label_dy,
                label = label),
            fontface = "plain",
            bg = "white",
            family = "Times New Roman",
            size = 5) +
  # Covariance label
  geom_text(data = edges %>% filter(type == "cov"),
            aes(x = 0.84, y = 1.5, label = covar),
            size = 5, fontface = "italic",
            color = "black",
            family = "Times New Roman") +
  # Grayscale fills (RIGHT lighter)
  scale_fill_manual(values = c("t-1" = "grey85",
                               "t"   = "grey92")) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(20, 20, 20, 20),
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 18)) +
  coord_fixed(ratio = 1) +
  labs(title = "Cross-Lagged Panel Model: GDS-15 and CCI")

# Display
p

# Save
ggsave("../../Output/Dta25 Output/Figures/F4_mi_CLPM_CCI.tif", p,
       dpi = 500, width = 8, height = 8)


#########################################
# CCI_Inf and Depression
##########################################
# Read coefficients
coefs <- read_csv("../../Output/Stage 2 Output/Statistics/clpm_inf_coefs.csv")

# Create nodes
nodes <- data.frame(
  id = 1:4,
  label = c("Depressive\nSymptoms\n(t-1)", "Informant\nLabeling\n(t-1)", 
            "Depressive\nSymptoms\n(t)", "Informant\nLabeling\n(t)"),
  x = c(1, 1, 3, 3),
  y = c(2, 1, 2, 1),
  time = c("t-1", "t-1", "t", "t")
)

# Create edges with coefficients
edges <- data.frame(
  from = c(1, 2, 2, 1, 1),
  to = c(3, 4, 3, 4, 2),
  type = c("auto", "auto", "cross", "cross", "cov"),
  label = c(
    coefs %>% filter(path == "Dep_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Cog") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Dep_to_Cog") %>% pull(label),
    ""  # Covariance - no label
  )
)

# reposition
edges$label_dx <- c(0, 0, -0.35, -0.15, 0)  # x nudges for each edge
edges$label_dy <- c(0.08, 0.08, 0, 0.12, 0)  # y nudges

# allow different colors
edges$subtype <- edges$type
edges$subtype[edges$type == "cross" & edges$from == 1] <- "cross_1"
edges$subtype[edges$type == "cross" & edges$from == 2] <- "cross_2"

# correlation by hand (from STATA)
covar <- 0.290

# Create plot
p <- ggplot() +
  # Add boxes for nodes
  geom_rect(data = nodes, 
            aes(xmin = x - 0.4, xmax = x + 0.4,
                ymin = y - 0.3, ymax = y + 0.3,
                fill = time),
            color = "black", size = 1) +
  # Add node labels
  geom_text(data = nodes, 
            aes(x = x, y = y, label = label),
            size = 5, fontface = "plain") +
  # Add arrows
  geom_segment(data = edges %>% filter(type != "cov"),
               aes(x = nodes$x[from] + 0.4, 
                   y = nodes$y[from],
                   xend = nodes$x[to] - 0.4, 
                   yend = nodes$y[to],
                   color = subtype),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1.2) +
  # Add covariance (curved line)
  geom_curve(data = edges %>% filter(type == "cov"),
             aes(x = nodes$x[from], 
                 y = nodes$y[from] - 0.3,
                 xend = nodes$x[to], 
                 yend = nodes$y[to] + 0.3),
             curvature = -0.3,
             linetype = "dashed",
             color = "gray50",
             arrow = arrow(length = unit(0.2, "cm"), 
                           ends = "both", type = "closed")) +
  # Add edge labels
  geom_text(data = edges %>% filter(type != "cov"),
            aes(x = (nodes$x[from] + nodes$x[to]) / 2 + label_dx,
                y = (nodes$y[from] + nodes$y[to]) / 2 + 
                  ifelse(type == "cross", 
                         ifelse(from == 1, 0.15, -0.15), 0) + label_dy,
                label = label),
            fontface = "bold",
            bg = "white") +
  # Add edge label for covariance
  geom_text(data = edges %>% filter(type == "cov"),
            aes(x = 0.9, y = 1.5, label = covar),
            size = 3.5, fontface = "italic",
            color = "gray30") +
  # Styling
  scale_fill_manual(values = c("t-1" = "#E8F4F8", "t" = "#B3D9E6")) +
  scale_color_manual(values = c("auto" = "black", 
                                "cross_1" = "#FF6B6B",
                                "cross_2" = "blue")) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)) +
  coord_fixed(ratio = 1) +
  labs(title = "Cross-Lagged Panel Model: GDS-15 and Informant CCI")

# Display
p

# Save
ggsave("../../Output/Stage 2 Output/Figures/mi_CLPM_Inf.jpeg", p,
       dpi = 500, width = 8, height = 8)

#########################################
# Diagnosis and Depression
##########################################
# Read coefficients
coefs <- read_csv("../../Output/Dta25 Output/Statistics/clpm_diagnosis_coefs.csv")

# Create nodes
nodes <- data.frame(
  id = 1:4,
  label = c("GDS-15 (t-1)", "MCI or Dementia\nDiagnosis (t-1)", 
            "GDS-15 (t)", "MCI or Dementia\nDiagnosis (t)"),
  x = c(1, 1, 3, 3),
  y = c(2, 1, 2, 1),
  time = c("t-1", "t-1", "t", "t")
)

# Create edges with coefficients
edges <- data.frame(
  from = c(1, 2, 2, 1, 1),
  to = c(3, 4, 3, 4, 2),
  type = c("auto", "auto", "cross", "cross", "cov"),
  label = c(
    coefs %>% filter(path == "Dep_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Cog") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Dep_to_Cog") %>% pull(label),
    ""  # Covariance - no label
  )
)

# reposition
edges$label_dx <- c(0, 0, -0.35, -0.15, 0)  # x nudges for each edge
edges$label_dy <- c(0.08, 0.08, 0, 0.12, 0)  # y nudges

# allow different colors
edges$subtype <- edges$type
edges$subtype[edges$type == "cross" & edges$from == 1] <- "cross_1"
edges$subtype[edges$type == "cross" & edges$from == 2] <- "cross_2"

# covariance by hand (from STATA)
covar <- 0.243

# Create plot
p <- ggplot() +
  # Add boxes for nodes
  geom_rect(data = nodes, 
            aes(xmin = x - 0.4, xmax = x + 0.4,
                ymin = y - 0.3, ymax = y + 0.3,
                fill = time),
            color = "black", size = 1) +
  # Add node labels
  geom_text(data = nodes, 
            aes(x = x, y = y, label = label),
            size = 5.5, fontface = "plain",
            family = "Times New Roman") +
  # Add arrows
  geom_segment(data = edges %>% filter(type != "cov"),
               aes(x = nodes$x[from] + 0.4, 
                   y = nodes$y[from],
                   xend = nodes$x[to] - 0.4, 
                   yend = nodes$y[to]),
               color = "black",
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1.2) +
  # Add covariance (curved line)
  geom_curve(data = edges %>% filter(type == "cov"),
             aes(x = nodes$x[from], 
                 y = nodes$y[from] - 0.3,
                 xend = nodes$x[to], 
                 yend = nodes$y[to] + 0.3),
             curvature = 0.3,
             linetype = "dashed",
             color = "black",
             arrow = arrow(length = unit(0.2, "cm"), 
                           ends = "both", type = "closed")) +
  # Add edge labels
  geom_text(data = edges %>% filter(type != "cov"),
            aes(x = (nodes$x[from] + nodes$x[to]) / 2 + label_dx,
                y = (nodes$y[from] + nodes$y[to]) / 2 + 
                  ifelse(type == "cross", 
                         ifelse(from == 1, 0.15, -0.15), 0) + label_dy,
                label = label),
            fontface = "plain",
            bg = "white",
            family = "Times New Roman",
            size = 5) +
  # Covariance label
  geom_text(data = edges %>% filter(type == "cov"),
            aes(x = 0.84, y = 1.5, label = covar),
            size = 5, fontface = "italic",
            color = "black",
            family = "Times New Roman") +
  # Grayscale fills (RIGHT lighter)
  scale_fill_manual(values = c("t-1" = "grey85",
                               "t"   = "grey92")) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(20, 20, 20, 20),
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 18)) +
  coord_fixed(ratio = 1) +
  labs(title = "Cross-Lagged Panel Model: Depression and MCI/Dementia Diagnosis")

# Display
p

# Save
ggsave("../../Output/Dta25 Output/Figures/F3_mi_CLPM_MCI.tif", p,
       dpi = 500, width = 8, height = 8)

#########################################
# MoCA and Depression
##########################################
# Read coefficients
coefs <- read_csv("../../Output/Dta25 Output/Statistics/clpm_moca_coefs.csv")

# Create nodes
nodes <- data.frame(
  id = 1:4,
  label = c("GDS-15\n(t-1)", "MoCA\n(t-1)", 
            "GDS-15\n(t)", "MoCA\n(t)"),
  x = c(1, 1, 3, 3),
  y = c(2, 1, 2, 1),
  time = c("t-1", "t-1", "t", "t")
)

# Create edges with coefficients
edges <- data.frame(
  from = c(1, 2, 2, 1, 1),
  to = c(3, 4, 3, 4, 2),
  type = c("auto", "auto", "cross", "cross", "cov"),
  label = c(
    coefs %>% filter(path == "Dep_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Cog") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Dep_to_Cog") %>% pull(label),
    ""  # Covariance - no label
  )
)

# reposition
edges$label_dx <- c(0, 0, -0.35, -0.15, 0)  # x nudges for each edge
edges$label_dy <- c(0.08, 0.08, 0, 0.12, 0)  # y nudges

# allow different colors
edges$subtype <- edges$type
edges$subtype[edges$type == "cross" & edges$from == 1] <- "cross_1"
edges$subtype[edges$type == "cross" & edges$from == 2] <- "cross_2"

# covariance by hand (from STATA)
covar <- 0.240

# Create plot
p <- ggplot() +
  # Add boxes for nodes
  geom_rect(data = nodes, 
            aes(xmin = x - 0.4, xmax = x + 0.4,
                ymin = y - 0.3, ymax = y + 0.3,
                fill = time),
            color = "black", size = 1) +
  # Add node labels
  geom_text(data = nodes, 
            aes(x = x, y = y, label = label),
            size = 4, fontface = "bold") +
  # Add arrows
  geom_segment(data = edges %>% filter(type != "cov"),
               aes(x = nodes$x[from] + 0.4, 
                   y = nodes$y[from],
                   xend = nodes$x[to] - 0.4, 
                   yend = nodes$y[to],
                   color = subtype),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1.2) +
  # Add covariance (curved line)
  geom_curve(data = edges %>% filter(type == "cov"),
             aes(x = nodes$x[from], 
                 y = nodes$y[from] - 0.3,
                 xend = nodes$x[to], 
                 yend = nodes$y[to] + 0.3),
             curvature = -0.3,
             linetype = "dashed",
             color = "gray50",
             arrow = arrow(length = unit(0.2, "cm"), 
                           ends = "both", type = "closed")) +
  # Add edge labels
  geom_text(data = edges %>% filter(type != "cov"),
            aes(x = (nodes$x[from] + nodes$x[to]) / 2 + label_dx,
                y = (nodes$y[from] + nodes$y[to]) / 2 + 
                  ifelse(type == "cross", 
                         ifelse(from == 1, 0.15, -0.15), 0) + label_dy,
                label = label),
            fontface = "bold",
            bg = "white") +
  # Add edge label for covariance
  geom_text(data = edges %>% filter(type == "cov"),
            aes(x = 0.9, y = 1.5, label = covar),
            size = 3.5, fontface = "italic",
            color = "gray30") +
  # Styling
  scale_fill_manual(values = c("t-1" = "#E8F4F8", "t" = "#B3D9E6")) +
  scale_color_manual(values = c("auto" = "black", 
                                "cross_1" = "#FF6B6B",
                                "cross_2" = "blue")) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)) +
  coord_fixed(ratio = 1) +
  labs(title = "Cross-Lagged Panel Model: Depression and MoCA")

# Display
p

# Save
ggsave("../../Output/Dta25 Output/Figures/mi_CLPM_moca.jpeg", p,
       dpi = 500, width = 8, height = 8)


#########################################
# Craft and Depression
##########################################
# Read coefficients
coefs <- read_csv("mi_clpm_craft_coefs.csv")

# Create nodes
nodes <- data.frame(
  id = 1:4,
  label = c("Depression\n(t-1)", "Craft\n(t-1)", 
            "Depression\n(t)", "Craft\n(t)"),
  x = c(1, 1, 3, 3),
  y = c(2, 1, 2, 1),
  time = c("t-1", "t-1", "t", "t")
)

# Create edges with coefficients
edges <- data.frame(
  from = c(1, 2, 2, 1, 1),
  to = c(3, 4, 3, 4, 2),
  type = c("auto", "auto", "cross", "cross", "cov"),
  label = c(
    coefs %>% filter(path == "Dep_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Cog") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Dep_to_Cog") %>% pull(label),
    ""  # Covariance - no label
  )
)

# reposition
edges$label_dx <- c(0, 0, -0.35, -0.15, 0)  # x nudges for each edge
edges$label_dy <- c(0.08, 0.08, 0, 0.12, 0)  # y nudges

# allow different colors
edges$subtype <- edges$type
edges$subtype[edges$type == "cross" & edges$from == 1] <- "cross_1"
edges$subtype[edges$type == "cross" & edges$from == 2] <- "cross_2"

# correlation by hand (from STATA)
covar <- 0.236

# Create plot
p <- ggplot() +
  # Add boxes for nodes
  geom_rect(data = nodes, 
            aes(xmin = x - 0.4, xmax = x + 0.4,
                ymin = y - 0.3, ymax = y + 0.3,
                fill = time),
            color = "black", size = 1) +
  # Add node labels
  geom_text(data = nodes, 
            aes(x = x, y = y, label = label),
            size = 4, fontface = "bold") +
  # Add arrows
  geom_segment(data = edges %>% filter(type != "cov"),
               aes(x = nodes$x[from] + 0.4, 
                   y = nodes$y[from],
                   xend = nodes$x[to] - 0.4, 
                   yend = nodes$y[to],
                   color = subtype),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1.2) +
  # Add covariance (curved line)
  geom_curve(data = edges %>% filter(type == "cov"),
             aes(x = nodes$x[from], 
                 y = nodes$y[from] - 0.3,
                 xend = nodes$x[to], 
                 yend = nodes$y[to] + 0.3),
             curvature = -0.3,
             linetype = "dashed",
             color = "gray50",
             arrow = arrow(length = unit(0.2, "cm"), 
                           ends = "both", type = "closed")) +
  # Add edge labels
  geom_text(data = edges %>% filter(type != "cov"),
            aes(x = (nodes$x[from] + nodes$x[to]) / 2 + label_dx,
                y = (nodes$y[from] + nodes$y[to]) / 2 + 
                  ifelse(type == "cross", 
                         ifelse(from == 1, 0.15, -0.15), 0) + label_dy,
                label = label),
            fontface = "bold",
            bg = "white") +
  # Add edge label for covariance
  geom_text(data = edges %>% filter(type == "cov"),
            aes(x = 0.9, y = 1.5, label = covar),
            size = 3.5, fontface = "italic",
            color = "gray30") +
  # Styling
  scale_fill_manual(values = c("t-1" = "#E8F4F8", "t" = "#B3D9E6")) +
  scale_color_manual(values = c("auto" = "black", 
                                "cross_1" = "#FF6B6B",
                                "cross_2" = "blue")) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)) +
  coord_fixed(ratio = 1) +
  labs(title = "Cross-Lagged Panel Model: Depression and Craft Delayed Recall",
       subtitle = "Controlling for demographics, health, and social factors")

# Display
p

# Save
ggsave("../Figures/mi_CLPM_craft.jpeg", p)

#########################################
# Trails and Depression
##########################################
# Read coefficients
coefs <- read_csv("mi_clpm_trails_coefs.csv")

# Create nodes
nodes <- data.frame(
  id = 1:4,
  label = c("Depression\n(t-1)", "Trails-B\n(t-1)", 
            "Depression\n(t)", "Trails-B\n(t)"),
  x = c(1, 1, 3, 3),
  y = c(2, 1, 2, 1),
  time = c("t-1", "t-1", "t", "t")
)

# Create edges with coefficients
edges <- data.frame(
  from = c(1, 2, 2, 1, 1),
  to = c(3, 4, 3, 4, 2),
  type = c("auto", "auto", "cross", "cross", "cov"),
  label = c(
    coefs %>% filter(path == "Dep_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Cog") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Dep_to_Cog") %>% pull(label),
    ""  # Covariance - no label
  )
)

# reposition
edges$label_dx <- c(0, 0, -0.35, -0.15, 0)  # x nudges for each edge
edges$label_dy <- c(0.08, 0.08, 0, 0.12, 0)  # y nudges

# allow different colors
edges$subtype <- edges$type
edges$subtype[edges$type == "cross" & edges$from == 1] <- "cross_1"
edges$subtype[edges$type == "cross" & edges$from == 2] <- "cross_2"

# correlation by hand (from STATA)
covar <- 0.209

# Create plot
p <- ggplot() +
  # Add boxes for nodes
  geom_rect(data = nodes, 
            aes(xmin = x - 0.4, xmax = x + 0.4,
                ymin = y - 0.3, ymax = y + 0.3,
                fill = time),
            color = "black", size = 1) +
  # Add node labels
  geom_text(data = nodes, 
            aes(x = x, y = y, label = label),
            size = 4, fontface = "bold") +
  # Add arrows
  geom_segment(data = edges %>% filter(type != "cov"),
               aes(x = nodes$x[from] + 0.4, 
                   y = nodes$y[from],
                   xend = nodes$x[to] - 0.4, 
                   yend = nodes$y[to],
                   color = subtype),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1.2) +
  # Add covariance (curved line)
  geom_curve(data = edges %>% filter(type == "cov"),
             aes(x = nodes$x[from], 
                 y = nodes$y[from] - 0.3,
                 xend = nodes$x[to], 
                 yend = nodes$y[to] + 0.3),
             curvature = -0.3,
             linetype = "dashed",
             color = "gray50",
             arrow = arrow(length = unit(0.2, "cm"), 
                           ends = "both", type = "closed")) +
  # Add edge labels
  geom_text(data = edges %>% filter(type != "cov"),
            aes(x = (nodes$x[from] + nodes$x[to]) / 2 + label_dx,
                y = (nodes$y[from] + nodes$y[to]) / 2 + 
                  ifelse(type == "cross", 
                         ifelse(from == 1, 0.15, -0.15), 0) + label_dy,
                label = label),
            fontface = "bold",
            bg = "white") +
  # Add edge label for covariance
  geom_text(data = edges %>% filter(type == "cov"),
            aes(x = 0.9, y = 1.5, label = covar),
            size = 3.5, fontface = "italic",
            color = "gray30") +
  # Styling
  scale_fill_manual(values = c("t-1" = "#E8F4F8", "t" = "#B3D9E6")) +
  scale_color_manual(values = c("auto" = "black", 
                                "cross_1" = "#FF6B6B",
                                "cross_2" = "blue")) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)) +
  coord_fixed(ratio = 1) +
  labs(title = "Cross-Lagged Panel Model: Depression and Craft Delayed Recall",
       subtitle = "Controlling for demographics, health, and social factors")

# Display
p

# Save
ggsave("../Figures/mi_CLPM_trails.jpeg", p)

#########################################
# Rey-AVLT and Depression
##########################################
# Read coefficients
coefs <- read_csv("mi_clpm_rey_coefs.csv")

# Create nodes
nodes <- data.frame(
  id = 1:4,
  label = c("Depression\n(t-1)", "Rey-AVLT\n(t-1)", 
            "Depression\n(t)", "Rey-AVLT\n(t)"),
  x = c(1, 1, 3, 3),
  y = c(2, 1, 2, 1),
  time = c("t-1", "t-1", "t", "t")
)

# Create edges with coefficients
edges <- data.frame(
  from = c(1, 2, 2, 1, 1),
  to = c(3, 4, 3, 4, 2),
  type = c("auto", "auto", "cross", "cross", "cov"),
  label = c(
    coefs %>% filter(path == "Dep_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Cog") %>% pull(label),
    coefs %>% filter(path == "Cog_to_Dep") %>% pull(label),
    coefs %>% filter(path == "Dep_to_Cog") %>% pull(label),
    ""  # Covariance - no label
  )
)

# reposition
edges$label_dx <- c(0, 0, -0.35, -0.15, 0)  # x nudges for each edge
edges$label_dy <- c(0.08, 0.08, 0, 0.12, 0)  # y nudges

# allow different colors
edges$subtype <- edges$type
edges$subtype[edges$type == "cross" & edges$from == 1] <- "cross_1"
edges$subtype[edges$type == "cross" & edges$from == 2] <- "cross_2"

# correlation by hand (from STATA)
covar <- 0.211

# Create plot
p <- ggplot() +
  # Add boxes for nodes
  geom_rect(data = nodes, 
            aes(xmin = x - 0.4, xmax = x + 0.4,
                ymin = y - 0.3, ymax = y + 0.3,
                fill = time),
            color = "black", size = 1) +
  # Add node labels
  geom_text(data = nodes, 
            aes(x = x, y = y, label = label),
            size = 4, fontface = "bold") +
  # Add arrows
  geom_segment(data = edges %>% filter(type != "cov"),
               aes(x = nodes$x[from] + 0.4, 
                   y = nodes$y[from],
                   xend = nodes$x[to] - 0.4, 
                   yend = nodes$y[to],
                   color = subtype),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               size = 1.2) +
  # Add covariance (curved line)
  geom_curve(data = edges %>% filter(type == "cov"),
             aes(x = nodes$x[from], 
                 y = nodes$y[from] - 0.3,
                 xend = nodes$x[to], 
                 yend = nodes$y[to] + 0.3),
             curvature = -0.3,
             linetype = "dashed",
             color = "gray50",
             arrow = arrow(length = unit(0.2, "cm"), 
                           ends = "both", type = "closed")) +
  # Add edge labels
  geom_text(data = edges %>% filter(type != "cov"),
            aes(x = (nodes$x[from] + nodes$x[to]) / 2 + label_dx,
                y = (nodes$y[from] + nodes$y[to]) / 2 + 
                  ifelse(type == "cross", 
                         ifelse(from == 1, 0.15, -0.15), 0) + label_dy,
                label = label),
            fontface = "bold",
            bg = "white") +
  # Add edge label for covariance
  geom_text(data = edges %>% filter(type == "cov"),
            aes(x = 0.9, y = 1.5, label = covar),
            size = 3.5, fontface = "italic",
            color = "gray30") +
  # Styling
  scale_fill_manual(values = c("t-1" = "#E8F4F8", "t" = "#B3D9E6")) +
  scale_color_manual(values = c("auto" = "black", 
                                "cross_1" = "#FF6B6B",
                                "cross_2" = "blue")) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)) +
  coord_fixed(ratio = 1) +
  labs(title = "Cross-Lagged Panel Model: Depression and Rey-AVLT",
       subtitle = "Controlling for demographics, health, and social factors",
       caption = "Note: Model does not converge")

# Display
p

# Save
ggsave("../Figures/mi_CLPM_rey.jpeg", p)





#########################################
#########################################
# Correlation heatmap
#########################################
#########################################

library(ggplot2)
library(reshape2)
library(openxlsx)

# Read (you already do this)
corr_matrix <- read.xlsx("correlation_matrix.xlsx", rowNames = TRUE)

# Names (your updated names)
var_names <- c("Self labeling", "Informant labeling", "Objective ability", "Diagnosis")
rownames(corr_matrix) <- var_names
colnames(corr_matrix) <- var_names

# Melt and mask upper triangle (keep lower + diagonal)
corr_mat <- as.matrix(corr_matrix)
corr_long <- melt(corr_mat)

corr_long$mask <- as.numeric(corr_long$Var1) >= as.numeric(corr_long$Var2)
corr_long_masked <- corr_long
corr_long_masked$value[!corr_long_masked$mask] <- NA
corr_long_masked$label <- ifelse(corr_long_masked$mask, sprintf("%.2f", corr_long_masked$value), "")

# Reverse y so diagonal goes top-left -> bottom-right
corr_long_masked$Var1 <- factor(corr_long_masked$Var1, levels = rev(var_names))
corr_long_masked$Var2 <- factor(corr_long_masked$Var2, levels = var_names)

# Plot:
ggplot() +
  # 1) regular tiles (non-diagonal) using the gradient scale
  geom_tile(
    data = subset(corr_long_masked, Var1 != Var2),
    aes(x = Var2, y = Var1, fill = value),
    color = "white",
    na.rm = TRUE
  ) +
  # 2) black diagonal on top
  geom_tile(
    data = subset(corr_long_masked, Var1 == Var2),
    aes(x = Var2, y = Var1),
    fill = "black",
    color = "white",
    na.rm = TRUE
  ) +
  # 3) labels for non-diagonal (black text)
  geom_text(
    data = subset(corr_long_masked, Var1 != Var2),
    aes(x = Var2, y = Var1, label = label),
    color = "black",
    size = 4,
    na.rm = TRUE
  ) +
  # 4) labels for diagonal (white text)
  geom_text(
    data = subset(corr_long_masked, Var1 == Var2),
    aes(x = Var2, y = Var1, label = label),
    color = "white",
    size = 4,
    na.rm = TRUE
  ) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-1, 1), na.value = "grey95"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(fill = "Correlation", x = "", y = "")
ggsave("../Figures/correlation_heatmap.png",
       width = 7,
       height = 6,
       units = "in",
       dpi = 600)

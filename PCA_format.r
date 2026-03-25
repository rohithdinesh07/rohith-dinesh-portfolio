library(tidyverse)
install.packages("ggrepel")
library(ggrepel)
counts <- read.table("gene_counts_E2_for_rohith.tsv.txt", header = TRUE, sep = "\t", row.names = 1, fill = TRUE)
counts
sum(duplicated(counts[, 1]))

counts[duplicated(counts[, 1]), 1]
rownames(counts) <- make.unique(as.character(counts[, 1]))
counts <- counts[, -c(1)]
counts <- counts[apply(counts, 1, var) > 0, ]
counts
pca <- prcomp(t(counts), scale. = TRUE)
t(counts)

pca

pca.var.per <- round(pca$sdev^2/sum(pca$sdev^2)*100, 1)

pca.var.per

barplot(pca.var.per, main = "PCA Variations")


pca.data <- data.frame(
  Sample = rownames(pca$x),
  X = pca$x[,1],
  Y = pca$x[,2])
pca.data


ggplot(data = pca.data, aes(x = X, y = Y, label = Sample, color = Sample)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_text_repel(size = 3) +
  labs(x = paste("PC1 - ", pca.var.per[1], " %"), y = paste("PC2 - ", pca.var.per[2], " %")) + 
  theme_bw() + 
  ggtitle("PCA Analysis")
loading_scores <- pca$rotation[ ,1]

loading_scores

genes <- sort(abs(loading_scores), decreasing = TRUE)

genes

top_50_genes <- names(genes[1:50])

top_50_genes

rotation.data<- data.frame(gene = top_50_genes,
                           loading = pca$rotation[top_50_genes, 1])
rotation.data


#' @title heatmap
#' @description onmath quantification analysis cluster heatmap
#' @param plot_data a matrix which will be ploted
#' @param samples a data frame about samples infomation
#' @param outdir a character path to output
#' @export
#'
om_heatmap <- function(plot_data, samples, outdir) {
  plot_data <- log10(plot_data + 1)
  my_col = om_pal()(9)

  if (length(unique(samples$condition)) > length(my_col)) {
    my_col <- colorRampPalette(my_col)(length(unique(samples$condition)))
  }
  Group <- my_col[1:length(unique(samples$condition))]
  names(Group) <- unique(samples$condition)
  ann_color = data.frame(group = samples$condition)
  rownames(ann_color) <- samples$sample
  ann_colors = list(group = Group)
  # theme_set(theme_onmath()+theme(axis.text.x = element_text(size =
  # rel(length(onmath_color)*0.2))))
  sample_num = length(colnames(plot_data))
  heatmap_width <- (sample_num - 5)/3 + 2
  heatmap_heigh <- (sample_num - 5)/3 + 4
  fontsize = (sample_num - 5)/10 + 4.5
  cellwidth <- (heatmap_width - 0.5) * 50/sample_num
  ## TODO legend size
  pdf(paste(outdir, "Diff.genes.heatmap.pdf", sep = "/"), width = heatmap_width,
      height = heatmap_heigh, onefile = F)
  pheatmap(plot_data, show_rownames = F, annotation_col = ann_color, annotation_colors = ann_colors,
           annotation_legend = T, annotation_names_col = F, color = rev(colorRampPalette(brewer.pal(10,
                                                                                                    "RdYlGn"))(100)), treeheight_row = 0, scale = "row", fontsize = fontsize,
           cellwidth = cellwidth, border_color = NA)
  dev.off()

  png(paste(outdir, "Diff.genes.heatmap.png", sep = "/"), width = heatmap_width,
      height = heatmap_heigh, units = "in", res = 300)
  pheatmap(plot_data, show_rownames = F, annotation_col = ann_color, annotation_colors = ann_colors,
           annotation_legend = T, annotation_names_col = F, color = rev(colorRampPalette(brewer.pal(10,
                                                                                                    "RdYlGn"))(100)), treeheight_row = 0, scale = "row", fontsize = fontsize,
           cellwidth = cellwidth, border_color = NA)
  dev.off()

}
#' @title boxplot
#' @description onmath quantification analysis boxplot
#' @param plot_data input data which will be ploted
#' @param samples a data frame about samples infomation
#' @param outdir a character path to output
#' @export
#'
om_box <- function(plot_data, samples, outdir) {
  # plot_data = gene_tpm_matrix
  samples = samples
  outdir = outdir
  plot_data <- log10(plot_data + 1)
  plot_data_df <- as.data.frame(cbind(rownames(plot_data), plot_data))
  names(plot_data_df) <- c("id", colnames(plot_data))
  data.m <- melt(plot_data_df, id = "id")
  data.m$variable <- as.character(data.m$variable)
  data.m$variable <- factor(data.m$variable, levels = samples$sample)
  data.m$value <- as.numeric(data.m$value)
  for (i in 1:length(samples$sample)) {
    data.m$group[data.m$variable == samples$sample[i]] <- samples$condition[i]
  }
  onmath_color <- colorRampPalette(onmath_color_palatte(pal = "om_color_base"))(length(unique(data.m$group)))
  onmath_color_bright <- colorRampPalette(onmath_color_palatte(pal = "om_color_bright"))(length(samples$sample))
  onmath_color_light <- colorRampPalette(onmath_color_palatte(pal = "om_color_light"))(length(unique(data.m$group)))
  theme_set(theme_onmath() + theme(axis.text.x = element_text(angle = -90, color = "black",
                                                              vjust = 0.5, hjust = 0, size = rel(1.2)), axis.text.y = element_text(size = rel(1.2)),
                                   legend.text = element_text(size = rel(0.8)), legend.key = element_blank()))

  boxplot1 <- ggplot(data.m, aes(x = variable, y = value, fill = group)) + geom_boxplot(notch = T) +
    guides(fill = F) + scale_fill_manual(values = onmath_color) + xlab("") +
    ylab("")
  boxplot2 <- ggplot(data.m, aes(x = variable, y = value, fill = group)) + geom_boxplot(notch = T) +
    guides(fill = guide_legend(nrow = 8, title = "group")) + scale_fill_manual(values = onmath_color) +
    xlab("") + ylab("")
  violin1 <- ggplot(data.m, aes(x = variable, y = value, fill = group)) + geom_violin() +
    guides(fill = F) + scale_fill_manual(values = onmath_color) + xlab("") +
    ylab("")
  violin <- ggplot(data.m, aes(x = variable, y = value, fill = group)) + geom_violin() +
    guides(fill = guide_legend(nrow = 8, title = "group")) + scale_fill_manual(values = onmath_color) +
    xlab("") + ylab("")
  density_plot <- ggplot(data.m, aes(value, color = variable, fill = group)) +
    geom_density(alpha = 0.4) + scale_fill_manual(values = onmath_color) + scale_color_manual(values = onmath_color_bright) +
    theme(axis.text.x = element_text(angle = 0)) + guides(fill = guide_legend(nrow = 8,
                                                                              title = "group"), color = guide_legend(nrow = 8, title = "sample")) + xlab("")
  #----print out----

  #----each plot----
  plot_output_path <- outdir
  sample_num = length(unique(data.m$variable))
  plot_width = 4 + sample_num/4
  plot_height = 4 + sample_num/8
  merge_width = 6 + sample_num/4
  merge_height = 10 + sample_num/8
  ggsave(filename = paste(plot_output_path, "Gene_expression.boxplot.png", sep = "/"),
         type = "cairo-png", plot = boxplot2, width = plot_width, height = plot_height)
  ggsave(filename = paste(plot_output_path, "Gene_expression.boxplot.pdf", sep = "/"),
         plot = boxplot2, width = plot_width, height = plot_height)
  ggsave(filename = paste(plot_output_path, "Gene_expression.violin.png", sep = "/"),
         type = "cairo-png", plot = violin, width = plot_width, height = plot_height)
  ggsave(filename = paste(plot_output_path, "Gene_expression.violin.pdf", sep = "/"),
         plot = violin, width = plot_width, height = plot_height)
  ggsave(filename = paste(plot_output_path, "Gene_expression.density_plot.png",
                          sep = "/"), type = "cairo-png", plot = density_plot, width = plot_width,
         height = plot_height)
  ggsave(filename = paste(plot_output_path, "Gene_expression.density_plot.pdf",
                          sep = "/"), plot = density_plot, width = plot_width, height = plot_height)
  #----merge plot----
  p <- grid.arrange(boxplot1, violin1, density_plot, nrow = 2, layout_matrix = rbind(c(1,
                                                                                       2), c(3, 3)))
  ggsave(filename = paste(plot_output_path, "Gene_expression.png", sep = "/"),
         type = "cairo-png", plot = p, width = merge_width, height = merge_height)
  ggsave(filename = paste(plot_output_path, "Gene_expression.pdf", sep = "/"),
         plot = p, width = merge_width, height = merge_height)
}
#
#' @export
#'
om_volcano <- function(diff_table, compare_name, logfc, qvalue, outdir) {

  y_line_pos = round(-log10(qvalue), 1)

  # dpa_results <- diff_table[,c('logFC','FDR')]
  dpa_results <- diff_table
  dpa_results$logFDR <- -log10(dpa_results$FDR)
  dpa_results$color <- "blue"
  up_name = unlist(strsplit(compare_name, split = "_vs_"))[1]
  down_name = unlist(strsplit(compare_name, split = "_vs_"))[2]
  for (i in 1:dim(dpa_results)[1]) {
    if (dpa_results$logFC[i] > logfc & dpa_results$FDR[i] < qvalue)
      dpa_results$color[i] <- "red" else if (dpa_results$logFC[i] < -(logfc) & dpa_results$FDR[i] < qvalue)
        dpa_results$color[i] <- "green"
  }

  # labels and titles
  dpa_results2 <- dpa_results
  count <- table(dpa_results2$color)
  count <- as.data.frame(count)

  red_count <- sum(count$Freq[which(count$Var1 == "red")])
  green_count <- sum(count$Freq[which(count$Var1 == "green")])
  all_count_number <- red_count + green_count
  # all_count <- paste('Differential Expressed Genes',all_count_number,sep = ':')

  if (all_count_number < 100) {
    dpa_results2$logFC <- ifelse(dpa_results2$logFC > 15, 15 + (dpa_results2$logFC -
                                                                  15)/logFC_max, dpa_results2$logFC)
    dpa_results2$logFC <- ifelse(dpa_results2$logFC < -15, -15 - (dpa_results2$logFC +
                                                                    15)/logFC_min, dpa_results2$logFC)
    logFDR_max = max(dpa_results2$logFDR)
    dpa_results2$logFDR <- ifelse(dpa_results2$logFDR > 50, 50 + (dpa_results2$logFDR -
                                                                    50)/logFDR_max, dpa_results2$logFDR)
  }

  logFC_max = max(dpa_results2$logFC)
  logFC_min = min(dpa_results2$logFC)
  logFC_limit <- ceiling(max(c(abs(logFC_min), logFC_max)))
  logFC_limit <- ifelse(logFC_limit < 8, 8, logFC_limit)
  logFC_limit <- ifelse(logFC_limit > 15, 15, logFC_limit)
  logFDR_limit <- ceiling(max(dpa_results2$logFDR))
  logFDR_limit <- ifelse(logFDR_limit > 50, 50, logFDR_limit)
  logFDR_limit <- ifelse(logFDR_limit < 35, 35, logFDR_limit)

  red_label <- paste("No.", up_name, "up-regulated genes:", red_count, sep = " ")
  green_label <- paste("No.", down_name, "up-regulated genes:", green_count, sep = " ")
  red <- brewer.pal(6, "Reds")[6]
  green <- brewer.pal(6, "Greens")[6]
  blue <- brewer.pal(4, "Blues")[4]

  theme_set(theme_onmath() + theme(legend.key = element_blank(), panel.grid.major = element_blank(),
                                   legend.position = "bottom", legend.direction = "vertical"))


  compare_number = 0
  p <- ggplot(dpa_results2, aes(logFC, logFDR, colour = color)) + geom_point(size = 0.6) +
    geom_hline(yintercept = y_line_pos, lty = 4, size = 0.45) + geom_vline(xintercept = -(logfc),
                                                                           lty = 4, size = 0.45) + geom_vline(xintercept = logfc, lty = 4, size = 0.45) +
    xlab("logFC") + ylab("-log10(FDR)")

  if ("compare" %in% colnames(dpa_results2)) {
    compare_number <- length(unique(dpa_results2$compare))
    facet_wrap_ncol = round(sqrt(compare_number))
    p <- p + guides(color = F) + scale_color_manual(values = c(red = red, green = green,
                                                               blue = blue)) + facet_wrap(~compare, ncol = facet_wrap_ncol)
  } else {
    p <- p + guides(color = guide_legend(title = "")) + scale_color_manual(values = c(red = red,
                                                                                      green = green, blue = blue), breaks = c("green", "red"), labels = c(green_label,
                                                                                                                                                          red_label)) + ggtitle(compare_name) + scale_y_continuous(breaks = c(0,
                                                                                                                                                                                                                              1.3, 3, 10, 20, 30), limits = c(0, logFDR_limit)) + scale_x_continuous(breaks = c(-8,
                                                                                                                                                                                                                                                                                                                -4, -2, -1, 0, 1, 2, 4, 8), limits = c(-logFC_limit, logFC_limit))

  }
  plot_height <- 8 + compare_number/4
  plot_width <- 6 + compare_number/4
  ggsave(filename = paste(outdir, "/", compare_name, ".Volcano_plot.pdf", sep = ""),
         plot = p, width = plot_width, height = plot_height)
  ggsave(filename = paste(outdir, "/", compare_name, ".Volcano_plot.png", sep = ""),
         type = "cairo-png", plot = p, width = plot_width, height = plot_height)

}
#'
#' @export
#'
om_pca <- function(plot_data, samples, outdir) {

  PCA_data_mat <- t(apply(plot_data[, 1:dim(plot_data)[2]], 2, as.numeric))
  PCA_data_mat <- log2(PCA_data_mat + 1)
  PCA <- prcomp(PCA_data_mat)
  Summary_PCA <- summary(PCA)

  PCA_data <- as.data.frame(PCA$x[, 1:dim(samples)[1]])
  sample_name <- rownames(PCA$x)
  match_index <- match(sample_name, samples$sample, nomatch = 0)
  group_name <- samples$condition[match_index]
  PCA_data$Sample <- sample_name
  PCA_data$Group <- group_name

  PCA_plot <- function(PCA_data) {
    p <- ggplot(PCA_data, aes(PC1, PC2)) + geom_point(aes(colour = Group), size = rel(3)) +
      geom_text(aes(label = Sample), vjust = 0, hjust = 0.5, color = "black",
                size = rel(2)) + geom_vline(xintercept = 0, linetype = 2, colour = "grey60",
                                            size = rel(0.5)) + geom_hline(yintercept = 0, linetype = 2, colour = "grey60",
                                                                          size = rel(0.5)) + theme_onmath() + scale_color_om() + labs(title = "PCA",
                                                                                                                                          x = paste0("PC1: ", Summary_PCA$importance[2, 1] * 100, "% variance"),
                                                                                                                                          y = paste0("PC2: ", Summary_PCA$importance[2, 2] * 100, "% variance"))
    p
  }

  ggsave(filename = paste(outdir, "PCA_plot.pdf", sep = "/"), plot = PCA_plot(PCA_data),
         width = 8, height = 6)
  ggsave(filename = paste(outdir, "PCA_plot.png", sep = "/"), plot = PCA_plot(PCA_data),
         width = 8, height = 6, type = "cairo", dpi = 300)

}
#'
#' @export
#'
om_correlation <- function(plot_data, samples, outdir) {
  data <- plot_data
  sample_types <- as.character(unique(samples$condition))
  rep_names <- as.character(samples$sample)
  data <- data[, colnames(data) %in% samples$sample, drop = F]
  nsamples <- length(sample_types)
  onmath_color <- onmath_color_palatte(pal = "om_color_base")
  sample_colors <- colorRampPalette(onmath_color)(nsamples)
  data <- log10(data + 1)
  data <- as.matrix(data)
  sample_cor <- cor(data, method = "pearson", use = "pairwise.complete.obs")
  sample_cor_df <- as.data.frame(sample_cor)
  sample_cor_df <- cbind(Sample = rownames(sample_cor_df), sample_cor_df)
  write.table(sample_cor_df, file = paste(outdir, "Sample.correlation.stat.txt",
                                          sep = "/"), quote = F, sep = "\t", row.names = F)

  Group <- sample_colors[1:length(unique(samples$condition))]
  names(Group) <- unique(samples$condition)
  ann_color = data.frame(group = samples$condition)
  rownames(ann_color) <- samples$sample
  ann_colors = list(group = Group)

  sample_num = length(colnames(plot_data))
  heatmap_width <- (sample_num - 5)/5 + 7
  heatmap_heigh <- (sample_num - 5)/5 + 6
  fontsize = (sample_num - 5)/10 + 7
  cellwidth <- (heatmap_width - 1) * 50/sample_num

  theme_set(theme_onmath() + theme(legend.position = c(0.5, 0.5)))

  pdf(paste(outdir, "Sample.correlation.heatmap.pdf", sep = "/"), width = heatmap_width,
      height = heatmap_heigh, onefile = F)
  pheatmap(sample_cor, annotation_col = ann_color, annotation_colors = ann_colors,
           annotation_row = ann_color, annotation_names_row = F, annotation_names_col = F,
           color = rev(redgreen(75)), border_color = NA, cellwidth = cellwidth, fontsize = fontsize)
  dev.off()

  # png(paste(outdir, 'Sample.correlation.heatmap.png', sep = '/'), width =
  # heatmap_width, height = heatmap_heigh, units = 'in', res = 300)
  png(paste(outdir, "Sample.correlation.heatmap.png", sep = "/"), width = (heatmap_width *
                                                                             300), height = (heatmap_heigh * 300), res = 300)
  pheatmap(sample_cor, annotation_col = ann_color, annotation_colors = ann_colors,
           annotation_row = ann_color, annotation_names_row = F, annotation_names_col = F,
           color = rev(redgreen(75)), border_color = NA, cellwidth = cellwidth, fontsize = fontsize)
  dev.off()

}
#'
#' @export
#'
om_cluster <- function(plot_data, out_prefix) {

  cluster_number <- length(unique(plot_data$cluster))
  col_theme <- colorRampPalette(onmath_color_palatte(pal = "om_color_flat"))(cluster_number)

  cluster_plot <- ggplot(plot_data, aes(x=variable, y=value, group = Gene_id, color=cluster)) +
    geom_line(alpha = 0.2) +
    scale_color_manual(guide=F, values = col_theme) +
    xlab("") + ylab("Scaled log10(tpm+1)") + theme_cluster

  if (cluster_number > 1) {
    facet_wrap_ncol = round(sqrt(cluster_number))
    cluster_plot <- cluster_plot + facet_wrap(~cluster, ncol = facet_wrap_ncol)
  }

  plot_height <- 6 + cluster_number/4
  plot_width <- 8 + cluster_number/4

  ggsave(paste(out_prefix, "png", sep = "."), plot = cluster_plot, width = plot_width, height = plot_height,
         dpi = 300, type = "cairo")
  ggsave(paste(out_prefix, "pdf", sep = "."), plot = cluster_plot, width = plot_width, height = plot_height,
         device = cairo_pdf)
}

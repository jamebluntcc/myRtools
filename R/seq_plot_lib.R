#' onmath rseqc analysis plots

#'
#' @export
#'

gc_line_plot <- function(plot_data, output) {

  sample_number <- length(unique(plot_data$sample))
  seq_len <- round(max(plot_data[, 1])/2)
  max_gc <- max(plot_data$value) + 0.1

  gc_plot <- ggplot(plot_data, aes(x = X.Base, y = value, colour = variable)) +
    geom_line() + geom_vline(xintercept = seq_len, linetype = 2) +
    scale_x_continuous(breaks = seq(from = 0, to = 2 * seq_len, by = seq_len),
                       labels = seq(from = 0, to = 2 * seq_len, by = seq_len)) +
    scale_y_continuous(breaks = seq(0, max_gc, by = round((max_gc)/4, 1)),
                       labels = percent(seq(0, max_gc, by = round((max_gc)/4, 1)))) +
    xlab("Postion") + ylab("Percent(%)") +
    guides(color = guide_legend(title = "")) +
    theme_onmath() + scale_colour_om()

  if (sample_number > 1) {
    facet_wrap_ncol = round(sqrt(sample_number))
    gc_plot <- gc_plot + facet_wrap(~sample, ncol = facet_wrap_ncol)
  }

  plot_height <- 6 + sample_number/4
  plot_width <- 8 + sample_number/4
  ggsave(paste(output, "png", sep = "."), plot = gc_plot, width = plot_width, height = plot_height,
         dpi = 300, type = "cairo")
  ggsave(paste(output, "pdf", sep = "."), plot = gc_plot, width = plot_width, height = plot_height,
         device = cairo_pdf)

}
#'
#' @export
#'

reads_quality_plot <- function(plot_data, output) {
  col <- plot_data$color
  names(col) <- col
  sample_number <- length(unique(plot_data$sample))

  p <- ggplot(plot_data, aes(x = Quality, y = Proportion, fill = color)) + geom_bar(stat = "identity") +
    scale_fill_manual(values = col) + geom_segment(aes(x = 30, y = 0, xend = 30,
                                                       yend = max(plot_data$Proportion)), colour = "red", linetype = "dashed", size = 1) +
    theme_bw() + guides(fill = F) + scale_y_continuous(breaks = seq(from = 0,
                                                                    to = max(plot_data$Proportion), by = 0.1), labels = scales::percent(seq(from = 0,
                                                                                                                                            to = max(plot_data$Proportion), by = 0.1))) + xlab("Quality Score")
  if (sample_number > 1) {
    facet_wrap_ncol = round(sqrt(sample_number))
    p <- p + facet_wrap(~sample, ncol = facet_wrap_ncol)
  }

  plot_height <- 6 + sample_number/4
  plot_width <- 8 + sample_number/4

  ggsave(filename = paste(output, "png", sep = "."), type = "cairo-png", plot = p, width = plot_width, height = plot_height)
  ggsave(filename = paste(output, "pdf", sep = "."), plot = p, width = plot_width, height = plot_height)
}
#'
#' @export
#'
## RSEQC
read_distribution_pie_plot <- function(plot_data, output) {

  col_theme <- colorRampPalette(onmath_color_palatte(pal = "om_color_flat"))(length(unique(plot_data$Group)))
  pie <- ggplot(plot_data, aes(x = "", y = portion, fill = label)) + geom_bar(width = 1,
                                                                              stat = "identity") + coord_polar("y", start = 0) + pie_theme + theme(axis.text.x = element_blank()) +
    scale_fill_manual(values = col_theme, guide = guide_legend(title = "Genomic features"))
  ggsave(filename = paste(output, "png", sep = "."), plot = pie, type = "cairo-png",
         width = 8, height = 8, dpi = 300)
  ggsave(filename = paste(output, "pdf", sep = "."), plot = pie, width = 8, height = 8)

}
#'
#' @export
#'
read_distribution_bar_plot <- function(plot_data, output) {
  col_theme <- colorRampPalette(onmath_color_palatte(pal = "om_color_flat"))(length(unique(plot_data$Group)))
  bar_plot <- ggplot(plot_data, aes(Sample, Tag_count, fill = Group)) + geom_bar(colour = "black",
                                                                                 position = "fill", stat = "identity", width = 0.8) + scale_fill_manual("Genomic features",
                                                                                                                                                        values = col_theme) + scale_y_continuous(labels = percent_format(), expand = c(0,
                                                                                                                                                                                                                                       0), breaks = seq(0, 1, 0.1)) + labs(x = NULL, y = NULL) + bar_plot_theme1
  sample_number <- length(unique(plot_data$Sample))
  plot_width = 6 + sample_number/5
  plot_height = 8 + sample_number/15
  ggsave(filename = paste(output, "png", sep = "."), plot = bar_plot, type = "cairo-png",
         width = plot_width, height = plot_height, dpi = 300)
  ggsave(filename = paste(output, "pdf", sep = "."), plot = bar_plot, width = plot_width,
         height = plot_height)

}
#'
#' @export
#'
gene_body_cov_plot <- function(plot_data, output) {

  sample_number <- length(unique(plot_data$sample))
  col_theme <- colorRampPalette(onmath_color_palatte(pal = "om_color_flat"))(sample_number)
  gene_body_cov_plot <- ggplot(plot_data, aes(variable, value, colour = sample,
                                              group = sample)) + geom_point(size = 1) + geom_line() + scale_y_continuous(labels = percent_format()) +
    scale_x_discrete(breaks = seq(0, 100, 5)) + scale_color_manual(values = col_theme) +
    labs(x = "Gene body percentile (5'->3')", y = "Coverage") + guides(fill = guide_legend(nrow = 8,
                                                                                           title = "group")) + theme_onmath()

  plot_width = 8 + sample_number/8
  plot_height = 6 + sample_number/16
  ggsave(filename = paste(output, "png", sep = "."), plot = gene_body_cov_plot,
         type = "cairo-png", width = plot_width, height = plot_height, dpi = 300)
  ggsave(filename = paste(output, "pdf", sep = "."), plot = gene_body_cov_plot,
         width = plot_width, height = plot_height)

}
#'
#' @export
#'
inner_distance_plot <- function(plot_data, out_prefix) {

  sample_number <- length(unique(plot_data$sample))
  col_theme <- colorRampPalette(onmath_color_palatte(pal = "om_color_flat"))(sample_number)

  plot <- ggplot(plot_data, aes(x = V2, y = proportion, fill = sample)) + geom_bar(stat = "identity",
                                                                                   width = 5) + scale_x_continuous(breaks = seq(-200, 200, 100)) + scale_fill_manual(values = col_theme) +
    scale_y_continuous(labels = percent_format(), expand = c(0, 0)) + labs(x = "Inner distance (bp)",
                                                                           y = "Percent of reads") + theme_onmath()


  if (sample_number > 1) {
    facet_wrap_ncol = round(sqrt(sample_number))
    plot <- plot + facet_wrap(~sample, ncol = facet_wrap_ncol)
  }


  plot_height <- 6 + sample_number/4
  plot_width <- 8 + sample_number/4

  ggsave(filename = paste(out_prefix, "png", sep = "."), plot = plot, type = "cairo-png",
         width = plot_width, height = plot_height, dpi = 300)
  ggsave(filename = paste(out_prefix, "pdf", sep = "."), plot = plot, width = plot_width,
         height = plot_height)

}
#'
#' @export
#'
reads_duplication_plot <- function(plot_data, out_prefix) {

  sample_number <- length(unique(plot_data$sample))
  col_theme <- colorRampPalette(onmath_color_palatte(pal = "om_color_flat"))(sample_number)
  max_proportion <- round(max(plot_data$proportion) + 0.1, 1)
  plot <- ggplot(plot_data, aes(Occurrence, proportion, group = sample, colour = sample)) +
    geom_point() + geom_line() + scale_y_continuous(breaks = seq(0, max_proportion,
                                                                 0.1), limits = c(0, max_proportion), labels = percent_format(), expand = c(0,
                                                                                                                                            0)) + scale_color_manual(values = col_theme) + labs(x = "Occurrence of reads",
                                                                                                                                                                                                y = "Percent of reads") + facet_grid(. ~ method) + guides(color = guide_legend(nrow = 16,
                                                                                                                                                                                                                                                                               title = "sample")) + theme_onmath()

  plot_height <- 6 + sample_number/24
  plot_width <- 12 + sample_number/16

  ggsave(filename = paste(out_prefix, "png", sep = "."), plot = plot, type = "cairo-png",
         width = plot_width, height = plot_height, dpi = 300)
  ggsave(filename = paste(out_prefix, "pdf", sep = "."), plot = plot, width = plot_width,
         height = plot_height)

}

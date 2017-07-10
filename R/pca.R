#' #' pca plot
#' #'
#'
#' pca_plot <- function(data,title = "PCA"){
#'   if(!all(c('Sample','Group','PC1','PC2') %in% names(data))){
#'     stop("Sample,Group,PC1,PC2 must be in header!")
#'   }
#'   p <- ggplot2::ggplot(data,aes(PC1,PC2)) +
#'     geom_point(aes(color = Group),size = rel(3.0)) +
#'     geom_text(aes(label = Sample),vjust = 0,hjust = 0.5,color = "black",size = rel(2.0)) +
#'     geom_vline(xintercept = 0,linetype = 2,colour = "grey60") +
#'     geom_hline(yintercept = 0,linetype = 2,colour = "grey60") +
#'     theme_om() + scale_color_om() +
#'     labs(title = title,
#'          x = paste0("PC1:")
#'
#' }

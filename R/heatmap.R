#' Creates a heatmap plot fork by raivokolde/pheatmap wrap and make it more simple
#'
#' The function also allows to aggregate the rows using kmeans clustering. This is
#' advisable if number of rows is so big that R cannot handle their hierarchical
#'
#' @param data numeric matrix of the values to be plotted.
#' @param color vector of colors used in heatmap.
#' @param borderColor color of cell borders on heatmap, use \code{grey} if no border should be drawn.
#' @param Scale character indicating if the values should be centered and scaled in
#' either the row direction or the column direction, or none. Corresponding values are
#' \code{"row"}, \code{"column"} and \code{"none"} default is \code{"column"}
#' @param clusterRows boolean values determining if rows should be clustered or \code{hclust} object,
#' @param clusterCols boolean values determining if columns should be clustered or \code{hclust} object.
#' @param clusteringDistanceRows distance measure used in clustering rows. Possible
#' values are \code{"correlation"} for Pearson correlation and all the distances
#' supported by \code{\link{dist}}, such as \code{"euclidean"}, etc. If the value is none
#' of the above it is assumed that a distance matrix is provided.
#' @param clusteringDistanceCols distance measure used in clustering columns. Possible
#' values the same as for clustering_distance_rows.
#' @param clusteringMethod clustering method used. Accepts the same values as
#' \code{\link{hclust}}.
#' @param treeHeightCol the height of a tree for columns, if these are clustered.
#' Default value 50 points.
#' @param legend logical to determine if legend should be drawn or not.
#' @param Main the title of the plot
#' @param fontSize base fontsize for the plot
#' @param fontSizeRow fontsize for rownames (Default: fontsize)
#' @param fontSizeCol fontsize for colnames (Default: fontsize)
#' @param filePath file path where to save the picture.
#' @param fileName file name. Filetype is decided by
#' the extension in the path. Currently following formats are supported: png, pdf, tiff,
#'  bmp, jpeg. Even if the plot does not fit into the plotting window, the file size is
#' calculated so that the plot would fit there, unless specified otherwise.
#' @param Width manual option for determining the output file width in inches.
#' @param Height manual option for determining the output file height in inches.
#' @param clusterGroup a list of character to divide your group,default is \code{NA}.
#' @return A heatmap plot file of pdf or png.
#'
heatmap_plot <- function(data,Scale = "column",showRowNames = F,showColNames=TRUE,borderColor = "grey",
                         clusterRows = TRUE,clusterCols = TRUE,
                         clusterDistanceRows = "euclidean",clusterDistanceCols = "euclidean",
                         clusterMethod = "complete",treeHeightRow = 0,
                         clusterLegend = TRUE,Main = NA,fontSize = 10,
                         fontSizeRow = fontSize,fontSizeCol = fontSize,
                         clusterGroup = NA,
                         Color = colorRampPalette(rev(RColorBrewer::brewer.pal(n=7,name="RdYlGn")))(100),
                         saveType = c("both","pdf","png"),fileName = NULL,filePath = "",width = 8,height = 6
){
  if(!any(is.na(clusterGroup))){
    cluster_group <- clusterGroup #list no factor
    if(is.null(names(cluster_group))){
      names(cluster_group) <- paste0('group',1:length(cluster_group))
    }
    cluster_group_vector = NULL
    for(i in 1:length(cluster_group)){
      cluster_group_vector <- c(cluster_group_vector,rep(names(cluster_group)[i],length(cluster_group[[i]])))
    }
    annotationCol <- data.frame(group = rep('group',length(cluster_group_vector)))
    annotationCol$group <- cluster_group_vector
    annColors <- list(group=NULL)
    totalColor <- colorRampPalette(om_pal()(9))(length(cluster_group))
    names(totalColor) <- unique(cluster_group_vector)
    annColors$group <- totalColor
    rownames(annotationCol) <- colnames(data) #mapping
  }else{
    annotationCol <- NA
    annColors <- NA
  }
    if(is.null(fileName)){
      heatmap_file <- 'pheatmap'
    }else{
      heatmap_file <- fileName
    }
    savetype <- match.arg(saveType)
    if(savetype == 'both'){
      pdf(file = paste0(filePath,heatmap_file,'.pdf'),width = width,height = height)
      pheatmap::pheatmap(mat = data,scale = Scale,color = Color,border_color = borderColor,
                         cluster_rows = clusterRows,cluster_cols = clusterCols,
                         clustering_distance_rows = clusterDistanceRows,
                         clustering_distance_cols = clusterDistanceCols,
                         show_rownames = showRowNames,show_colnames = showColNames,
                         clustering_method = clusterMethod,main = Main,legend = clusterLegend,
                         fontsize = fontSize,fontsize_col = fontSizeCol,fontsize_row = fontSizeRow,
                         annotation_col = annotationCol,annotation_colors = annColors,treeheight_row = treeHeightRow)
      dev.off()
      png(filename = paste0(filePath,heatmap_file,'.png'),width = width,height = height,units = "in",res = 300,type = "cairo")
      pheatmap::pheatmap(mat = data,scale = Scale,color = Color,border_color = borderColor,
                         cluster_rows = clusterRows,cluster_cols = clusterCols,
                         clustering_distance_rows = clusterDistanceRows,
                         clustering_distance_cols = clusterDistanceCols,
                         show_rownames = showRowNames,show_colnames = showColNames,
                         clustering_method = clusterMethod,main = Main,legend = clusterLegend,
                         fontsize = fontSize,fontsize_col = fontSizeCol,fontsize_row = fontSizeRow,
                         annotation_col = annotationCol,annotation_colors = annColors,treeheight_row = treeHeightRow)
      dev.off()
    }else if(savetype == 'png'){
      png(filename = paste0(filePath,heatmap_file,'.png'),width = width,height = height,units = "in",res = 300,type = "cairo")
      pheatmap::pheatmap(mat = data,scale = Scale,color = Color,border_color = borderColor,
                         cluster_rows = clusterRows,cluster_cols = clusterCols,
                         clustering_distance_rows = clusterDistanceRows,
                         clustering_distance_cols = clusterDistanceCols,
                         show_rownames = showRowNames,show_colnames = showColNames,
                         clustering_method = clusterMethod,main = Main,legend = clusterLegend,
                         fontsize = fontSize,fontsize_col = fontSizeCol,fontsize_row = fontSizeRow,
                         annotation_col = annotationCol,annotation_colors = annColors,treeheight_row = treeHeightRow)
      dev.off()
    }else{
      pdf(file = paste0(filePath,heatmap_file,'.pdf'),width = width,height = height)
      pheatmap::pheatmap(mat = data,scale = Scale,color = Color,border_color = borderColor,
                         cluster_rows = clusterRows,cluster_cols = clusterCols,
                         clustering_distance_rows = clusterDistanceRows,
                         clustering_distance_cols = clusterDistanceCols,
                         show_rownames = showRowNames,show_colnames = showColNames,
                         clustering_method = clusterMethod,main = Main,legend = clusterLegend,
                         fontsize = fontSize,fontsize_col = fontSizeCol,fontsize_row = fontSizeRow,
                         annotation_col = annotationCol,annotation_colors = annColors,treeheight_row = treeHeightRow)
      dev.off()
    }
}

#' heatmap
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
  if(!is.na(clusterGroup)){
    cluster_group <- clusterGroup #list no factor
    if(is.null(names(cluster_group))){
      names(cluster_group) <- paste0('group',1:length(cluster_group))
    }
    annotationCol <- as.data.frame(cluster_group)
    annColors <- list()
    totalColorNum <- 0
    for(i in 1:dim(annotationCol)[2]){
      totalColorNum <- totalColorNum + length(unique(annotationCol[,i]))
      annColors[[i]] <- unique(annotationCol[,i])
    }
    names(annColors) <- names(cluster_group)
    #annotationCol <- apply(annotationCol,2,as.factor) #transform to factor
    totalColor <- colorRampPalette(RColorBrewer::brewer.pal(9,"Set1"))(totalColorNum)
    count_setp <- 0
    for(i in 1:length(annColors)){
      names(annColors[[i]]) <- rep("",length(annColors[[i]]))
      for(j in 1:length(annColors[[i]])){
        count_setp <- count_setp + 1
        names(annColors[[i]])[j] <- annColors[[i]][j]
        annColors[[i]][j] <- totalColor[count_setp]
      }
    }
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

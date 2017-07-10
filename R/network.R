#' network
wgcna_clean <- function(link,node,weight_data = 0.5){
  wgcna_data <- list(link=link,node=node)
  names(wgcna_data$link)[1:3] <- c('from','to','weight')
  wgcna_data$link <- wgcna_data$link %>%
    select(from:weight) %>%
    filter(weight > weight_data)
  names(wgcna_data$node)[c(1,3)] <- c('node','color')
  wgcna_data$node <- wgcna_data$node %>% select(node,color)
  if(length(unique(wgcna_data$node$color)) <= 1){
    stop('wgcna node color must be > 1')
  }
  return(wgcna_data)
}


network_plot <- function(data,layout = 'fr'){
  graphData <- igraph::graph.data.frame(data$link,data$node,directed = F) %>% igraph::simplify()
  net <- ggraph::ggraph(graphData,layout = layout) +
         geom_edge_link() +
         geom_node_point(aes(colour = color)) +
         scale_color_om() +
         theme_net()
  return(net)
}



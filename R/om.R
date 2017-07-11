#' Theme OM
#'
#' the theme is the onmath basic color and theme scheme
#' @inheritParams ggplot2::theme_bw
#' @export
#'
theme_om <- function(base_size = 14){
  theme_bw()+
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.title = element_text(face = "bold",size = base_size),
      axis.title.x = element_text(vjust = -0.2),
      axis.ticks = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.background = element_rect(colour = NA),
      legend.title = element_text(face = "italic"),
      strip.text = element_text(face = "bold"),
      strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0")
      )
}
#'
#' @export
#'
theme_net <- function(...){
  theme_bw() + theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.background = element_rect(colour = NA),
    legend.title = element_text(face = "italic")
  )
}
#'
#' @export
#'
theme_onmath <- function(base_size = 14) {
  theme_bw() + theme(panel.background = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.background = element_blank(),
                     panel.border = element_blank(),
                     axis.line = element_line(colour = "black"),
                     axis.text = element_text(color = "black", face = "bold"),
                     axis.title = element_text(face = "bold",size = base_size),
                     axis.title.x = element_text(vjust = -0.2),
                     axis.title.y = element_text(angle = 90,vjust = 2),
                     plot.title = element_text(face = "bold", size = rel(1.2),hjust = 0.5),
                     legend.key = element_blank(),
                     legend.title = element_text(face = "italic"),
                     strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
                     strip.text = element_text(face = "bold")
                     )
}
#'
#' @export
#'
theme_onmath_border <- theme_onmath() + theme(panel.border = element_rect(colour = "black"))
#'
#' @export
#'
theme_cluster <- theme_onmath() + theme(axis.text.x = element_text(vjust = -0.2, size = rel(.8), angle = 90))
#'
#' @export
#'
theme_pie <- theme_minimal() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                                     panel.border = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(),
                                     legend.title = element_text(face = "italic"), plot.title = element_text(size = 14,
                                                                                                             face = "bold"))

#' @family om colour palatte
#' @export
#'

onmath_color_palatte <- function(pal){
  onmath_color <- list(
    om_color_base = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33"),
    om_color_flat = c('#34495e','#3498db', '#2ecc71', '#f1c40f', '#e74c3c', '#9b59b6', '#1abc9c','#f39c12', '#d35400'),
    om_color_dark = c("#170C2EFF","#751029FF","#52194CFF","#473B75FF","#4D709CFF","#6F766BFF","#92ADC4FF","#A8643BFF","#B65B23FF","#94220EFF","#272D17FF","#202126FF"),
    om_color_bright = c("#5FB233FF","#6A7F93FF","#F57206FF","#EB0F13FF","#8F2F8BFF","#1396DBFF","#FBCF35FF","#ED4C1CFF","#9C7E70FF","#5AC2F1FF","#11776CFF"),
    om_color_base1 = c("#0C5BB0FF","#EE0011FF","#15983DFF","#EC579AFF","#FA6B09FF","#149BEDFF","#A1C720FF","#FEC10BFF","#16A08CFF","#9A703EFF"),
    om_color_light  = c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999")
  )
  if(pal %in% names(onmath_color)){
    return(onmath_color[[pal]])
  }else{
    stop(paste('no',pal,'in onmath_color_palatte!',sep = " "))
  }
}
#' @export
#' @rdname om_pal
#'
om_pal <- function(){
  om_color <- onmath_color$om_color_base
  scales::manual_pal(values = om_color)
}

#' @export
#' @rdname scale_om
#'
scale_fill_om <- function(...){
  ggplot2::discrete_scale("fill","om",om_pal(),...)
}
#' @export
#' @rdname scale_om
#'
scale_colour_om <- function(...){
  ggplot2::discrete_scale("colour","om",om_pal(),...)
}
#' @export
#' @rdname scale_om
#'
scale_color_om <- scale_colour_om


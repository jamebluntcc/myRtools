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
#' @family colour om
#' export
#'
om_pal <- function(){
  #default color
  om_color <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")
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

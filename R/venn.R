# venn plot
defaultFillColor = c("#386cb0","#fdb462","#7fc97f","#ef3b2c")
venn_plot <- function(data,fillColor,fileName,main=NULL,fontSize=1.5,fontColor='white'){
  venn.diagram(
    x = data,
    filename = fileName,
    main = main,
    label.col = fontColor,
    cex = fontSize,
    fontfamily="serif",
    fontface="bold",
    cat.col=fillColor,
    cat.fontfamily="serif",
    rotation.degree = 0,
    margin = 0.1
  )
}

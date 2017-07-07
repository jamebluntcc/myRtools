#' plot a venn graphics
#' @description plot a venn graphics it wrap VennDiagram and make venn more simple
#' @author chencheng <jamebluntxy@gmail.com>
#' @param data A list for your group data
#' @param fillColor A vector for fill your venn plot default \code{fillColor} is provided
#' @param fileName A character for your outputfile name
#' @param main A character for the title in your venn plot default \code{main} is NULL
#' @param fontSize A number for your label size default \code{fontSize} is 1.5
#' @param fontColor A character for your label color default \code{fontColor} is white
#' @return a tiff or png file for your venn graphics
#' @note Since the \code{venn.diaram()} is not support pdf type file so your \code{fillName} not allow pdf type file
#' @examples
#'
#' oneName <- function() paste(sample(LETTERS,5,replace=TRUE),collapse="")
#' geneNames <- replicate(1000,oneName())
#' GroupA <- sample(geneNames,400,replace = F)
#' GroupB <- sample(geneNames,300,replace = F)
#' GroupC <- sample(geneNames,200,replace = F)
#' GroupD <- sample(geneNames,500,replace = F)
#' venn_plot(data=list(A=GroupA,B=GroupB,C=GroupC,D=GroupD),fileName='test_venn_plot.tiff')
#' @export
#'
venn_plot <- function(data,fillColor=NULL,fileName,main=NULL,fontSize=1.5,fontColor='white'){
  if(is.null(fillColor)){
    defaultColor <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c")
    dataNum <- length(data)
    fillColor <- defaultColor[1:dataNum]
  }
  VennDiagram::venn.diagram(
    x = data,
    filename = fileName,
    col = "transparent",
    fill = fillColor,
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

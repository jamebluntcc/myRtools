#' plot a venn graphics
#' @description plot a venn graphics it wrap VennDiagram and make venn more simple
#' @author chencheng <chencheng@onmath.cn>
#' @param data a list for your group data
#' @param fillColor a vector for fill your venn plot default \code{fillColor} is provided
#' @param fileName a character for your outputfile name
#' @param main a character for the title in your venn plot default \code{main} is NULL
#' @param fontSize a number for your label size default \code{fontSize} is 1.5
#' @param fontColor a character for your label color default \code{fontColor} is white
#' @param saveType a character for your save file type png or tiff
#' @param Margin a number for your venn margin more Margin less size in your venn graphics
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
venn_plot <- function(data,fillColor=NULL,fileName=NULL,main=NULL,fontSize=1.5,fontColor='white',saveType = c("tiff","png"),Margin = 0.1){
  if(is.null(fillColor)){
    defaultColor <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c")
    dataNum <- length(data)
    fillColor <- defaultColor[1:dataNum]
  }
  if(is.null(fileName)){
    venn_file <- 'venn'
  }else{
    venn_file <- fileName
  }
  savetype <- match.arg(saveType)

  VennDiagram::venn.diagram(
    x = data,
    filename = paste(fileName,savetype,sep = "."),
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
    margin = Margin
  )
}

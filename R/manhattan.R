#' Creates a manhattan plot fork by qqman/manhattan wrap and make it more simple
#'
#' Creates a manhattan plot from PLINK assoc output (or any data frame with
#' chromosome, position, and p-value).
#'
#' @param data A data.frame with columns "BP," "CHR," "P," and optionally, "SNP."
#' @param Chr A string denoting the column name for the chromosome. Defaults to
#'   PLINK's "CHR." Said column must be numeric. If you have X, Y, or MT
#'   chromosomes, be sure to renumber these 23, 24, 25, etc.
#' @param Bp A string denoting the column name for the chromosomal position.
#'   Defaults to PLINK's "BP." Said column must be numeric.
#' @param P A string denoting the column name for the p-value. Defaults to
#'   PLINK's "P." Said column must be numeric.
#' @param Snp A string denoting the column name for the SNP name (rs number).
#'   Defaults to PLINK's "SNP." Said column should be a character.
#' @param Col A character vector indicating which colors to alternate.
#' @param ChrLabs A character vector equal to the number of chromosomes
#'   specifying the chromosome labels (e.g., \code{c(1:22, "X", "Y", "MT")}).
#' @param SuggestiveLine Where to draw a "suggestive" line. Default
#'   -log10(1e-5). Set to FALSE to disable.
#' @param GenomewideLine Where to draw a "genome-wide sigificant" line. Default
#'   -log10(5e-8). Set to FALSE to disable.
#' @param HighLight A character vector of SNPs in your dataset to highlight.
#'   These SNPs should all be in your dataset.
#' @param Logp If TRUE, the -log10 of the p-value is plotted. It isn't very
#'   useful to plot raw p-values, but plotting the raw value could be useful for
#'   other genome-wide plots, for example, peak heights, bayes factors, test
#'   statistics, other "scores," etc.
#' @param AnnotatePval If set, SNPs below this p-value will be annotated on the plot.
#' @param AnnotateTop If TRUE, only annotates the top hit on each chromosome that is below the annotatePval threshold.
#' @param ... Arguments passed on to other plot/points functions
#'
#' @return A manhattan plot file of pdf or png.
#'
#' @keywords visualization manhattan
#'
#' @import utils
#' @import graphics
#' @import stats
#'
#' @examples
#' manhattan(gwasResults)
#'
#' @importFrom calibrate textxy
#'
#' @export
manhattan_plot <- function(data,Chr="CHR",Bp="BP",P="p",Snp="SNP",AnnotatePval = NULL,AnnotateTop = TRUE,
                           Col=NULL,ChrLabs = NULL,SuggestiveLine = -log10(5e-05),GenomeWideLine = -log10(5e-08),
                           Highlight = NULL,Logp = TRUE,fileName = NULL,filePath = "",width=8,height=6,saveType = c('both','png','pdf'),...){
  if(is.null(Col)){
    manhatton_col = om_pal()(9)
  }
  if(is.null(fileName)){
    manhattan_file = 'manhattan'
  }else{
    manhattan_file = fileName
  }
  save_type <- match.arg(saveType)

  if(save_type == 'both'){
    png(filename = paste0(filePath,manhattan_file,'.png'),width = width,height = height,res = 300,type = "cairo",units = "in")
    qqman::manhattan(na.omit(data),chr=Chr,bp=Bp,p=P,snp=Snp,
                     annotatePval=AnnotatePval,annotateTop=AnnotateTop,
                     col=manhatton_col,chrlabs=ChrLabs,
                     suggestiveline=SuggestiveLine,genomewideline=GenomeWideLine,
                     highlight=Highlight,logp=Logp)
    dev.off()
    pdf(file = paste0(manhattan_file,'.pdf'),width = width,height = height)
    qqman::manhattan(na.omit(data),chr=Chr,bp=Bp,p=P,snp=Snp,
                     annotatePval=AnnotatePval,annotateTop=AnnotateTop,
                     col=manhatton_col,chrlabs=ChrLabs,
                     suggestiveline=SuggestiveLine,genomewideline=GenomeWideLine,
                     highlight=Highlight,logp=Logp)
    dev.off()
  }else if(save_type == 'png'){
    png(filename = paste0(manhattan_file,'.png'),width = width,height = height,res = 300,type = "cairo",units = "in")
    qqman::manhattan(na.omit(data),chr=Chr,bp=Bp,p=P,snp=Snp,
                     annotatePval=AnnotatePval,annotateTop=AnnotateTop,
                     col=manhatton_col,chrlabs=ChrLabs,
                     suggestiveline=SuggestiveLine,genomewideline=GenomeWideLine,
                     highlight=Highlight,logp=Logp)
    dev.off()
  }else{
    pdf(file = paste0(manhattan_file,'.pdf'),width = width,height = height)
    qqman::manhattan(na.omit(data),chr=Chr,bp=Bp,p=P,snp=Snp,
                     annotatePval=AnnotatePval,annotateTop=AnnotateTop,
                     col=manhatton_col,chrlabs=ChrLabs,
                     suggestiveline=SuggestiveLine,genomewideline=GenomeWideLine,
                     highlight=Highlight,logp=Logp)
    dev.off()
  }
}

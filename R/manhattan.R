#' manhattan
#'
manhattan_plot <- function(data,Chr="CHR",Bp="BP",P="p",Snp="SNP",AnnotatePval = NULL,AnnotateTop = TRUE,
                           Col=NULL,ChrLabs = NULL,SuggestiveLine = -log10(5e-05),GenomeWideLine = -log10(5e-08),
                           Highlight = NULL,Logp = TRUE,fileName = NULL,filePath = "",width=8,height=6,saveType = c('both','png','pdf'),...){
  if(is.null(Col)){
    manhatton_col = RColorBrewer::brewer.pal("Set1",n=9)
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

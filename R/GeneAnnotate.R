
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Return a data frame of gene annotations
#' Takes a dataframe having output from limma, cleans up to remove unwanted columns and annotates it
#' @param ids : List of ENSEMBL IDs
#' @param organism : mouse for Mus musculus (mouse) Rat for Rattus norvegicus (Rat) and human for Homo sapiens (humans)
#' @return Dataframe with limma data and the annotation
#' @import org.Mm.eg.db EnsDb.Mmusculus.v75 org.Hs.eg.db org.Rn.eg.db EnsDb.Hsapiens.v75 dplyr
#' @export




GeneAnnotate <- function(ids,organism) {
  if (is.na(organism)) {
    stop(paste("Please provide an organism human or mouse\n"), call. = FALSE)
  }

  if(organism=="mm9"){
    data('Mus_musculus.NCBIM37.67')
    geneannotation=Mus_musculus.NCBIM37.67
  }
  else if(organism=="mm10"){
    data('Mus_musculus.GRCm38.82')
    geneannotation=Mus_musculus.GRCm38.82
  }
  else if(organism=="mm39"){
    data('Mus_musculus.GRCm39.104')
    geneannotation=Mus_musculus.GRCm39.104
  }
  else if(organism=="hg19"){
    data('gencode.v19.annotation')
    geneannotation=gencode.v19.annotation
  }
  else if(organism=="hg38"){
    data('Homo_sapiens.GRCh38.104')
    geneannotation=Homo_sapiens.GRCh38.104.gtf
  }
  else if(organism=="Rat"){
    #load('~/dsdata/NGSshare/hg19_data/RData/gencode.v19.annotation.RData')
    data('Rattus_norvegicus.Rnor_6.0.87')
    geneannotation=Rattus_norvegicus.Rnor_6.0.87
  }else{
    stop("Wrong organism")
  }
  geneannotation = as.data.frame(geneannotation)
  genes <- geneannotation %>% filter (gene_id %in% ids) %>%
    dplyr::rename(biotype=gene_biotype, SYMBOL=gene_name, ENSEMBL=gene_id) %>%
    mutate(geneloc=paste(chr,':',start,'-',end,sep='')) %>%
    dplyr::select(SYMBOL,ENSEMBL,ENTREZID,biotype,geneloc) %>% arrange(ENSEMBL)

  genes <-as.data.frame(genes)
  rownames(genes)=genes$ENSEMBL
  return(genes)
}



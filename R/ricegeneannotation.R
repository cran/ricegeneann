#' @name ricegeneannotation
#' @author Xiang LI <ynaulx@@gmail.com> Linna MA <973649882@@qq.com>
#' 
#' @title Annotation of genes of rice (Oryza Sativa)
#' @description 
#' \code{ricegeneann} Annotate the gene of rice (Oryza Sativa)
#' 
#' @param myID A vector of entrez id
#' @param type Keytype of input id
#
#' @examples 
#' convert_id <- ricegeneannotation('Os01g0100500','RAP')
#' convert_id <- ricegeneannotation(myID = 'Os01g0100500',
#'                             type = 'RAP')
#
#' @export 
#' 
#' @return Return a vector or a datafram
ricegeneannotation <- function(myID,type){
  if (type %in% c('rap','RAP','transcriptid','TRANSCRIPTID')) {
    result <- IRGSP[which(IRGSP$gene_id == myID | IRGSP$transcript_id == myID),]
  }else{
    ID_temp <- unique(riceidconverter::RiceIDConvert(myID,type,'RAP',FALSE)[,2])
    result <- subset(IRGSP, IRGSP$gene_id %in% ID_temp)
  }
  if (nrow(result) == 0) {
    stop('-----> No gene can be mapped....')
  }else{
    return(result)
  }
}


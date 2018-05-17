#'Query and return the rank of a taxa
#'
#' @param taxa A string.
#' @return the basic taxonomic information of the interested taxa.
#' @import data.table
#' @import dplyr
#' @examples
#' \dontrun{
#' rank_query("Salpida")
#' }
#' @export
#'

rank_query <- function(in_taxa) {
  ls <- alltaxalist(data_m) %>% filter(taxa == in_taxa)
  out_rank <- tolower(as.character(ls$rank))
  if((strsplit(out_rank, ""))[[1]][1] == "o") {
    paste(in_taxa, " is an ", out_rank, ".", sep = "")
  } else {
    paste(in_taxa, " is a ", out_rank, ".", sep = "")
  }
}

#'Return the most frequent lower taxa groups of a selected higher taxa group.
#' @param taxa A string.
#' @param rank A string.
#' @param n A number.
#' @import dplyr
#' @return a frequency dataframe of \code{rank}
#' @examples
#'\dontrun{
#'topn("Animalia", "Phylum", 5)
#'}
#'@export

topn <- function(taxa, rank, n) {
  df <- as.data.frame(subset(data_m, Kingdoms == taxa | Phyla == taxa | Classes == taxa | Orders == taxa | Families == taxa | Genera == taxa))
  if(rank == "Phylum") {
    df_mid <- count(df, df$Phyla)
  } else if(rank == "Class") {
    df_mid <- count(df, df$Classes)
  } else if(rank == "Order") {
    df_mid <- count(df, df$Order)
  } else if(rank == "Family") {
    df_mid <- count(df, df$Families)
  } else if(rank == "Genus") {
    df_mid <- count(df, df$Genera)
  }
  df_mid <- as.data.frame(df_mid)
  colnames(df_mid) <- c(taxa, "freq")
  df_end <- df_mid[with(df_mid, order(-freq)),]
  df_end_n <- df_end[c(1:n),]
  colnames(df_end_n) <- c(rank, "freq")
  return(df_end_n)
}

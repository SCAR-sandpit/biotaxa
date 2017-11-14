#'Return the frequency ranking of an interested lower taxon based on a higher taxon
#' @param taxa A string.
#' @param rank A string.
#' @return a frequency dataframe of \code{rank}
#' @examples
#'\dontrun{
#'frequencyrank("Animalia", "Phylum")
#'}
#'@export

frequencyrank <- function(taxa, rank) {
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
  colnames(df_end) <- c(rank, "freq")
  return(df_end)
}

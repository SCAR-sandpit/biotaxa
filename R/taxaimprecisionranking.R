library('dplyr')
data <- read.csv("/Users/hhsieh/Documents/ANTABIS/RASp/RAS species list/Three Bigs/data_m.csv")

taxaimprecisionranking <- function(taxa, rank) {
  dd <- subset(data, Kingdoms == taxa | Classes == taxa | Orders == taxa
    | Families == taxa | Genera == taxa#,
    #Classes == rank | Orders == rank | Families == rank | Genera == rank
    )
  #return(head(dd))
  #return(dim(dd))
  species_complete <- dd[c(which(dd$AphiaIDs != "")), ]
  #return(species_complete)
  #return(head(species_complete))
  #head(species_complete)
  #colnames(dd) <- c("higher", "lower")
  #colnames
  #taxa %>% group_by(lower) %>% summarise(length(lower))
}

taxamodel_cor <- function(taxa, rank, method) {
  tryCatch({
    df <- subset(data_m, Kingdoms == taxa | Phyla == taxa | Classes == taxa | Orders == taxa | Families == taxa | Genera == taxa)
    dt = as.data.table(unique(df))
    setkey(dt, "year")
    if(rank == "Phylum") {
      dt[, id := as.numeric(factor(Phyla, levels = unique(Phyla)))]
      ranklabel = "phyla"
    } else if(rank == "Class") {
      dt[, id := as.numeric(factor(Classes, levels = unique(Classes)))]
      ranklabel = "classes"
    } else if(rank == "Order") {
      dt[, id := as.numeric(factor(Orders, levels = unique(Orders)))]
      ranklabel = "orders"
    } else if(rank == "Family") {
      dt[, id := as.numeric(factor(Families, levels = unique(Families)))]
      ranklabel = "families"
    } else if(rank == "Genus") {
      dt[, id := as.numeric(factor(Genera, levels = unique(Genera)))]
      ranklabel = "genera"
    } else if(rank == "Species") {
      dt[, id := as.numeric(factor(AphiaIDs, levels = unique(AphiaIDs)))]
      ranklabel = "species"
    }
    setkey(dt, "year", "id")
    dt.out <- dt[J(unique(year)), mult = "last"]#[, Phylum := NULL]
    dt.out[, id := cummax(id)]
    numtaxa <- cummax(as.numeric(factor(dt$id)))
    taxa_dt <- aggregate(numtaxa, list(year = dt$year), max )
    colnames(taxa_dt) <- c("year", "taxa count")
    N_obs <- taxa_dt$'taxa count'
    times <- as.numeric(taxa_dt$year)
    if(method == "logistic") {
      SS<-getInitial(N_obs~SSlogis(times,alpha,xmid,scale),data=data.frame(N_obs=N_obs,times=times))
      K_start <- SS["alpha"]
      R_start <- 1/SS["scale"]
      N0_start <- SS["alpha"]/(exp(SS["xmid"]/SS["scale"])) + 1
      log_formula<-formula(N_obs ~ K * N0 * exp(R * times) / (K + N0 * (exp(R * times) - 1)))
      m<-nls(log_formula,start = list(K = K_start, R = R_start, N0 = N0_start))
      corr_coef <- cor(N_obs,predict(m))
      res <- list(taxa=taxa, rank=rank, method=method, corr_coef=corr_coef)
      return(res)
    } else if(method == "Michaelis-Menten") {
      MM <- getInitial(N_obs~SSmicmen(times, Vm, K),data=data.frame(N_obs=N_obs,times=times))
      Vm_start <- MM["Vm"]
      K_start <- MM["K"]
      model <- nls(N_obs ~ Vm * times / (K + times), start = list(Vm = Vm_start, K = K_start))
      corr_coef <- cor(N_obs, predict(model))
      res <- list(taxa=taxa, rank=rank, method=method, corr_coef=corr_coef)
      return(res)
    }
  }, error = function(e) {list(taxa = taxa, rank = rank, method = method, corr_coef = cat("model fails to converge", "\n"))})
}

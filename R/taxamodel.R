#'Fitting the taxonomic accumulation of a given taxa with user-selected logistic or Michaelis-Menten model
#'
#' @param taxa A string.
#' @param rank A string.
#' @param method A string.
#' @return an accumulative curve of taxa overlapped with a fitting curve of selected model
#' @import data.table
#' @importFrom dplyr count
#' @import stats
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @import minpack.lm
#' @examples
#' \dontrun{
#' taxamodel("Animalia", "Phylum", "logistic")
#' }
#' @export

taxamodel <- function(taxa, rank, method) {
  tryCatch({
    df <- subset(data_m, Kingdoms == taxa | Phyla == taxa | Classes == taxa |
        Orders == taxa | Families == taxa | Genera == taxa)
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
    plot(taxa_dt$year, taxa_dt$`taxa count`, xlab = "Year", ylab = paste("Number of", ranklabel, sep = " "), ylim = c(0, max(taxa_dt$"taxa count")*1.35))
    title(taxa)

    if(method == "logistic") {
      N_obs <- taxa_dt$'taxa count'
      times <- as.numeric(taxa_dt$year)

      SS<-getInitial(N_obs~SSlogis(times,alpha,xmid,scale),data=data.frame(N_obs=N_obs,times=times))
      K_start <- SS["alpha"]
      R_start <- 1/SS["scale"]
      N0_start <- SS["alpha"]/(exp(SS["xmid"]/SS["scale"])) + 1

      log_formula<-formula(N_obs ~ K * N0 * exp(R * times) / (K + N0 * (exp(R * times) - 1)))
      m<-nls(log_formula,start = list(K = K_start, R = R_start, N0 = N0_start), control = list(maxiter = 500), trace = TRUE)

      corr_coef <- cor(N_obs,predict(m))
      lines(times,predict(m),col="red",lty=2,lwd=2)
      n = length(times)

      K = summary(m)$coefficient[1]
      R = summary(m)$coefficient[2]
      N0 = summary(m)$coefficient[3]

      ## add variances - first, find standard errors
      K_se = summary(m)$coefficients[4]
      R_se = summary(m)$coefficients[5]
      N0_se = summary(m)$coefficients[6]

      ## compute standard deviations
      K_sd = K_se * sqrt(n)
      R_sd = R_se * sqrt(n)
      N0_sd = N0_se * sqrt(n)

      # compute upper bounds of model prediction
      UP = (K + K_sd) * (N0 + N0_sd) * exp((R + R_sd)*times)/((K + K_sd)+(N0 + N0_sd)*(exp((R + R_sd)*times)-1))
      lines(times, UP, col = 'red', lty = "dashed")
      LW = (K - K_sd) * (N0 - N0_sd) * exp((R - R_sd)*times)/((K - K_sd)+(N0 - N0_sd)*(exp((R - R_sd)*times)-1))
      lines(times, LW, col ='red', lty = 'dashed')
      return('correlation coefficient' = corr_coef)
    } else if(method == "Michaelis-Menten") {

      # refer to this page https://stackoverflow.com/questions/27547548/solving-error-message-step-halving-factor-reduced-below-minimum-in-nls-step-a

      N_obs <- taxa_dt$'taxa count'
      times <- as.numeric(taxa_dt$year)
      #ddd <- data.frame(N_obs, times)
      #write.csv(ddd, "/Users/hhsieh/Desktop/ddd.csv", row.names = FALSE)


      #MM <- getInitial(N_obs~SSmicmen(times, Vm, K),data=data.frame(N_obs=N_obs,times=times))

      #Vm_start <- MM["Vm"]
      #K_start <- MM["K"]
      #return(c(Vm_start, K_start))

      model <- nlsLM(N_obs ~ Vm * times / (K + times), start = list(K = max(N_obs)/2, Vm = max(N_obs)), trace = TRUE)
      summary(model)


      #model <- nlsLM(1/N_obs ~ K/(Vm * times) + 1/Vm, start = list(K = max(N_obs)/2, Vm = max(N_obs)), trace = TRUE)
      #summary(model)

      #model <- nls(1/N_obs ~ K/(Vm*times) + 1/Vm, start = list(K = max(N_obs)/2, Vm = max(N_obs)), trace = TRUE)
      #summary(model)


      #summary(model)


      #model <- nls(N_obs ~ SSmicmen(times, Vmax, Km), data = data.frame(N_obs, times), alg = "port", nls.control(minFactor = 0.0000000001))




      #model <- try(nlsLM(N_obs ~ Vm * times / (K + times), start = list(Vm = Vm_start, K = K_start), control = list(maxiter = 500), trace = TRUE))

      #corr_coef <- cor(N_obs, predict(model))
      #return(corr_coef)
      #lines(times,predict(model),col="red",lty=2,lwd=2)
      #n = length(times)
      ## add model predictions
      #a = summary(model)$coefficient[1]
      #b = summary(model)$coefficient[2]
      #lines(times,predict(model),col="red",lty=2,lwd=2)
      ## add variances - first, find standard errors
      #a_se = summary(model)$coefficients[3]
      #b_se = summary(model)$coefficients[4]
      ## compute standard deviations
      #a_sd = a_se * sqrt(n)
      #b_sd = b_se * sqrt(n)
      # compute upper bounds of model prediction
      #UP = (a + a_sd) * times / (b - b_sd + times)
      #lines(times, UP, col = 'red', lty = "dashed")
      #LW = (a - a_sd) * times / (b + b_sd + times)
      #lines(times, LW, col ='red', lty = 'dashed')
      #return('correlation coefficient' = corr_coef)
    }
  }#, error = function(e) {list(taxa = taxa, rank = rank, method = method, corr_coef = cat("model fails to converge", "\n"))}
    )
}

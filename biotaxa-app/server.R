#setwd("/Users/hhsieh/Documents/ANTABIS/RASp/RAS species list/Three Bigs")
#data <- read.csv("data_m.csv", sep = ",", header = T, row.names = NULL)

list.of.packages <- c("ggplot2", "data.table", "shiny", "drc", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(drc)


shinyServer(function(input, output) {
  #viewData <- reactive({
  #  df <- subset(data_m, Kingdoms == input$taxa | Phyla == input$taxa | Classes == input$taxa | #Orders == input$taxa | Families == input$taxa | Genera == input$taxa)
#  })
#

  modelfit <- reactive({
    df <- subset(input$file1, Kingdoms == input$taxa | Phyla == input$taxa | Classes == input$taxa | Orders == input$taxa | Families == input$taxa | Genera == input$taxa)
    dt = as.data.table(unique(df))
    setkey(dt, "year")
    if (input$rank == "Phylum" | input$rank == "phylum") {
      dt[, id := as.numeric(factor(Phyla, levels = unique(Phyla)))]
    } else if (input$rank == "Class" | input$rank == "class") {
      dt[, id := as.numeric(factor(Classes, levels = unique(Classes)))]
    } else if (input$rank == "Order" | input$rank == "order") {
      dt[, id := as.numeric(factor(Orders, levels = unique(Orders)))]
    } else if (input$rank == "Family" | input$rank == "family") {
      dt[, id := as.numeric(factor(Families, levels = unique(Families)))]
    } else if (input$rank == "Genus" | input$rank == "genus") {
      dt[, id := as.numeric(factor(Genera, levels = unique(Genera)))]
    } else if (input$rank == "Species" | input$rank == "species") {
      dt[, id := as.numeric(factor(AphiaIDs, levels = unique(AphiaIDs)))]
    }

    dt.out <- dt[J(unique(year)), mult = "last"]#[, Phylum := NULL]
    dt.out[, id := cummax(id)]
    numtaxa <- cummax(as.numeric(factor(dt$id)))
    taxa_dt <- aggregate(numtaxa, list(year = dt$year), max)
    colnames(taxa_dt) <- c("year", "taxacount")
    N_obs <- taxa_dt$'taxacount'
    times <- as.numeric(taxa_dt$year)
  })

  ranklable <- reactive({
      if (input$rank == "Phylum") {
      paste("phyla")
    } else if (input$rank == "Class") {
      paste("classes")
    } else if (input$rank == "Order") {
      paste("orders")
    } else if (input$rank == "Family") {
      paste("families")
    } else if (input$rank == "Genus") {
      paste("genera")
    } else if (input$rank == "Species") {
      paste("species")
    }
})


  #output$dataview <- renderTable({
  #  head(viewData(), n = 6)
  #}, caption = "Brief Data View", caption.placement = getOption("xtable.caption.placement", "top"), cex = 5)

  output$dataview <- renderTable({
    req(input$file1)

    df <- read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep)

    if(input$disp == "head") {
      return(head(df))
    }
    else if (input$disp == "tail") {
      return(tail(df))
    }
  }

  )

  output$taxacurve <- renderPlot({

    req(input$file1)
    df <- read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep)
    df <- subset(df, Kingdoms == input$taxa | Phyla == input$taxa | Classes == input$taxa | Orders == input$taxa | Families == input$taxa | Genera == input$taxa)
    dt = as.data.table(unique(df))
    setkey(dt, "year")
    if (input$rank == "Phylum" | input$rank == "phylum") {
      dt[, id := as.numeric(factor(Phyla, levels = unique(Phyla)))]
      ranklabel = "phyla"
    } else if (input$rank == "Class" | input$rank == "class") {
      dt[, id := as.numeric(factor(Classes, levels = unique(Classes)))]
      ranklabel = "class"
    } else if (input$rank == "Order" | input$rank == "order") {
      dt[, id := as.numeric(factor(Orders, levels = unique(Orders)))]
      ranklabel = "order"
    } else if (input$rank == "Family" | input$rank == "family") {
      dt[, id := as.numeric(factor(Families, levels = unique(Families)))]
      ranklabel = "family"
    } else if (input$rank == "Genus" | input$rank == "genus") {
      dt[, id := as.numeric(factor(Genera, levels = unique(Genera)))]
      ranklabel = "genus"
    } else if (input$rank == "Species" | input$rank == "species") {
      dt[, id := as.numeric(factor(AphiaIDs, levels = unique(AphiaIDs)))]
      ranklabel = "species"
    }

    dt.out <- dt[J(unique(year)), mult = "last"]#[, Phylum := NULL]
    dt.out[, id := cummax(id)]
    numtaxa <- cummax(as.numeric(factor(dt$id)))
    taxa_dt <- aggregate(numtaxa, list(year = dt$year), max)
    colnames(taxa_dt) <- c("year", "taxacount")

    plt_title <- input$taxa

    minx <- min(as.vector(taxa_dt$year))
    maxx <- max(as.vector(taxa_dt$year))

    miny <- min(as.vector(taxa_dt$taxacount))
    maxy <- max(as.vector(taxa_dt$taxacount))

    ylab = paste("Number of", ranklabel, sep = " ")

    N_obs <- taxa_dt$'taxacount'
    times <- as.numeric(taxa_dt$year)
    p <- ggplot(taxa_dt, aes(x = year, y = taxacount, colour = "#FF9999"
        , group = 1)) + geom_point(colour = "cornflowerblue")  + theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (32)), legend.position = "right", axis.text.x = element_text(angle = 60, hjust = 1, size = 22), axis.text.y = element_text(angle = 60, hjust = 1, size = 22), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 26), axis.title.x = element_text(size = 26))
    p <- p +  labs(title = plt_title, x = "Year", y = ylab)

    if (input$fitting == "no curve") {
      p
    } else if(input$fitting == "logistic") {

      ryegrass.m1 <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = L.4())

      pred <- suppressWarnings(as.data.frame(predict(
        ryegrass.m1,
        newdata = data.frame(N_obs = N_obs, times = times),
        interval = "prediction", level = 0.95)));
      pred$times <- times;

      LW = pred[,2]
      UP = pred[,3]

      p <- p + geom_line(data = data.frame(pred, taxa_dt$year), aes(taxa_dt$year, Prediction), colour = "#FF9999")
      p <- p + geom_ribbon(aes(ymin = LW, ymax = UP), linetype = 2, alpha = 0.1)
      p
    } else if(input$fitting == "Michaelis_Menten") {

      model.drm <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = MM.2())

      #newtimes <- times
      preds <- suppressWarnings(predict(model.drm, times = times, interval = "prediction", level = 0.95))

      LW = preds[,2]
      UP = preds[,3]

      p <- p + geom_line(data = data.frame(preds, taxa_dt$year), aes(taxa_dt$year, Prediction), colour = "#FF9999")

      #p <- p + geom_line(data = data.frame(preds, taxa_dt$year), aes(taxa_dt$year, Prediction), colour = "#FF9999")
      p <- p + geom_ribbon(aes(ymin = LW, ymax = UP), linetype = 2, alpha = 0.1)
      p

    } else if(input$fitting == "Asymtopic_Regression_Model") {

      model.drm <- drm(N_obs ~ times, data = data.frame(N_obs = N_obs, times = times), fct = AR.3())

      preds <- suppressWarnings(predict(model.drm, times = times, interval = "prediction", level = 0.95))

      #preds <- predict(model.drm, times = newtimes, interval = "prediction", level = 0.95)

      LW = preds[,2]
      UP = preds[,3]


      p <- p + geom_line(data = data.frame(preds, taxa_dt$year), aes(taxa_dt$year, Prediction), colour = "#FF9999")
      p <- p + geom_ribbon(aes(ymin = LW, ymax = UP), linetype = 2, alpha = 0.1)
      p
    }
  })


})





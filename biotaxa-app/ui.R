library(shiny)

shinyUI(fluidPage(
  titlePanel('Discovering Biodiversity in Antarctica and the Southern Ocean'),

  sidebarLayout(
    sidebarPanel(
      helpText("This app visualizes the accumulative curve of discovered species belong to same higher taxa.
               Two user-input parameters, 'taxa' and 'rank', are required.
               'taxa' refers to a specific taxa in a kingdom, a phylum, a class, a family or a genus to which all inquired species belong to.
               'rank' refers to the taxonomic rank below kingdom (i.e. phylum, class, family, genus or species) that the user would like to visualize in the accumulative curve.
               Please note that the rank of user-input 'rank' must be lower than that of user-input 'taxa'.
               The default setting exemplifies the accumulative curve of discovered phyla of Animalia over time."),

      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values, text/plain",
                         ".csv")),
      # Horizontal line ---
      tags$hr(),

      #Input: Checkbox if file has header ---
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ---
      radioButtons("sep", "Seprator",
                    choices = c(Comma = ",",
                                Semicolon = ";",
                                Tab = "\t"),
                    selected = ","),

      # Horizontal line ---
      tags$hr(),

      # Input: Select number of rows to display ---
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               Tail = "tail"),
                   selected = "head"),

      # Horizonal line ---
      tags$hr(),

      textInput("taxa", "Enter taxa:","Animalia"),
      textInput("rank", "Enter rank:","Phylum"),
      helpText("By selecting fitting curve, you choose to visualise the fitting curve and the  prediction intervals of a logistic or a Michaelis-Menten regression model."),
      #checkboxInput("model", label = "fitting curve (logistic regression)", value = FALSE)#,
      radioButtons("fitting", "Fitting Curve",
                    choices = c(None = "no curve",
                                logistic = "logistic",
                                Michaelis_Menten = "Michaelis_Menten",
                                Asymtopic_Regression_Model = "Asymtopic_Regression_Model"
                      ),
                    selected = "no curve")

      #actionButton("newplot", "Update View")
    ),
    mainPanel(
      tableOutput("dataview"),
      plotOutput("taxacurve"),
      plotOutput("fittingcurve"),
      downloadButton("downloadPlot", "Download")
    )
)
))

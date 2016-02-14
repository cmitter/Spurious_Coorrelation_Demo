library(shiny)
library(shinythemes)
library(MASS)

shinyUI(fluidPage(theme=shinytheme("united"),
                  headerPanel(
                       HTML('Spurious Correlations Demo')
                       ),
                  fluidRow(
                       column(4,
                              wellPanel(
                                   radioButtons("dist","Distribution type:",
                                                list("Normal"="norm","Uniform"="unif","t"="t","F"="F","Gamma"="gam","Exponential"="exp","Chi-square"="chisq","Log-normal"="lnorm","Beta"="beta")),
                                   uiOutput("dist1"),
                                   uiOutput("dist2"),
                                   sliderInput("n","Sample size:",10,100,30),
                                   sliderInput("var","Number of variables:",30,1000,100),
                                   checkboxInput("density","Show density curve",FALSE),
                                   conditionalPanel(
                                        condition="input.density==true",
                                        numericInput("bw","bandwidth:",1)
                                   ),
                                   downloadButton("dldat", "Download Sample", class="btn-warning")
                              )
                       ),
                       column(8,
                              tabsetPanel(
                                   tabPanel("Usage",htmlOutput("usage")),
                                   #tabPanel("Summary",verbatimTextOutput("summary")),
                                   tabPanel("Distr. of random variables",plotOutput("plot",height="600px")),
                                   tabPanel("Distr. of Correlations",plotOutput("plot1",height="600px")),
                                   tabPanel("Distr. of Accuracy values",plotOutput("plot2",height="600px"))
                                   
                              )
                       )
                  )
                  ))

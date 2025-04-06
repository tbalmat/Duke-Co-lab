#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 4, Spring 2021
# Genome Wide Association Study Pleiotropy App
# Inter-GWAS Association Network Using the visNetwork Package
#
# Version 1, basic graph relating phenotypes of two GWAS studies by common SNP
#
# Features:
# Within-GWAS strength of association (phenotype to SNP) filtering
# visnetWork physics feature
# Edge transparency adjustment
# 
# Shiny user interface
#####################################################################################################

# Information on shiny and visnetwork available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf
# https://cran.r-project.org/web/packages/visnetwork/visnetwork.pdf

library(shiny)
library(shinythemes)
library(visNetwork)

# Dir location
dr <- c("local"="C:/Projects/Duke/Co-lab/Shiny-Spring-2021/Session-4-visNetwork",
        "cloud"="/cloud/project/Duke-Co-lab/Shiny/Session-4-visNetwork")[1]
setwd(dr)

shinyUI(
  fluidPage(

    includeCSS("App/V1/style.css"),
    title="Co-Lab Shiny",

    # Use a div to provide a slight left margin
    div(
      HTML("<h2>Duke University Co-lab Shiny Workshop</h2><br><h3>GWAS Pleiotropy Network, Version 1</h3><br><br>"),
      style="margin-left: 30px"
    ),

    div(
      fluidRow(

        # Prompts
        column(width=2,
          sidebarPanel(width=12,
            sliderInput("log_10_p", HTML("log<sub>10</sub>(p) min filter"), min=4, max=12, value=5.5, step=0.25),
            HTML("<br>"),
            radioButtons("physics", "Physics", choiceNames=c("on", "off"), choiceValues=c(T, F), selected=F, inline=T),
            HTML("<br>"),
            sliderInput("eopacity", "Edge opacity", min=0, max=1, value=0.35, step=0.05)
          )
        ),

        # Graph
        column(width=10, visNetworkOutput("g1", width="100%", height="900px"))

      ),

      style="margin-left: 20px"

    )

  )
)
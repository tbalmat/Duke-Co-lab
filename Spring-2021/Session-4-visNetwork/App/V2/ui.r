#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 4, Spring 2021
# Genome Wide Association Study Pleiotropy App
# Inter-GWAS Association Network Using the visNetwork Package
#
# Version 2
# Features:
# Within-GWAS strength of association (phenotype to SNP) filtering
# visnetWork physics feature
# Edge transparency adjustment
# -------------------------------------
# Following added in this version
# -------------------------------------
# Phenotype or SNP vertex selection
# Node filtering by number of edges
# Subnetting graph to a selected node and all other nodes with shared edge
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

    includeCSS("App/V2/style.css"),
    title="Co-Lab Shiny",

    # Use a div to provide a slight left margin
    div(
      HTML("<h2>Duke University Co-lab Shiny Workshop</h2><br><h3>GWAS Pleiotropy Network, Version 2</h3><br><br>"),
      style="margin-left: 30px"
    ),

    div(
      fluidRow(

        # Prompts
        column(width=2,
          sidebarPanel(width=12,
            radioButtons("vertexType", "Vertices", choices=c("Phenotype", "SNP"), selected="Phenotype", inline=F),
            HTML("<br>"),
            sliderInput("log_10_p", HTML("log<sub>10</sub>(p) min filter"), min=4, max=12, value=5.5, step=0.25),
            HTML("<br>"),
            sliderInput("nedgemin", "Vertex n-edge (min) filter", min=0, max=100, value=0, step=1),
            HTML("<br>"),
            radioButtons("physics", "Physics", choiceNames=c("on", "off"), choiceValues=c(T, F), selected=F, inline=T),
            HTML("<br>"),
            sliderInput("eopacity", "Edge opacity", min=0, max=1, value=0.35, step=0.05),
            HTML("<br><hr>shift-click vertex to subnet<hr><br>"),
            actionButton("regen", "Regenerate graph"),
            HTML("<br><br>"),
            actionButton("restoreVertex", "Restore after subnet"),
            HTML("<br><br>"),
            actionButton("redrawEdge", "Redraw edges"),
            # Hidden reactive fields
            # These are used by functions in server() to direct activity based on current state(s) of the graph
            # Note that the first conditionalPanel() parameter ("false") is a java expression
            conditionalPanel(condition="false",
                             textInput("reactiveInst", "reactiveInst", value=""),
                             textInput("renderInst", "renderInst", value="render")))),
  
        # Graph
        column(width=10, visNetworkOutput("g1", width="100%", height="900px"))

      ),

      style="margin-left: 20px"

    )

  )
)
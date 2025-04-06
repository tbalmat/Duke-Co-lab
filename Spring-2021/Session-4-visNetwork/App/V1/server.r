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
# Shiny server file
#####################################################################################################

# Information on shiny and visnetwork available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf
# https://cran.r-project.org/web/packages/visnetwork/visnetwork.pdf

#####################################################################################################
# GWAS data description
#
# Source:  synthesized
#
# Columns:
# GWAS ................... GWAS set
# phenotype .............. textual description of phenotype
# SNP .................... rsID of SNP
# p ...................... significance of allele transition regression coefficient
#####################################################################################################

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")

library(shiny)
library(visNetwork)

##########################################################################################################
# Set static filter and appearance parameters
##########################################################################################################

# Vertex colors for GWAS 1 and 2
vc1 <- "#66AAFF"
vc2 <- "#FFEE66"

# Edge colors for GWAS 1 and 2 when SNPs form edges (phenotypes forms vertices)
ec1 <- "#080808"
ec2 <- "#C02020"

# Vertex and edge font sizes
vfsz <- 8
efsz <- 8

# Font stroke (outline) color
fsc1 <- "#909090"

##########################################################################################################
# GWAS data retrieval function
# Retrieve individual data frames for each GWAS
# Save in global memory so that all function can access observations
# Exclude observations with non-positive p values
##########################################################################################################

readData <- function() {
  # Subset to observations with positive p
  gwasdat <- subset(read.table("Data/GWASResults.csv", header=T, sep=",", strip.white=T), p>0)
  # Compute log og p values
  gwasdat[,"log_10_p"] <- -log(gwasdat[,"p"])/log(10)
  return(gwasdat)
}

##########################################################################################################
# Function to assemble graph components (vertices and edges)
##########################################################################################################

assembleNetComponents <- function(gwasdat, log_10_p, physics, eopacity) {

  # Compose data set
  # Join phenotypes from GWAS sets by common SNPs
  gwas <- merge(gwasdat[which(gwasdat[,"GWAS"]==1 & gwasdat[,"log_10_p"]>=log_10_p),c("phenotype", "SNP", "log_10_p")], 
                gwasdat[which(gwasdat[,"GWAS"]==2 & gwasdat[,"log_10_p"]>=log_10_p),c("phenotype", "SNP", "log_10_p")],
                by="SNP", suffixes=1:2)
  colnames(gwas) <- c("lab", "v1", "log_10_p1", "v2", "log_10_p2")

  # Compose vertices
  if(nrow(gwas)>0) {
    # Tabulate edges by GWAS and vertex
    v1 <- aggregate(1:nrow(gwas), by=list(gwas[,"v1"]), length)
    colnames(v1) <- c("lab", "n")
    v2 <- aggregate(1:nrow(gwas), by=list(gwas[,"v2"]), length)
    colnames(v2) <- c("lab", "n")
    # Compose edge hover labels
    if(nrow(gwas)>0)
      gwas[,"hovtext"] <- paste(gwas[,"v1"], ", log<sub>10</sub>(p)=", round(gwas[,"log_10_p1"], 2),
                                "<br>", gwas[,"v2"], ", ", "log<sub>10</sub>(p)=", round(gwas[,"log_10_p2"], 2), sep="")
    # Assign vertex color by GWAS set, edge color static
    vcolor <- c(rep(vc1, nrow(v1)), rep(vc2, nrow(v2)))
    vtcolor <- vcolor
    ecolor <- ec1
    ehcolor <- ec2
    vertex0 <- data.frame("set"=c(rep(1, nrow(v1)), rep(2, nrow(v2))), rbind(v1, v2), "hovtext"=c(v1[,"lab"], v2[,"lab"]))
  } else {
    vertex0 <- data.frame()
  }

  # Compose visNetwork vertex and edge sets to be returned
  if(nrow(vertex0)>0) {
    # Compose vertices
    vertex <- data.frame("id"=1:(nrow(vertex0)),
                         "fixed"=F,
                         "label"=vertex0[,"lab"],
                         "color"=vcolor,
                         "font"=list("color"=vtcolor, "size"=vfsz, strokeWidth=1, "strokeColor"=fsc1),
                         "value"=vertex0[,"n"]/max(vertex0[,"n"], na.rm=T),
                         "title"=vertex0[,"hovtext"],
                         # Include groups for legend configuration (only with phenotypes as vertices)
                         "group"=c("GWAS 1","GWAS 2")[vertex0[,"set"]])
    rownames(vertex) <- NULL
    # Compose vertex IDs (they are required for unambiguous identification in edge construction)
    vid <-setNames(vertex[,"id"], vertex[,"label"])
    # Compose edges
    if(nrow(gwas)>0) {
      edge <- data.frame("from"=vid[gwas[,"v1"]],
                         "to"=vid[gwas[,"v2"]],
                         "label"=gwas[,"lab"], 
                         # Hover text
                         "title"=gwas[,"hovtext"],
                         "hoverWidth"=0,
                         "selectionWidth"=0,
                         "color"=list("color"=ecolor, "opacity"=eopacity, "highlight"=ehcolor),
                         "font"=list("color"="white", "size"=efsz, strokeWidth=1, "strokeColor"=fsc1),
                         "physics"=T,
                         "smooth"=T)
    } else {
      edge <- data.frame()
    }

  } else {
    vertex <- data.frame()
    edge <- data.frame()
  }

  print("net assembled")
  return(list("vertex"=vertex, "edge"=edge))

}

##########################################################################################################
# Function to compose graph using visNetwork() functions
##########################################################################################################

composeNet <- function(vertex, edge, physics) {

  g <- visNetwork(vertex, edge) %>% 
         visGroups(groupname="GWAS 1", color=vc1, font=list("color"="white", "size"=12)) %>%
         visGroups(groupname="GWAS 2", color=vc2, font=list("color"="#202020", "size"=12)) %>%
         visLegend(useGroups=T, position="right") %>%
         visOptions(highlightNearest=list("enabled"=T, "hover"=T)) %>%
         visInteraction(hover=T, hoverConnectedEdges=T, navigationButtons=T)
  if(physics) {
    g <- g %>% visPhysics(timestep=0.25, minVelocity=10, maxVelocity=50,
                          barnesHut=list("avoidOverlap"=0.5, "springLength"=200, "springConstant"=0.5, "damping"=0.5),
                          repulsion=list("nodeDistance"=100))
                          #stabilization=list("enabled"=T, "iterations"=1000))
  } else {
    g <- g %>% visPhysics(enabled=F)
  }
  print("net composed")
  return(g)
}

##########################################################################################################
# Shiny server function
##########################################################################################################

shinyServer(
  function(input, output, session) {

    # Retrieve GWAS observations
    gwasdat <- readData()

    observe({

      # Retain values of reactive variables
      log_10_p <- input$log_10_p
      physics <- input$physics
      eopacity <- input$eopacity

      # Assemble network components
      net <- assembleNetComponents(gwasdat, log_10_p, physics, eopacity)

      # Render graph
      output$g1 <- renderVisNetwork(composeNet(net[["vertex"]], net[["edge"]], physics))

    })

  }
)


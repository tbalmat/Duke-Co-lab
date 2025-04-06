#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 4, plotly, November 2020
# Genome Wide Association Study Pleiotropy App
# Shiny user interface and server script
#####################################################################################################

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

# Information on plotly available at:
# https://cran.r-project.org/web/packages/plotly/plotly.pdf
# https://plotly.com/r/

options(stringsAsFactors=F)
library(shiny)
library(plotly)
library(shinythemes)

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

# Dir location
# Local
setwd("C:\\Projects\\Duke\\Co-lab\\Shiny-Fall-2020\\Session-4-plotly")
# RStudio Cloud
# setwd("/cloud/project/Duke-Co-lab/Shiny/Session-4-plotly")

# Load functions
source("App/Pleiotropy/ui.r")
source("App/Pleiotropy/server.r")
source("App/Pleiotropy/Pleiotropy-Functions.r")

# Read observations
# Note the placement in the global environment, since it is accessed throughout other functions
gwasdat <- readData()

# Launch app
runApp(list("ui"=ui, "server"=server), launch.browser=T)


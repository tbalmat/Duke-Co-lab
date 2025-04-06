#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 3, plotly, Spring 2021
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
setwd("C:/Projects/Duke/Co-lab/Shiny-Spring-2021/Session-3-plotly")

# Load functions
source("App/Pleiotropy/ui.r")
source("App/Pleiotropy/server.r")
source("App/Pleiotropy/Pleiotropy-Functions.r")

# Read observations
# Note the placement in the global environment, since it is accessed throughout other functions
gwasdat <- readData()

# Launch app
runApp(list("ui"=ui, "server"=server), launch.browser=T)


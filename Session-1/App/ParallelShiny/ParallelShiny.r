# Duke University Co-lab Shiny Workshop, Session 1, October 2019

# Launch two apps in parallel
# Note that each app specifies a unique tcp port for http requests
# The uir.r of each app has an anchor tag that targets the other app's listening port
# This way, one app can execute the other

library(parallel)

# Report available cores
#detectCores(all.tests = FALSE, logical = TRUE)

# Specify top level directory containing all ui.r and server.r files

# Local dir
setwd("C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-1-NPDHist-CPDF\\App")

# Rstudio Cloud dir
#setwd("/cloud/project/Duke-Co-lab/Shiny/Session-1-NPDHist-CPDF/App")

# Define a function, to be called in parallel, that launches the specified app
execApp <- function(app) {

  library("shiny")

  if(app=="NPDHist") {
    appDir <- "NPDHist/ShellExecution"
    tcpPort <- 4291
  } else if(app=="CPDF") {
    appDir <- "CPDF/ShellExecution/CPDF-FYSliderBar"
    tcpPort <- 4292
  }

  runApp(appDir=appDir,
         launch.browser=T,
         host = getOption("shiny.host", "127.0.0.1"),
         port=tcpPort,
         display.mode="normal")

}

# Create a two-core parallel cluster
cl <- makePSOCKcluster(rep("localhost", 2))

# Execute two apps
clusterApply(cl, c("NPDHist", "CPDF"), execApp)

# Stop cluster
stopCluster(cl)

# Clean up
rm(cl)
gc()

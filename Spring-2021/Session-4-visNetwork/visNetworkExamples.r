library(visNetwork)

nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges, width = "100%")

visDocumentation()
vignette("Introduction-to-visNetwork") # with CRAN version
# shiny ?
shiny::runApp(system.file("shiny", package = "visNetwork"))
library(visNetwork)

# Generate three nodes
nodes <- data.frame(id = 1:3)
visNetwork(nodes)

# Create edges joining node 1 to 1 and node 2 to 3
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges)

# Introduction to visNetwork
vignette("Introduction-to-visNetwork") # with CRAN version

# Shiny visNetwork demo
shiny::runApp(system.file("shiny", package = "visNetwork"))

# java documentation
visDocumentation()
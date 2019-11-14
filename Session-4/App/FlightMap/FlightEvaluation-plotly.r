# Duke University Co-lab Shiny Workshop, Session 4, November 2019

# Launch app defined in ui.r and server.r in specified appDir
# Note the specification of a tcp port that the process will listen on for http requests

library("shiny")

# Specify directory containing ui.r and server.r

# Local dir
ad <- "C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-4-plotly\\App\\FlightMap"

# Rstudio Cloud dir
#ad <- "/cloud/project/Duke-Co-lab/Shiny/Session-4-plotly/App/FlightMap"

# Execute 
runApp(appDir=ad,
       launch.browser=T,
       host = getOption("shiny.host", "127.0.0.1"),
       port=4298,
       display.mode = c("auto", "normal", "showcase"))

# Duke University Co-lab Shiny Workshop, Session 3, November 2019

# Launch app defined in ui.r and server.r in specified appDir
# Note the specification of a tcp port that the process will listen on for http requests

library("shiny")

# Specify directory containing ui.r and server.r

# Local dir
ad <- "C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-3-ggplot\\App"

# Rstudio Cloud dir
#ad <- "/cloud/project/Duke-Co-lab/Shiny/Session-3-ggplot/App"

# Execute 
runApp(appDir=ad,
       launch.browser=T,
       host = getOption("shiny.host", "127.0.0.1"),
       port=4297,
       display.mode = c("auto", "normal", "showcase"))

# Duke University Co-lab Shiny Workshop, Session 3, November 2019

# Launch app defined in ui.r and server.r in specified appDir
# Note the specification of a tcp port that the process will listen on for http requests

library("shiny")

# Specify directory containing ui.r and server.r

# Local dir
ad <- "C:/Projects/Duke/Co-lab/Shiny-Spring-2021/Session-4-visNetwork/App/V3"

# Execute 
runApp(appDir=ad,
       launch.browser=T,
       host = getOption("shiny.host", "127.0.0.1"),
       port=4301,
       display.mode = c("auto", "normal", "showcase"))

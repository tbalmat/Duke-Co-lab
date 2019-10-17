library("shiny")

runApp(appDir="C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-1-NPDHist-CPDF\\App\\NPDHist\\ShellExecution",
       launch.browser=T,
       host = getOption("shiny.host", "127.0.0.1"),
       port=4291,
       display.mode = c("auto", "normal", "showcase"))
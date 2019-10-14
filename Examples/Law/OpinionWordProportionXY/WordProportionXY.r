library("shiny")

runApp(appDir="C:\\Projects\\Duke\\Law\\LexisNexisCaseAnalysis\\LexisNexisDecisionSample\\OpinionTextAnalysis\\OpinionTextAnalysisApp\\WordProportionXY",
       launch.browser=T,
       host = getOption("shiny.host", "127.0.0.1"),
       #port=4291,
       display.mode = c("auto", "normal", "showcase"))
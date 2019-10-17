# Shiny App
# Navigation page for multiple apps

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")

library(shiny)

# A Shiny app consists of ui() and server() functions

# ui() can contain R statements (open a database and query it to populate selection lists, etc.), but its primary
# purpose is to format your web page (notice the explicit use of HTML tags)

# The HTML() function instructs Shiny to pass contained text to the browser verbatim, and is useful for formatting
# your page

# server() is a function containing R statements and function calls 
# Any base function, functions declared in loaded packages (importantly, Shiny, here), or functions that you create
# in global memory cacn be called

# runApp() is a Shiny function that launches your default browser, renders a page based on the ui() function passed,
# then executes the server() function

ui <- function(req) {

  navbarPage(title="Co-lab Shiny Workshop",
    windowTitle="Co-Lab Shiny",
    tabPanel("Normal Random Histograms", HTML("<br><b>Your Shiny app here!</b><br><br>ui() and server()")),
    tabPanel("OPM Overview", HTML("<br><b>Another great Shiny app here!</b><br><br>ui() and server()"))
  )

}

server <- function(input, output, session) {}

# Execute
runApp(list("ui"=ui, "server"=server), launch.browser=T)
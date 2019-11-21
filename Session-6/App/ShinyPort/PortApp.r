#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 6, November 2019
# Local Port Demonstration App
#####################################################################################################

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

options(stringsAsFactors=F)
library(shiny)
library(shinythemes)
library(DT)

# Specify directory location
dr <- c("local"="C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-4-plotly",
        "cloud"="/cloud/project/Duke-Co-lab/Shiny/Session-4-plotly")[1]
setwd(dr)

# Specify app index (1 or 2)
app <- 1

# Specify TCP/IP port
port <- c(3997, 3998)

#####################################################################################################
# Shiny user interface function
#####################################################################################################

ui <- function(req) {

  fluidPage(

    titlePanel(
      div(HTML("<font size=+2 color=white>&nbsp;Duke University Shiny TCP/IP Port Demonstration</font>"),
          style="height:38px; background-color:#8080c0"),
      windowTitle="Co-Lab Shiny"
    ),

    fluidRow(
      HTML(paste("<br><a href=http://127.0.0.1:", port[app%%2+1], ">to app ", app%%2+1, "</a>", sep="")),
      style="margin-left:40px"
    ),

    fluidRow(
      column(width=4,
        HTML("<br><br><br><br>"),
        dataTableOutput("tab")
      ),
      style="margin-left:20px"
    )

  )

}


#####################################################################################################
# Shiny server function
#####################################################################################################

server <- function(input, output, session) {

  # Render a table
  if(app==1) {
    output$tab <- renderDataTable(Loblolly,
                                  options=list(bLengthChange=F, bFilter=F),
                                  caption=HTML("<b><font size=+1><center>Loblolly Data</b></font><br>"))
  } else {
    output$tab <- renderDataTable(Orange,
                                  options=list(bLengthChange=F, bFilter=F),
                                  caption=HTML("<b><font size=+1><center>Orange Data</b></font><br>"))
  }
}


#####################################################################################################
# Launch app, listening on specified port
#####################################################################################################

runApp(list("ui"=ui, "server"=server), port=port[app], launch.browser=F, host="127.0.0.1")


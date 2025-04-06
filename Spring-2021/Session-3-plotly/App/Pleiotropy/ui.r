#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 3, plotly, Spring 2021
# Genome Wide Association Study Pleiotropy App
# Shiny user interface function
#####################################################################################################

ui <- function(req) {

  fluidPage(

    # Set browser tab title
    title="GWAS Pleiotropy",

    # Reposition and alter appearance of notification window
    tags$style(HTML(".shiny-notification {font-size:14px; color:black; font-weight:bold; width:50%;
                     height=200px; position:fixed; top:calc(50%); left:calc(25%)}")),

    # Specify theme
    theme=shinytheme("flatly"),

    # Render navbar layout
    navbarPage(

      title=HTML("Duke University GWAS Pleiotropy Analysis&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
      windowTitle="Co-Lab Shiny",

      # Tab 1:  Cross-phenotype link
      tabPanel("Cross-phenotype link",
        fluidRow(width=12,
          HTML("<br>"),

          # Prompt
          sidebarPanel(width=2,
            sliderInput("t1MinLogp", HTML("min log<sub>10</sub>(p)"), min=0, max=10, step=0.1, value=5)
          ),

          # Graph 1
          column(width=5,
            HTML("<center><H3>GWAS 1</H3><br>"),
            plotlyOutput("t1Plot1", height="800px"),
            HTML("</center>")
          ),

          # Graph 2
          column(width=5,
            HTML("<center><H3>GWAS 2</H3><br>"),
            plotlyOutput("t1Plot2", height="800px"),
            HTML("</center>")
          )

        )
      ),

      # Panel 2:  Bipartite graph 
      tabPanel("Bipartite association graph",
        fluidRow(widh=12,
          HTML("<br>"),

          # Prompt
          column(width=2,
            sidebarPanel(width=12,
              sliderInput("t2MinLogp", HTML("min log<sub>10</sub>(p)"), min=0, max=10, step=0.1, value=5)
            )
          ),

          # Graph
          column(width=8,
            HTML("<center>"),
            plotlyOutput("t2Plot", height="800px"),
            HTML("</center>")
          )

        )
      )

    )

  )

}

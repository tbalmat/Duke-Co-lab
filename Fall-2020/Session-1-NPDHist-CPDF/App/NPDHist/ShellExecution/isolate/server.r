# Shiny App
# Generate a histogram from random normal values

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")

library(shiny)
library(ggplot2)

shinyServer (

  function(input, output, session) {
  
    # Function to generate either a ggplot object or NULL if standard deviation non-positive
    npdPlot <- function(n, w, mn, sd, barColor, dispCurve, curveColor) {
      if(sd>0) {
        g <- ggplot() +
          # Bars
          geom_histogram(aes(x=rnorm(n, mean=mn, sd=sd)), color="white", binwidth=w, fill=barColor)
        if(dispCurve)
          # Continuous normal model curve
          g <- g + geom_line(aes(x=seq(mn-4*sd, mn+4*sd, length.out=100),
                                 y=n*w*dnorm(seq(mn-4*sd, mn+4*sd, length.out=100), mn, sd)), color=curveColor, size=2)
          # Static portion of plot
          g <- g + scale_x_continuous(limits=c(mn-4*sd, mn+4*sd)) +
          scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
          theme(plot.title=element_text(size=14, hjust=0.5),
                plot.subtitle=element_text(size=12, hjust=0.5),
                plot.caption=element_text(size=12, hjust=0.5),
                panel.background=element_blank(),
                panel.grid.major.x=element_blank(),
                panel.grid.major.y=element_blank(),
                panel.grid.minor=element_blank(),
                panel.border=element_rect(fill=NA, color="gray75"),
                panel.spacing.x=unit(0, "lines"),
                axis.title.x=element_text(size=12),
                axis.title.y=element_text(size=12),
                axis.text.x=element_text(size=10),
                axis.text.y=element_text(size=10),
                strip.text=element_text(size=10),
                strip.background=element_blank(),
                legend.position="bottom",
                legend.background=element_rect(color="gray"),
                legend.key=element_rect(fill="white"),
                legend.box="horizontal",
                legend.text=element_text(size=8),
                legend.title=element_text(size=8)) +
          labs(title=paste(format(n, big.mark=","), " normal(", mn, ", ", sd, ") pseudo-random values\n", sep=""), x="\nz", y="frequency\n")
      } else {
        g <- NULL
      }
      return(g)
    }

    # Function to test parameter values and return message or NULL (if all parameters valid)
    msg <- function(w, sd) {
      txt <- NULL
      if(w==0) {
        txt <- "Hmmm:  perhaps bar width should be larger"
        color <- "black"
      } else if(sd<=0) {
        txt <- "Error:  standard deviation must be positive"
        color <- "red"
      }
      if(!is.null(txt)) {
        return(paste("<font color=", color, ">", txt, "&nbsp;&nbsp;&nbspTime:  ", Sys.time(), "</font>", sep=""))
      } else {
        return(NULL)
      }
    }
  
    # Use of cat() displays messages in R console, stderr() causes disply in red and writes to log (Shiny server)
    #cat("AAA", file=stderr())

    # Create and render plot
    # Note the use of input$n, input$w, etc. in the reactive function (renderPlot)
    # In this case, the result is the same as using m <- reactive(input$m) followed by reference to m()
    # Cases have been observed where reactive values are not available (such as when placing the executed
    # function directly in the renderPlot() call)
    output$plot <- renderPlot(npdPlot(input$n, input$w, input$mean, input$sd, input$barColor,
                              isolate(input$dispCurve), input$curveColor))

    output$plot <- renderPlot({npdPlot(isolate(input$n), isolate(input$w), isolate(input$mean), isolate(input$sd), isolate(input$barColor),
                              input$dispCurve, isolate(input$curveColor))
                              cat("A")})

    # Generate a parameter (in)validation message (message is NULL if valid)
    output$msg <- renderText(HTML(msg(input$w, input$sd)))

  }

)
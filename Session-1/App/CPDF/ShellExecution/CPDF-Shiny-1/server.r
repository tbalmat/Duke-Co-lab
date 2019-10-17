# Shiny App
# Visually explore cross-sectional features of highly aggregated U.S. federal employee data
# Version 2, Shiny
# Server function

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(shiny)
library(ggplot2)

shinyServer (

  function(input, output, session) {

    #########################################
    # Configure common theme
    #########################################

    ggTheme <- ggplot2::theme(plot.title=element_text(size=12, hjust=0.5),
                     #plot.caption=element_text(size=12, hjust=0.5),
                     panel.background=element_blank(),
                     panel.grid.major.x=element_blank(),
                     panel.grid.major.y=element_blank(),
                     panel.grid.minor=element_blank(),
                     panel.border=element_rect(fill=NA, color="gray75"),
                     panel.spacing=unit(0, "inches"),
                     axis.title.x=element_text(size=10),
                     axis.title.y=element_text(size=10),
                     axis.text.x=element_text(size=8, angle=90, hjust=1, vjust=0.5),
                     axis.text.y=element_text(size=8),
                     #axis.ticks=element_blank(),
                     strip.text=element_text(size=8),
                     strip.background=element_blank(),
                     legend.position="bottom",
                     legend.background=element_rect(color="gray"),
                     legend.key=element_rect(fill="white"),
                     legend.box="horizontal",
                     legend.text=element_text(size=8),
                     legend.title=element_text(size=8))
    #typeof(ggTheme)

    #########################################
    # Message functions
    #########################################

    # Display message function for tab 1
    t1DisplayMessage <- function(msg, color="black")
      output$t1Msg <- renderText({HTML(paste("<font color=", color, ">", msg, "</font>", "<br><br>Time:  ", Sys.time(), "</font>", sep=""))})

    # Clear message function for tab 1
    t1ClearMessage <- function()
      output$t1Msg <- renderText(NULL)

    # Display message function for tab 2
    t2DisplayMessage <- function(msg, color="black")
      output$t2Msg <- renderText({HTML(paste("<font color=", color, ">", msg, "</font>", "<br><br>Time:  ", Sys.time(), "</font>", sep=""))})


    # Clear message function for tab 2
    t2ClearMessage <- function()
      output$t2Msg <- renderText(NULL)

    #########################################
    # Plot functions
    #########################################

    ####
    # Compose plot function for tab 1
    ####

    t1ComposePlot <- function(gdat, gType, diffVar, indepVar, depVarMean, pointSize, pointAlpha,
                              panelVar, panelRows, panelCols) {
      # Initialize plot
      g <- ggplot()
      # Some error trapping should be employed
      # Convert differentiation variable to discrete for aes() color assignment
      # Convert "none" values to NULL
      # They cause the associated aes parameter to be ignored
      if(diffVar!="none") {
        gdat[,diffVar] <- factor(gdat[,diffVar])
      } else {
        diffVar <- NULL
      }
      if(pointSize=="none")
        pointSize <- NULL
      if(pointAlpha=="none")
        pointAlpha <- NULL
      # Compose point or line graph
      if(gType=="point") {
        if(is.null(diffVar)) {
          g <- g + geom_point(data=gdat,
                              aes_string(x=indepVar, y=depVarMean),
                              size=pointSize, alpha=pointAlpha)
        } else {
          g <- g + geom_point(data=gdat,
                              aes_string(x=indepVar, y=depVarMean, color=diffVar),
                              size=pointSize, alpha=pointAlpha) +
          scale_color_manual(values=colorRampPalette(c("blue", "red"))(length(unique(gdat[,diffVar]))))
        }
      } else {
        if(is.null(diffVar)) {
          g <- g + geom_line(data=gdat, aes_string(x=indepVar, y=depVarMean))
        } else {
          g <- g + geom_line(data=gdat, aes_string(x=indepVar, y=depVarMean, color=diffVar)) +
          scale_color_manual(values=colorRampPalette(c("blue", "red"))(length(unique(gdat[,diffVar]))))
        }
      }
      # Facet, if requested
      if(panelVar!="none")
        g <- g + facet_wrap(panelVar, nrow=panelRows, ncol=panelCols,
                            labeller=as_labeller(function(x) paste(panelVar, " = ", x, sep="")))
      # Configure axis labes and apply theme
      g <- g +  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
        ggTheme +
        labs(x=paste("\n", indepVar, sep=""), y=paste("mean ", sub("mean_", "", depVarMean), "\n", sep=""))
      # Return composed plot
      return(g)
    }

    ####
    # Compose plot function for tab 2
    ####

    t2ComposePlot <- function(gdat, indepVar, panelVar, panelRows, panelCols, loessSpan) {
      # Initialize plot
      g <- ggplot()
      # Some error trapping should be employed
      # Initialize plot
      g <- ggplot()
      # Add smooth probability mass plot
      g <- g + geom_smooth(data=gdat, aes_string(x=indepVar, y="p"),
                           method="loess", se=F, span=loessSpan, fullrange=T, color="Black", size=0.6)
      # Facet, if requested
      if(panelVar!="none")
        g <- g + facet_wrap(panelVar, nrow=panelRows, ncol=panelCols,
                            labeller=as_labeller(function(x) paste(panelVar, " = ", x, sep="")))
      # Configure axis labes and apply theme
      g <- g +  ggTheme +
        labs(x=paste("\n", indepVar, sep=""), y="mass\n")
      # Return composed plot
      return(g)
    }

    #########################################
    # Event handler functions
    #########################################

    ####
    # Tab 1 plot action button event
    # Note that variable created within this function are local to it (isolated from other environments)
    ####

    observeEvent(input$t1ActionPlot, {

      t1ClearMessage()

      # Copy prompt values to local variables
      # These will be used in reactive functions to avoid immediate update on modification
      depVar <- input$t1DepVar
      indepVar <- input$t1IndepVar
      diffVar <- input$t1DiffVar
      graphType <- input$t1GraphType
      pointSize <- input$t1PointSize
      pointAlpha <- input$t1PointAlpha
      panelVar <- input$t1PanelVar
      panelRows <- input$t1PanelRows
      panelCols <- input$t1PanelCols

      # All parameter values should be validated before proceeding
      errMsg <- ""
      if(depVar==indepVar)
        errMsg <- "dependent and independent variables must be different"

      if(errMsg=="") {

        # Filter by agency
        # Aggregate mean of dependent var by independent var, differentiation var, and facet var
        # Compose name for mean variable
        depVarMean <- paste("mean_", depVar, sep="")
        # Compose list of variables to aggregate by
        # Compose aggregated data col names also
        aggVar <- list(cpdf[,indepVar])
        gdatNames <- input$t1IndepVar
        if(input$t1DiffVar!="none") {
          aggVar[[length(aggVar)+1]] <- cpdf[,diffVar]
          gdatNames <- c(gdatNames, diffVar)
        }
        if(input$t1PanelVar!="none") {
          aggVar[[length(aggVar)+1]] <- cpdf[,panelVar]
          gdatNames <- c(gdatNames, panelVar)
        }

        # Aggregate
        gdat <- aggregate(1:nrow(cpdf),
                          by=aggVar,
                          function(i) sum(cpdf[i,depVar]*cpdf[i,"n"])/sum(cpdf[i,"n"]))
        colnames(gdat) <- c(gdatNames, depVarMean)

        # Order occupational categories in standard P, A, T, C, O sequence, if present
        if("occCat" %in% colnames(gdat))
          gdat[,"occCat"] <- factor(gdat[,"occCat"], levels=c("P", "A", "T", "C", "O"))

        #print(gdat)
        #cat(paste(graphType, diffVar, indepVar, depVarMean), sep=", ")

        # Generate and render plot
        output$t1Plot <- renderPlot(t1ComposePlot(gdat, graphType, diffVar, indepVar, depVarMean, pointSize, pointAlpha,
                                                  panelVar, panelRows, panelCols))

      } else {

        output$t1Plot <- renderPlot(NULL)
        t1DisplayMessage(errMsg, "red")

      }

    })

    ####
    # Tab 2 plot action button event
    # Note that variable created within this function are local to it (isolated from other environments)
    ####

    observeEvent(input$t2ActionPlot, {

      t1ClearMessage()

      # Copy prompt values to local variables
      # These will be used in reactive functions to avoid immediate update on modification
      indepVar <- input$t2IndepVar
      panelVar <- input$t2PanelVar
      panelRows <- input$t2PanelRows
      panelCols <- input$t2PanelCols
      loessSpan <- input$t2LoessSpan

      # All parameter values should be validated before proceeding
      errMsg <- ""
      if(loessSpan<=0)
        errMsg <- "LOESS span must be positive"

      if(errMsg=="") {

        # Compute observed mass distribution(s) of independent var
        if(panelVar=="none") {
          ng <- sum(cpdf[,"n"])
          gdat <- aggregate(cpdf[,"n"], by=list(cpdf[,indepVar]), function(n) sum(n)/ng)
          colnames(gdat) <- c(indepVar, "p")
        } else {
          gdat <- aggregate(cpdf[,"n"], by=list(cpdf[,indepVar], cpdf[,panelVar]), sum)
          colnames(gdat) <- c(indepVar, panelVar, "n")
          ng <- aggregate(gdat[,"n"], by=list(gdat[,panelVar]), sum)
          colnames(ng) <- c(panelVar, "n")
          gdat <- merge(gdat, ng, by.x=panelVar, by.y=panelVar)
          gdat <- data.frame(gdat[,c(panelVar, indepVar, "n.x")], "p"=gdat[,"n.x"]/gdat[,"n.y"])
          colnames(gdat) <- c(panelVar, indepVar, "n", "p") 
        }

        # Order occupational categories in standard P, A, T, C, O sequence, if present
        if("occCat" %in% colnames(gdat))
          gdat[,"occCat"] <- factor(gdat[,"occCat"], levels=c("P", "A", "T", "C", "O"))

        #print(gdat)
        #cat(paste(indepVar, panelVar, loessSpan), sep=", ")

        # Generate and render plot
        output$t2Plot <- renderPlot(t2ComposePlot(gdat, indepVar, panelVar, panelRows, panelCols, loessSpan))

      } else {

        output$t2Plot <- renderPlot(NULL)
        t2DisplayMessage(errMsg, "red")

      }

    })

  }

)
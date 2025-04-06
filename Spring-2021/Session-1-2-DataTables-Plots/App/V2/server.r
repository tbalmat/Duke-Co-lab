# Duke University Co-lab Shiny Workshop, Session 1-2, Spring 2021

# Shiny App
# Compose tables of aggregated OPM CPDF U.S. federal employee career variables
# Visually explore cross-sectional features of subsets of employees contained in disjoint rows
# of aggregation table

# Version 2, first Shiny implementation

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")

library(shiny)
library(DT)
library(ggplot2)

shinyServer(

  function(input, output, session) {

    #######################################################################################################
    # Configure common theme for plots
    #######################################################################################################

    ggTheme <- ggplot2::theme(plot.title=element_text(size=12, hjust=0.5),
                     #plot.caption=element_text(size=12, hjust=0.5),
                     panel.background=element_blank(),
                     panel.grid.major.x=element_blank(),
                     panel.grid.major.y=element_blank(),
                     panel.grid.minor=element_blank(),
                     panel.border=element_rect(fill=NA, color="gray75"),
                     panel.spacing=unit(0, "inches"),
                     axis.title.x=element_text(size=12),
                     axis.title.y=element_text(size=12),
                     axis.text.x=element_text(size=10, angle=90, hjust=1, vjust=0.5),
                     axis.text.y=element_text(size=10),
                     #axis.ticks=element_blank(),
                     strip.text=element_text(size=10),
                     strip.background=element_blank(),
                     legend.position="bottom",
                     legend.background=element_rect(color="gray"),
                     legend.key=element_rect(fill="white"),
                     legend.box="horizontal",
                     legend.text=element_text(size=10),
                     legend.title=element_text(size=10))

    #######################################################################################################
    # Function to compose aggregation table
    #######################################################################################################

    t1ComposeTable <- function(depVar, indepVar, agencyPos, occPos) {

      if(length(depVar)>0 & length(indepVar)>0) {

        # Compose subset indices for each joint category of specified independent var(s)
        # These will be used to compute tabular statistics and give subsets for box plots
        # Note the global declaration so that the data frame is accessible outside of this function
        iagg <<- aggregate(1:nrow(cpdf),
                           by=lapply(indepVar,
                                     function(v)
                                       # Note that agency and occupation position limits are defined in ui.r
                                       if(v=="Agency" & agencyPos<agencyPosLimit[2]) {
                                         substring(cpdf[,v], 1, agencyPos)
                                       } else if(v=="Occupation" & occPos<occPosLimit[2]) {
                                         substring(cpdf[,v], 1, occPos)
                                       } else {
                                         cpdf[,v]
                                       }),
                           function(k) k)
        colnames(iagg) <<- c(indepVar, "k")

        if(is.data.frame(iagg)) {

          # Aggregate mean and quartiles of dependent var for each independent var combination
          # Create a data frame from specified independent vars with aggregation stats appended in corresponding cols
          aggdat <- data.frame(iagg[,indepVar],
                      # rbind list elements returned by apply() into a single matrix
                      do.call(rbind,
                        apply(as.matrix(1:nrow(iagg)), 1,
                          function(i) {
                            k <- iagg[i,"k"][[1]]
                            data.frame(length(k),
                                       mean(cpdf[k,depVar]),
                                       # Use transpose, otherwise n and mean are recycled for each quantile
                                       t(quantile(cpdf[k,depVar], c(0.25, 0.5, 0.75))))
                          })))
          colnames(aggdat) <- c(input$t1IndepVar, "n", "mean", "q25", "q50", "q75")

          if(is.data.frame(aggdat)) {

            # Render table of aggregated results with urls to agency and occupation links

            output$t1Table <-
              DT::renderDataTable(
                datatable(aggdat,
                          rownames=F, escape=F, selection="single",
                          options=list(pageLength=20, bLengthChange=F, bFilter=F, #autoWidth=F,
                                       #columnDefs=list(list(className="dt-right", targets=4:5),
                                       #                list(className="dt-center", targets=6)),
                                       #                #list(width='10px', targets=1)),
                                       order=list(list(1, "asc"))) # cols are 0-based
                         )
              )

          } else {

            output$t1Table <- DT::renderDataTable(NULL)
            showNotification("No data to display.  No observations satisfy query criteria.", type="error")

          }

        } else {

          output$t1Table <- DT::renderDataTable(NULL)
          showNotification("No data to display.  No observations satisfy query criteria.", type="error")

        }

      } else {

        output$t1Table <- DT::renderDataTable(NULL)
        showNotification("No data to display.  Unknown dependent or independent variable(s).", type="error")

      }

    }

    ###########################################################################################################
    # Function to compose box plot for subset of observations corresponding to specified aggregation table row
    ###########################################################################################################

    t2RenderPlot <- function(t1RowSelected, depVar, aggVar, agencyPos, occPos, indepVar, panelVar, panelRows,
                             panelCols, pointDisplay, pointDiffVar, pointSize, pointJitterWidth, pointAlpha) {

      # Require distinct dependent, independent, and panel variables
      if(length(unique(c(depVar, indepVar, panelVar)))==3) {

        # Require independent and panel variables to be different from table aggregation variable(s)
        if(length(intersect(c(indepVar, panelVar), aggVar))==0) {

          # Retrieve subset indices
          k <- iagg[t1RowSelected,"k"][[1]]

          # Compose graph data set
          # Truncate agency and occupation, if necessary
          # Coerse x to a factor, otherwise it is treated as continuous and produces a plot with a single box
          if(indepVar=="Agency" & agencyPos<agencyPosLimit[2]) {
            gdat <- data.frame(factor(substring(cpdf[k,indepVar], 1, agencyPos)))
          } else if(indepVar=="Occupation" & occPos<occPosLimit[2]) {
            gdat <- data.frame(factor(substring(cpdf[k,indepVar], 1, occPos)))
          } else {
            gdat <- data.frame(factor(cpdf[k,indepVar]))
          }
          colnames(gdat) <- indepVar

          # Append dependent var
          gdat[[depVar]] <- cpdf[k,depVar]

          # Include panel and point differentiation vars, if specified
          if(panelVar!="none")
            gdat[panelVar] <- cpdf[k,panelVar]
          if(pointDiffVar!="none")
            gdat[pointDiffVar] <- cpdf[k,pointDiffVar]

          # Order occupational category in P, A, T, C, O sequence when specified
          if("OccupationalCategory" %in% colnames(gdat))
            gdat[,"OccupationalCategory"] <- factor(gdat[,"OccupationalCategory"], levels=c("P", "A", "T", "C", "O"))

          # Initialize plot
          g <- ggplot()

          # Include points first so that boxes overlay them
          if(pointDisplay) {
            g <- g + geom_jitter(data=gdat, aes_string(x=indepVar, y=depVar),
                                 width=pointJitterWidth, size=pointSize, alpha=pointAlpha)
            # Assign point color from point differentiation var
            if(pointDiffVar!="none") {
              # Create an additional mapping by appending an aes() for color
              # Note that jitter points are the last layer to have been added
              # Note, also, the required spelling of "colour" ("color" causes contention between two color scales during render)
              g[["layers"]][[length(g[["layers"]])]][["mapping"]][["colour"]] <- aes_string(color=pointDiffVar)[[1]]
              g <- g + scale_color_manual(name=pointDiffVar,
                                          values=colorRampPalette(c("blue", "red"))(length(unique(gdat[,pointDiffVar]))))
            }
          }

          # Boxplot with error bars
          g <- g + geom_boxplot(data=gdat, aes_string(x=indepVar, y=depVar), fill=NA, outlier.shape=NA) +
          stat_boxplot(data=gdat, aes_string(x=indepVar, y=depVar), geom='errorbar', color="gray50", width=0.4)
            
          # Facet, if requested
          if(panelVar!="none") {
            g <- g + facet_wrap(panelVar, #nrow=panelRows, ncol=panelCols,
                                labeller=as_labeller(function(x) paste(panelVar, " = ", x, sep="")))
            # Include panel rows and cols, if specified
            # This avoids issuance of warnings when nrow or ncol are NULL
            if(!is.na(panelRows))
              g[["facet"]][["params"]][["nrow"]] <- panelRows
            if(!is.na(panelCols))
              g[["facet"]][["params"]][["ncol"]] <- panelCols
          }

          # Configure axis labels and apply theme
          g <- g +  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
            ggTheme +
            labs(x=paste("\n", indepVar, sep=""), y=paste(depVar, "\n", sep=""))

          # Render
          output$t2Plot <- renderPlot(g)

        } else {

          output$t2Plot <- renderPlot(NULL)
          showNotification("Independent and panel variables must be different from table aggregation (independent) variables.", type="error")

        }

      } else {

        output$t2Plot <- renderPlot(NULL)
        showNotification("Dependent, independent, and panel variables must be unique.", type="error")

      }

    }

    #######################################################################################################
    # Action for change in tab 1 input variables
    # Compose aggregation table corresponding to current input values
    #######################################################################################################

    observeEvent(
      c(input$t1DepVar, input$t1IndepVar, input$t1AgencyPos, input$t1OccPos), {

      # Render table
      t1ComposeTable(input$t1DepVar, input$t1IndepVar, input$t1AgencyPos, input$t1OccPos)

      }, ignoreInit=T
    )

    #######################################################################################################
    # Action for table row selection
    # Render box plot for subset of observations corresponding to selected table row
    #######################################################################################################

    observeEvent(
      input$t1Table_rows_selected, {

        # Save current selected row, for use in other events
        t1SelectedRow <<- input$t1Table_rows_selected

        # Render plot
        # Pass input variables as parameters to avoid reactivity within plot function
        t2RenderPlot(t1SelectedRow, input$t1DepVar, input$t1IndepVar, input$t1AgencyPos, input$t1OccPos,
                     input$t2IndepVar, input$t2PanelVar, input$t2PanelRows, input$t2PanelCols,
                     input$t2PointDisplay, input$t2PointDiffVar, input$t2PointSize, input$t2PointJitterWidth,
                     input$t2PointAlpha)

        # Set plot tab as current
        updateTabsetPanel(session, "cpdfTabs", selected="t2")

      }, ignoreInit=T
    )

    #######################################################################################################
    # Action for tab 2 plot button
    # Compose box plot for subset of observations corresponding to selected table row and
    # and current plot parameter values on tab 2
    #######################################################################################################

    observeEvent(
      input$t2ActionPlot, {

      # Render plot
      # Pass input variables as parameters to avoid reactivity within plot function
      t2RenderPlot(t1SelectedRow, input$t1DepVar, input$t1IndepVar, input$t1AgencyPos, input$t1OccPos,
                   input$t2IndepVar, input$t2PanelVar, input$t2PanelRows, input$t2PanelCols,
                   input$t2PointDisplay, input$t2PointDiffVar, input$t2PointSize, input$t2PointJitterWidth,
                   input$t2PointAlpha)

      }, ignoreInit=T
    )
   
  }

)
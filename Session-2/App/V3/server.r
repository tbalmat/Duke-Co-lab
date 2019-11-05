# Duke University Co-lab Shiny Workshop, Session 2, November 2019

# Shiny App
# Compose tables of aggregated OPM CPDF U.S. federal employee career variables
# Visually explore cross-sectional features of subsets of employees contained in disjoint rows
# of aggregation table

# Version 3

# Table features:
# Convert table construction to a callable function
# Use notification windows for error reporting
# Change default rows per page and enable rows per page adjustment in data table
# Enable global search function (with regex capability)
# Implement csv download feature
# Include HTML anchor tags for agency and occupation
# Convert row selection to cell selection (to prevent plot generation when clicking an anchor)
# Set default order of rows using columns corresponding to independent variables
# Format numeric columns
# Customize column headers with HTML
# Enable cell editing
# Include column search fields
# Include an HTML formatted caption

# Plot features:
# Use notification windows for error reporting
# Display a progress indicator during rendering
# Attempt to clear existing plot prior to rendering new one
# Display observation count for each panel
# Include control for box intensity
# Replace point display (yes/no) with option for fixed or variable sized (n) points 
#

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


    #######################################################################################################
    # Function to compose summary table for specified query parameters
    # Information on data tables available at:
    # https://rstudio.github.io/DT/
    # https://cran.r-project.org/web/packages/DT/DT.pdf
    #######################################################################################################

    t1RenderTable <- function(depVar, indepVar, agencyPos, occPos) {

      # Compose subset indices for each joint category of specified independent var(s)
      # These will be used to compute tabular statistics and give subsets for box plots
      # Note the global declaration so that the data frame is accessible outside of this function
      iagg <<- aggregate(1:nrow(cpdf),
                         by=lapply(indepVar,
                                   function(v)
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
        colnames(aggdat) <- c(indepVar, "n", "mean", "q25", "q50", "q75")

        if(is.data.frame(aggdat)) {

          # Convert agency and occupation values to HTML anchor tags targeting OPM definitions
          # Note that graph observations are filtered by observation index, so modifying agency
          # does not affect the graph data source
          if("Agency" %in% colnames(aggdat))
            aggdat[,"Agency"] <- apply(as.matrix(aggdat[,"Agency"]), 1,
                                       function(a)
                                         paste("<a href=https://www.usa.gov/federal-agencies/",
                                               substring(a, 1, 1), " target=_blank>", a, "</a>", sep=""))
          if("Occupation" %in% colnames(aggdat))
            aggdat[,"Occupation"] <- apply(as.matrix(aggdat[,"Occupation"]), 1,
                                       function(occ)
                                         paste("<a href=https://www.google.com/search?q=opm+job+series+", occ,
                                               " target=_blank>", occ, "</a>", sep=""))

          # Render table of aggregated results with urls to agency and occupation links
          # Information on data tables options available at:
          # https://datatables.net/reference/option/
          output$t1Table <-
            DT::renderDataTable(
              datatable(# Format numeric values in the aggregated data
                        # Note that format converts values to character, so they must be right justified later
                        data.frame(aggdat[,1:length(indepVar)],
                                   format(aggdat[,"n"], big.mark=","),
                                   format(aggdat[,c("mean", "q25", "q50", "q75")], big.mark=",", nsmall=2)),

                        # Include a caption
                        caption=HTML(paste("<b><font size=+1 color=#0000b0><center>U.S. Federal Employee Human Capital ",
                                           depVar, " Summary for ", paste(indepVar, collapse=", ", sep=""),
                                           "</center></font></b>", sep="")),

                        # Suppress row names, do not escape HTML tags
                        # Automatically hide nav buttons when rows less than pages per row
                        rownames=F, escape=F, autoHideNavigation=T,

                        # Use cell selection (default is row) to avoid rendering plot when url selected in cell
                        # Limit selection to single cell (multiple is an option, which causes the _cell_clicked
                        # list to be appended with another entry each time a cell is clicked)
                        selection=list(mode="single", target="cell"),

                        # Redefine column header (from default data frame colnames)
                        # Note the use of embedded HTML for subscripts
                        colnames=c(indepVar, "n", "mean",
                                   HTML("quantile<sub>0.25</sub>", sep=""),
                                   HTML("quantile<sub>0.5</sub>", sep=""),
                                   HTML("quantile<sub>0.75</sub>", sep="")),

                        # Cells can be edited, which modifies the source data frame (what you do with the modified DF is for you to decide)
                        editable="cell",

                        # Table appearance can be redefined in CSS options
                        class="cell-border stripe", 

                        # Include column filters ("bottom" will display them below table)
                        filter="top",

                        # Include button extension (for download)
                        extensions="Buttons",

                        # Configure other table options
                        # Information on data tables options available at https://rstudio.github.io/DT/options.html
                        options=list( # Specify what is to appear and in what order
                                      # Additional information available at https://datatables.net/reference/option/dom
                                      # Info on position objects available at https://stackoverflow.com/questions/49035864/positioning-datatables-elements-with-dom-option
                                      # Symbols are: (t)able,
                                      #              p(r)ocessing display element (not sure what this is),
                                      #              (i)nfo summary (showing rec i of n)
                                      #              (p)age control
                                      #              (B)uttons (download, print)
                                      #              (l)ength of page (number of records) control
                                      #              (f)ind
                                      # Simple method
                                      # dom="tripBlf",
                                      # Method to specify row and col layout
                                      dom=paste("<'row'<'col-sm-3'l><'col-sm-5'f><'col-sm-3'><'col-sm-1'B>>",
                                                "<'row'<'col-sm-12'tr>>",
                                                "<'row'<'col-sm-5'i><'col-sm-7'p>>", sep=""),

                                      # Alternative to enabling display of page length (rows per page) and global filter controls
                                      #bLengthChange=T, bFilter=T,

                                      # Set rows per page
                                      pageLength=15, lengthMenu=seq(5, 50, 5),

                                      # Allow regex style searches
                                      search=list(regex=T, caseInsensitive=T),

                                      # Let DT set col widths (basically to width of max entry)
                                      autoWidth=T,

                                      # Format columns
                                      # Note that col indices are 0-based
                                      columnDefs=list(list(className="dt-right", targets=length(indepVar):(ncol(aggdat)-1))),
                                      #               list(className="dt-center", targets=6)),
                                      #               list(width='10px', targets=1)),

                                      # Sort order of columns (ascending for all independent var cols)
                                      # Note that col indices are 0-based
                                      # Simple method for two fixed columns
                                      # order=list(list(0, "asc"), list(1, "asc")),
                                      # Method using number of independent vars
                                      order=lapply(1:length(indepVar), function(i) list(i-1, "asc")),

                                      # Download button
                                      buttons=list(list(extend="collection", buttons=c('csv', 'excel', 'pdf'), text='Download'))
                                    )

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

    }


    #######################################################################################################
    # Function to compose and render plot for subset of observations corresponding to specified table row
    # Information on ggplot options available at:
    # https://ggplot2.tidyverse.org/reference/
    # https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
    #######################################################################################################

    t2RenderPlot <- function(t1RowSelected, depVar, aggVar, agencyPos, occPos, indepVar, panelVar, panelCols,
                             boxIntensity, pointStyle, pointDiffVar, pointSize, pointJitterWidth, pointAlpha) {

      # Clear plot
      # This has no effect because shiny recognizes multiple renderPlot() calls and ignores all but the final one
      # output$t2Plot <- renderPlot(NULL)
      # shinyjs functions behave in a similar fashion (hide is ignored when followed by show in the same event)
      #shinyjs::hide("t2Plot")

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

          # Boxplot with error bars
          # Assign grayscale color from specified intensity (note the inverse 1-0 relationship of grayscale to visual intensity)
          g <- g + geom_boxplot(data=gdat, aes_string(x=indepVar, y=depVar), color=paste("gray", (1-boxIntensity)*100, sep=""), fill=NA, outlier.shape=NA) +
          stat_boxplot(data=gdat, aes_string(x=indepVar, y=depVar), geom='errorbar', color=paste("gray", (1-boxIntensity)*100, sep=""), width=0.4)

          # Facet, if requested
          if(panelVar!="none") {
            g <- g + facet_wrap(panelVar, #nrow=panelRows, ncol=panelCols,
                                labeller=as_labeller(function(x) paste(panelVar, " = ", x, sep="")))
            # Include panel cols, if specified
            # This avoids issuance of warnings when nrow or ncol are NULL
            if(!is.na(panelCols))
              g[["facet"]][["params"]][["ncol"]] <- panelCols
          }

          # Include points, if requested
          if(pointStyle=="fixed") {
            # Assign size from point style
            g <- g + geom_jitter(data=gdat, aes_string(x=indepVar, y=depVar), size=pointSize, width=pointJitterWidth, alpha=pointAlpha)
          } else if(pointStyle=="n-sized") {
            # Accumulate observation counts within combinations of gdat variables when point size is a function of obs count
            g <- g + geom_jitter(data=setNames(aggregate(1:nrow(gdat), by=gdat, length), c(colnames(gdat), "n")),
                                 aes_string(x=indepVar, y=depVar, size="n"), width=pointJitterWidth, alpha=pointAlpha)
          }

          # Assign point color from point differentiation var
          if(pointStyle!="none" & pointDiffVar!="none") {
            # Create a mapping by appending an aes() for color
            # Note, also, the required spelling of "colour" ("color" causes contention between two color scales during render)
            g[["layers"]][[length(g[["layers"]])]][["mapping"]][["colour"]] <- aes_string(color=pointDiffVar)[[1]]
            g <- g + scale_color_manual(name=pointDiffVar,
                                          values=colorRampPalette(c("blue", "red"))(length(unique(gdat[,pointDiffVar]))))
          }

          # Display panel observation counts
          # An attempt was made to render these prior to the boxes, but this caused reordering of the
          # indepVar factor levels on the x-axis
          # Accumulate frequency by panel var levels
          if(panelVar=="none") {
            nobs <- data.frame("n"=nrow(gdat))
          } else {
            nobs <- aggregate(1:nrow(gdat), by=list(gdat[,panelVar]), length)
            colnames(nobs) <- c(panelVar, "n")
          }
          # Position text at first indepVar level and max of depVar
          g <- g + geom_text(data=nobs, aes(x=levels(gdat[,indepVar])[1], y=max(gdat[,depVar]),
                             label=paste("n = ", format(n, big.mark=","), sep="")), hjust=0, vjust=1)

          # Configure axis labels and apply theme
          g <- g +  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
            ggTheme +
            labs(x=paste("\n", indepVar, sep=""), y=paste(depVar, "\n", sep=""))

          # Render
          output$t2Plot <- renderPlot({ # Enable progress inicator
                                        progress <- shiny::Progress$new()
                                        on.exit(progress$close())
                                        progress$set(message="composing plot", value=1)
                                        # Generate plot
                                        print(g)})

        } else {

          showNotification("Independent and panel variables must be different from table aggregation (independent) variables.", type="error")
          output$t2Plot <- renderPlot(NULL)

        }

      } else {

        showNotification("Dependent, independent, and panel variables must be unique.", type="error")
        output$t2Plot <- renderPlot(NULL)

      }

      #shinyjs::toggle("t2Plot")

    }


    #######################################################################################################
    # Action for compose table button event
    #######################################################################################################

    observeEvent(
      input$t1ActionComposeTable, {

        # Require one dependent and at least one independent variable
        if(length(input$t1DepVar)>0 & length(input$t1IndepVar)>0) {

          # Pass input variables as parameters to avoid reactivity within plot function
          t1RenderTable(input$t1DepVar, input$t1IndepVar, input$t1AgencyPos, input$t1OccPos)

        } else {

          output$t1Table <- DT::renderDataTable(NULL)
          showNotification("No data to display.  Unknown dependent or independent variable(s).", type="error")

        }

      }
    )


    #######################################################################################################
    # Action for table cell selection
    # Render box plot for subset of observations corresponding to row of selected cell
    #######################################################################################################

    observeEvent(
      input$t1Table_cells_selected, {

        if(length(input$t1Table_cell_clicked)>0) {

          # Ignore row selection when cell contains an HTML anchor tag
          # Note that _cell_clicked returns a list with elements:
          # row = row in data frame corresponding to cell clicked (reordering rows maintains row IDs)
          # col = 0-based col of cell clicked
          # value = data frame cell contents
          # Note that if selection mode of multiple is specified in renderTable(), then multiple cells
          # will be reported by _cell_clicked, all at the top list level
          # Referencing list elements by name returns the first encountered, ignoring multiples
          if(substring(as.character(input$t1Table_cell_clicked[["value"]]), 1, 7)!="<a href") {

            # Save current selected row, for use in other events
            t1SelectedRow <<- input$t1Table_cell_clicked[["row"]]

            # Render plot
            # Pass input variables as parameters to avoid reactivity within plot function
            t2RenderPlot(t1SelectedRow, input$t1DepVar, input$t1IndepVar, input$t1AgencyPos, input$t1OccPos,
                         input$t2IndepVar, input$t2PanelVar, input$t2PanelCols, input$t2BoxIntensity,
                         input$t2PointStyle, input$t2PointDiffVar, input$t2PointSize, input$t2PointJitterWidth,
                         input$t2PointAlpha)

            # Set plot tab as current
            updateTabsetPanel(session, "cpdfTabs", selected="t2")

          }

        }

      }
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
                     input$t2IndepVar, input$t2PanelVar, input$t2PanelCols, input$t2BoxIntensity, 
                     input$t2PointStyle, input$t2PointDiffVar, input$t2PointSize, input$t2PointJitterWidth,
                     input$t2PointAlpha)

      }
    )
   
  }

)

# Duke University Co-lab Shiny Workshop, Session 1-2, Spring 2021

# Shiny App
# Compose tables of aggregated OPM CPDF U.S. federal employee career variables
# Visually explore cross-sectional features of subsets of employees contained in disjoint rows
# of aggregation table

# Version 5

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")
options(shiny.maxRequestSize=5e7) 

library(shiny)
library(DT)
library(ggplot2)

shinyServer(

  function(input, output, session) {

    # Clear any previous upload file name, in case app restarted from browser refresh operation
    shinyjs::reset("browseFile")

    # Hide download button in aggregation table panel
    # This is useful when refreshing browser to clear last value
    updateCheckboxInput(session, "displayTableDownloadButton", value=F)

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
    # Function:  Error handler
    #######################################################################################################

    msgWindow <- function(level="ERROR", title="", msg, size="m", buttonText="OK")
      showModal(modalDialog(HTML(paste("<b>", level, "</b><br><br>", paste(msg, collapse="<br><br>", sep=""), sep="")),
                            title=paste("CPDF Analysis ", title, sep=""),
                            size=size, easyClose=T, footer=modalButton(buttonText)))

    #######################################################################################################
    # Function to compose aggregation table
    #######################################################################################################

    t1ComposeTable <- function(depVar, indepVar, agencyPos, occPos) {

      tryCatch(
        {
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

              # Configure progress indicator
              progress <- shiny::Progress$new()
              progress$set(message="Aggregating data", value=0.10)

              # Aggregate mean and quartiles of dependent var for each independent var combination
              # Create a data frame from specified independent vars with aggregation stats appended in corresponding cols
              # Note the global declaration so that aggregated data area available outside of this function
              aggdat <<- data.frame(iagg[,indepVar],
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
              colnames(aggdat) <<- c(indepVar, "n", "mean", "q25", "q50", "q75")

              if(is.data.frame(aggdat)) {

                # Enable download button visibility
                updateCheckboxInput(session, "displayTableDownloadButton", value=T)

                # Render table of aggregated results - Info at https://datatables.net/reference/option/
                progress$set(message="Rendering table", value=0.50)
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
                progress$close()

              } else {

                output$t1Table <- DT::renderDataTable(NULL)
                msgWindow(title="Compose Table", msg="No data to display.  No observations satisfy query criteria.")
                updateCheckboxInput(session, "displayTableDownloadButton", value=F)

              }

            } else {

              output$t1Table <- DT::renderDataTable(NULL)
              msgWindow(title="Compose Table", msg="No data to display.  No observations satisfy query criteria.")
              updateCheckboxInput(session, "displayTableDownloadButton", value=F)

            }

          }
          # Test warning and error conditions
          #warning("Test warning", call.=F)
          #stop("Test error", call.=F)
        },
        warning=function(err) msgWindow("WARNING", "Compose table", err),
        error=function(err) msgWindow("ERROR", "Compose table", err)
      )

    }

    ###########################################################################################################
    # Function to compose box plot for subset of observations corresponding to specified aggregation table row
    ###########################################################################################################

    t2RenderPlot <- function(t1RowSelected, depVar, aggVar, agencyPos, occPos, indepVar, panelVar, panelRows,
                             panelCols, pointDisplay, pointDiffVar, pointSize, pointJitterWidth, pointAlpha) {

      tryCatch(
        {
          # Require distinct dependent, independent, and panel variables
          if(length(unique(c(depVar, indepVar, panelVar)))==3) {

            # Require independent and panel variables to be different from table aggregation variable(s)
            if(length(intersect(c(indepVar, panelVar), aggVar))==0) {

              # Configure progress indicator
              progress <- shiny::Progress$new()
              progress$set(message="Assembling graph data", value=0.1)

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
               progress$set(message="Constructing graph", value=0.20)
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
              progress$set(message="Rendering graph", value=0.75)
              output$t2Plot <- renderPlot(g)
              progress$close()

            } else {

              output$t2Plot <- renderPlot(NULL)
              msgWindow(title="Render Plot", msg="Plot independent and panel variables must be different from table aggregation independent variable(s).")

            }

          } else {

            output$t2Plot <- renderPlot(NULL)
            msgWindow(title="Render Plot", msg="Plot dependent, independent, and panel variables must be unique.")

          }
          # Test warning and error conditions
          #warning("Test warning", call.=F)
          #stop("Test error", call.=F)
        },
        warning=function(err) msgWindow("WARNING", "Compose plot", err),
        error=function(err) msgWindow("ERROR", "Compose plot", err)
      )

    }

    #######################################################################################################
    # Observe event function
    # Action for compose table reactive control event    #######################################################################################################

    # The following observer establishes a reactive environment for all variables being observed
    # A change (either on-screen or programatically, using functions such as updateTextInput()) in value
    # of any observed reactive variable causes the corresponding observeEvent() function to be executed

    observeEvent(c(input$t1DepVar, input$t1IndepVar, input$t1AgencyPos, input$t1OccPos),

      t1ComposeTable(input$t1DepVar, input$t1IndepVar, input$t1AgencyPos, input$t1OccPos),
      ignoreInit=T

    )

    #######################################################################################################
    # Observe event function
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
    # Configure a download handler for the aggregation table
    #######################################################################################################

    output$downloadAggregationTable <-
      downloadHandler(filename="CPDFAggregatedResults.csv",
                      contentType="text/csv",
                      # The file parameter is the temporary file to be written to
                      # This file is passed to the browser for download
                      content=function(file) {
                                tryCatch({
                                  write.table(aggdat, file, row.names=F, col.names=T, quote=T, sep=",")},
                                  warning=function(err) msgWindow("WARNING", "Download aggregated data table", err),
                                  error=function(err) msgWindow("ERROR", "Download aggregated data table", err)
                                )
                              }
                     )

    #######################################################################################################
    # Observe event function
    # Action plot button
    # Compose box plot for subset of observations corresponding to selected table row and
    #######################################################################################################

    observeEvent(input$t2ActionPlot, {

        # Render plot
        # Pass input variables as parameters to avoid reactivity within plot function
        t2RenderPlot(t1SelectedRow, input$t1DepVar, input$t1IndepVar, input$t1AgencyPos, input$t1OccPos,
                     input$t2IndepVar, input$t2PanelVar, input$t2PanelRows, input$t2PanelCols,
                     input$t2PointDisplay, input$t2PointDiffVar, input$t2PointSize, input$t2PointJitterWidth,
                     input$t2PointAlpha)

    }, ignoreInit=T)

    #######################################################################################################
    # Configure a download handler for the sample CPDF file
    #######################################################################################################

    output$sampleCPDFdownload <-
      downloadHandler(filename="SampleCPDF.csv.gz",
                      contentType=NULL,
                      content=function(file) {
                                  tryCatch(
                                    {
                                      # Read sample CPDF file (convert from gzip)
                                      x <- read.table(paste(datDir, "/CPDFSampleDataBuzzfeed-500.csv.gz", sep=""), header=T, sep=",")
                                      # Write sample data to gz file
                                      # Note that the file parameter contains the name of a temp file with .gz extension
                                      write.table(x, gzfile(file), row.names=F, col.names=T, quote=F, sep=",")
                                      close(gzfile(file))
                                    },
                                    warning=function(err) msgWindow("WARNING", "Download sample CPDF", err),
                                    error=function(err) msgWindow("ERROR", "Download sample CPDF", err)
                                  )
                              }
                     )

    #######################################################################################################
    # Observe event function:  Upload file
    # This event corresponds to the fileInputAccept button created in the input$browseFile event
    #######################################################################################################

    observeEvent(input$fileInputAccept, {

      tryCatch(
        {
          # Close dialog
          removeModal()

          # Read entire file
          progress <- shiny::Progress$new()
          progress$set(message="Reading Data", value=0.33)
          cpdf <<- read.table(gzfile(input$browseFile[,"datapath"]), header=T, sep=",", strip.white=T)
          close(gzfile(input$browseFile[,"datapath"]))
          progress$set(message="Reading Data", value=0.80)

          # Omit invalid observations
          if("Age" %in% names(cpdf))
            cpdf <<- subset(cpdf, !is.na(cpdf[,"Age"]))

          # Convert occupation to four position alpha (for some reason, read.table from gz file converts this column to numeric)
          if("Occupation" %in% names(cpdf))
            cpdf[,"Occupation"] <<- sprintf("%04.0f", cpdf[,"Occupation"])

          progress$close()

        },
        warning=function(err) msgWindow("WARNING", "Read file", err),
        error=function(err) msgWindow("ERROR", "Read file", err)
      )

    }, ignoreInit=T)

    #######################################################################################################
    # Observe event function:  Browse file selected
    #######################################################################################################

    observeEvent(input$browseFile, {

      tryCatch(
        {
          # Read first lines of file
          # Note the increased max file size by adjusting the shiny.maxRequestSize option at the beginning of this script  
          x <- scan(gzfile(input$browseFile[,"datapath"]), "character", n=5, sep="\n", quote="", quiet=T)
          close(gzfile(input$browseFile[,"datapath"]))

          # Display lines for review
          showModal(
            modalDialog( 
              HTML(paste("UPLOADED FILE STRUCTURE IS:<br><br>",
                         paste(x, collapse="<br>", sep=""),
                         sep="")),
              title="CPDF File Upload", size="l", easyClose=T,
              footer=tagList(
                       # Create a reactive action button to allow user to acceept file
                       # The application will execute the observe event for input$fileInputAccept when the button is pressed
                       div(actionButton("fileInputAccept", "Accept"), style="display:inline-block; vertical-align:top; margin-top:-5px; margin-left:10px"),
                       div(modalButton("OK"), style="display:inline-block; vertical-align:top; margin-top:-5px; margin-left:10px")
                     )
            )
          )

        },
        warning=function(err) msgWindow("WARNING", "Browse file selected", err),
        error=function(err) msgWindow("ERROR", "Browse file selected", err)
      )

    }, ignoreInit=T)

    #######################################################################################################
    # Observe event function:  Display Quick Start panel
    #######################################################################################################

    observeEvent(input$quickStartLink, {

      # Generate UI elements in a new draggable panel
      insertUI(
        # Place after supplementalText tag
        selector = "#supplementalText",
        where = "afterEnd",
        immediate=T,
        # Generate a panel with imported HTML elements
        ui=fixedPanel(id="quickStartText", draggable=T, top="5%", left="5%", width="90%",
             div("Quick Start", style="height:30px; background-color:#0066bb; border-radius:5px 5px 0px 0px; color:white; text-align:center; padding:5px"),
             div(
               div(
                 includeHTML("SupplementalMaterial/QuickStart.html"),
                 #HTML("<br><br>"),
                 actionButton("hideQuickStart", "hide"),
                 style="padding:30px"
               ),
               style="background-color:#eeeeee; opacity:1.0; border-width:3px; border-style:solid; border-color:#0066bb; border-radius:0px 0px 5px 5px"
             )
           )
      )

    }, ignoreInit=T)

    #######################################################################################################
    # Observe event function:  Remove Quick Start panel
    #######################################################################################################

    observeEvent(input$hideQuickStart, {

      removeUI(
        # ID references the div(id=) created during insertion
        selector = "#quickStartText",
        immediate=T
      )

    }, ignoreInit=T)

    #######################################################################################################
    # Observe event function:  Display About panel
    #######################################################################################################

    observeEvent(input$aboutLink, {

      # Generate UI elements in a new draggable panel
      insertUI(
        # Place after supplementalText tag
        selector = "#supplementalText",
        where = "afterEnd",
        immediate=T,
        # Generate a panel with imported HTML elements
        ui=fixedPanel(id="aboutText", draggable=T, top="5%", left="5%", width="90%",
             div("About", style="height:30px; background-color:#0066bb; border-radius:5px 5px 0px 0px; color:white; text-align:center; padding:5px"),
             div(
               div(
                 includeHTML("SupplementalMaterial/About.html"),
                 HTML("<br><br>"),
                 actionButton("hideAbout", "hide"),
                 style="padding:30px"
               ),
               style="background-color:#eeeeee; opacity:1.0; border-width:3px; border-style:solid; border-color:#0066bb; border-radius:0px 0px 5px 5px"
             )
           )
      )

    }, ignoreInit=T)

    #######################################################################################################
    # Observe event function:  Remove About panel
    #######################################################################################################

    observeEvent(input$hideAbout, {

      removeUI(
        # ID references the div(id=) created during insertion
        selector = "#aboutText",
        immediate=T
      )

    }, ignoreInit=T)

  }

)
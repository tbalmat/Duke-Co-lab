    #######################################################################################################
    # Function to compose summary table for specified query parameters
    # Information on data tables available at:
    # https://rstudio.github.io/DT/
    # https://cran.r-project.org/web/packages/DT/DT.pdf
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
                progress$set(message="Rendering table", value=0.50)
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
                              #editable="cell",

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
                                            pageLength=20, lengthMenu=seq(5, 50, 5),

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

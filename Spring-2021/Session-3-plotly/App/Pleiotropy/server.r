#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 3, plotly, Spring 2021
# Genome Wide Association Study Pleiotropy App
# Shiny server function
#####################################################################################################

server <- function(input, output, session) {

  ###################################################################
  # Observe event for tab 1 min p filter and plot 1 point selection
  ###################################################################
 
  observe({

    # Subset observations to those satisfying p constraint
    gwasp <- subset(gwasdat, log_10_p>input$t1MinLogp)
    rownames(gwasp) <- NULL

    # Identify observations by GWAS set
    kg1 <- which(gwasp[,"GWAS"]==1)
    kg2 <- which(gwasp[,"GWAS"]==2)

    # Compose list of unique phenotypes in set 2
    ph2 <- unique(gwasp[kg2,"phenotype"])

    # Compute y-axis limits
    ylim <- c(input$t1MinLogp-0.25, max(gwasp[kg1,"log_10_p"]))

    # Render left hand plot using GWAS set 1 observations
    # Note the use of ggplotly() to convert a ggplot() object
    # The value of the ggplotly() source parameter will be used by t1Plot2 when referencing points
    # clicked on t1Plot1
    t1p1 <- t1ComposePlot1(gwasp[kg1,], ylim)
    output$t1Plot1 <- renderPlotly(ggplotly(t1p1, source="t1Plot1"))

    # Render right hand plot when a point is selected in left hand plot
    output$t1Plot2 <- renderPlotly({

      # Construct event triggered by click of point in left hand plot
      # Save event data (aesthetic data, including SNP, from geom_point() call used in constructing
      # left hand plot)
      #evdat <- event_data("plotly_hover", source="t1Plot1")
      evdat <- event_data("plotly_click", source="t1Plot1")

      # Construct right hand plot using left hand plot event data (for point that was clicked)
      if(!is.null(evdat)) {

        print(evdat)

        # Isolate GWAS 2 points with SNP equal that of point selected in plot 1
        # Note that event_data point IDs are 0-based
        SNP <- t1p1[["data"]][["SNP"]][evdat[1,"pointNumber"]+1]
        ksel <- which(gwasp[kg2,"SNP"]==SNP)
        #print(SNP)
        #print(ksel)
        #print(gwasp[kg2[ksel],])

        # Compute y-axis limits
        ylim <- c(input$t1MinLogp-0.25, max(gwasp[kg2,"log_10_p"]))

        # Compose plot
        t1p2 <- t1ComposePlot2(gwasp[kg2,], ksel, ylim)

        # Convert to a plotly object
        t1p2 <- ggplotly(t1p2, source="t1Plot2")

        # Add selected plot 1 point labels
        for(i in ksel) {
          # Identify x-axis position of current phenotype
          xpos <- which(ph2==gwasp[kg2[i],"phenotype"])
          t1p2 <- add_annotations(t1p2, text=paste(gwasp[kg2[i],"phenotype"], "; ", gwasp[kg2[i],"SNP"], sep=""),
                                  x=xpos, y=gwasp[kg2[i],"log_10_p"], yshift=20, xref="x", yref="y", font=list("size"=12),
                                  color="black", bgcolor="white", bordercolor="black", opacity=0.65, showarrow=F)
        }

        # Identify selected plot 1 point 
        xyhl <- data.frame("phenotype"=t1p1[["data"]][["phenotype"]][evdat[1,"pointNumber"]+1],
                           "SNP"=t1p1[["data"]][["SNP"]][evdat[1,"pointNumber"]+1],
                           "log_10_p"=t1p1[["data"]][["log_10_p"]][evdat[1,"pointNumber"]+1])
        #print(xyhl)
        # Render plot 1 with selected point highlighted
        output$t1Plot1 <- renderPlotly(ggplotly(t1ComposePlot1(gwasp[kg1,], ylim, xyhl), source="t1Plot1"))

        # Return plot 2 object to be rendered
        return(t1p2)

      } else {

        return(NULL)

      }

    })

  })


  ############################################################
  # Observe event for tab 2 min p filter and point selection 
  ############################################################
 
  observe({

    #print("AAAAAAAA")

    # Subset observations to those satisfying p constraint
    gwasp <- subset(gwasdat, log_10_p>input$t2MinLogp)
    rownames(gwasp) <- NULL

    # Proceed only if observations exist for specified p
    if(nrow(gwasp)>0) {

      # Identify observations from each GWAS set
      kg1 <- which(gwasp[,"GWAS"]==1)
      kg2 <- which(gwasp[,"GWAS"]==2)

      # Compose an edge for each pair of phenotypes in disjoint GWAS sets with a shared SNP
      # Note that edges consist of pairs of observation indices, one for a phenotype from each set
      edgeDat <- merge(gwasp[kg1,c("SNP", "phenotype", "log_10_p")], gwasp[kg2,c("SNP", "phenotype", "log_10_p")], by="SNP")
      colnames(edgeDat) <- c("SNP", "phenotype1", "log_10_p1", "phenotype2", "log_10_p2")

      # Proceed only if edges exist
      if(nrow(edgeDat)>0) {

        # Order vertices by frequency of edge appearance
        # Refactoring excludes levels that have no observations (filtered out by p)
        tb <- sort(table(factor(edgeDat[,"phenotype1"], levels=unique(edgeDat[,"phenotype1"]))))
        vertex1 <- names(tb)
        tb <- sort(table(factor(edgeDat[,"phenotype2"], levels=unique(edgeDat[,"phenotype2"]))))
        vertex2 <- names(tb)

        # Compose vectors of edgeDat indices for each set
        # These are used in composing hover labels for vertices (vlab) and edges (vlab2, in edge selection)
        # One list element per set, each with one index vector per phenotype in 
        kedgeDat <- lapply(list(1, 2),
                      function(i)
                       lapply(get(paste("vertex", i, sep="")),
                         function(ph) {
                           list("phenotype"=ph, "k"=which(edgeDat[,paste("phenotype", i, sep="")]==ph))
                         }))

        # Compose hover labels for each vertex set
        # Two sets of labels are created for each set:
        #   one for vertex labels (containing all rsIDs associated with a phenotype)
        #   one for edge labels (containing all rsIDs that are shared with alternate set phenotypes)
        # Delimit labels with HTML breaks for hover label appearance
        # Enumerate edges per vertex
        vlab <- lapply(list(1, 2),
                  function(i)
                    do.call(rbind,
                      apply(as.matrix(1:length(kedgeDat[[i]])), 1,
                        function(j) {
                          ph <- kedgeDat[[i]][[j]][["phenotype"]]
                          kg <- which(gwasp[,"phenotype"]==ph)
                          ke <- kedgeDat[[i]][[j]][["k"]]
                          data.frame("phenotype"=ph,
                                     "nvertex"=length(kg),
                                     "rsIDvertex"=paste(sort(unique(gwasp[kg,"SNP"])), collapse="<br>", sep=""),
                                     "nedge"=length(ke),
                                     "rsIDedge"=ifelse(length(ke)>0, paste(sort(unique(edgeDat[ke,"SNP"])), collapse="<br>", sep=""), " "))
                        })))

        # Specify plot x abcissae for vertices and edges
        x1 <- 1.2
        x2 <- 1.98

        # Adjust plot y ordinates of vertices and edges so that smaller set is centered within those of larger set
        # y1 for GWAS set 1, y2 for GWAS set 2
        nv1 <- length(vertex1)
        nv2 <- length(vertex2)
        if(nv1<nv2) {
          y1 <- (1:nv1)+as.integer((nv2-nv1)/2)
          y2 <- 1:nv2
        } else {
          y1 <- 1:nv1
          y2 <- (1:nv2)+as.integer((nv1-nv2)/2)
        }

        # Construct edge y ordinates
        # Convert each vertex value to corresponding graph ordinate (current placement in vertex vectors)
        # Enumerate edges per vertex pair (used to adjust opacity instead of plotting multiple, identical edges)
        edge <- cbind(match(edgeDat[,"phenotype1"], vertex1), match(edgeDat[,"phenotype2"], vertex2))
        edge <- aggregate(rep(1, nrow(edge)), by=list(edge[,1], edge[,2]), sum)
        colnames(edge) <- c("y1", "y2", "nedge")
        edge[,"y1"] <- y1[edge[,"y1"]]
        edge[,"y2"] <- y2[edge[,"y2"]]

        # Specify appearance attributes
        rgblab <- rgb(0.3, 0.3, 0.8)
        rgbvertex <- rgblab
        rgbedge <- rgblab
        rgbemph <- "#E06000" #rgb(0.8, 0.5, 0.5)
        rgbvertexsel <- rgb(0, 0.7, 0)
        
        # Segregate edges by frequency
        # Individual sets of lines will be rendered, one for each set
        # This is to compensate for inability of plot_ly() to render individual attributes, such as opacity
        kegroup <- aggregate(1:nrow(edge), by=list(round(edge[,"nedge"]/max(edge[,"nedge"]), 1)), function(k) k)
        opacityedge <- 0.25+kegroup[[1]]/max(kegroup[[1]])*0.6
        widthedge <- rep(1.5, length(kegroup[[1]]))
        #print(kegroup)
        #print(opacityedge)
        #print(widthedge)

        # Flag reactive trigger for filter controls
        # This forces evaluation of the click event in renderPlotly(), below
        reacTrigger <- T

        output$t2Plot <- renderPlotly({
          #print("BBBBBBB")
          # Get click event data
          evdat <- event_data("plotly_click", source="g1")
          # Highlight graph elements when not here due to a reactive event (as opposed
          # to a plot click)
          if(!reacTrigger & !is.null(evdat)) {
            print(evdat)
            if("customdata" %in% colnames(evdat)) {
              evcurve <- evdat[1,"customdata"]
            } else {
              evcurve <- ""
            }
            # Specify vertices to emphasize (all connected edges and vertices in alternate set are also emphasized)
            if(evcurve=="vertex1") {
              # Subtract offset of first element, since the shorter set was centered
              emphset1 <- evdat[,"y"]-min(y1)+1
              emphset2 <- edge[which(edge[,"y1"]==evdat[,"y"]),2]-min(y2)+1
              selset <- 1
            } else if(evcurve=="vertex2") {
              emphset2 <- evdat[,"y"]-min(y2)+1
              emphset1 <- edge[which(edge[,"y2"]==evdat[,"y"]),1]-min(y1)+1
              selset <- 2
            } else if(!evcurve %in% c("edge1", "edge2")) {
              emphset1 <- vector("integer")
              emphset2 <- vector("integer")
              selcurve <- 0
            }
          } else {
            emphset1 <- vector("integer")
            emphset2 <- vector("integer")
            selset <- 0
            # Reset reactive trigger flag, so that click events are recognized
            # Note the reference of parent environment (where reacTrigger is defined)
            reacTrigger <<- F
          }
          t2ComposePlot(x1, y1, x2, y2, vertex1, vertex2, emphset1, emphset2, selset,
                        rgbemph, vlab, rgblab, rgbvertex, rgbvertexsel, edge, kegroup,
                        rgbedge, widthedge, opacityedge, edgeDat, kedgeDat)
        })

      } else {

          output$t2Plot <- renderPlotly(NULL)
          showNotification("No data to display.  No edges exist.", type="warning")

      }

    } else {

      output$t2Plot <- renderPlotly(NULL)
      showNotification("No data to display.  No observations satisfy p criteria.", type="warning")

    }

  })

}

#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 4, November 2019
# Genome Wide Association Study Pleiotropy App
# Shiny user interface script
#####################################################################################################

# Information on shiny available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf

options(stringsAsFactors=F)
library(shiny)
library(plotly)
library(shinythemes)

#####################################################################################################
# GWAS data description
#
# Source:  synthesized
#
# Columns:
# GWAS ................... GWAS set
# phenotype .............. textual description of phenotype
# SNP .................... rsID of SNP
# p ...................... significance of allele transition regression coefficient
#####################################################################################################

# Dir location

# Local
setwd("C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-4-plotly")

# RStudio Cloud
# setwd("/cloud/project/Duke-Co-lab/Shiny/Session-4-plotly")

#####################################################################################################
# Function:  read observations for GWAS sets to be compared
#####################################################################################################

readData <- function() {
  gwasdat <- read.table("Data/GWASResults.csv", header=T, sep=",", strip.white=T)
  # Omit observations with non-positive significance values
  gwasdat <- subset(gwasdat, p>0)
  # Convert p values to log_10 
  gwasdat[,"log_10_p"] <- -log(gwasdat[,"p"])/log(10)
  # Convert phenotype into a factor for x-axis label positioning in tab 1 plots
  gwasdat[,"phenotype"] <- factor(gwasdat[,"phenotype"])
  return(gwasdat)
}

#####################################################################################################
# Function:  Compose cross phenotype plot 1 on tab 1
# Phenotype, SNP GWAS Coupled Plots Using Hover Feature of Plotly
# Plot 1, on left, displays a box plot of GWAS points (p by phenotype) for one set of phenotypes
# Plot 2, on right, displays a box plot of GWAS points for a set of phenotypes with a null intersection with those in plot 1
# When a point on plot 1 is clicked, all points in plot 2, corresponding to the selected SNP, are highlighted
# Note that GWAS points are limited to those with a p value below a specified threshold (p0)
#####################################################################################################

t1ComposePlot1 <- function(data, ylim, xyHighlight=NULL) {

  # Initialize plot
  g <- ggplot(data=data)

  # Include vertical x-y points at each phenotype location, one point for each SNP, y=p
  # Jitter and reduce opacity to avoid overlap
  # All points in blue
  g <- g + geom_jitter(aes(x=phenotype, SNP=SNP, y=log_10_p), width=0.1, color="blue3", alpha=0.5)

  # Highlight point in red
  if(!is.null(xyHighlight))
    g <- g + geom_point(data=xyHighlight, aes(x=phenotype, SNP=SNP, y=log_10_p), size=3, color="red", alpha=1)

  # Finish hit
  g <- g +
  scale_y_continuous(limits=ylim, label=function(y) round(y, 2)) +
  theme(panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=10, angle=45, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=10)) +
  labs(title="", x="", y="-log_10(p)\n")

  return(g)

}

#####################################################################################################
# Function:  Compose cross phenotype plot 2 on tab 1
# Phenotype, SNP GWAS Coupled Plots Using Hover Feature of Plotly
# Plot 1, on left, displays a box plot of GWAS points (p by phenotype) for one set of phenotypes
# Plot 2, on right, displays a box plot of GWAS points for a set of phenotypes with a null intersection with those in plot 1
# When a point on plot 1 is clicked, all points in plot 2, corresponding to the selected SNP, are highlighted
# Note that GWAS points are limited to those with a p value below a specified threshold (p0)
#####################################################################################################

t1ComposePlot2 <- function(data, ksel, ylim) {

  # Initialize plot
  g <- ggplot()

  # Display unselected points in blue
  g <- g + geom_jitter(data=data[setdiff((1:nrow(data)), ksel),],
                       aes(x=phenotype, SNP=SNP, y=log_10_p), width=0.1, color="blue3", alpha=0.5)

  # Display selected points (corresponding to SNP from point selected on graph 1) in red
  if(length(ksel)>0)
    g <- g + geom_point(data=data[ksel,], aes(x=phenotype, SNP=SNP, y=log_10_p), size=3, color="red", alpha=1)

  # Annotations are ignored by ggplotly()
  # They will be added by calling function
  #annotate(gsnp[kp2[k[1]],], x=Phenotype, y=log_10_p+0.1, label=paste(gsnp[kp2[k],"Phenotype"], "; ", gsnp[kp2[k],"rsID"], sep="")),
  #         size=4, hjust=0, color="green", fill="white", fontface="bold", alpha=1) +

  # Finish it off
  g <- g +
  scale_y_continuous(limits=ylim, label=function(y) round(y, 2)) +
  theme(panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        axis.text.x=element_text(size=10, angle=45, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=10)) +
  labs(title="", x="", y="-log_10(p)\n")

}

#####################################################################################################
# Function:  Compose bipartite graph for tab 2
# Two GWAS sets are compared, with vertices on left and right (for set 1 and 2, respectively) and
# Vertices joining vertices for phenotypes that share common associated SNPs (note that SNPs are
# filtered by level of phenotype/SNP association within each GWAS set)
# Hovering over vertices displays all within-GWAS SNPs associated with the corresponding phenotype
# Clicking a phenotype point highlights edges to phenotype vertices in the alternate GWAS set that
# share SNPs
# An additional point is displayed when selecting a phenotype and hovering over this presents the
# SNPs shared by the edge that the additional point appears on
#####################################################################################################

t2ComposePlot <- function(x1, y1, x2, y2, vertex1, vertex2, emphset1, emphset2, selset,
                          rgbemph, vlab, rgblab, rgbvertex, rgbvertexsel, edge, kegroup,
                          rgbedge, widthedge, opacityedge, edgeDat, kedgeDat) {

  g <- # Begin with vertex1 (phenotype from GWAS set 1) labels (on left)
       plot_ly(
         x=x1-0.1, y=y1,
         # Mode of text displays values in text parameter
         type="scatter", mode="text", text=vertex1,
         # Align text to left of (x,y), making it appear right aligned
         textposition="left",
         textfont=list(color=replace(rep("black", length(y1)), emphset1, rgbemph), size=10),
         # Specify hover labels ("text specifies values in text parameter, "none" disables hover labels)
         # Note that hovertemplate can be used, instead of using hoverinfo and hoverlabel, to compose
         # strings, using paste(), formatted with embedded %{x}, %{y}, %{text}, and HTML tags
         hoverinfo="text",
         hovertext=vlab[[1]][["rsIDvertex"]],
         # Format hover labels
         hoverlabel=list("bgcolor"=replace(rep(rgblab, length(y1)), emphset1, rgbemph), bordercolor="white",
                         font=list(size=10, color="white")),
         # Omit the addition of trace number in hover labels
         name="",
         # Identify plot for specification in server() function, making event_data reactive
         source="g1") %>%
       # Vertex2 (phenotype set 2) labels (on right)
       add_trace(inherit=F, x=x2+0.1, y=y2, type="scatter", mode="text", text=vertex2,
         textposition="right", textfont=list(color=replace(rep("black", length(y2)), emphset2, rgbemph), size=10),
         hoverinfo="text", hovertext=vlab[[2]][["rsIDvertex"]], name="",
         hoverlabel=list("bgcolor"=replace(rep(rgblab, length(y2)), emphset2, rgbemph), bordercolor="white",
                         font=list(size=10, color="white"))) %>%
       # Vertex1 count (SNPs for phenotype set 1)
       add_trace(inherit=F, x=x1-0.025, y=y1, type="scatter", mode="text", text=vlab[[1]][["nedge"]],
         textposition="left", textfont=list(color=replace(rep(rgblab, length(y1)), emphset1, rgbemph), size=10),
         hoverinfo="none") %>%
       # Vertex2 count (SNPs for phenotype set 2)
       add_trace(inherit=F, x=x2+0.025, y=y2, type="scatter", mode="text", text=vlab[[2]][["nedge"]],
         textposition="right", textfont=list(color=replace(rep(rgblab, length(y2)), emphset2, rgbemph), size=10),
         hoverinfo="none") %>%
       # Set 1 column header
       add_text(inherit=F, x=x1-0.1, y=max(y1)+2, text="<b>GWAS set 1</b>", textposition="left",
         textfont=list(size=12), hoverinfo="none") %>%
       # Set 2 column header
       add_text(inherit=F, x=x2+0.1, y=max(y2)+2, text="<b>GWAS set 2</b>", textposition="right",
         textfont=list(size=12), hoverinfo="none") %>%
       # Notes
       add_text(inherit=F, rep(0.12, 4), y=-(1:4), textfont=list(size=10), textposition="right", hoverinfo="none",
         text=c("Hover over phenotypes for a list of SNPs in associated GWAS set",
                "Hover over edge endpoints for a list of SNPs in both GWAS sets",
                "Click edge endpoints to highlight inter-GWAS phenotype connection edges",
                "Hover over highlighted edged dots for a list of SNPs shared by connected Phenotypes")) %>%
         add_trace(inherit=F, x=rep(0.1, 3), y=-(2:4), type="scatter", mode="markers",
                   marker=list("color"=c(rgbvertex, rgbvertex, rgbvertexsel), "size"=10), hoverinfo="none") %>%
       # Hide axes, grid, legend
       plotly::layout(title=NULL,
         xaxis=list(range=c(0, 3), showgrid=F, zeroline=F, showline=F, ticks="", showticklabels=F),
         yaxis=list(autorange=T, showgrid=F, zeroline=F, showline=F, ticks="", showticklabels=F),
         showlegend=F,
         margin=list("l"=0, "r"=0, "b"=0, "t"=0, pad=0))
  
  # Append edges (connecting phenotypes from both sets, by common SNP)
  # One set, with variable width and opacity, for each nedge frequency group
  # Note that this is done in edge groups since plot_ly does not support multiple widths or opacities in
  # add_segment() parameters
  for(i in 1:length(kegroup[[2]])) {
    # Edges for unselected vertices
    k <- which(!(edge[kegroup[[2]][[i]],"y1"]-min(y1)+1) %in% emphset1)
    if(length(k)>0) {
      y1b <- edge[kegroup[[2]][[i]],"y1"][k]
      y2b <- edge[kegroup[[2]][[i]],"y2"][k]
      g <- add_segments(g, inherit=F, x=x1, xend=x2, y=y1b, yend=y2b,
             line=list("color"=rgbedge, "width"=widthedge[i]), opacity=opacityedge[i], hoverinfo="none")
    }
    # Edges for selected vertices
    k <- which((edge[kegroup[[2]][[i]],"y1"]-min(y1)+1) %in% emphset1 &
               (edge[kegroup[[2]][[i]],"y2"]-min(y2)+1) %in% emphset2)
    if(length(k)>0) {
      y1b <- edge[kegroup[[2]][[i]],"y1"][k]
      y2b <- edge[kegroup[[2]][[i]],"y2"][k]
      g <- add_segments(g, inherit=F, x=x1, xend=x2, y=y1b, yend=y2b,
             line=list("color"=rgbemph, "width"=widthedge[i]), opacity=1, hoverinfo="none")
    }
  }
  
  # Append selection points on all selected edges (after edges so that points are not overlayed)
  # These are the points that appear on the interior of edges
  # They represent SNPs common to the two phenotypes that the edge connects
  if(selset %in% 1:2) {
    for(i in 1:length(kegroup[[2]])) {
      k <- which((edge[kegroup[[2]][[i]],"y1"]-min(y1)+1) %in% emphset1 &
                 (edge[kegroup[[2]][[i]],"y2"]-min(y2)+1) %in% emphset2)
      if(length(k)>0) {
        y1b <- edge[kegroup[[2]][[i]],"y1"][k]
        y2b <- edge[kegroup[[2]][[i]],"y2"][k]
        # Compute coordinates on edges in which to place selection points
        slope <- (y2b-y1b)/(x2-x1)
        if(selset==1) {
          xc <- x1+0.1
          yc <- y1b+0.1*slope
        } else {
          xc <- x2-0.1
          yc <- y2b-0.1*slope
        }
        # Compose edge selection point labels from joint rsID set, one for each edge
        elab <- apply(as.matrix(1:length(yc)), 1,
                  function(i)
                    paste(intersect(edgeDat[kedgeDat[[1]][[y1b[i]-min(y1)+1]][["k"]],"SNP"],
                                    edgeDat[kedgeDat[[2]][[y2b[i]-min(y2)+1]][["k"]],"SNP"]), collapse="<br>", sep=""))
        # Append points
        g <- add_markers(g, inherit=F, x=xc, y=yc,
               marker=list("color"=rgbvertexsel, "size"=10),
               hoverinfo="text", hovertext=elab, name="",
               hoverlabel=list("bgcolor"=rgbvertexsel, bordercolor="white", font=list(size=10, color="white")),
               customdata=paste("edge", selset, sep=""))
      }
    }
  }
  
  # Append vertices last, so that they hide vertex endpoints (omit clash of different vertex and edge colors)
  # Vertex1, points adjoining edges at left (phenotypes for GWAS 1)
  # Prohibit display of text along with marker (inherit=F)
  g <- add_markers(g, inherit=F, x=x1, y=y1,
         marker=list("color"=replace(rep(rgbvertex, length(y1)), emphset1, rgbemph), "size"=10),
         hoverinfo="text", hovertext=vlab[[1]][["rsIDedge"]], name="",
         hoverlabel=list("bgcolor"=replace(rep(rgblab, length(y1)), emphset1, rgbemph), bordercolor="white",
                         font=list(size=10, color="white")), customdata="vertex1") %>%
       # Points adjoining edges at right (phenotypes for GWAS 2)
       add_markers(inherit=F, x=x2, y=y2,
         marker=list("color"=replace(rep(rgbvertex, length(y2)), emphset2, rgbemph), "size"=10),
         hoverinfo="text", hovertext=vlab[[2]][["rsIDedge"]], name="",
         hoverlabel=list("bgcolor"=replace(rep(rgblab, length(y2)), emphset2, rgbemph), bordercolor="white",
                         font=list(size=10, color="white")), customdata="vertex2")

  return(g)

}

#####################################################################################################
# Shiny user interface function
#####################################################################################################

ui <- function(req) {

      navbarPage(

        theme=shinytheme("flatly"),
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

}

#####################################################################################################
# Shiny server function
#####################################################################################################

server <- function(input, output, session) {

  ########################################
  # Update trigger for tab 1 min p filter
  ########################################
 
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

    # Render left hand plot one using GWAS set 1 observations
    # Note the use of ggplotly() to convert a ggplot() object
    # The value of the ggplotly() source parameter will be used by t1Plot2 when referencing points
    # clicked on t1Plot1
    t1p1 <- t1ComposePlot1(gwasp[kg1,], ylim)
    output$t1Plot1 <- renderPlotly(ggplotly(t1p1, source="t1Plot1"))

    # Render right hand plot, identifying GWAS set 2 phenotypes with SNP in common with that of point
    # selected in plot 1
    output$t1Plot2 <- renderPlotly({

      # Reference plot and event from which to respond
      #evdat <- event_data("plotly_hover", source="t1Plot1")
      evdat <- event_data("plotly_click", source="t1Plot1")

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

        # Any idea what we are attempting here?
        xyhl <- data.frame("phenotype"=t1p1[["data"]][["phenotype"]][evdat[1,"pointNumber"]+1],
                           "SNP"=t1p1[["data"]][["SNP"]][evdat[1,"pointNumber"]+1],
                           "log_10_p"=t1p1[["data"]][["log_10_p"]][evdat[1,"pointNumber"]+1])
        #print(xyhl)
        output$t1Plot1 <- renderPlotly(ggplotly(t1ComposePlot1(gwasp[kg1,], ylim, xyhl), source="t1Plot1"))

        return(t1p2)

      } else {

        return(NULL)

      }

    })

  })


  ########################################
  # Update trigger for tab 2 min p filter
  ########################################
 
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
            #print(evdat)
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

#####################################################################################################
# Execution begins here
#####################################################################################################

# Read observations
# Note the placement in the global environment, since it is accessed throughout other functions
gwasdat <- readData()

# Launch app
runApp(list("ui"=ui, "server"=server), launch.browser=T)


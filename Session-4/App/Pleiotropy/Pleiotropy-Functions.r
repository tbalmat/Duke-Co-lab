#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 4, plotly, November 2020
# Genome Wide Association Study Pleiotropy App
# Data management and plot generation functions
#####################################################################################################

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
# Plot 1, on left, displays a jitter plot of GWAS points (p by phenotype) for one set of phenotypes
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

  # Finish it
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
# Plot 2, on right, displays a jitter plot of GWAS points for a set of phenotypes from GWAS set 2
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

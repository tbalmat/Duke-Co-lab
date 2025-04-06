#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 5, November 2019
# Genome Wide Association Study Pleiotropy App
# Inter-GWAS Association Network Using the visNetwork Package
#####################################################################################################

# Information on shiny and visnetwork available at:
# https://shiny.rstudio.com/
# https://github.com/rstudio/shiny
# https://cran.r-project.org/web/packages/shiny/shiny.pdf
# https://cran.r-project.org/web/packages/visnetwork/visnetwork.pdf

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

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")

library(shiny)
library(visNetwork)
library(shinythemes)

# Dir location
# Local
setwd("C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-5-visnetwork")
# RStudio Cloud
# setwd("/cloud/project/Duke-Co-lab/Shiny/Session-5-visnetwork")

##########################################################################################################
# Set static filter and appearance parameters
##########################################################################################################

# Vertex colors for GWAS 1 and 2
vc1 <- "#66AAFF"
vc2 <- "#FFEE66"

# Edge colors for GWAS 1 and 2 when SNPs form edges (phenotypes forms vertices)
ec1 <- "#080808"
ec2 <- "#C02020"

# Edge colors for GWAS 1 and 2 when phenotypes form edges (SNPs form vertices)
ec3 <- "#66AAFF"
ec4 <- "#FFEE66"

# Font stroke color
sc1 <- "#909090"

##########################################################################################################
# GWAS data retrieval function
# Retrieve individual data frames for each GWAS
# Save in global memory so that all function can access observations
# Exclude observations with non-positive p values
##########################################################################################################

readData <- function() {
  gwas <- read.table("Data/GWASResults.csv", header=T, sep=",", strip.white=T)
  # Subset GWAS set 1
  k <- which(gwas[,"GWAS"]==1 & gwas[,"p"]>0)
  gwas1 <<- data.frame(gwas[k,c("phenotype", "SNP")], "log_10_p"=-log(gwas[k,"p"])/log(10))
  k <- which(gwas[,"GWAS"]==2 & gwas[,"p"]>0)
  gwas2 <<- data.frame(gwas[k,c("phenotype", "SNP")], "log_10_p"=-log(gwas[k,"p"])/log(10))
}

##########################################################################################################
# Function to assemble graph components (vertices and edges)
##########################################################################################################

assembleNetComponents <- function() {

  # Compose data set, with joined GWAS set phenotype as vertices and SNPs as edges
  # or SNP as vertices and edges formed by phenotypes from either GWAS set 
  if(vertexType=="Phenotype") {

    # Phenotype as vertex, SNP as edge
    gwas <- merge(gwas1[which(gwas1[,"log_10_p"]>=log_10_p),], 
                  gwas2[which(gwas2[,"log_10_p"]>=log_10_p),], by="SNP", suffixes=1:2)
    colnames(gwas) <- c("lab", "v1", "log_10_p1", "v2", "log_10_p2")

    if(nrow(gwas)>0) {
      # Tabulate edges by vertex
      v1 <- {x <- table(gwas[,"v1"]); data.frame("v"=names(x), "lab"=names(x), "n"=as.vector(x))}
      v2 <- {x <- table(gwas[,"v2"]); data.frame("v"=names(x), "lab"=names(x), "n"=as.vector(x))}
      # Filter by edge count
      # Retain vertices with min edge count, retain all enjoined vertices
      gwas <- gwas[which(gwas[,"v1"] %in% v1[which(v1[,"n"]>=nedgemin),"v"] |
                         gwas[,"v2"] %in% v2[which(v2[,"n"]>=nedgemin),"v"]),]
      v1 <- v1[which(v1[,"v"] %in% unique(gwas[,"v1"])),]
      v2 <- v2[which(v2[,"v"] %in% unique(gwas[,"v2"])),]
      # Compose edge hover labels
      if(nrow(gwas)>0)
        gwas[,"hovtext"] <- paste(gwas[,"v1"], ", log<sub>10</sub>(p)=", round(gwas[,"log_10_p1"], 2),
                                  "; ", gwas[,"v2"], ", ", "log<sub>10</sub>(p)=", round(gwas[,"log_10_p2"], 2), sep="")
      # Assign vertex color by GWAS set, edge color static
      vcolor <- c(rep(vc1, nrow(v1)), rep(vc2, nrow(v2)))
      vtcolor <- vcolor
      ecolor <- ec1
      ehcolor <- ec2
      vertex0 <- data.frame("set"=c(rep(1, nrow(v1)), rep(2, nrow(v2))), rbind(v1, v2), "hovtext"=c(v1[,"v"], v2[,"v"]))
    } else {
      vertex0 <- data.frame()
    }

    vsizefactor <- 1

  } else {

    # SNP as vertex, phenotype as edge
    # Limit vertices to the intersection of SNPs in both GWAS sets
    k1 <- which(gwas1[,"SNP"] %in% gwas2[which(gwas2[,"log_10_p"]>=log_10_p),"SNP"] & gwas1[,"log_10_p"]>=log_10_p)
    k2 <- which(gwas2[,"SNP"] %in% gwas1[which(gwas1[,"log_10_p"]>=log_10_p),"SNP"] & gwas2[,"log_10_p"]>=log_10_p)

    if(length(k1)>0 & length(k2)>0) {

      # Compose one vertex from each SNP set
      # Note that vertices are represented even when not enjoined to other vertices
      # This enables analysis of inter-set SNP phenotype relationships even when no phenotypes
      # relate the SNP to another SNP (no edges lead to or from a SNP)
      # Construct sets of phenotype within SNP
      SNP <- split(rbind(data.frame("set"=1, gwas1[k1,c("phenotype", "log_10_p")]),
                         data.frame("set"=2, gwas2[k2,c("phenotype", "log_10_p")])),
                   c(gwas1[k1,"SNP"], gwas2[k2,"SNP"]))
      # Compose hover text from phenotypes within SNP
      # Include proportion edges by GWAS set within each vertex, for vertex color assignment
      vertex0 <- do.call(rbind,
                   apply(as.matrix(1:length(SNP)), 1,
                         function(i) {
                           hovtext <- paste("GWAS set, Phenotype, log<sub>10</sub>(p)<br>",
                                        paste(paste(SNP[[i]][,"set"], ", ", SNP[[i]][,"phenotype"], ", ",
                                                    round(SNP[[i]][,"log_10_p"], 2), sep=""), collapse="<br>", sep=""), sep="")
                           data.frame("v"=names(SNP)[i], "hovtext"=hovtext, "n"=length(SNP[[i]][,"set"]),
                                      "p1"=length(which(SNP[[i]][,"set"]==1))/length(SNP[[i]][,"set"]))
                         }))

      # Construct edges
      # Generate SNP sets by phenotype (each SNP pair with phenotype becomes an edge)
      ph <- split(rbind(data.frame("set"=1, gwas1[k1,c("SNP", "log_10_p")]),
                        data.frame("set"=2, gwas2[k2,c("SNP", "log_10_p")])),
                  c(gwas1[k1,"phenotype"], gwas2[k2,"phenotype"]))
      # Generate one edge per SNP pair, phenotype combination
      gwas <- do.call(rbind,
                apply(as.matrix(1:length(ph)), 1,
                      function(i)
                        if(length(ph[[i]][,"SNP"])>1) {
                          # Generate all combinations of SNP pairs
                          ij <- t(combn(1:nrow(ph[[i]]), 2))
                          # Order SNP pairs so that first is alphabetically less than second
                          # This is needed for unambiguous pairs in collapsing, later
                          k <- which(ph[[i]][ij[,1],"SNP"]>ph[[i]][ij[,2],"SNP"])
                          x <- ij[k,2]
                          ij[k,2] <- ij[k,1]
                          ij[k,1] <- x
                          data.frame("set"=ph[[i]][1,"set"], "v1"=ph[[i]][ij[,1],"SNP"], "v2"=ph[[i]][ij[,2],"SNP"],
                                     "lab"=names(ph)[i],
                                     "hovtext"=paste(ph[[i]][ij[,1],"SNP"], ", log<sub>10</sub>(p)=", round(ph[[i]][ij[,1],"log_10_p"], 2),
                                                     "; ", ph[[i]][ij[,2],"SNP"], ", log<sub>10</sub>(p)=", round(ph[[i]][ij[,2],"log_10_p"], 2), sep=""),
                                     "log_10_p1"=ph[[i]][ij[,1],"log_10_p"], "log_10_p2"=ph[[i]][ij[,2],"log_10_p"])
                        } else {
                          data.frame()
                        }))
      # Optional:  collapse SNP pairs (edges) and compose composite string of phenotypes for hover text
      #gwas <- do.call(rbind,
      #          apply(as.matrix(which(!duplicated(gwas[,"v1"], gwas[,"v2"]))), 1,
      #                function(i) {
      #                  k <- which(gwas[,"v1"]==gwas[i,"v1"] & gwas[,"v2"]==gwas[i,"v2"])
      #                  k <- k[order(gwas[k,"set"], gwas[k,"lab"])]
      #                  hovtext <- paste("GWAS set, Phenotype, SNP1, log<sub>10</sub>(p1), SNP2, log<sub>10</sub>(p2)<br>",
      #                               paste(paste(gwas[k,"set"], ", ", gwas[k,"lab"], ", ", gwas[k,"v1"], ", ",
      #                                           round(gwas[k,"log_10_p1"], 2), ", ", gwas[k,"v2"], ", ",
      #                                           round(gwas[k,"log_10_p2"], 2), sep=""), collapse="<br>", sep=""), sep="")
      #                  data.frame("v1"=gwas[i,"v1"], "v2"=gwas[i,"v2"], "lab"="o", "hovtext"=hovtext)
      #                }))

      # Omit vertices and edges when edge count below threshold
      # Threshold of 0 retains vertices without edges to enable analysis inter-set, single SNP relations
      if(nedgemin>0) {
        x <- table(c(gwas[,"v1"], gwas[,"v2"]))
        v <- names(x)[which(x>=nedgemin)]
        # Retain all vertices having either edge count at threshold or enjoined to a vertex at threshold
        # This renders the network associated with at-threshold vertices
        k <- which(gwas[,"v1"] %in% v | gwas[,"v2"] %in% v)
        vertex0 <- vertex0[which(vertex0[,"v"] %in% unique(c(gwas[k,"v1"], gwas[k,"v2"]))),]
        # Retain associated edges
        gwas <- gwas[k,]
      }

      # Compute vertex color based on set1, set2 proportion of edges within SNP
      # Edges colored by GWAS set
      # Note the association (at time of development) of blue with set 1 and yellow (orange/red) with set 2
      # Vertices with high proportion of set 1 phenotypes toward blue, high set 2 toward red
      # Green indicates uniform (balanced) distribution
      vcolor <- rgb(1-vertex0[,"p1"], 1-abs(vertex0[,"p1"]-0.5), vertex0[,"p1"])
      vtcolor="#66aaff"
      ecolor <- c(ec3, ec4)[gwas[,"set"]]
      ehcolor <- ec2
      vsizefactor <- 0.25

    } else {

      vertex0 <- data.frame()

    }

  }

  # Compose global vertex and edge sets
  if(nrow(vertex0)>0) {
    # Vertices
    vertex <<- data.frame("id"=1:(nrow(vertex0)),
                          "fixed"=F,
                          "label"=vertex0[,"v"],
                          "color"=vcolor,
                          "font"=list("color"=vtcolor, "size"=8, strokeWidth=1, "strokeColor"=sc1),
                          "value"=vsizefactor*vertex0[,"n"]/max(vertex0[,"n"], na.rm=T),
                          "title"=vertex0[,"hovtext"])
    # Include groups for legend configuration (only with phenotypes as vertices)
    if(vertexType=="Phenotype")
       vertex[,"group"] <<- c("GWAS 1","GWAS 2")[vertex0[,"set"]]
    rownames(vertex) <<- NULL
    # Compose vertex IDs (they are required for unambiguous identification in edge construction)
    vid <-setNames(vertex[,"id"], vertex[,"label"])
    # Compose edges
    if(nrow(gwas)>0) {
      edge <<- data.frame("from"=vid[gwas[,"v1"]],
                          "to"=vid[gwas[,"v2"]],
                          "label"=gwas[,"lab"], 
                          # Hover text
                          "title"=gwas[,"hovtext"],
                          "hoverWidth"=0,
                          "selectionWidth"=0,
                          "color"=list("color"=ecolor, "opacity"=eopacity, "highlight"=ehcolor),
                          "font"=list("color"="white", "size"=5, strokeWidth=1, "strokeColor"=sc1),
                          #"length"=20,
                          "physics"=T,
                          "smooth"=T)
    } else {
      edge <<- data.frame()
    }

  } else {
    vertex <<- data.frame()
    edge <<- data.frame()
  }

  print("net assembled")

}

##########################################################################################################
# Function to compose graph using visNetwork() functions
##########################################################################################################

composeNet <- function() {
  g <- visNetwork(vertex, edge) %>% 
         visGroups(groupname="GWAS 1", color=vc1, font=list("color"="white", "size"=12)) %>%
         visGroups(groupname="GWAS 2", color=vc2, font=list("color"="#202020", "size"=12)) %>%
         visLegend(useGroups=T, position="right") %>%
         visOptions(highlightNearest=list("enabled"=T, "hover"=T)) %>%
         visInteraction(hover=T, hoverConnectedEdges=T, navigationButtons=T) %>%
         visPhysics(timestep=0.25, minVelocity=10, maxVelocity=50, 
                    barnesHut=list("avoidOverlap"=0.5, "springLength"=200, "springConstant"=0.5, "damping"=0.5),
                    repulsion=list("nodeDistance"=100),
                    stabilization=list("enabled"=T, "iterations"=1000)) %>%
         # Enclose java functions in {} brackets, otherwise they hang with no message 
         #visEvents(type="once", startStabilizing="function() {
         #                                           alert('begin stabilization')
         #                                         }") %>%
         visEvents(type="once", stabilized="function() {
                                              //alert('stab')
                                              Shiny.onInputChange('stabilized', '0')
                                            }") %>%
         # Double click events fire two click events, so use shift-click for doubles
         visEvents(type="on", click="function(obj) {
                                       if(obj.event.srcEvent.shiftKey) {
                                         //alert('shift-click')
                                         Shiny.onInputChange('shiftClick', obj)
                                       } else {
                                         //alert('click')
                                         Shiny.onInputChange('click', obj)
                                       }
                                     }") 
         #visEvents(type="on", doubleClick="function(obj) Shiny.onInputChange('doubleClick', obj)")
  print("net composed")
  return(g)
}

##########################################################################################################
# Shiny user interface function
##########################################################################################################

ui <- function(req) {
  fluidPage(

    includeCSS("App/style.css"),
    title="Co-Lab Shiny",

    # Use a div to provide a slight left margin
    div(
      HTML("<h2>Duke University Co-lab Shiny Workshop</h2><br><h3>GWAS Pleiotropy Network</h3><br><br>"),
      style="margin-left: 30px"
    ),

    div(
      fluidRow(

        # Prompts
        column(width=2,
          sidebarPanel(width=12,
            radioButtons("vertexType", "Vertices", choices=c("Phenotype", "SNP"), selected="Phenotype", inline=F),
            HTML("<br>"),
            sliderInput("log_10_p", HTML("log<sub>10</sub>(p) min filter"), min=4, max=12, value=5.5, step=0.25),
            HTML("<br>"),
            sliderInput("nedgemin", "Vertex n-edge (min) filter", min=0, max=100, value=0, step=1),
            HTML("<br>"),
            radioButtons("physics", "Physics", choiceNames=c("on", "off"), choiceValues=c(T, F), selected=F, inline=T),
            HTML("<br>"),
            sliderInput("eopacity", "Edge opacity", min=0, max=1, value=0.35, step=0.05),
            HTML("<br><hr>shift-click vertex to subnet<hr><br>"),
            actionButton("regen", "Regenerate graph"),
            HTML("<br><br>"),
            actionButton("restoreVertex", "Restore after subnet"),
            HTML("<br><br>"),
            actionButton("redrawEdge", "Redraw edges"),
            # Hidden reactive fields
            # These are used by functions in server() to direct activity based on current state(s) of the graph
            # Note that the first conditionalPanel() parameter ("false") is a java expression
            conditionalPanel(condition="false",
                             textInput("reactiveInst", "reactiveInst", value=""),
                             textInput("renderInst", "renderInst", value="render")))),
  
        # Graph
        column(width=10, visNetworkOutput("g1", width="100%", height="900px"))

      ),

      style="margin-left: 20px"

    )

  )
}

##########################################################################################################
# Shiny server function
##########################################################################################################

server <- function(input, output, session) {

  # Set default physics state
  updateRadioButtons(session=session, inputId="physics", selected=F)

  # Set initial rendering state to true
  # All ui variables, except renderInst (because it is in a conditional panel?), are initialized to
  # default values on initial load and during session$reload()
  # After an input$renderInst event, renderInst contains "", which prevents rendering on reload
  # Therefore, use initrend, which initialized to T during load and reload, then set to F after render
  initrend <- T

  # Reactive control for updating global variables, constructing network components, and rendering graph
  # Note that this function is executed once during initialization and whenever vertexType changes
  observeEvent(input$renderInst, {
    print(paste("renderInst (", input$renderInst, "), initrend=", initrend, sep=""))
    if(input$renderInst=="render" | initrend) {
      # Update global vars
      vertexType <<- input$vertexType
      log_10_p <<- input$log_10_p
      nedgemin <<- input$nedgemin
      eopacity <<- input$eopacity
      # Assemble network components
      assembleNetComponents()
      # Render graph
      if(nrow(vertex)>0) {
        # Net regen is always done with physics enabled, but we want it to be disablead after regen
        # Direct disabling of physics (using visPhysics(enabled=F)) has no effect when called immediately after
        # renderVisNetwork(), but is effective when executed frimm within a shiny reactive function
        # So, although not ideal, force disable of physics by toggling the reaction control with physics par val
        g <- composeNet()
        output$g1 <- renderVisNetwork(g)
        updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
        updateRadioButtons(session=session, inputId="physics", selected=F)
      } else {
        output$g1 <- NULL
      }
      # Reset initialization renderInst flags 
      initrend <<- F
      # Note that updating input$renderInst triggers the current function once more
      updateTextInput(session=session, inputId="renderInst", value="")
    }
  }, ignoreInit=F)

    # Reactive instruction control - used for triggering actions that do not function when called, typically
  # visNetwork functions that are combined with others (called in sequence)
  # Although not ideal, reactive calls, here, to functions that would ideally be coupled with others, is effective
  observe({
    print(paste("reactiveInst (", input$reactiveInst, ")", sep=""))
    if(input$reactiveInst=="physicsOff") {
      visPhysics(visNetworkProxy("g1"), enabled=F)
    } else if(input$reactiveInst=="vertexFixedOff") {
      visUpdateNodes(visNetworkProxy("g1"), data.frame("id"=vertex[,"id"], "fixed"=F))
    }
    updateTextInput(session=session, inputId="reactiveInst", value="")
  })
  
  # Vertex type event, configure vertices and edges based on type specified
  observeEvent(input$vertexType, {
    print("vertexType")
    updateTextInput(session=session, inputId="renderInst", value="render")
  }, ignoreInit=T)

  # Log_10_p filter event
  observeEvent(input$log_10_p, {
    print("logp")
    updateTextInput(session=session, inputId="renderInst", value="render")
  }, ignoreInit=T)

  # Min edge count filter event
  observeEvent(input$nedgemin, {
    print("nedgemin")
    updateTextInput(session=session, inputId="renderInst", value="render")
  }, ignoreInit=T)  

  # Edge opacity event
  # Update edge opactiy and render graph (do not reconstruct network)
  observeEvent(input$eopacity, {
    print("eopactiy")
    eopacity <<- input$eopacity
    if(nrow(vertex)>0) {
      edge[,"color.opacity"] <<- eopacity
      g <- composeNet()
      output$g1 <- renderVisNetwork(g)
      updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
      updateRadioButtons(session=session, inputId="physics", selected=F)
    } else {
      output$g1 <- NULL
    }
  }, ignoreInit=T)  

  # Regenerate event, reconstruct initial graph 
  observeEvent(input$regen, {
    session$reload()
  }, ignoreInit=T)

  # Stabilized event
  # Disable physics after stabilization during initial network construction
  # This prevents dynamic repositioning of vertices as connected vertices are moved
  # Note that edges are not redrawn during dynamic movement, but are with the stabilize() function
  observeEvent(input$stabilized, {
    print("stabilized")
    visPhysics(visNetworkProxy("g1"), enabled=F)
  })
  
  # Physics event
  # Enable or disable physics operations (enabling causes repositioning of nodes, if not fixed, and edges)
  # Do not disable on first evaluation, during program initialization
  observeEvent(input$physics, {
    print("physics")
    if(input$physics) {
      visPhysics(visNetworkProxy("g1"), enabled=T, timestep=0.25, minVelocity=10, maxVelocity=50,
                 solver=c("barnesHut", "repulsion")[1],
                 barnesHut=list("avoidOverlap"=0.5, "springLength"=100, "springConstant"=0.5, "damping"=0.5),
                 #repulsion=list("nodeDistance"=1000),
                 stabilization=list("enabled"=T, "iterations"=1000))
    } else {
      visPhysics(visNetworkProxy("g1"), enabled=F)
    }
  }, ignoreInit=T)

  # Redraw edge event
  # Redraw by fixing vertex positions, stabilizing, then freeing vertex psitions
  observeEvent(input$redrawEdge, {
    print("redrawEdge")
    # Fix positions
    visUpdateNodes(visNetworkProxy("g1"), data.frame("id"=vertex[,"id"], "fixed"=T))
    # Stabilize
    visStabilize(visNetworkProxy("g1"))
    # Free positions
    updateTextInput(session=session, inputId="reactiveInst", value="vertexFixedOff")
  }, ignoreInit=T)

  # Vertex select event
  # Compose set of vertex IDs that includes the selected vertex and all vertices adjacent to it
  # The following function executed as a result of selectNode event configured in visEvents(), above
  # Although the event successfully updates input$nodeSelect (causing a reactive observe to execute),
  # current node information is not available at the time of execution of observeEvent() (values current
  # prior to the selectNode event are returned)
  # Therefore, compose vertex set using edge configutation
  observeEvent(input$nodeSelect, {
    # Construct set by including selected vertex and all others with edge originating at selected vertex
    # Note that edges are constructed by joining GWAS set one vertices to those of GWAS set two, so that all edges
    # are directed from set 1 to set 2
    # Therefore, include all vertices with an edge originating at the selected vertex and all vertices with edge that
    # terminates at the selected node
    print("nodeSelect")
    v0 <- input$nodeSelect[[1]][[1]][1]
    print(v0)
    k <- which(vertex[,"id"] %in% c(v0, edge[which(edge[,"from"]==v0),"to"], edge[which(edge[,"to"]==v0),"from"]))
    print(k)
  }, ignoreInit=T)

  # Vertex click event
  # Verify that a vertex has been clicked
  # At present, simply print selected vertex and all connected to it
  observeEvent(input$click, {
    print("click")
    # Identify selected vertex
    v <- input$click[["nodes"]]
    if(length(v)>0) {
      v0 <- v[[1]][1]
      print(v0)
      # Identify all vertices connected to selected vertex
      k <- which(vertex[,"id"] %in% c(v0, edge[which(edge[,"from"]==v0),"to"], edge[which(edge[,"to"]==v0),"from"]))
      print(k)
    }
  }, ignoreInit=T)

  # Vertex shift-click event
  # Verify that a vertex has been clicked
  # Hide all vertices not connected to selected vertex and all edges attached to hidden vertices
  observeEvent(input$shiftClick, {
    print("shiftClick")
    # Identify selected vertex
    v <- input$shiftClick[["nodes"]]
    if(length(v)>0) {
      v0 <- v[[1]][1]
      print(v0)
      # Identify all edges connected to selected vertex
      ke <- which(edge[,"from"]==v0 | edge[,"to"]==v0)
      # Identify all vertices connected to selected vertex
      kv <- which(vertex[,"id"] %in% unlist(edge[ke,c("from", "to")]))
      # Hide vertices that are not connected to selected vertex
      vertex[,"hidden"] <<- {x <- rep(T, nrow(vertex)); x[kv] <- F; x}
      vertex[,"physics"] <<- {x <- rep(F, nrow(vertex)); x[kv] <- T; x}
      # Hide edges connected to invisible vertices
      # Edges do not have a visible property (ugh!)
      # Setting transparency leaves labels and hover text, so save and delete edge rows
      # Do not replace original edges if edge0 exists (indicating higher order subsetting requested)
      # This enables complete reconstruction of graph
      # Note that edge0 is removed by the restore hidden subnet function
      if(!exists("edge0", envir=.GlobalEnv))
        edge0 <<- edge
      edge <<- edge[ke,]
      g <- composeNet()
      output$g1 <- renderVisNetwork(g)
      updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
      updateRadioButtons(session=session, inputId="physics", selected=F)
    }
  }, ignoreInit=T)

  # Restore hidden vertices event
  observeEvent(input$restoreVertex, {
    print("restoreVertex")
    # Identify hidden vertices
    if("hidden" %in% names(vertex) & exists("edge0", envir=.GlobalEnv)) {
      k <- which(vertex[,"hidden"])
      if(length(k)>0) {
        vertex[,"hidden"] <<- F
        vertex[,"physics"] <<- T
        edge <<- edge0
        g <- composeNet()
        output$g1 <- renderVisNetwork(g)
        updateTextInput(session=session, inputId="reactiveInst", value="physicsOff")
        updateRadioButtons(session=session, inputId="physics", selected=F)
        rm(edge0, envir=.GlobalEnv)
      }
    }
  }, ignoreInit=T)

}

##########################################################################################################
# Execution begins here
##########################################################################################################

# Retrieve GWAS observations
readData()

# Launch app
runApp(list("ui"=ui, "server"=server), launch.browser=T)


# Duke University Co-lab Shiny Workshop, Spring 2020

# Shiny App

# Generate vertex and edge data of select NC locations for import into Neo4j
# Vertices:  one facility location (source of something to be delivered or communicated to customers),
# some number of customer locations (clustered by location), and some number of carriers (to execute
# route from carrier location to facility then to each customer location cluster)
# Edges:  between each carrier and the facility, between the facility and minimum distance route from
# facility and all edges within a cluster, and between each customer location within a cluster

# A Shiny app consists of ui() and server() functions

# ui() can contain R statements (open a database and query it to populate selection lists, etc.), but its primary
# purpose is to format your web page (notice the explicit use of HTML tags)

# The HTML() function instructs Shiny to pass contained text to the browser verbatim, and is useful for formatting
# your page

# server() is a function containing R statements and function calls 
# Any base function, functions declared in loaded packages (importantly, Shiny, here), or functions that you create
# in global memory cacn be called

# runApp() is a Shiny function that launches your default browser, renders a page based on the ui() function passed,
# then executes the server() function

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
#options(device="windows")
library(shiny)
library(stringi)
library(ggplot2)
library(mapproj)

###########################################################################################
# User interface function
###########################################################################################

ui <- function(req) {

  fluidPage(

    includeCSS("style.css"),

      div(
  
        HTML("<br><H3>Duke University Co-lab - Demand, Supply, Delivery Clustering</H3>"),
  
        # Prompts
        # Number of customers and carriers
        # Center of service area of facility (latitude and longitude in degrees north of equator and west,
        # negative, from Greenwich), and service radius of facility (miles)
        column(width=2,
          HTML("<br><br><br>"),
          sidebarPanel(width=12,
            div(
              fluidRow(
                sliderInput("ncarr", "number of carriers", min=1, max=10, step=1, value=5, width="100%"),
                HTML("<br>"),
                sliderInput("ncust", "number of customers", min=10, max=250, step=10, value=50, width="100%"),
                HTML("<br>"),
                sliderInput("srvlat", "service area latitude", min=20, max=50, step=0.5, value=35.5, width="100%"),
                HTML("<br>"),
                sliderInput("srvlong", "service area longitude", min=-100, max=-50, step=0.5, value=-80, width="100%"),
                HTML("<br>"),
                sliderInput("srvrad", "service area radius", min=10, max=250, step=10, value=100, width="100%")
              ),
              style="margin-left: 10px; margin-right: 10px"
            )
          )
        ),
  
        column(width=10,
          HTML("<br><br><br><br>"),
          # Graph
          fluidRow(width=12,
                   HTML("<center>"),
                   column(width=12, plotOutput("plot", width="800px", height="800px")),
                   HTML("</center>")
          ),
          # Message line
          HTML("<br><br><br>"),
          fluidRow(width=12,
                   HTML("<center>"),
                   # Object type htmlOutput required to render HTML font tag (color, etc.)
                   column(width=12, htmlOutput("msg")),
                   HTML("</center>")
          )
        ),
  
        style="margin-left: 10px"
  
      )
  
    )

}

###########################################################################################
# Function:
# Compute edge distance as minimum arc length on Earth sized sphere between each pair of locations
# Compute arc length using angle of isosceles triangle (with base connecting edge endpoints on
# Earth surface, and angle of interst at Earth center, and legs of length equal to Earth radius)
# Each row of loc must contain cloumns lat1, long1, lat2, long2, where the 1 suffix gives
# coordinates of location 1 and the 2 suffix gives coordinates of location 2
###########################################################################################

edgedist <- function(loc) {
  # Compute coordinates in 3-space for each location in each pair
  x1 <- cbind(cos(loc[,"long1"])*cos(loc[,"lat1"]), sin(loc[,"long1"])*cos(loc[,"lat1"]), sin(loc[,"lat1"]))
  x2 <- cbind(cos(loc[,"long2"])*cos(loc[,"lat2"]), sin(loc[,"long2"])*cos(loc[,"lat2"]), sin(loc[,"lat2"]))
  # Compute squared location differences in 3 dimensions
  d <- (x1-x2)*(x1-x2)
  # Compute angle at Earth center from half of triangle base (square root of sum of squared differences in 3D)
  tribase <- sqrt(d[,1]+d[,2]+d[,3])
  th <- 2*asin(tribase/2)
  # Arc distance is angle * Earth radius
  return(th*erad)
}

###########################################################################################
# Server function
###########################################################################################

server <- function(input, output, session) {

  observe({

    # Assign reactive values to static variables
    # The new values are used to update computations and plot whenever any on-screen prompt value changes
    # Note that all data are re-read, locations are selected, and clustering accomplished
    # whenever any input parameter is modified
    # Functions should be developed to restrict operations to associated parameters that affect results
    ncarr <- input$ncarr
    ncust <- input$ncust
    srvlat <- input$srvlat
    srvlong <- input$srvlong
    srvrad <- input$srvrad

    # Compute miles per degree in latitude and longitude at center of service coordinates
    # Note that miles per degree, latitudinally, is constant across the surface of a sphere,
    # while it varies longitudinally depending on the latitude of a location
    # On Earth, there are approximately 66.67 surface miles per degree of latitude (also in
    # longitude, but only at the equator)
    # Diminish miles per degree of longitude from 66.67 at the equator to o at the pole
    # Miles per deg longitude is computed from the circumference of a disc elevated a height
    # corresponding to degrees latitude
    # Note the assumption of lat/long coordinates limited to the northern hemisphere
    mpdeg <- c("lat"=2*erad*pi/360, "long"=2*erad*cos(srvlat*degtorad)*pi/360)
    mpdeg <- setNames(c(mpdeg, mpdeg["long"]/mpdeg["lat"]), c("lat", "long", "longlatratio"))
  
    # Retrieve location data (city, state, lat, and long)
    # Restrict to lat and long coordinates within a disc centered at service area coordinates with specified radius
    # Miles per degree latitude is constant, but per degree longitude varies (from 0 at pole to a maximum equal to
    # the latitude constant value, at the equator), depending on latitude
    # For quick filtering, compute distance from service center on a 2D disc, instead of true spherical distance
    # To approximate a disc, decrease longitude degree (increase degrees per mile) so that miles across range
    # is approximately equal that of latitude range at the center of the service area 
    loc <- subset(read.table("zipcode-database-primary.csv", header=T, sep=",", strip.white=T),
             ((Lat-srvlat)*mpdeg["lat"])**2 + ((Long-srvlong)*mpdeg["long"]*mpdeg["longlatratio"])**2 <= srvrad*srvrad)[,c("City", "State", "Lat", "Long")]
    
    # Collapse location record for each city, reorder rows in longitude, latitude order, so that neighboring
    # vertices receive approximately near ID labels
    loc <- loc[which(!duplicated(tolower(cbind(loc[,"City"], loc[,"State"])))),]
    loc <- loc[order(loc[,"Long"], loc[,"Lat"]),]
    rownames(loc) <- NULL
    
    # Convert location names to proper case and convert lat/long from degrees to radians
    loc[,"City"] <- stri_trans_general(loc[,"City"], id="Title")
    loc[,c("Lat", "Long", "LatDeg", "LongDeg")] <- cbind(loc[,c("Lat", "Long")]*degtorad, loc[,c("Lat", "Long")])
  
    # Select facility, carrier, and customer locations
    # Choose a facility from top twenty locations nearest service area center
    kfacility <- sample(order((loc[,"LatDeg"]-srvlat)**2 + ((loc[,"LongDeg"]-srvlong)*mpdeg["longlatratio"])**2)[1:min(20, nrow(loc))], 1)
    # Select carriers from quadrants about the facility, each with side equal to a portion of the service radius
    # Service radius is in miles, but degrees are needed for selection (adjust longitudinal range for approximate disc in 2D)
    qdside <- setNames(c(srvrad/mpdeg["lat"], srvrad/mpdeg["long"]/mpdeg["longlatratio"])*carrquadfactor, c("lat", "long"))
    qdbound <- cbind(c(loc[kfacility,"LongDeg"]-qdside["long"], loc[kfacility,"LongDeg"], loc[kfacility,"LongDeg"]+qdside["long"]),
                     c(loc[kfacility,"LatDeg"]-qdside["lat"], loc[kfacility,"LatDeg"], loc[kfacility,"LatDeg"]+qdside["lat"]))
    kcarrier <- vector("numeric", ncarr)
    for(i in 1:ncarr) {
      # Alternate to opposing corners on even iterations, clockwise for odd
      if(i%%4==1) {
        k <- which(loc[,"LongDeg"]>qdbound[1,1] & loc[,"LongDeg"]<qdbound[2,1] & loc[,"LatDeg"]>qdbound[2,2] & loc[,"LatDeg"]<qdbound[3,2])
      } else if(i%%4==2) {
        k <- which(loc[,"LongDeg"]>qdbound[2,1] & loc[,"LongDeg"]<qdbound[3,1] & loc[,"LatDeg"]>qdbound[1,2] & loc[,"LatDeg"]<qdbound[2,2])
      } else if(i%%4==3) {
        k <- which(loc[,"LongDeg"]>qdbound[1,1] & loc[,"LongDeg"]<qdbound[2,1] & loc[,"LatDeg"]>qdbound[1,2] & loc[,"LatDeg"]<qdbound[2,2])
      } else {
        k <- which(loc[,"LongDeg"]>qdbound[2,1] & loc[,"LongDeg"]<qdbound[3,1] & loc[,"LatDeg"]>qdbound[2,2] & loc[,"LatDeg"]<qdbound[3,2])
      }
      # Sample a location (exclude prior chosen locations, the facility is excluded in above quadrant subsetting)
      kcarrier[i] <- sample(setdiff(k, kcarrier), 1)
    }
    # Select customers from remaining locations
    kcustomer <- sample(setdiff(1:nrow(loc), c(kfacility, kcarrier)), ncust, replace=F)
  
    # Compose vertex IDs
    vid <- c(paste("fac", kfacility, sep=""), paste("car", kcarrier, sep=""), paste("cus", kcustomer, sep=""))
  
    # Cluster customer locations (one cluster per carrier)
    cclust <- kmeans(loc[kcustomer,c("Lat", "Long")], ncarr)
  
    # Combine pairs of locations for edge construction
    # Omit edges between carriers, since routes between them are infeasible
    # Restrict customer to customer edges to within cluster customer locations
    # Omit edges between carriers and customers (later, one edge will be created per carrier
    # to the nearest customer selected from each cluster tha is nearest the facility)
    epair <- rbind(# Carrier to facility
                   cbind(kcarrier, rep(kfacility, ncarr)),
                   # Facility to each customer (all but one per cluster will be ignored)
                   cbind(rep(kfacility, ncust), kcustomer),
                   # Between customers within cluster
                   do.call(rbind, tapply(kcustomer, cclust[["cluster"]], function(k) t(combn(k, 2)))))
    colnames(epair) <- c("v1", "v2")
  
    # Compute distance of arcs between locations
    edist <- edgedist(matrix(c(loc[epair[,"v1"],"Lat"], loc[epair[,"v1"],"Long"],
                               loc[epair[,"v2"],"Lat"], loc[epair[,"v2"],"Long"]),
                      ncol=4, dimnames=list(NULL, c("lat1", "long1", "lat2", "long2"))))
    
    # Compose edges:  carrier to facility
    k <- which(epair[,"v1"] %in% kcarrier & epair[,"v2"]==kfacility)
    edge <- data.frame("type"="car-fac", "v1"=paste("car", epair[k,"v1"], sep=""),
                       "v2"=paste("fac", epair[k,"v2"], sep=""), "distance"=round(edist[k], 1))
    
    # Compose edges:  facility to minimum distance customer location in each cluster and
    # carrier to nearest customer selected from cluster (nearest to facility)
    # Identify, from each cluster, the customer location nearest the facility
    # These are used to compose facility to customer edges
    # The customer locations are used to compose carrier to customer location edges (nearest)
    kfc <- tapply(which(epair[,"v1"]==kfacility & epair[,"v2"] %in% kcustomer), cclust[["cluster"]],
                  function(k)
                    k[which(edist[k]==min(edist[k]))[1]])
    edge <- rbind(edge, data.frame("type"="fac-cus", "v1"=paste("fac", epair[kfc,"v1"], sep=""),
                                   "v2"=paste("cus", epair[kfc,"v2"], sep=""), "distance"=round(edist[kfc], 1)))
    # Compose edges:  carrier to nearest customer in nearest cluster (using carrier and cluster center locations)
    # Compute distance for all combinations of carrier and cluster centers
    if(ncarr>1) {
      # Maintain order of carriers
      iclust <- cbind(as.vector(apply(as.matrix(kcarrier), 1, function(k) rep(k, nrow(cclust[["centers"]])))),
                      rep(1:nrow(cclust[["centers"]]), ncarr))
      dcc <- edgedist(matrix(c(loc[iclust[,1],"Lat"], loc[iclust[,1],"Long"],
                               cclust[["centers"]][iclust[,2],"Lat"], cclust[["centers"]][iclust[,2],"Long"]),
                      ncol=4, dimnames=list(NULL, c("lat1", "long1", "lat2", "long2"))))
      # Identify least total distance combination of carrier and cluster pairings
      # Evaluate each permutation of carrier to cluster pairing
      prm <- pracma::perms(1:ncarr)
      d <- apply(prm, 1, function(j) {
                           # Compute indices into dcc vector for each carrier
                           # Note that j is a vector of length ncarr
                           # Pos i of j indicates which cluster distance value to use for carrier i
                           # Compute offsets into distance vector for each carrier
                           i <- (0:(ncarr-1))*ncarr+j
                           sum(dcc[i])
                         })
      # Assign carriers to clusters from minimum total distance
      carclust <- prm[which(d==min(d)[1]),]
    } else {
      carclust=1
    }
    
    # Locate, for each carrier and cluster pair, the customer location nearest the carrier location
    # Exclude the customer location nearest facility, to prevent a closed path between the facility,
    # first vertex, and carrier location when constructing simple paths (vertices not repeated)
    edge <- rbind(edge, do.call(rbind,
              apply(as.matrix(1:ncarr), 1,
                function(i) {
                  # Compute distances between current carrier and all customers in assigned cluster
                  # Index customer locations in assigned cluster, excluding locations nearest facility
                  kc <- setdiff(kcustomer[which(cclust[["cluster"]]==carclust[i])], epair[kfc[carclust[i]],"v2"])
                  # Assemble matrix of lat/long coordinates for each pair
                  x <- cbind(rep(loc[kcarrier[i],"Lat"], length(kc)),
                             rep(loc[kcarrier[i],"Long"], length(kc)),
                             loc[kc,"Lat"], loc[kc,"Long"])
                  colnames(x) <- c("lat1", "long1", "lat2", "long2")
                  dcc <- edgedist(x)
                  # Identify minimum distance customer
                  kmd <- which(dcc==min(dcc))[1]
                  # Note the direction of customer to carrier (intended to reflect a final edge in a route)
                  data.frame("type"="cus-car", "v1"=paste("cus", kc[kmd], sep=""),
                             "v2"=paste("car", kcarrier[i], sep=""), "distance"=round(dcc[kmd], 1))
                })))
    
    # Compose edges:  within cluster customer to customer edges
    k <- which(epair[,"v1"] %in% kcustomer & epair[,"v2"] %in% kcustomer)
    edge <- rbind(edge, data.frame("type"="cus-cus", "v1"=paste("cus", epair[k,"v1"], sep=""),
                                   "v2"=paste("cus", epair[k,"v2"], sep=""), "distance"=round(edist[k], 1)))
    
    # Write vertex configuration
    #write.table(data.frame(# Vertex type
    #                       c("facility", rep("carrier", ncarr), rep("customer", length(kcustomer))),
    #                       # Neo4j vertex ID and copy that can be referenced in cypher script
    #                       vid, vid,
    #                       # City
    #                       loc[c(kfacility, kcarrier, kcustomer),"City"],
    #                       # State
    #                       loc[c(kfacility, kcarrier, kcustomer),"State"],
    #                       # Customer location cluster
    #                       c(0, carclust, cclust[["cluster"]])),
    #            "Traversal-Vertex.csv", row.names=F, col.names=c("class", ":ID", "id", "city", "state", "cluster"), sep=",", quote=F)
    
    # Optional:  Write edge configuration for Neo4j import
    #write.table(edge, "Traversal-Edge.csv", row.names=F, col.names=T, sep=",", quote=F)
    
    # Plot map of selected geographic points
  
    # Identify maximum distance (lat and long) of customer coordinates to facility
    # Use it to crop map so that vertical and horiaontal axes have identical scale
    degoffset <- max(c(abs(loc[,"LatDeg"]-loc[kfacility,"LatDeg"]), abs(loc[,"LongDeg"]-loc[kfacility,"LongDeg"])))*2
    
    output$plot <- renderPlot(
      ggplot() +
        geom_polygon(data=map_data("state", state.name[which(state.abb %in% unique(loc[,"State"]))]),
          aes(x=long, y=lat, group=group), color="gray90", fill="white") +
        geom_point(data=loc[kcustomer,], aes(x=LongDeg, y=LatDeg, color=as.character(cclust[["cluster"]]),
                   shape="customer"), size=4, alpha=0.5) +
        geom_point(data=data.frame(cclust[["centers"]]/degtorad), aes(x=Long, y=Lat, fill=as.character(1:ncarr),
                   shape="zone center"), size=4, alpha=0.5) +
        geom_point(data=data.frame(cclust[["centers"]]/degtorad), aes(x=Long, y=Lat), shape=21, size=5, alpha=0.5) +
        geom_point(data=loc[kcarrier,], aes(x=LongDeg, y=LatDeg, color=as.character(carclust),
                   shape="carrier"), size=4.5, alpha=1) +
        geom_point(data=loc[kcarrier,], aes(x=LongDeg, y=LatDeg), shape=21, size=6, color="black") +
        geom_point(data=loc[kfacility,], aes(x=LongDeg, y=LatDeg, shape="facility"), size=4.5) +
        geom_point(data=loc[kfacility,], aes(x=LongDeg, y=LatDeg), shape=5, size=6) +
        scale_color_discrete(name="zone") +
        scale_shape_manual(name="", values=c("facility"=9, "carrier"=10, "customer"=16, "zone center"=21)) +
        scale_fill_discrete(guide=F) +
        # Set lat and long limits
        coord_map(xlim=c(loc[kfacility,"LongDeg"]-degoffset/mpdeg["longlatratio"], loc[kfacility,"LongDeg"]+degoffset/mpdeg["longlatratio"]),
                  ylim=c(loc[kfacility,"LatDeg"]-degoffset, loc[kfacility,"LatDeg"]+degoffset)) +
        theme(axis.title=element_text(size=14),
              axis.text=element_text(size=14),
              legend.title=element_text(size=14),
              legend.text=element_text(size=14))
    )

  })

}

###########################################################################################
# Execution begins here
###########################################################################################

#setwd("/cloud/project/Duke-Co-lab/GraphDeliveryMap")
setwd("C:\\Projects\\Duke\\Co-lab\\Shiny-Spring-2020\\GraphDeliveryMap")

# Specify Earth radius (miles), pi, and degrees to radians factor
erad <- 3959
pi <- 4*atan(1)
degtorad <- pi/180

# Specify carrier quadrant factor determines the portion of service radius to use as side length
# of quadrants about facility to be used as zones for sampling carrier locations
carrquadfactor <- 0.5


# Execute Shiny application
runApp(list("ui"=ui, "server"=server), launch.browser=T)

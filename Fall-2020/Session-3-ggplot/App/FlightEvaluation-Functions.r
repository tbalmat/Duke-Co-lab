#####################################################################################################
# Duke University Co-lab Shiny Workshop, Session 3, October 2020
# U.S. Domestic Flight Evalution App
# Functions
#####################################################################################################

###############################################################################################################
#### Function:  Read source data files (all zip files in FlightData directory)
###############################################################################################################

readData <- function(dirDat) {

  cat("Read-0\n")

  # Flight data
  # Restrict columns used
  col <- c("FlightDate", "Month", "DayOfWeek", "DOT_ID_Reporting_Airline", "OriginAirportID",
           "DestAirportID", "DepDelay", "ArrDelay",  "Cancelled", "CarrierDelay", "OriginState", "DestState")
  
  # Limit to Continental U.S. origins and destinations
  st <- c("AL", "AR", "AZ", "CA", "CO", "CT", "FL", "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
          "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
          "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
  
  # Read all zip files in specified directory
  dr <- dir(dirDat, pattern="*.zip")
  progress <- shiny::Progress$new()
  fldat <<- do.call(rbind,
                    apply(as.matrix(1:length(dr)), 1,
                          function(i) {
                            progress$set(message="reading flight data", value=i/length(dr))
                            zfile <- paste(dirDat, "/", dr[i], sep="")
                            cfile <- sub("1987_present", "(1987_present)", dr[i])
                            cfile <- sub(".zip", ".csv", cfile)
                            fd <- subset(read.table(unz(zfile, cfile), header=T, sep=",")[,col],
                                         OriginState %in% st & DestState %in% st)
                          }))
  progress$close()

  # Read airline data
  aldat <<- read.table(paste(dirDat, "/77763800_T_CARRIER_DECODE.csv", sep=""), header=T, sep=",")
  colnames(aldat) <<- tolower(colnames(aldat))
  aldat <<- aldat[which(aldat[,"airline_id"] %in% unique(fldat[,"DOT_ID_Reporting_Airline"]) & aldat[,"region"]=="Domestic" &
                        aldat[,"thru_date_source"]==""),c("airline_id", "carrier", "carrier_name")]
  # Assign airline IDs as row names to facilitate efficient searching of airline carrier and carrier name
  rownames(aldat) <<- aldat[,"airline_id"]
  # Configure airline labels 
  alLabel <<- setNames(paste(aldat[,"carrier_name"], " (", aldat[,"carrier"], ")", sep=""), rownames(aldat))

  # Read airport data, including lat and long
  apdat <<- read.table(paste(dirDat, "/77765270_T_MASTER_CORD.csv", sep=""), header=T, sep=",")
  colnames(apdat) <<- tolower(colnames(apdat))
  apdat <<- apdat[which((apdat[,"airport_id"] %in% unique(fldat[,"OriginAirportID"]) |
                         apdat[,"airport_id"] %in% unique(fldat[,"DestAirportID"])) & apdat[,"airport_is_latest"]==1),
                  c("airport_id", "airport", "display_airport_name", "display_airport_city_name_full",
                    "airport_state_code", "latitude", "longitude", "utc_local_time_variation")]

  cat("Read-1\n")

}

###############################################################################################################
#### Function:  Aggregate proportion flights by origin and destination within aggVar (typically month or weekday)
#### Limit to carrier delayed flights if requested
###############################################################################################################

aggfdat <- function(aggVar, carrierDelay, includeCancel) {

  cat("agg-0\n")
  progress <- shiny::Progress$new()
  progress$set(message="aggregating", value=0.5)

  if(nrow(fldat)>0) {

    # Subset to include carrier delays and cancellations, as requested
    k0 <- which((fldat[,"CarrierDelay"]>0 | !carrierDelay) & (fldat[,"Cancelled"]==0 | includeCancel))

    if(length(k0)>0) {

      # Aggregate within levels of specified aggregation var
      if(aggVar!="") {
        fd <- do.call(rbind,
                      apply(as.matrix(unique(fldat[k0,aggVar])), 1,
                        function(x) {
                          k1 <- k0[which(fldat[k0,aggVar]==x)]
                          fd <- aggregate(k1, by=list(fldat[k1,"OriginAirportID"], fldat[k1,"DestAirportID"]), length)
                          # Compute proportion of all flights by aggregation var, origin, and destination
                          return(setNames(data.frame(x, fd[,1:2], fd[,3]/length(k1)),
                                          c(aggVar, "OriginAirportID", "DestAirportID", "p")))
                        }))
      } else {
        fd <- aggregate(k0, by=list(fldat[k0,"OriginAirportID"], fldat[k0,"DestAirportID"]), length)
        fd[,3] <- fd[,3]/length(k0)
        colnames(fd) <- c("OriginAirportID", "DestAirportID", "p")
      }

      progress$set(message="merge origin airport", value=0.75)

      # Join flight origin airport to lat and long data
      fd <- merge(fd, apdat[,c("airport_id", "airport", "latitude", "longitude")], by.x="OriginAirportID", by.y="airport_id")
      colnames(fd)[which(colnames(fd) %in% c("airport", "latitude", "longitude"))] <- c("airportOrigin", "latitudeOrigin", "longitudeOrigin")

      progress$set(message="merge destination airport", value=1)

      # Join flight destination airport to lat and long data
      fd <- merge(fd, apdat[,c("airport_id", "airport", "latitude", "longitude")], by.x="DestAirportID", by.y="airport_id")
      colnames(fd)[which(colnames(fd) %in% c("airport", "latitude", "longitude"))] <- c("airportDest", "latitudeDest", "longitudeDest")

    } else {
      fd <- data.frame()
    }
  } else {
    fd <- data.frame()
  }

  progress$close()
  cat("agg-1\n")
  return(fd)

}

###############################################################################################################
#### Function:  Plot flights from origin to destination on map of the continental U.S.
#### Adjust arc size, color, and alpha from proportion values (p) in source data (flights aggregated by
#### origin, destination, and possibly a third variable (typically month or weekday)
#### Size, color, and alpha scales specified in sizeRange, colorRange, colorScaleMid, and alphaRange
###############################################################################################################

composePlotMap <- function(pthreshFlight, pthreshAirportLabel, colorRange=c("blue", "green", "red"),
                           colorScaleMid=NA, sizeRange=c(0.25, 2), alphaRange=c(0.25, 0.85),
                           facetVar="", facetLabel="", facetRows=NULL, ttl="", subttl="", xlab="", ylab="") {

  library(ggplot2)
  library(ggrepel)

  cat("graph-map-0\n")
  progress <- shiny::Progress$new()
  progress$set(message="composing flight map", value=1)

  # Restrict flights to proportion >= p-threshold
  kflgtp <- which(fldatb[,"p"]>pthreshFlight)

  # Compose unique list of airports with proportion flights above label threshold
  klab <- kflgtp[which(fldatb[kflgtp,"p"]>pthreshAirportLabel)]
  if(length(klab)>0) {
    if(facetVar!="") {
      # Compose within facet var and retain facet var value, so that sets are produced for each facet level
      aplab <- aggregate(klab, by=list(fldatb[klab,facetVar]),
                 function(k) {
                   # Assemble vector of unique airports and retrieve associated lat and long
                   ap <- unique(c(fldatb[k,"OriginAirportID"], fldatb[k,"DestAirportID"]))
                   return(apdat[which(apdat[,"airport_id"] %in% ap),c("airport", "latitude", "longitude")])
                 })
      aplab <- do.call(rbind,
                       apply(as.matrix(1:nrow(aplab)), 1,
                         function(i) data.frame(aplab[i,1], aplab[i,2][[1]], aplab[i,2][[2]], aplab[i,2][[3]])))
      colnames(aplab) <- c(facetVar, "airport", "latitude", "longitude")
    } else {
      # Compose within entire set of flights, since grouping (faceting) not requested
      # Assemble vector of unique airports and retrieve associated lat and long
      ap <- unique(c(fldatb[klab,"OriginAirportID"], fldatb[klab,"DestAirportID"]))
      aplab <- apdat[which(apdat[,"airport_id"] %in% ap),c("airport", "latitude", "longitude")]
    }
  } else {
    aplab <- data.frame("airport"=character(), "latitude"=numeric(), "longitude"=numeric())
  }

  # Compute mid-point of color scale as mean proportion, if not specified
  if(is.na(colorScaleMid))
    colorScaleMid <- mean(fldatb[kflgtp,"p"])

  # Initialize plot
  g <- ggplot() +
       # Include map of continental U.S.
       geom_polygon(data=map_data("state"), aes(x=long, y=lat, group=group), color="gray85", fill="white") +
       # Include arcs connecting each unique pair of flight origins and destinations
       # Note that the arc color, size, and transparency are a function of the proportion of flights within aggegation (facet) var
       # Because these appeaer in the aes() call, ggplot will assign a unique color, size, and alpha to segmented ranges of proportions
       geom_curve(data=fldatb[kflgtp,],
                  aes(x=longitudeOrigin, xend=longitudeDest, y=latitudeOrigin, yend=latitudeDest, color=p, size=p, alpha=p),
                  curvature=0.1, arrow=arrow(angle=20, length=unit(0.1, "in"), type="closed")) +
       # Define arc color, size, and alpha ranges for aesthetic and legend assignment
       scale_color_gradient2(name="proportion flights", low=colorRange[1], mid=colorRange[2], high=colorRange[3], midpoint=colorScaleMid) +
       scale_size_continuous(name="proportion flights", range=sizeRange, guide=F) +
       scale_alpha_continuous(name="proportion flights", range=alphaRange, guide=F)

  # Include airport labels, if present
  # Note the use of repel to prevent labels from overlapping
  # Each label is given unique text (airport name), since label appears in the aes() call
  if(nrow(aplab)>0)
    g <- g + geom_label_repel(data=aplab, aes(x=longitude, y=latitude, label=airport),
                              size=3, angle=0, direction="both", label.padding = 0.2, alpha=0.65)

  # Facet by specified variable
  # It is assumed that flight data are aggregated by the specified variable (developer's responsibility!)
  if(facetVar!="")
    g <- g + facet_wrap(facetVar, nrow=facetRows, labeller=as_labeller(facetLabel))

  # Add theme and titles
  g <- g + theme(plot.title=element_text(size=12, hjust=0.5),
                 plot.subtitle=element_text(size=10, hjust=0.5),
                 plot.caption=element_text(size=12, hjust=0.5),
                 panel.background=element_blank(),
                 panel.grid.major.x=element_blank(),
                 panel.grid.major.y=element_blank(),
                 panel.grid.minor=element_blank(),
                 panel.border=element_rect(fill=NA, color="gray75"),
                 panel.spacing.x=unit(0, "lines"),
                 axis.ticks = element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 strip.text=element_text(size=12),
                 strip.background=element_blank(),
                 #legend.position="bottom",
                 legend.background=element_rect(color="gray"),
                 legend.key=element_rect(fill="white"),
                 legend.box="vertical",
                 legend.text=element_text(size=12),
                 legend.title=element_text(size=12)) +
           labs(title=ttl, subtitle=subttl, x=xlab, y=ylab)

  progress$close()
  cat("graph-map-1\n")

  return(g)

}

###############################################################################################################
#### Function:  Plot density ridges of carrier or arrival delay
###############################################################################################################

composePlotDensity <- function(x, xlim=c(-200, 200), y, yOrder="",
                               yFillColor=c("dodgerblue4", "gold3"), reverseFillColor=F, fillAlpha=0.5,
                               facetVar="", facetLabel="", facetRows=NULL, vline="", ttl="", subttl="") {

  library(ggridges)

  cat("graph-dens-0\n")
  progress <- shiny::Progress$new()
  progress$set(message="composing density plot", value=1)

  # Omit cancellations and limit to delayed flights if requested
  # Compose graph data set (independent var, x)
  if(x=="CarrierDelay") {
    kfl <- which(fldat[,"Cancelled"]==0 & fldat[,"CarrierDelay"]>0)
    gdat <- data.frame("x"=fldat[kfl,"CarrierDelay"])
  } else {
    # Arrival delay (less departure delay)
    kfl <- which(fldat[,"Cancelled"]==0)
    gdat <- data.frame("x"=fldat[kfl,"ArrDelay"]-fldat[kfl,"DepDelay"])
  }

  # Include dependent var, y
  if(y=="DayOfWeek") {
    gdat[,"y"] <- factor(weekdayLabel[fldat[kfl,"DayOfWeek"]],
                         levels=weekdayLabel[sort(unique(fldat[kfl,"DayOfWeek"]))])
  } else if(y=="Month") {
    gdat[,"y"] <- factor(monthLabel[fldat[kfl,"Month"]],
                         levels=monthLabel[sort(unique(fldat[kfl,"Month"]))])
  } else if(y=="DOT_ID_Reporting_Airline") {
    gdat[,"y"] <- factor(aldat[as.character(fldat[kfl,"DOT_ID_Reporting_Airline"]),"carrier"])
  }

  # Include facet var
  if(facetVar!="")
    gdat[,"facetVar"] <- factor(fldat[kfl,facetVar])

  # Convert y factor levels for ordering on y-axis (mean(x), median(x))
  if(yOrder=="mean") {
    lv <- aggregate(gdat[,"x"], by=list(gdat[,"y"]), function(x) mean(x, na.rm=T))
    gdat[,"y"] <- factor(gdat[,"y"], levels=lv[order(lv[,2]),1])
  } else if(yOrder=="median") {
    lv <- aggregate(gdat[,"x"], by=list(gdat[,"y"]), function(x) median(x, na.rm=T))
    gdat[,"y"] <- factor(gdat[,"y"], levels=lv[order(lv[,2]),1])
  }

  # Compose fill color vector using number of discrete shades = number of y values
  nfill <- length(levels(gdat[,"y"]))
  fillv <- (colorRampPalette(yFillColor))(nfill)

  # Reverse fill color, if requested
  if(!reverseFillColor) {
    kfill <- 1:nfill
  } else {
    kfill <- nfill:1
  }
  names(fillv) <- levels(gdat[,"y"])[kfill]

  # Compose x and y axis labels
  if(x=="CarrierDelay") {
    xlab <- "\ncarrier delay (minutes)"
  } else {
    xlab <- "\narr-dep delay differential (minutes)"
  }
  ylab <- "density\n"

  # Initialize plot
  g <- ggplot() +
       # Include density ridges with fill color based on level of y-variable
       geom_density_ridges(data=gdat, aes(x=x, y=y, fill=y), alpha=fillAlpha, show.legend=F) +
       scale_fill_manual(values=fillv)

  # Apply x-axis limits, if requested
  if(length(xlim)==2)
    g <- g + scale_x_continuous(limits=xlim)

  # Facet by specified variable
  # It is assumed that flight data are aggregated by the specified variable (developer's responsibility!)
  if(facetVar!="")
    g <- g + facet_wrap(~facetVar, nrow=facetRows, labeller=as_labeller(facetLabel))

  # Apply theme, title, and axis labels
  g <- g + theme(plot.title=element_text(size=12, hjust=0.5),
             plot.subtitle=element_text(size=10, hjust=0.5),
             plot.caption=element_text(size=12, hjust=0.5),
             panel.background=element_blank(),
             panel.grid.major.x=element_blank(),
             panel.grid.major.y=element_blank(),
             panel.grid.minor=element_blank(),
             panel.border=element_rect(fill=NA, color="gray75"),
             panel.spacing.x=unit(0, "lines"),
             axis.title.x=element_text(size=12),
             axis.title.y=element_text(size=12),
             axis.text.x=element_text(size=12),
             axis.text.y=element_text(size=12),
             strip.text=element_text(size=12),
             strip.background=element_blank(),
             legend.position="bottom",
             legend.background=element_rect(color="gray"),
             legend.key=element_rect(fill="white"),
             legend.box="horizontal",
             legend.text=element_text(size=8),
             legend.title=element_text(size=8)) +
           labs(title=ttl, subtitle=subttl, x=xlab, y=ylab)

  # Include vertical lines (0, mean, or median), if requested
  # Note that the coordinates created by geom_density_ridges are extracted from the first element of the plot
  # Configure vertical line coordinates
  if(vline %in% c("median", "mean", "0")) {
    # Compose beginning and ending coordinates for each line
    # Panels correspond to facet panels, groups correspond to y axis values (one for each density distribution)
    g2 <- ggplot_build(g)[["data"]][[1]][,c("PANEL", "group", "x", "ymin", "density", "scale", "iscale")]
    # Locate, for each panel and group, x-intercept near median, mean, or 0
    k0 <- which(!is.na(g2[,"x"]))
    lines <- aggregate(k0, by=list(g2[k0,"PANEL"], g2[k0,"group"]),
                       function(k) {
                         scale <- g2[k[1],"scale"]
                         iscale <- g2[k[1],"iscale"]
                         dx <- (g2[k[2],"x"]-g2[k[1],"x"])*scale
                         # Retrieve observation indices corresponding to subset fot current Panel and group
                         if(facetVar!="") {
                           km <- which(gdat[,"facetVar"]==levels(gdat[,"facetVar"])[g2[k[1],"PANEL"]] &
                                       gdat[,"y"]==levels(gdat[,"y"])[g2[k[1],"group"]])
                         } else {
                           km <- which(gdat[,"y"]==levels(gdat[,"y"])[g2[k[1],"group"]])
                         }
                         if(vline=="median") {
                           # Locate base of interval containing median
                           # Accumulate density * x interval until 0.5 reached
                           k2 <- findInterval(median(gdat[km,"x"]), g2[k,"x"]*scale)
                         } else if(vline=="mean") {
                           # Locate base of interval containing mean
                           k2 <- findInterval(mean(gdat[km,"x"]), g2[k,"x"]*scale)
                         } else {
                           # Locate base of interval containing 0
                           k2 <- findInterval(0, g2[k,"x"])
                         }
                         # Compose base of x interval (x-intercept) and ordinates for beginning and ending of each line 
                         c(g2[k[k2],"x"]*scale, g2[k[k2],"ymin"], 1)
                       })
    # Identify facet panel value, group (y), x-intercept, and y end points
    if(facetVar!="") {
      lines <- data.frame(levels(gdat[,"facetVar"])[lines[,1]], lines[,2], lines[,3][,1], lines[,3][,2], lines[,3][,3])
    } else {
      lines <- data.frame(lines[,1], lines[,2], lines[,3][,1], lines[,3][,2], lines[,3][,3])
    }
    colnames(lines) <- c("facetVar", "group", "x", "y0", "y1")
    g <- g + geom_segment(data=lines, aes(x=x, xend=x, y=y0, yend=y0+y1), size=0.65)
  }

  progress$close()
  cat("graph-dens-1\n")

  return(g)

}

# Color utilities
# display.brewer.all()
# (colorRampPalette(c("dodgerblue4","gray")))(12)
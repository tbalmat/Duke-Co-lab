# Shiny App
# Visually explore cross-sectional features of highly aggregated U.S. federal employee data
# Version 1, R scripts to be converted to Shiny

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(shiny)
library(ggplot2)

#######################################################################################################
# Read U.S. Office of Personnel Management Central Personnel Data File (CPDF)

# Source:  Buzzfeed (https://www.buzzfeednews.com/article/jsvine/sharing-hundreds-of-millions-of-federal-payroll-records)
# Limited to general schedule (GS) grades 1 through 15, fiscal years 1988 through 2011, full-time employees

# OPM codebook:  https://www.opm.gov/policy-data-oversight/data-analysis-documentation/data-policy-guidance/reporting-guidance/part-a-human-resources.pdf

# Columns:
# fy ........... U.S. federal government fiscal year
# agency ....... federal agency employed (synthetically generated for workshop)
# age .......... employee age (five year increments, noised induced by OPM)
# grade ........ general schedule (GS) grade
# occCat ....... occupational category 
# yearsEd ...... years of education
# n ............ number of observations (employees) in fy, agency, age, grade, occCat, yearsEd combination
# sumPay ....... sum of basic pay in fy, agency, age, grade, occCat, yearsEd combination

# There is one record for each unique combination of fy, agency, age, grade, occCat, yearsEd combination
# n and sumPay are aggregated within fy, agency, age, grade, occCat, yearsEd combinations
#######################################################################################################

setwd("C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-1-NPDHist-CPDF\\App\\CPDF")
cpdf <- read.table("CPDFAggregateDataBuzzfeed-Agency.csv", header=T, sep=",", strip.white=T)

# Compute mean pay per category (all employees in category assigned identical, mean, pay)
cpdf[,"pay"] <- cpdf[,"sumPay"]/cpdf[,"n"]

#######################################################################################################
# Configure theme and axis labels for plots
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
typeof(ggTheme)

ggLabs <- labs(x="\nfiscal year", y="pay<sub>mean</sub>\n")
typeof(ggLabs)

#######################################################################################################
# Mean pay by year (basic plot)
#######################################################################################################

# Aggregate mean pay by fy
gdat <- aggregate(1:nrow(cpdf),
                  by=list(cpdf[,"fy"]),
                  function(i) sum(cpdf[i,"sumPay"])/sum(cpdf[i,"n"]))
colnames(gdat) <- c("fy", "meanPay")

# Compose basic plot
ggplot(data=gdat) +
  geom_line(aes(x=fy, y=meanPay)) +
  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  ggTheme +
  ggLabs


#######################################################################################################
# Mean pay by year (differentiating line types)
#######################################################################################################

# Specify line type differentiation variable (NULL for no differentiation)
diffVar <- "agency"

# Aggregate mean pay by fy and differentiation variable (if one specified)
gdat <- aggregate(1:nrow(cpdf),
                  by=lapply(c("fy", diffVar), function(v) cpdf[,v]),
                  function(i) sum(cpdf[i,"sumPay"])/sum(cpdf[i,"n"]))
colnames(gdat) <- c("fy", diffVar, "meanPay")

# Initialize plot
g <- ggplot()
typeof(g) # G is just a list and is modifiable - print() renders the actual plot

# Include line(s)
if(is.null(diffVar)) {
  g <- g + geom_line(data=gdat, aes(x=fy, y=meanPay))
} else {
  g <- g + geom_line(data=gdat, aes_string(x="fy", y="meanPay", linetype=diffVar)) +
  scale_linetype_manual(values=setNames(c("22", "44", "solid"), unique(gdat[,"agency"])))
}

# Configure axis labels and apply theme
g <- g +  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  ggTheme +
  ggLabs

# Render
print(g)

#######################################################################################################
# Mean pay by year (differentiating color with faceting)
#######################################################################################################

# Specify color differentiation variable (NULL for no differentiation)
diffVar <- "occCat"

# Specify facet variable
# One facet panel for each level of the variable will be generated
# Specify NULL for no panels
panelVar <- "agency"
panelRows <- NULL
panelCols <- 1

# Aggregate mean pay by fy, differentiation variable, and facet variable
gdat <- aggregate(1:nrow(cpdf),
                  by=lapply(c("fy", diffVar, panelVar), function(v) cpdf[,v]),
                  function(i) sum(cpdf[i,"sumPay"])/sum(cpdf[i,"n"]))
colnames(gdat) <- c("fy", diffVar, panelVar, "meanPay")

# Initialize plot
g <- ggplot()

# Include line(s)
if(is.null(diffVar)) {
  g <- g + geom_line(data=gdat, aes(x=fy, y=meanPay))
} else {
  g <- g + geom_line(data=gdat, aes_string(x="fy", y="meanPay", color=diffVar)) +
  scale_color_manual(values=colorRampPalette(c("blue", "red"))(length(unique(gdat[,diffVar]))))
}

# Facet, if requested
if(!is.null(panelVar))
  g <- g + facet_wrap(panelVar, nrow=panelRows, ncol=panelCols,
                      labeller=as_labeller(function(x) paste(panelVar, " = ", x, sep="")))

# Configure axis labes and apply theme
g <- g +  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  ggTheme +
  ggLabs

print(g)


#######################################################################################################
# Mean of specified var dependent on one independent var (differentiating color with faceting)
#######################################################################################################

# Specify color differentiation variable (NULL for no differentiation)
diffVar <- "occCat"

# Specify facet variable
# One facet panel for each level of the variable will be generated
# Specify NULL for no panels
panelVar <- "grade"
panelRows <- NULL
panelCols <- NULL

# Aggregate mean of dependent var by independent var, differentiation var, and facet var
depVar <- "age"
depVarMean <- paste("mean_", depVar, sep="")
indepVar <- "fy"
gdat <- aggregate(1:nrow(cpdf),
                  by=lapply(c(indepVar, diffVar, panelVar), function(v) cpdf[,v]),
                  function(i) sum(cpdf[i,depVar]*cpdf[i,"n"])/sum(cpdf[i,"n"]))
colnames(gdat) <- c(indepVar, diffVar, panelVar, depVarMean)

# Order occupational categories in standard P, A, T, C, O sequence, if present
if("occCat" %in% colnames(gdat))
  gdat[,"occCat"] <- factor(gdat[,"occCat"], levels=c("P", "A", "T", "C", "O"))

# Specify graph type, point size, and transparency
gType <- "line"
pointSize=3
pointAlpha <- 0.5

# Initialize plot
g <- ggplot()

# Include points or lines
if(gType=="point") {
  if(is.null(diffVar)) {
    g <- g + geom_point(data=gdat, aes_string(x=indepVar, y=depVarMean))
  } else {
    g <- g + geom_point(data=gdat, aes_string(x=indepVar, y=depVarMean, color=diffVar),
                        size=dotSize, alpha=dotAlpha) +
    scale_color_manual(values=colorRampPalette(c("blue", "red"))(length(unique(gdat[,diffVar]))))
  }
} else {
  if(is.null(diffVar)) {
    g <- g + geom_line(data=gdat, aes_string(x=indepVar, y=depVarMean))
  } else {
    g <- g + geom_line(data=gdat, aes_string(x=indepVar, y=depVarMean, color=diffVar)) +
    scale_color_manual(values=colorRampPalette(c("blue", "red"))(length(unique(gdat[,diffVar]))))
  }
}  

# Facet, if requested
if(!is.null(panelVar))
  g <- g + facet_wrap(panelVar, nrow=panelRows, ncol=panelCols,
                      labeller=as_labeller(function(x) paste(panelVar, " = ", x, sep="")))

# Configure axis labes and apply theme
g <- g +  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  ggTheme +
  labs(x=paste("\n", indepVar, sep=""), y=paste(depVar, "<sub>mean</sub>\n", sep=""))

print(g)


#######################################################################################################
# Distribution of observations (employees) by one variable, paneled by another
#######################################################################################################

# Specify independent and facet variables
indepVar <- "grade"
panelVar <- "fy"
panelRows <- NULL
panelCols <- NULL
loessSpan <- 0.75

# Compute observed mass distribution(s) of independent var
if(is.null(panelVar)) {
  ng <- sum(cpdf[,"n"])
  gdat <- aggregate(cpdf[,"n"], by=list(cpdf[,indepVar]), function(n) sum(n)/ng)
  colnames(gdat) <- c(indepVar, "p")
} else {
  gdat <- aggregate(cpdf[,"n"], by=list(cpdf[,indepVar], cpdf[,panelVar]), sum)
  colnames(gdat) <- c(indepVar, panelVar, "n")
  ng <- aggregate(gdat[,"n"], by=list(gdat[,panelVar]), sum)
  colnames(ng) <- c(panelVar, "n")
  gdat <- merge(gdat, ng, by.x=panelVar, by.y=panelVar)
  gdat <- data.frame(gdat[,c(panelVar, indepVar, "n.x")], "p"=gdat[,"n.x"]/gdat[,"n.y"])
  colnames(gdat) <- c(panelVar, indepVar, "n", "p") 
}

# Initialize plot
g <- ggplot()

# Add smooth probability mass plot
g <- g + geom_smooth(data=gdat, aes_string(x=indepVar, y="p"),
                     method="loess", se=F, span=loessSpan, fullrange=T, color="Black", size=0.6)

# Facet, if requested
if(!is.null(panelVar))
  g <- g + facet_wrap(panelVar, nrow=panelRows, ncol=panelCols,
                      labeller=as_labeller(function(x) paste(panelVar, " = ", x, sep="")))

# Configure axis labes and apply theme
g <- g +  ggTheme +
  labs(x=paste("\n", indepVar, sep=""), y="P<sub>mass</sub>\n")

# Render
print(g)

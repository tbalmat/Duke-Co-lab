# Duke University Co-lab Shiny Workshop, Session 2, November 2019

# Compose tables of aggregated OPM CPDF U.S. federal employee career variables
# Visually explore cross-sectional features of subsets of employees contained in disjoint rows
# of aggregation table

# Version 1, R scripts to be converted to a Shiny app

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(ggplot2)

#######################################################################################################
# Data
#######################################################################################################

# Source:  Buzzfeed (https://www.buzzfeednews.com/article/jsvine/sharing-hundreds-of-millions-of-federal-payroll-records)

# Observations are limited to:
# FY between 1988 and 2011
# WorkSchedule=F, PayPlan=GS, and Grade between 01 and 15
# OccupationCategory in {P, A, T, C, O}
# EducationLevel between 01 and 22
# AdjustedBasicPay > 10
# Top five agencies (left two positions) by observation frequency

# OPM codebook:  https://www.opm.gov/policy-data-oversight/data-analysis-documentation/data-policy-guidance/reporting-guidance/part-a-human-resources.pdf

# Columns:
# PseudoID ................... unique (OPM randomly assigned) employee ID
# FY ......................... U.S. federal government fiscal year
# Agency ..................... federal agency employed
# Grade ...................... general schedule (GS) grade
# OccupationalCategory ....... occupational category 
# Occupation ................. occupation
# Age ........................ employee age (five year increments, noised induced by OPM)
# EducationYears ............. years of education
# BasicPay ................... adjusted basic pay, in 2011 $U.S.

#######################################################################################################
# Read observations
#######################################################################################################

# Local
setwd("C:/Projects/Duke/Co-lab/Shiny-Fall-2019/Session-2-DataTables-Plots")

# RStudio Cloud
#setwd("/cloud/project/Duke-Co-lab/Shiny/Session-2-DataTables-Plots")

# Randomly select one of four employee subsets from the full Buzzfeed data set
fn <- paste("Data/CPDFSampleDataBuzzfeed-", sprintf("%02.0f", sample(1:4, 1)), ".csv.gz", sep="")
cpdf <- read.table(gzfile(fn), header=T, sep=",", strip.white=T)

# Omit invalid observations
cpdf <- subset(cpdf, !is.na(cpdf[,"Age"]))

# Convert occupation to four position alpha (for some reason, read.table from gz file converts this column to numeric)
cpdf[,"Occupation"] <- sprintf("%04.0f", cpdf[,"Occupation"])

gc()

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
typeof(ggTheme)

#######################################################################################################
# Aggregate one dependent numeric variable by category of other independent variables 
#######################################################################################################

# Specify dependent (numeric) variable
depVar <- c("Grade", "Age", "EducationYears", "BasicPay")[4]

# Specify independent aggregation variable(s)
indepVar <- c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation")[c(5, 7)]

# Compose subset indices for each joint category of independent var(s)
# These will be used to compute tabular statistics and give subsets for box plots
iagg <- aggregate(1:nrow(cpdf),
                  by=lapply(indepVar, function(v) cpdf[,v]),
                  function(k) k)
colnames(iagg) <- c(indepVar, "k")

# Aggregate mean and quartiles of dependent var for each independent var combination
# Creat a data frame from specified independent vars with aggregation stats appended in corresponding cols
aggdat <- data.frame(iagg[,indepVar],
            # rbind returned list elements into a single matrix
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


#######################################################################################################
# Aggregate a dependent numeric variable, allowing truncation of agency or occupation to two positions
#######################################################################################################

# Specify dependent (numeric) variable
depVar <- c("Grade", "Age", "EducationYears", "BasicPay")[1]

# Specify independent aggregation variable(s)
indepVar <- c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation")[c(5, 7)]

# Specify truncation for agency and/or occupation
agt2 <- T
occt2 <- T

# Compose subset indices for each joint category of independent var(s)
# These will be used to compute tabular statistics and give subsets for box plots
iagg <- aggregate(1:nrow(cpdf),
                  by=lapply(indepVar, function(v)
                                          if(v=="Agency" & agt2 | v=="Occupation" & occt2) {
                                            substring(cpdf[,v], 1, 2)
                                          } else {
                                            cpdf[,v]
                                          }),
                  function(k) k)
colnames(iagg) <- c(indepVar, "k")

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

#######################################################################################################
# Compose box plot using data subset corresponding to a specified aggregation table row
# Basic plot using default ggplot settings
#######################################################################################################

# Specify row of subset to plot
aggrow <- 13

# Retrieve subset indices
k <- iagg[aggrow,"k"][[1]]

# Specify plot independent var (should be tested for nonequality of aggregation independent vars)
gindepVar <- c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation")[7]

# Compose graph data set
# Coerse the x variable to a factor, otherwise it is treated as continuous and produces a plot with a single box
# Order occupational category in P, A, T, C, O sequence when specified
if(gindepVar!="OccupationalCategory") {
  gdat <- data.frame("x"=factor(cpdf[k,gindepVar]), "y"=cpdf[k,depVar])
} else {
  gdat <- data.frame("x"=factor(cpdf[k,gindepVar], levels=c("P", "A", "T", "C", "O")), "y"=cpdf[k,depVar])
}

# Initialize plot
g <- ggplot() +
  geom_boxplot(data=gdat, aes(x=x, y=y))

# Configure axis labels and apply theme
g <- g +  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  ggTheme +
  labs(x=paste("\n", gindepVar, sep=""), y=paste(depVar, "\n", sep=""))

print(g)

#######################################################################################################
# Compose box plot using data subset corresponding to a specified aggregation table row
# Include a panel variable
# Disable default display of geom_boxplot points
# Overlay all points with x-jitter
# Add point coloration by a specified categorical variable
# Adjust point size by independent var frequency
# Adjust point transparency (alpha)
#######################################################################################################

# Specify row of subset to plot
aggrow <- 1

# Retrieve subset indices
k <- iagg[aggrow,"k"][[1]]

# Specify plot independent var (should be tested for nonequality of aggregation independent vars)
gindepVar <- c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation")[4]

# Specify facet variable
# One facet panel for each level of the variable will be generated
# Specify NULL for no panels
#panelVar <- NULL
panelVar <- c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation")[1]
panelRows <- NULL
panelCols <- NULL

# Specify whether or not to display individual observation points
pointDisplay <- T

# Specify point color differentiation variable (NULL for no differentiation)
#diffVar <- NULL
diffVar <- c("FY", "Grade", "Age", "EducationYears", "Agency", "OccupationalCategory", "Occupation")[6]

# Specify point transparency
pointAlpha <- 0.25

# Compose graph data set
# Coerse x to a factor, otherwise it is treated as continuous and produces a plot with a single box
gdat <- data.frame(factor(cpdf[k,gindepVar]), cpdf[k,c(depVar, panelVar, diffVar)])
colnames(gdat)[1] <- gindepVar

# Order occupational category in P, A, T, C, O sequence when specified
if("OccupationalCategory" %in% colnames(gdat))
  gdat[,"OccupationalCategory"] <- factor(gdat[,"OccupationalCategory"], levels=c("P", "A", "T", "C", "O"))

# Initialize plot
g <- ggplot()

# Include points first, so that boxes overlay them
if(pointDisplay) {
  g <- g + geom_jitter(data=gdat, aes_string(x=gindepVar, y=depVar), width=0.2, alpha=pointAlpha)
  # Assign point color from diffVar
  if(!is.null(diffVar)) {
    # Create an additional mapping by appending an aes() for color
    # Note that jitter points are the last layer to have been added
    # Note, also, the required spelling of "colour" ("color" causes contention between two color scales during render)
    g[["layers"]][[length(g[["layers"]])]][["mapping"]][["colour"]] <- aes_string(color=diffVar)[[1]]
    g <- g + scale_color_manual(name=diffVar, values=colorRampPalette(c("blue", "red"))(length(unique(gdat[,diffVar]))))
  }
}

# Boxplot with error bars
g <- g + geom_boxplot(data=gdat, aes_string(x=gindepVar, y=depVar), fill=NA, outlier.shape=NA) +
stat_boxplot(data=gdat, aes_string(x=gindepVar, y=depVar), geom='errorbar', color="gray50", width=0.4)
  
# Facet, if requested
if(!is.null(panelVar))
  g <- g + facet_wrap(panelVar, nrow=panelRows, ncol=panelCols,
                      labeller=as_labeller(function(x) paste(panelVar, " = ", x, sep="")))

# Configure axis labels and apply theme
g <- g +  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  ggTheme +
  labs(x=paste("\n", gindepVar, sep=""), y=paste(depVar, "\n", sep=""))

# Render
print(g)

# Examine
str(g)

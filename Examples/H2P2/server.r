# Duke University H2P2 GWAS Project
# Web Portal - GWAS Phenotype Query

# service module

options(stringsAsFactors=F)
library(shiny)
library(DT)
library(RODBC)
library(ggplot2)
library(stringr)

# define server functions to support UI
shinyServer(

  function(input, output, session) {

    accessLevel <- "public"
    qdat <- data.frame("Phenotype"=NULL, "rsID"=NULL)
    qdatg <- data.frame()

    # parse url parameters
    # assign default query values if present
    observe({urlPar <- parseQueryString(session$clientData$url_search)
              if(!is.null(urlPar$phenotype))
                updateSelectInput(session, "phenotypeSelect", selected=urlPar$phenotype)
              if(!is.null(urlPar$rsID))
                updateTextInput(session, "rsIDSelect", value=urlPar$rsID)
              if(!is.null(urlPar$gene))
                updateTextInput(session, "geneSelect", value=urlPar$gene)
              if(!is.null(urlPar$pLowerBound))
                updateTextInput(session, "pBound0", value=urlPar$pLowerBound)
              if(!is.null(urlPar$eQTL))
                updateCheckboxInput(session, "eQTLSelect", value=urlPar$eQTL)
    })

    # compose SQL statement from user specified query components
    composeSQL <- function() {

      # select statement

      # evaluate record limit
      # validate numerical values
      # omit leading and trailing spaces
      nRec <- input$nRec
      while(substring(nRec, 1, 1)==" ")
        nRec <- substring(nRec, 2, nchar(nRec))
      n <- nchar(nRec)
      while(substring(nRec, n, n)==" ") {
        nRec <- substring(nRec, 1, n-1)
        n <- nchar(nRec)
      }
      if(nRec!="") {
        # validate numerical value
        if(length(grep("[^0123456789]", nRec))==0) {
          sqlText <- paste("select top ", nRec, sep="")
        } else {
          sqlText <- "Error:  Invalid records to return value"
        }
      } else {
        sqlText <- "select"
      }

      # append columns to select
      if(substring(sqlText, 1, 5)!="Error")
        sqlText <- paste(sqlText, " s.rsID, s.Chromosome, s.Position, s.Gene, s.SNPType, p.Phenotype,",
                                  " g.MajorAllele, g.MinorAllele, g.pValue, g.Beta,",
                                  " case when(eq.SNPID is not null)then 'yes' else '' end as eQTL\r\n",
                                  " from GWAS g join SNP s on g.SNPID=s.ID\r\n",
                                  "      join Phenotype p on g.PhenotypeID=p.ID\r\n",
                                  "      left join eQTL eq on s.ID=eq.SNPID\r\n", sep="")

      # restrict phenotypes by access level
      if(accessLevel!="phen+")
        sqlText <- paste(sqlText, " join PhenotypePublicUse ppub on p.ID=ppub.ID\r\n", sep="")

      # filter eQtl SNPs, if requested
      if(input$eQTLSelect)
        sqlText <- paste(sqlText, " join eqtl on s.ID=eqtl.SNPID and eqtl.Status=1\r\n")

      # where clause
      if(substring(sqlText, 1, 5)!="Error") {

        sqlText <- paste(sqlText, " where 1=1\r\n", sep="")

        # where phenotype
        if(tolower(input$phenotypeSelect)!="all")
          sqlText <- paste(sqlText, " and p.Phenotype='", input$phenotypeSelect, "'\r\n", sep="")

        # where rsID(s)
        # convert semicolon delimited list of rsIDs to in('rsID1', 'rsID2', ...)
        # omit spaces in rsID list
        rsID <- gsub(" ", "", input$rsIDSelect)
        if(rsID!="") {
          # convert multiple adjacent semicolons to single ones
          while(length(grep(";;", rsID))>0)
            rsID <- gsub(";;", ";", rsID)
          # omit leading semicolon
          if(substring(rsID, 1, 1)==";")
            rsID <- substring(rsID, 2, nchar(rsID))
          # omit trailing semicolon
          n <- nchar(rsID)
          if(substring(rsID, n, n)==";")
            rsID=substring(rsID, 1, n-1)
          # compose apostrophe, comma delimited list of rsIDs
          rsID <- paste("('", gsub(";", "', '", rsID), "')", sep="")
          # compare left and right (semicolon delimited) components of SNP rsID to rsID list
          sqlText <- paste(sqlText, " and (charindex(';', s.rsID)=0 and s.rsID in", rsID,
                                           " or charindex(';', s.rsID)>0",
                                           "    and (substring(s.rsID, 1, charindex(';', s.rsID)-1) in", rsID,
                                           "         or substring(s.rsID, charindex(';', s.rsID)+1, len(s.rsID)-charindex(';', s.rsID)) in", rsID, "))\r\n", sep="")
        }

        # where SNP type in('SNP1', 'SNP2', ...)
        snpType <- c(input$snpTypeSelect1, input$snpTypeSelect2)
        if(length(snpType)>0)
          if(snpType[1] != "All")
            sqlText <- paste(sqlText, " and s.SNPType in('", paste(snpType, collapse="', '"), "')\r\n", sep="")

        # where gene(s)
        # convert semicolon delimited list of genes to in('gene1', 'gene2', ...)
        # omit spaces in gene list
        gene <- gsub(" ", "", input$geneSelect)
        if(gene!="") {
          # convert multiple adjacent semicolons to single ones
          while(length(grep(";;", gene))>0)
            gene <- gsub(";;", ";", gene)
          # omit leading semicolon
          if(substring(gene, 1, 1)==";")
            gene <- substring(gene, 2, nchar(gene))
          # omit trailing semicolon
          n <- nchar(gene)
          if(substring(gene, n, n)==";")
            gene=substring(gene, 1, n-1)
          # surround by (' and '), convert semicolons to ','
          gene <- paste("('", gsub(";", "', '", gene), "')", sep="")
          # include where clause
          sqlText <- paste(sqlText, " and s.Gene in", gene, "\r\n", sep="")
        }

        # where SNP within chromosome and position neighborhood
        if(!is.na(input$snpRegionChromosome)) {
          sqlText <- paste(sqlText, " and s.Chromosome=", input$snpRegionChromosome, sep="")
          if(!is.na(input$snpRegionStartPosition))
            sqlText <- paste(sqlText, " and s.Position>=", input$snpRegionStartPosition, sep="")
          if(!is.na(input$snpRegionEndPosition))
            sqlText <- paste(sqlText, " and s.Position<=", input$snpRegionEndPosition, "\r\n", sep="")
        }

        # where study population
        #if(input$populationSelect!="All")
        #  if(input$populationSelect!="ESN+GWD") {
        #    sqlText <- paste(sqlText, " and study_population like '%", input$populationSelect, "%'", sep="")
        #  } else {
        #    sqlText <- paste(sqlText, " and study_population like '%ESN%' or study_population like '%GWD%'", sep="")
        #  }

        # where p-value bounds
        if(substring(sqlText, 1, 5)!="Error") {
          # include p-value constraint
          # validate numerical values
          # omit leading and trailing spaces
          # first, p0
          if(!is.na(input$pBound0)) {
            p <- input$pBound0
            while(substring(p, 1, 1)==" ")
              p <- substring(p, 2, nchar(p))
            n <- nchar(p)
            while(substring(p, n, n)==" ") {
              p <- substring(p, 1, n-1)
              n <- nchar(p)
            }
            if(p!="")
              # validate numerical value
              if(length(grep("[^.0123456789]", p))==0) {
                sqlText <- paste(sqlText, " and g.pValue between 0 and ", 10**-as.numeric(p), "\r\n", sep="")
              } else {
                sqlText <- "Error:  Invalid p lower bound"
              }
          }
          # now, p1
          if(substring(sqlText, 1, 5)!="Error") {
            if(!is.na(input$pBound0)) {
              p <- input$pBound1
              while(substring(p, 1, 1)==" ")
                p <- substring(p, 2, nchar(b))
              n <- nchar(p)
              while(substring(p, n, n)==" ") {
                p <- substring(p, 1, n-1)
                n <- nchar(p)
              }
              if(p!="")
                # validate numerical value
                if(length(grep("[^.0123456789]", p))==0) {
                  sqlText <- paste(sqlText, " and g.pValue>=", 10**-as.numeric(p), "\r\n", sep="")
                } else {
                  sqlText <- "Error:  Invalid p upper bound"
                }
            }
          }
        }

      }

      # order by clause
      #if(substring(sqlText, 1, 5)!="Error") {
      #  ob <- ""
      #  for(scol in c(input$sortCol1, input$sortCol2, input$sortCol3, input$sortCol4))
      #    if(scol!="")
      #      ob <- paste(ob, ifelse(ob=="", "", ", "),
      #        c("rsID"="rsID", "SNP Sub-type"="SNPType", "Phenotype"="Phenotype", "Gene"="Gene",
      #          "p-Value"="pValue", "Beta"="Beta")[scol], sep="")
      #  if(ob!="")
      #    sqlText <- paste(sqlText, " order by ", ob, sep="")
      #}
      sqlText <- paste(sqlText, " order by pValue", sep="")

      return(sqlText)

    }

    # execute composed SQL statement
    execSQL <- function(sqlText, resourceToLog="Unknown") {
      db <- odbcDriverConnect(connection="driver={ODBC Driver 13 for SQL Server}; server=SSI-KOLAB-PDB1; database=H2P2; uid=H2P2User; pwd=H2P2User", readOnlyOptimize=T)
      if(!is.null(sqlText))
        queryResults <- sqlQuery(db, sqlText, stringsAsFactors=F, as.is=T)
      if(!is.null(resourceToLog))
        sqlQuery(db, paste("insert into ResourceLog(Resource) values('", resourceToLog, "')", sep=""))
      odbcClose(db)
      if(!is.null(sqlText)) {
        return(queryResults)
      } else {
        return(NA)
      }
    }

    displayMessage <- function(msg, color="black")
      output$message <- renderText({HTML(paste("<font color=", color, ">", msg, "</font>", "<br><br>Time:  ", Sys.time(), "</font>", sep=""))})

    clearMessage <- function()
      output$message <- renderText(NULL)

    # enable/disable extended phenotype access
    observeEvent(
      input$accessReq, {
        if(input$accessText=="phen+") {
          accessLevel <<- input$accessText
          updateActionButton(session, "accessReq", label="+")
        } else {
          accessLevel <<- "public"
          updateActionButton(session, "accessReq", label="x")
        }
        updateTextInput(session, "accessText", value="")
        phenotypeList <- execSQL(paste("H2P2AppSource @style='PhenotypeList', @AccessLevel='", accessLevel, "'", sep=""), resourceToLog=NULL)$Phenotype
        updateSelectInput(session, "phenotypeSelect", choices=phenotypeList)
      }
    )

    # construct phenotype/SNP GWAS results table
    observeEvent(
      input$reviewPhenotypeQuery, {
        clearMessage()
        # compose SQL statement, execute query, and display results
        sqlText <- composeSQL()
        #cat(sqlText, file=stderr())
        if(tolower(substring(sqlText, 1, 5))!="error") {
          clearMessage()
          # retain queried rows for further processing when rows selected in client
          # note the global assignment, to be made avilable in selection functions
          qdat <<- execSQL(sqlText, resourceToLog="Query-PhenotypicAssociations01")
          if(is.data.frame(qdat)) {
            if(nrow(qdat)>0) {
              # split comma delimited genes and compose genecards.org urls for each 
              geneURL <- apply(as.matrix(qdat[,"Gene"]), 1,
                               function(x) paste("<a href=https://www.genecards.org/cgi-bin/carddisp.pl?gene=",
                                                 gsub(" ", "", str_split(x, ",", simplify=T)), " target=_blank>",
                                                 gsub(" ", "", str_split(x, ",", simplify=T)), "</a>", sep="",
                                                 collapse="&nbsp&nbsp"))
              # Compose table of results with urls to genecards, GTE, and GGV
              output$SNPPhenotypeResults <-
                DT::renderDataTable(
                  datatable(data.frame(qdat[,c("SNPType", "rsID")],
                                       "Gene"=geneURL,
                                       "Phenotype"=qdat[,"Phenotype"],
                                       "p"=ifelse(qdat[,"pValue"]>0, sprintf("%8.4f", -log(qdat[,"pValue"])/log(10)), "-"),
                                       "Beta"=sprintf("%10.4f", qdat[,"Beta"]),
                                       "eQTL"=qdat[,"eQTL"],
                                       "ExtLink"=paste("<a href=https://www.gtexportal.org/home/snp/", qdat[,"rsID"],
                                                       " target=_blank>GTEx</a> &nbsp&nbsp&nbsp ",
                                                       "<a href=http://popgen.uchicago.edu/ggv/?data=1000genomes&chr=",
                                                       qdat[,"Chromosome"], "&pos=", qdat[,"Position"], " target=_blank>",
                                                       "Geography of Genetic Variation (GGV)</a>", sep="")),
                            rownames=F, escape=F, selection="single",
                            colnames=c("SNPType", "rsID", "Gene", "Phenotype", HTML("-log<sub>10</sub>(p)"), "Beta", "eQTL", "Ext. Links"),
                            options=list(pageLength=100, bLengthChange=F, bFilter=F, #autoWidth=F,
                                         columnDefs=list(list(className="dt-right", targets=4:5),
                                                         list(className="dt-center", targets=6)),
                                                         #list(width='10px', targets=1)),
                            order=list(list(4, "desc"))) # cols are 0-based
                           )
                )
            } else {
              output$SNPPhenotypeResults <- DT::renderDataTable(NULL)
              displayMessage("No data to display.  No observations satisfy query criteria.", "black")
            }
          } else {
            output$SNPPhenotypeResults <- DT::renderDataTable(NULL)
            displayMessage(qdat[2], "red")
          }
          output$LCLPhenotypGenotypeDetail <- DT::renderDataTable(NULL)
          output$genotypeDistribution <- renderPlot(NULL)
          # move focus to SNP Details tab
          updateTabsetPanel(session, "phenotypeTabs", selected="Results: SNP Details")
        } else {
          displayMessage(sqlText, "red")
          output$SNPPhenotypeResults <- DT::renderDataTable(NULL)
          output$LCLPhenotypGenotypeDetail <- DT::renderDataTable(NULL)
          output$genotypeDistribution <- renderPlot(NULL)
        }
      }
    )

    # save phenotype/SNP GWAS results
    output$savePhenotypeQuery <-
      downloadHandler(filename=function() "H2P2PhenotypicAssociationQueryResults.csv", contentType="text/csv",
                      content=function(con) {
                                clearMessage()
                                sqlText <- composeSQL()
                                if(tolower(substring(sqlText, 1, 5))!="error") {
                                  write.table(execSQL(sqlText, resourceToLog="Query-PhenotypicAssociationsDownload"), con, row.names=F, col.names=T, quote=F, sep=", ")
                                } else {
                                  displayMessage(sqlText, "red")
                                  write.table(sqlText, con, row.names=F, col.names=F, quote=F)
                                }
                              })

    # query LCL phenotypic and genotype results for specified phenotype and SNP
    # results are stored in global qdatg data frame
    queryPhenotypeRSID <- function(phenotype, rsID) {
      qdatg <<- execSQL(paste("H2P2AppSource @style='PhenotypicAssocSNPDetail', @AccessLevel='public', ",
                              "              @Phenotype='", phenotype, "', @rsID='", rsID, "', @Population='All'", sep=""),
                        resourceToLog="Query-PhenotypicAssociations02")
    }

    # populate phenotypic response detail tab for specified phenotype and rsID
    # it is assumed that associated observations have been queried and exist in qdatg
    constructLCLPhenotypeGenotypeTable <- function(phenotype, rsID) {
      if(nrow(qdatg)>0)
        # populate LCLdetail table
        output$LCLPhenotypGenotypeDetail <-
          DT::renderDataTable(data.frame(qdatg[, c("LCLID", "Chromosome", "Position", "Genotype")],
                                         "PhenotypicMean"=round(qdatg[,"PhenotypicMean"], 4)), selection="none",
                              options=list(pageLength=100, bLengthChange=F, bFilter=F,
                                           columnDefs=list(list(className="dt-right", targets=c(1, 2, 4)))) # cols are 0-based
                             )
    }

    # construct box plot of phenotypic mean by genotype for specified phenotype and rsID
    # it is assumed that associated observations have been queried and exist in qdatg
    constructLCLPhenotypeGenotypeBoxPlot <- function(phenotype, rsID) {
      clearMessage()
      # filter by selected population, if any
      if(nrow(qdatg)>0) {
        if(length(input$genotypeDistributionPopulationSelect)>0) {
          if(input$genotypeDistributionPopulationSelect[1]=="All") {
            k <- 1:nrow(qdatg)
          } else {
            k <- which(qdatg[,"Population"] %in% input$genotypeDistributionPopulationSelect)
          }
        } else {
          k <- vector()
        }
      } else {
        k <- vector()
      }
      # generate plot if associated observations exist
      if(length(k)>0) {
        # create working copy of data set
        qdatgw <- qdatg
        # convert phenotypic response to log-2, if requested
        if(input$genotypeDistributionLog2Transform)
          qdatgw[,"PhenotypicMean"] <- log(qdatg[,"PhenotypicMean"])/log(2)
        # get measurements for axis specification
        mp <- max(qdatgw[k,"PhenotypicMean"], na.rm=T)
        dp <- mp-min(qdatgw[k,"PhenotypicMean"], na.rm=T)
        # compute y axis top pos and coordinate for display of n and median
        yAxisTop <- mp+0.15*dp
        # configure decimal positions to display
        decPos <- ifelse(abs(mp)>100, 1, ifelse(abs(mp)>10, 2, ifelse(abs(mp)>1, 3, 4)))
        # generate single panel plot, if requested, otherwise panel by population
        if(!input$genotypeDistributionPopulationPanel) {
          # count obervations and compute median by genotype
          # aggregate returns function results in a list, so parse and separate n and median columns from 2nd returned col
          ngt <- aggregate(qdatgw[k,"PhenotypicMean"], by=list(qdatgw[k,"Genotype"]),
                           function(x) c(length(which(!is.na(x))), median(x, 0.5, na.rm=T)), simplify=T)
          ngt <- data.frame(ngt[,1], as.character(ngt[,2][,1]), sprintf(paste("%0.0", decPos, "f", sep=""), ngt[,2][,2]))
          colnames(ngt) <- c("Genotype", "n", "q50")
          # prepend labels to first entry
          ngt[1,"n"] <- paste("n = ", ngt[1,"n"], sep="")
          ngt[1,"q50"] <- paste("med = ", ngt[1,"q50"], sep="")
          output$genotypeDistribution <- renderPlot(ggplot() +
                                                      geom_boxplot(data=qdatgw[k,], aes(x=Genotype, y=PhenotypicMean), outlier.shape=NA, fill="#99ccff") +
                                                      geom_jitter(data=qdatgw[k,], aes(x=Genotype, y=PhenotypicMean), width=0.1, fill="blue3", alpha=0.25) +
                                                      stat_boxplot(data=qdatgw[k,], aes(x=Genotype, y=PhenotypicMean), geom='errorbar', color="gray50", width=0.4) +
                                                      # display first genotype n and median with "n =" and "med =" prefixes
                                                      # subsequent genotypes without prefix
                                                      geom_text(data=ngt, aes(x=Genotype, y=yAxisTop*0.99, label=paste(n, "\n", q50, sep=""))) +
                                                      scale_y_continuous(limits=c(NA, yAxisTop), labels=function(y) format(y, big.mark=',', scientific=F)) +
                                                      theme(panel.background=element_blank(),
                                                            panel.grid.major.x=element_blank(),
                                                            panel.grid.major.y=element_blank(),
                                                            panel.grid.minor=element_blank(),
                                                            panel.border=element_rect(fill=NA),
                                                            plot.title=element_text(size=12, face="bold", hjust=0.5),
                                                            plot.background=element_rect(linetype="solid", color="gray75", size=1),
                                                            plot.margin=unit(c(0.5, 1.2, 0.5, 0.5), "in"), 
                                                            axis.title.x=element_text(size=10),
                                                            axis.title.y=element_text(size=10)) +
                                                      labs(title=paste("\nPhenotype:  ", phenotype, "\n\nSNP:  ", rsID, "\n\n", sep=""),
                                                           x="\n\ngenotype", y="phenotypic response\n\n"))
        } else {
          # count obervations and compute median by population and genotype
          # aggregate returns function results in a list, so parse and separate n and median columns from 3rd returned col
          # convert n to char to enable prefix label for first entry
          # convert median to decimal formatted string to enable prefix
          ngt <- aggregate(qdatgw[k,"PhenotypicMean"], by=list(qdatgw[k,"Population"], qdatgw[k,"Genotype"]),
                           function(x) c(length(which(!is.na(x))), median(x, 0.5, na.rm=T)), simplify=T)
          ngt <- data.frame(ngt[,1:2], as.character(ngt[,3][,1]), sprintf(paste("%0.0", decPos, "f", sep=""), ngt[,3][,2]))
          colnames(ngt) <- c("Population", "Genotype", "n", "q50")
          # prepend labels to first entry
          ngt[1,"n"] <- paste("n = ", ngt[1,"n"], sep="")
          ngt[1,"q50"] <- paste("med = ", ngt[1,"q50"], sep="")
          output$genotypeDistribution <- renderPlot(ggplot() +
                                                      geom_boxplot(data=qdatgw[k,], aes(x=Genotype, y=PhenotypicMean), outlier.shape=NA, fill="#99ccff") +
                                                      geom_jitter(data=qdatgw[k,], aes(x=Genotype, y=PhenotypicMean), width=0.1, fill="blue3", alpha=0.25) +
                                                      stat_boxplot(data=qdatgw[k,], aes(x=Genotype, y=PhenotypicMean), geom='errorbar', color="gray50", width=0.4) +
                                                      # display first genotype n and median with n = and med = prefix
                                                      # subsequent genotypes without prefix
                                                      geom_text(data=ngt, aes(x=Genotype, y=yAxisTop*0.95, label=paste(n, "\n", q50, sep="")), size=3) +
                                                      scale_y_continuous(limits=c(NA, yAxisTop), labels=function(y) format(y, big.mark=',', scientific=F)) +
                                                      facet_wrap(~Population) +
                                                      theme(panel.background=element_blank(),
                                                           panel.grid.major.y=element_blank(),
                                                            panel.grid.minor=element_blank(),
                                                            panel.border=element_rect(fill=NA),
                                                            panel.spacing=unit(0, "in"),
                                                            plot.title=element_text(size=12, face="bold", hjust=0.5),
                                                            plot.background=element_rect(linetype="solid", color="gray75", size=1),
                                                            plot.margin=unit(c(0.5, 1.2, 0.5, 0.5), "in"), 
                                                            axis.title.x=element_text(size=10),
                                                            axis.title.y=element_text(size=10)) +
                                                      labs(title=paste("\nPhenotype:  ", phenotype, "\n\nSNP:  ", rsID, "\n\n", sep=""),
                                                           x="\n\ngenotype", y="phenotypic response\n\n"))
        }
      } else {
        displayMessage("No data to display", "black")
        output$genotypeDistribution <- renderPlot(NULL)
      }
    }

    # action event for results table row selection
    # query LCL phenotypic response and genotype data
    # construct table and box plot of LCL results
    observeEvent(
      input$SNPPhenotypeResults_rows_selected, {
        k <- input$SNPPhenotypeResults_rows_selected
        queryPhenotypeRSID(qdat[k,"Phenotype"], qdat[k,"rsID"])
        constructLCLPhenotypeGenotypeTable(qdat[k,"Phenotype"], qdat[k,"rsID"])
        constructLCLPhenotypeGenotypeBoxPlot(qdat[k,"Phenotype"], qdat[k,"rsID"])
        # set phenotype X genotype plot tab as current
        updateTabsetPanel(session, "phenotypeTabs", selected="PhGnPlot")
      }
    )

    # action event for phenotype/genotype box plot population selection
    # reconstruct box plot of LCL phenotypic response and genotype distribution
    # limit to specified population(s)
    observeEvent(
      input$genotypeDistributionPopulationSelect, {
        k <- input$SNPPhenotypeResults_rows_selected
        constructLCLPhenotypeGenotypeBoxPlot(qdat[k,"Phenotype"], qdat[k,"rsID"])
      }
    )

    # action event for phenotype/genotype box plot panel by population selection
    # reconstruct box plot of LCL phenotypic response and genotype distribution
    # panel by population, if requested
    observeEvent(
      input$genotypeDistributionPopulationPanel, {
        k <- input$SNPPhenotypeResults_rows_selected
        constructLCLPhenotypeGenotypeBoxPlot(qdat[k,"Phenotype"], qdat[k,"rsID"])
      }
    )
 
    # action event for phenotype/genotype box plot phenotypic mean log-2 transformation selection
    # reconstruct box plot of LCL phenotypic response and genotype distribution
    # panel by population, if requested
    observeEvent(
      input$genotypeDistributionLog2Transform, {
        k <- input$SNPPhenotypeResults_rows_selected
        constructLCLPhenotypeGenotypeBoxPlot(qdat[k,"Phenotype"], qdat[k,"rsID"])
      }
    )

    # action event to save individual LCL phenotype/SNP GWAS results
    # note that the form that presents the saveLCLResults button is generated by selecting a row on the
    # SNPPhenotypeResults table, which executes a query of individual LCL response for the currently
    # selected phenotype and rsID corresponding to the SNPPhenotypeResults row selected
    # query results are available in the global qdatg data frame
    output$saveLCLResults <-
      downloadHandler(filename=function() "H2P2PhenotypicAssociationLCLQueryResults.csv", contentType="text/csv",
                      content=function(con) {
                                execSQL(NULL, resourceToLog="Query-PhenotypicAssociationsLCLDownload")
                                write.table(qdatg, con, row.names=F, col.names=T, quote=F, sep=", ")
                              })

  }
)

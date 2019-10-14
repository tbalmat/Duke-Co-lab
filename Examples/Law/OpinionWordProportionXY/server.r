# Duke University Law Appeal Text Analysis

# Shiny service module

options(stringsAsFactors=F)
library(shiny)
library(RODBC)
library(XML)
library(ggplot2)
library(igraph)
library(ggraph)
library(grid)
library(ggrepel)

shinyServer(

  function(input, output, session) {

    # Function:  execute query
    execQuery <- function(sql) {
      library(RODBC)
      db <- odbcDriverConnect(connection="driver={SQL Server}; server=DESKTOP-BTIITJP\\SQLEXPRESS; database=LexisNexis; trusted_connection=true", readOnlyOptimize=T)
      d <- sqlQuery(db, sql, stringsAsFactors=F, as.is=T)
      odbcClose(db)
      return(d)
    }

    # Function:  retrieve opinion text for specified case class and opinion type
    opWords <- function(caseClass, opType, omitWords) {

      opText <- execQuery(paste(" select od.opinionText",
                                " from   CaseHeader ch join OpinionHeader oh on ch.ID=oh.CaseID",
                                "        join OpinionDetail od on oh.CaseID=od.CaseID",
                                "        and oh.OpinionID=od.OpinionID",
                                " where  1=1",
                                ifelse(caseClass!="", paste(" and ch.Class='", caseClass, "'", sep=""), ""),
                                ifelse(opType!="", paste(" and oh.Type='", opType, "'", sep=""), ""), sep=""))[,1]
        # Omit punctuation, special symbols, numerals, pronouns, and words of length < three
        w <- unlist(lapply(unlist(strsplit(tolower(opText), " ")), function(a)  gsub("[^a-z]", "", a)))
        k <- which(nchar(w)>2 & !w %in% omitWords)
        return(w[k])

    }

    # Function:  plot word proportions of one class against another 
    plotCaseClass <- function(savePng=F) {

      output$msg <- renderText("")

      qclass <- c(input$caseClass1, input$caseClass2)

      # Aggregate proportions for both classes
      w <- opWords(caseClass=qclass[1], opType="", omitWords=c(pronouns, omitWords))
      wag <- aggregate(rep(1, length(w)), by=list(w), sum)
      w1 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))
      w <- opWords(caseClass=qclass[2], opType="", omitWords=c(pronouns, omitWords))
      wag <- aggregate(rep(1, length(w)), by=list(w), sum)
      w2 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))

      # Join by word
      w <- merge(w1, w2, by.x="term", by.y="term", all.x=T, all.y=T)
      colnames(w) <- c("term", "p1", "p2")

      # Convert NA to 0
      w[which(is.na(w[,"p1"])),"p1"] <- 0
      w[which(is.na(w[,"p2"])),"p2"] <- 0

      # Filter by p
      prange <- input$pRange
      k <- which(w[,"p1"]>=prange[1] & w[,"p1"]<=prange[2] & w[,"p2"]>=prange[1] & w[,"p2"]<=prange[2])

      g <- ggplot() +
        geom_abline(intercept=min(min(w[k,"p1"], w[k,"p2"])), slope=1, color="gray75") +
        geom_point(data=w[k,], aes(x=p1, y=p2), color="blue3", alpha=0.35) +
        geom_text_repel(data=w[k,], aes(label=term, x=p1, y=p2), size=3, segment.size=0.25, segment.color="gray50") +
        theme(plot.title=element_text(size=12, hjust=0.5),
              plot.subtitle=element_text(size=10, hjust=0.5),
              plot.caption=element_text(size=12, hjust=0.5),
              panel.background=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_rect(fill=NA, color="gray75"),
              panel.spacing.x=unit(0, "lines"),
              axis.title.x=element_text(size=10),
              axis.title.y=element_text(size=10),
              axis.text.x=element_text(size=8),
              axis.text.y=element_text(size=8),
              strip.text=element_text(size=8),
              strip.background=element_blank(),
              legend.position="bottom",
              legend.background=element_rect(color="gray"),
              legend.key=element_rect(fill="white"),
              legend.box="horizontal",
              legend.text=element_text(size=8),
              legend.title=element_text(size=8)) +
        labs(title="Lexis Nexis Case Opinion Samples (212 cases)",
             subtitle=paste("Pairwise Word Proportions for Case Classes ", names(caseClass)[which(caseClass==qclass[2])],
                            " and ", names(caseClass)[which(caseClass==qclass[1])], "\n", sep=""),
             x=paste("\n", names(caseClass)[which(caseClass==qclass[1])], sep=""),
             y=paste(names(caseClass)[which(caseClass==qclass[2])], "\n", sep=""))

        # Render
        if(!savePng) {
          output$wordProportionXY <- renderPlot(g)
        } else {
          png(paste(input$graphDir, "\\WordDistribution-", qclass[2], "-by-", qclass[1], "-p-", prange[1], "-", prange[2],
                    ".png", sep=""), res=300, height=2400, width=2400)
          print(g)
          dev.off()
        }

    }

    # Function:  plot word proportions of one class against another 
    plotOpType <- function(savePng=F) {

      output$msg <- renderText("")

      qop <- c(input$opType1, input$opType2)

      # Aggregate proportions for both classes
      w <- opWords(caseClass="", opType=qop[1], omitWords=c(pronouns, omitWords))
      wag <- aggregate(rep(1, length(w)), by=list(w), sum)
      w1 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))
      w <- opWords(caseClass="", opType=qop[2], omitWords=c(pronouns, omitWords))
      wag <- aggregate(rep(1, length(w)), by=list(w), sum)
      w2 <- data.frame("term"=wag[,1], "p"=wag[,2]/sum(wag[,2]))

      # Join by word
      w <- merge(w1, w2, by.x="term", by.y="term", all.x=T, all.y=T)
      colnames(w) <- c("term", "p1", "p2")

      # Convert NA to 0
      w[which(is.na(w[,"p1"])),"p1"] <- 0
      w[which(is.na(w[,"p2"])),"p2"] <- 0

      # Filter by p
      prange <- input$pRange
      k <- which(w[,"p1"]>=prange[1] & w[,"p1"]<=prange[2] & w[,"p2"]>=prange[1] & w[,"p2"]<=prange[2])

      g <- ggplot() +
        geom_abline(intercept=min(min(w[k,"p1"], w[k,"p2"])), slope=1, color="gray75") +
        geom_point(data=w[k,], aes(x=p1, y=p2), color="blue3", alpha=0.35) +
        geom_text_repel(data=w[k,], aes(label=term, x=p1, y=p2), size=3, segment.size=0.25, segment.color="gray50") +
        theme(plot.title=element_text(size=12, hjust=0.5),
              plot.subtitle=element_text(size=10, hjust=0.5),
              plot.caption=element_text(size=12, hjust=0.5),
              panel.background=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_rect(fill=NA, color="gray75"),
              panel.spacing.x=unit(0, "lines"),
              axis.title.x=element_text(size=10),
              axis.title.y=element_text(size=10),
              axis.text.x=element_text(size=8),
              axis.text.y=element_text(size=8),
              strip.text=element_text(size=8),
              strip.background=element_blank(),
              legend.position="bottom",
              legend.background=element_rect(color="gray"),
              legend.key=element_rect(fill="white"),
              legend.box="horizontal",
              legend.text=element_text(size=8),
              legend.title=element_text(size=8)) +
        labs(title="Lexis Nexis Case Opinion Samples (212 cases)",
             subtitle=paste("Pairwise Word Proportions for Opinion Types ", names(opType)[which(opType==qop[2])],
                            " and ", names(opType)[which(opType==qop[1])], "\n", sep=""),
             x=paste("\n", names(opType)[which(opType==qop[1])], sep=""),
             y=paste(names(opType)[which(opType==qop[2])], "\n", sep=""))

        # Render
        if(!savePng) {
          output$wordProportionXY <- renderPlot(g)
        } else {
          png(paste(input$graphDir, "\\WordDistribution-", qop[2], "-by-", qop[1], "-p-", prange[1], "-", prange[2],
                    ".png", sep=""), res=300, height=2400, width=2400)
          print(g)
          dev.off()
        }

    }

    # Reactive environment events
    observeEvent(
      input$actionViewPlot1, {
        plotCaseClass(savePng=F)
      }
    )
    observeEvent(
      input$actionSavePlot1, {
        plotCaseClass(savePng=T)
      }
    )
    observeEvent(
      input$actionViewPlot2, {
        plotOpType(savePng=F)
      }
    )
    observeEvent(
      input$actionSavePlot2, {
        plotOpType(savePng=T)
      }
    )

    # Execution begins here

    # Construct case class and opinion type options
    q <- execQuery("select Class, Description from CaseClass order by Class")
    caseClass <- setNames(q[,1], q[,2])
    q <- execQuery("select Type, Description from OpinionType order by Type")
    opType <- setNames(q[,1], q[,2])
    updateSelectInput(session, "caseClass1", choices=caseClass, selected=caseClass[1])
    updateSelectInput(session, "caseClass2", choices=caseClass, selected=caseClass[2])
    updateSelectInput(session, "opType1", choices=opType, selected=opType[1])
    updateSelectInput(session, "opType2", choices=opType, selected=opType[2])

    # Construct list of key words, either to be omitted from or identified in text
    pronouns <- tolower(scan("..\\..\\Pronouns.csv", what="character"))
    omitWords <- tolower(scan("..\\..\\OmitWords.csv", what="character"))
    latinLegal <- tolower(xmlToDataFrame("..\\..\\LatinLegalTerms.xml")[,1])

  }
)

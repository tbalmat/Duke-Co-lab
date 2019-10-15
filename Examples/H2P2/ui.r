# Duke University H2P2 GWAS Project
# Web Portal - Phenotypic Associations Page

# Shiny user interface module

options(stringsAsFactors=F)
library(RODBC)
library(shiny)
library(DT)

# create lists and other static data referenced below
# it has been verified that, although outside of the shinyUI function, objects created here
# do not appear in the scope of other processes, despite the documentation indicating global creation
db <- odbcDriverConnect(connection="driver={ODBC Driver 13 for SQL Server}; server=SSI-KOLAB-PDB1; database=H2P2; uid=H2P2User; pwd=H2P2User", readOnlyOptimize=T)
phenotypeList <- sqlQuery(db, "H2P2AppSource @style='PhenotypeList', @AccessLevel='public'")$Phenotype
odbcClose(db)

shinyUI(
  fluidPage(

    fluidRow(width=12,

      # Browser tab icon
      tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),

      # logo and announcements
      HTML("<div align=center style=\"background-image:url('pattern-blue.gif'); padding-bottom:10px\">
              <a href=http://h2p2.oit.duke.edu>
              <img src=H2P2Logo3.png width=320px height=110px style=padding:15px>
              </a>
              <br>
              <font size=2 color=white>
              <marquee height=20px behavior=slide direction=up scrollamount=2 scrolldelay=50>
                <center>Welcome to the web portal for H2P2 and related projects connecting genetics, cell biology, and human disease.</center>
              </marquee>
              <br>
              <marquee height=20px behavior=slide direction=up scrollamount=2 scrolldelay=75>
                <center>Click below to explore summary data or to query your favorite SNP, gene, or phenotype!
                  Contact us with any questions/concerns.</center>
              </marquee>
              </font>
              <br>
              <font size=2 color=white>
              <marquee height=20px behavior=slide direction=up scrollamount=2 scrolldelay=100>
                <center>
                  <a href=https://www.cell.com/cell-host-microbe/fulltext/S1931-3128(18)30377-9 target='_blank' style='text-decoration:none'>
                    <font color=#99ceff>Read the H2P2 paper at <b>Cell Host &amp; Microbe</b></font></a>
                </center>
              </marquee>
              </font>
            </div>"),

        # links (menu)
        HTML("<div align=center style='padding-top:10px; padding-bottom:10px; background-color:#eeeeee'>
                <a href=http://h2p2.oit.duke.edu/H2P2/H2P2Home>Home</a> &nbsp;&nbsp;&nbsp;
                <a href=http://h2p2.oit.duke.edu/H2P2/About>About</a> &nbsp;&nbsp;&nbsp;
                <a href=http://h2p2.oit.duke.edu/H2P2/QuickStart>Quick-start-guide</a> &nbsp;&nbsp;&nbsp;
                <a href=http://h2p2.oit.duke.edu/H2P2/Contact>Contact</a>
              </div>")

    ),

    HTML("<br><br>"),

    HTML("<center><font color=steelblue size=3><b>Phenotypic Associations</b></font><br><br></center>"),

    tabsetPanel(id="phenotypeTabs",

      # prompts
      tabPanel("Query/Filter",
        HTML("<br><font color=steelblue><b>1. Specify phenotype, SNP, gene, and/or position</b></font><hr style='margin-top:5px; margin-bottom:15px'>"),
        selectInput("phenotypeSelect", "Phenotype", phenotypeList, width="45%"),
        textInput("rsIDSelect", "rsID (list of semicolon separated IDs, empty = All)", width="100%"),
        textInput("geneSelect", "Gene (list of semicolon separated IDs, empty = All)", width="100%"),
        fluidRow(
          column(width=3, HTML("<br><b>Genomic Location (hg19):</b>")),
          column(width=2, numericInput(inputId="snpRegionChromosome", label="Chromosome", value=NA, min=1, max=22)),
          column(width=2, numericInput(inputId="snpRegionStartPosition", label="Start Position", value=NA, min=0)),
          column(width=2, numericInput(inputId="snpRegionEndPosition", label="End Position", value=NA, min=0))
        ),
        HTML("<br><font color=steelblue><b>2. Filter</b></font><hr style='margin-top:5px; margin-bottom:15px'>"),
        fluidRow(
          column(width=3,
            checkboxGroupInput("snpTypeSelect1", "SNP Sub-type",
              choices=c("All"="All",
                        "downstream"="downstream",
                        "exonic"="exonic",
                        "exonic;splicing"="exonic;splicing",
                        "intergenic"="intergenic",
                        "intronic"="intronic",
                        "ncRNA_exonic"="ncRNA_exonic",
                        "ncRNA_exonic;splicing"="ncRNA_exonic;splicing"),
              inline=F, selected="All", width="100%")
          ),
          column(width=3,
            checkboxGroupInput("snpTypeSelect2", HTML("&nbsp;"),
              choices=c("ncRNA_intronic"="ncRNA_intronic",
                        "ncRNA_splicing"="ncRNA_splicing",
                        "splicing"="splicing",
                        "upstream"="upstream",
                        "upstream;downstream"="upstream;downstream",
                        "UTR3"="UTR3",
                        "UTR5"="UTR5",
                        "UTR3;UTR5"="UTR3;UTR5"),
              inline=F, width="100%")
          ),
          column(width=3,
            fluidRow(HTML("&nbsp;")),
            checkboxInput("eQTLSelect", "eQTL")
          )
        ),
        #radioButtons("populationSelect", "Study Population",
        #             choices=c("All"="All", "IBS"="IBS", "ESN"="ESN", "GWD"="GWD", "ESN+GWD"="ESN+GWD", "KHV"="KHV"),
        #                       inline=T,  width="100%"),
        #HTML("<br>"),
        fluidRow(
          #column(width=3, numericInput("pBound0", HTML("-log<sub>10</sub>(p) Lower Bound"), 3.0)), #, placeholder="none")),
          #column(width=3, numericInput("pBound1", HTML("-log<sub>10</sub>(p) Upper Bound"), NULL)) #, placeholder="none"))
          column(width=3, textInput("pBound0", HTML("-log<sub>10</sub>(p) Lower Bound"), 3.0, placeholder="none")),
          column(width=3, textInput("pBound1", HTML("-log<sub>10</sub>(p) Upper Bound"), placeholder="none"))
        ),
        fluidRow(
          column(width=12,
            textInput("nRec", "Records to return", 1000, width="12%")
          )
        ),
        #fluidRow(
        #  column(width=3, selectInput("sortCol1", "Sort by 1", choices=c("", "rsID", "SNP Sub-type", "Phenotype", "Gene", "p-Value", "Beta"))),
        #  column(width=3, selectInput("sortCol2", "Sort by 2", choices=c("", "rsID", "SNP Sub-type", "Phenotype", "Gene", "p-Value", "Beta"))),
        #  column(width=3, selectInput("sortCol3", "Sort by 3", choices=c("", "rsID", "SNP Sub-type", "Phenotype", "Gene", "p-Value", "Beta"))),
        #  column(width=3, selectInput("sortCol4", "Sort by 4", choices=c("", "rsID", "SNP Sub-type", "Phenotype", "Gene", "p-Value", "Beta")))
        #),
        HTML("<br><font color=steelblue><b>3. Review</b></font><hr style='margin-top:5px; margin-bottom:15px'>"),
        fluidRow(width=12,
          column(width=10,
            actionButton("reviewPhenotypeQuery", "Query and Review", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"),
            HTML("&nbsp;&nbsp;"),
            downloadButton("savePhenotypeQuery", "Query and Save", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)")
          ),
          column(width=1,
            HTML("<div align=right>"),
            textInput("accessText", NULL, width="150px"),
            HTML("</div>")
          ),
          column(width=1,
            actionButton("accessReq", "x", style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)")
          )
        ),
        HTML("<br>")
      ),

      # results

      tabPanel("Results: SNP Details",
        div(style="font-size:85%",
          HTML("<br>"),
          htmlOutput("message"),
          HTML("<br>"),
          DT::dataTableOutput("SNPPhenotypeResults", width="1400px")
        )
      ),
      tabPanel(title="Results: Phenotype X Genotype Plot", value="PhGnPlot",
        div(style="font-size:85%",
          HTML("<br>"),
          fluidRow(width=12,
            column(width=5, div(align="right", HTML("<br>Include Populations:"))),
            column(width=7, checkboxGroupInput("genotypeDistributionPopulationSelect", "",
              choices=c("All"="All", "ESN"="ESN", "IBS"="IBS", "GWD"="GWD", "KHV"="KHV"),
              selected="All", inline=T))
          ),
          fluidRow(width=12,
            column(width=5, ""),
            column(width=7, checkboxInput("genotypeDistributionPopulationPanel", "Panel by Population", value=F))
          ),
          fluidRow(width=12,
            column(width=5, ""),
            column(width=7, checkboxInput("genotypeDistributionLog2Transform", "log-2 Transform", value=F))
          ),
          HTML("<br><center>"),
          plotOutput("genotypeDistribution", width="550px", height="550px"),
          HTML("</center>")
        )
      ),
      tabPanel(title="Results: Individual LCL Data",
        div(style="font-size:85%",
          HTML("<br>"),
          fluidRow(width=12,
            column(width=6, DT::dataTableOutput("LCLPhenotypGenotypeDetail", width="600px")),
            column(width=1, downloadButton("saveLCLResults", "Download LCL Results",
                   style="color:white; background:linear-gradient(#54b4eb, #2fa4e7 60%, #0088dd)"))
          )
        )
      )
    ),

    HTML("<br>"),

    # footer - fluid row spans entire width of page
    fluidRow(width=12,
      HTML("

        <div align=center style='padding-top:10px; padding-bottom:2px; background-color:#eeeeee'>
          <table width=90% style='padding-left:10%; padding-right:10%'><tr>
            <td width=40%>
              <a href='https://medschool.duke.edu/' title='Duke School of Medicine' alt='Duke School of Medicine' target='_blank'>
              <img src='duke-som-logo.png' height=20px/></a>
            </td>
            <td width=40% style='text-align:left; padding-bottom:5px'>
              <a href='https://medschool.duke.edu/about-us' target='_blank' style='color:#0060a0;padding-right:10px; text-decoration:none'>About Us</a>
              <a href='https://medschool.duke.edu/education' target='_blank' style='color:#0060a0; padding-right:10px; text-decoration:none'>Education</a>
              <a href='https://medschool.duke.edu/research' target='_blank' style='color:#0060a0; padding-right:10px; text-decoration:none'>Research</a>
              <a href='https://medschool.duke.edu/about-us/contact-us' target='_blank' style='color:#0060a0; text-decoration:none'>Contact Us</a>
            </td>
            <td width=20%>
              <span style='float:right; padding-bottom:5px; padding-right:10%'>
                <a href='https://www.youtube.com/user/DukeMedSchool' title='Duke Med @ YouTube' alt='Duke Med @ YouTube' target='_blank'>
                <img src='YouTube.png' height=30px style='vertical-align:middle; padding-right:10px'/></a>
                <a href='https://www.twitter.com/dukemedschool' title='Duke Med @ Twitter' alt='Duke Med @ Twitter' target='_blank'>
                <img src='Twitter.png' height=30px style='vertical-align:middle; padding-right:10px'/></a>
                <a href='https://medschool.duke.edu/news-rss.xml' title='Duke Med @ RSS' alt='Duke Med @ RSS' target='_blank'>
                <img src='RSS.png' height=30px style='vertical-align:middle'/></a>
              </span>
            </td>
          </tr></table>
        </div>

        <div align=center style=\"background-image:url('pattern-blue.gif'); opacity:1; padding-top:10px; padding-bottom:10px\">
          <table width=90% style='padding-left:10%; padding-right:10%'><tr>
            <td width=40%>
              <a href='https://medschool.duke.edu' target='_blank' style='color:white; padding-right:10px; text-decoration:none'>medschool.duke.edu</a>
              <a href='https://duke.edu' target='_blank' style='color:white; padding-right:10px; text-decoration:none'>duke.edu</a>
              <a href='https://www.dukehealth.org/' target='_blank' style='color:white; text-decoration:none'>dukehealth.org</a>
            </td>
            <td width=60%>
              <font color=#e0e0e0><small>© 2018 Duke University and Duke University Health System, All rights reserved</small></font>
            </td>
          </tr></table>
        </div>")

    )

  )
)

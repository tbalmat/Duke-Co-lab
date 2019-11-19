# Duke University Co-lab Shiny Workshop
# Generate example GWAS data sets

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

setwd("C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-4-plotly\\Data")

# Columns to generate:  rsID, phenotype, p (GWAS significance of regression)
# rsID, phenotype pairs are unique

nph1 <- 25
nph2 <- 15
nsnp <- 1000

# SNPs
snp <- paste("rs", sample(as.integer(seq(157493, 10035247, length.out=nsnp)), nsnp, replace=F, prob=1:nsnp), sep="")
psnp <- runif(nsnp)

# GWAS set 1
# Phenotypes
ph1a <- c("Large", "Medium", "Small", paste("Stage-", 1:4, sep=""), "Inferior", "Superior", "Medial", "Anterior", "Posterior", "Lateral", "Proximal", "Distal", "Medulaphysitc", "Uxtrunelior", "Culvintraterior")
ph2a <- c("Natriuretic", "Ablated", "Fractalized", "Intracranial", "Diminished", "Hyperbolic", "Metaboloscripted", "Demoliphased", "Redunderphasic", "Didactoresistant")
ph3a <- c("Semiquaver", "Convoluter", "Infarction", "Angiomatosis", "Syndrophrase", "Campylobacteriosis", "Morphogenesis", "Palmitoyltransferase", "Causalgia", "Tynophinitis")
ph1 <- unique(paste(sample(ph1a, nph1, replace=T), "-", sample(ph2a, nph1, replace=T), "-", sample(ph3a, nph1, replace=T), sep=""))

# GWAS set 2
# Generate phenotypes
ph1b <- c("ULQuadrant", "URQuadrant", "LLQuadrant", "LRQuadrant", paste("Mediophase-", 1:4, sep=""), "Penumbracentric", "Medial", "Orbitangential")
ph2b <- c("Quintoptic", "Narcissystemic", "Altabutistic", "Antonemic", "Ehadhamenic", "Quadratic", "Adriatic", "Rhomboid", "Occulated")
ph3b <- c("Croissantsanbutterphobia", "Anglotonophobia", "Ingridbergmania", "Rehabonotemptia", "Envirocogniphobia", "Postautumnophobia", "Dermafiberglassophobia", "Misplacia")
ph2 <- unique(paste(sample(ph1b, nph2, replace=T), "-", sample(ph2b, nph2, replace=T), "-", sample(ph3b, nph2, replace=T), sep=""))

p <- seq(1e-10, 1e-5, 5e-10)

# Combine phenotypes with SNPs by set, assign random p value
GWAS <- rbind(do.call(rbind,
                      apply(as.matrix(ph1), 1,
                        function(ph) {
                          n <- sample(1:50, 1, prob=exp(-(seq(0, 4, length.out=50)-2)**2)) 
                          data.frame("GWAS"=1,
                                     "phenotype"=ph,
                                     "SNP"=sample(snp, n, replace=F, prob=psnp),
                                     "p"=sample(p, n, replace=T, prob=p+1-p*p))
                        })),
              do.call(rbind,
                      apply(as.matrix(ph2), 1,
                        function(ph) {
                          n <- sample(1:50, 1, prob=exp(-(seq(0, 4, length.out=50)-2)**2)) 
                          data.frame("GWAS"=2,
                                     "phenotype"=ph,
                                     "SNP"=sample(snp, n, replace=F, prob=psnp),
                                     "p"=sample(p, n, replace=T, prob=p+1-p*p))
                        })))

write.table(GWAS, "GWASResults.csv", row.names=F, col.names=T, sep=",", quote=F)


# Compose highly aggregated CPDF data set to be used in examples for Oct 2019 Co-lab Shiny workshop
# Use Buzzfeed data since they are available on-line to the public
# Limit to fiscal years 1988 through 2011 for consistency with the SSRI Human Capital project

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)

library(RODBC)

db <- odbcDriverConnect(connection="driver={SQL Server}; server=ssri-swrk-pap36; database=HC_Dev; trusted_connection=true", readOnlyOptimize=T)

sql <- "
select a.FY,
       age.AgeRangeMid as Age,
       convert(tinyint, a.grade) as Grade,
       a.OccupationalCategory,
       case a.educationlevel
            when('01')then 8
            when('02')then 8
            when('03')then 10
            when('04')then 12
            when('05')then 12
            when('06')then 12
            when('07')then 13
            when('08')then 13
            when('09')then 14
            when('10')then 14
            when('11')then 15
            when('12')then 16
            when('13')then 16
            when('14')then 17
            when('15')then 19
            when('16')then 20
            when('17')then 18
            when('18')then 19
            when('19')then 18
            when('20')then 19
            when('21')then 20
            when('22')then 20
       end as EducationYears,
       a.AdjustedBasicPay*fy.CPIMultiplier as BasicPay
from   cpdfnondodstatusBuzzfeed a
       join fiscalyear fy on a.fy=fy.fy
       join agerange age on a.agerange=age.agerange
where  a.FY between 1988 and 2011
       and a.WorkSchedule='f' and a.payplan='gs' and a.grade between '01' and '15'
       and a.occupationalcategory in('p', 'a', 't', 'c', 'o')
       and a.educationlevel between '01' and '22' and a.adjustedbasicpay>10"

x <- sqlQuery(db, sql)

odbcClose(db)

nrow(x)

y <- aggregate(x[,"BasicPay"],
               by=list(x[,"FY"], x[,"Age"], x[,"Grade"], x[,"OccupationalCategory"], x[,"EducationYears"]),
               function(x) c("n"=length(x), "medBasicPay"=sum(x)))
y <- cbind(y, y[,"x"][,2])
y[,"x"] <-  y[,"x"][,1]
colnames(y) <- c("fy", "age", "grade", "occCat", "yearsEd", "n", "sumPay")

# Subset observations to 1988 through 2011 (consistent with the HC project)
setwd("C:\\Projects\\Duke\\Co-lab\\Shiny\\Session-1-NPDHist-Tables\\CourseMaterial")
cpdf <- subset(read.table("CPDFAggregateDataBuzzfeed.csv", header=T, sep=",", strip.white=T), fy>=1988 & fy <= 2011)
rownames(cpdf) <- NULL

# Synthesize agency
# Distribute observations into three separate observations, one for each of three agencies
# Probability of agency selection aligned with observed frequency in source OPM data
t <- proc.time()
x <- do.call(rbind,
             apply(as.matrix(1:nrow(cpdf)), 1,
                   function(i) {
                     if(cpdf[i,"occCat"]=="P") {
                       n <- rmultinom(1, cpdf[i,"n"], prob=c(0.5, 0.25, 0.25))
                     } else if(cpdf[i,"occCat"]=="A") {
                       n <- rmultinom(1, cpdf[i,"n"], prob=c(0.25, 0.5, 0.25))
                     } else if(cpdf[i,"occCat"]=="T") {
                       n <- rmultinom(1, cpdf[i,"n"], prob=c(0.25, 0.25, 0.5))
                     } else {
                       n <- rmultinom(1, cpdf[i,"n"], prob=c(1/3, 1/3, 1/3))
                     }
                     k <- which(n>0)
                     data.frame(cpdf[i,"fy"],
                                c("VA", "HHS", "AG")[k],
                                cpdf[i,"age"], cpdf[i,"grade"], cpdf[i,"occCat"], cpdf[i,"yearsEd"],
                                n[k], cpdf[i,"sumPay"]*(n[k]/cpdf[i,"n"]))
                   }))
colnames(x) <- c("fy", "agency", "age", "grade", "occCat", "yearsEd", "n", "sumPay")
proc.time()-t

write.table(x, "CPDFAggregateDataBuzzfeed-Agency.csv", row.names=F, col.names=T, sep=", ", quote=F)



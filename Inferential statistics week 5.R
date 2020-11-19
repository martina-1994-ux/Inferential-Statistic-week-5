###The initial phase consisted of loading the required packages and data.

library(ggplot2)
library(dplyr)
library(statsr)

load("gss.Rdata")
dim(gss)

#Subset GSS

df_study <- select(gss,year,region,consci) %>% na.omit() %>%
  mutate(negative=grepl("Hardly",consci)) %>%
  mutate(recent=as.factor(ifelse(year>=2006,"R","H")))

###Before performing inference, we can perform some exploratory data analysis (EDA) using both summary statistics/tables and visual plots.

###Looking at the entire historical series:

alltime <- df_study
summary(alltime)

alltime_table <- table(alltime$region,alltime$negative)
alltime_table

prop.table(alltime_table)


###The table do seem to indicate that there is a difference between regions: while the count table shows consistency on the magnitude of the 'positive' view of scientific community across the regions, using the proportion table highlights how they differ.

###It becomes easier to see this difference visually:


g <- ggplot(alltime) + aes(x=region,fill=negative) + geom_bar(position = "fill") +
  labs(x="Respondent's Region",y="Proportion",title="Impact of Region of Residence on Positive View of Scientific Community") +
  scale_fill_discrete(name="Opinion",labels=c("Positive","Negative"))
g

###This study also included a look at the recent data - defined as 2006 and beyond - since it could indicate broader societal changes that would be of interest.


since2006 <- filter(alltime,recent=="R")
summary(since2006)

since2006_table <- table(since2006$region,since2006$negative)
prop.table(since2006_table)

###Similar to the 'alltime' data, the recent data shows a clear preference for 'positive' message, with some variations between regions.

###Again, visually:

h <- ggplot(since2006) + aes(x=region,fill=negative) + geom_bar(position = "fill") + 
  labs(x="Respondent's Region",y="Proportion",title="Impact of Region of Residence on Positive View of Scientific Community") +
  scale_fill_discrete(name="Opinion",labels=c("Positive","Negative"))
h

###Finally, it is interesting to perform a comparison of both the historical dataset, as well as the recent one:

i <- ggplot(alltime) + aes(x=recent,fill=negative) + geom_bar(position = "fill") + facet_grid(.~region) +
  labs(x="Historical versus Recent",y="Proportion",title="Impact of Region of Residence on Positive View of Scientific Community") +
  scale_fill_discrete(name="Opinion",labels=c("Positive","Negative"))

i

###When looking at a comparison of both series - alltime/historical and recent - we see that there have been improvements in having a more positive view of the scientific community over the years in almost all regions.

###The visual plots alone indicate that there may be regional differences, but we need to perform actual inference to confirm that.

str(alltime)
str(since2006)

###Notice that both variables of interest - region and negative - have two or more levels (interpreting the logical TRUE/FALSE in 'negative' as levels).

###The chi square tests consists of calculating expected values assuming that the null hypothesis (H0) is true. This is done through the following calculation:

study_table1 <- table(alltime$region,alltime$negative)
study_table1

sum(study_table1<=5)

study_table2 <- table(since2006$region,since2006$negative)
study_table2

sum(study_table2<=5)

###As we can see, both tables have sufficient sample size for the chi square test of independence.

###At this point, we can conclude both conditions have been met.

###Finally, we're able to perform the inference calculation using the chi-square test.

c_alltime<-chisq.test(alltime$region,alltime$negative)
c_alltime

c_since2006<-chisq.test(since2006$region,since2006$negative)
c_since2006

###As can be seen above, the result from both series - alltime and since2006 - result in relatively high X-squared values and subsequently very low p-values.

###The high X-squared statistic in both cases - alltime and since2006 - with 8 degrees of freedom leads to very low p-values. Since the p-values are below alpha (0.05), we can conclude that there is sufficient evidence to reject H0 (null hypothesis).

###In the context of the research question, it mean that there is evidence that the confidence in scientific community varies by region.

###Ultimately, though, this result cannot be used to determine causality. This occurs because the GSS is an observational study, and not an experiment with randomized assignment to treatment.

###The practical result of this study is that outreach campaigns from the scientific community may increase their acceptance if they consider including confidence-building content when addressing different regions.